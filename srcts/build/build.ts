// Builds blockr.ui's client into inst/js, which the R package ships and git keeps,
// since R CMD check and CRAN run no Node.
// Run: npm run build
// It writes the files that the design gives
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.7.4#the-bundle):
//
// - React's file, react-<hash>.js, holds React and ReactDOM, minified.
//   React's entry points, such as `react` and `react-dom/client`,
//   are written as ES modules of their own, such as react.js and react-dom-client.js,
//   which read React from that file.
//   So all of them read one React.
// - React Aria's files hold the whole of its three packages, minified:
//   react-aria-components.js, react-aria.js and react-stately.js, each bundled as an entry of its own.
//   The code that they share goes into files of its own, such as react-aria-3f2a9c1d.js,
//   which esbuild splits off.
//   They leave their imports of React's entry points imports, so that they run on React's file.
// - The file of blockr.ui's components, blockr-ui.js, holds blockr.ui's own code, unminified,
//   from srcts/index.ts.
//   The build preset of ./preset.ts bundles it, as it bundles a package's modules,
//   so the file leaves its imports of React's entry points and of React Aria's packages imports.
//   The vendor check of ./vendor-check.ts fails the build,
//   where the file would bundle a package from node_modules.
// - The entries of the page's import map go into import-map.json,
//   each import by the file in inst/js that it resolves to.
//   R puts them into the import map in the head of the page that it serves.
// - What each file imports up front, the static imports of ./preload.ts, goes into preload.json.
//   R names those files in the page's head,
//   so that the browser fetches them at once,
//   rather than each once the file that imports it has loaded.
// - The licenses of what the files in inst/js hold from other projects go into LICENSES beside them.
//   It has a line for each package from node_modules, with its version,
//   its author where its package.json names one, and the files that hold its code.
//   The text of a license follows the lines that share it.
//   The build fails for a package that npm installs without a license file,
//   so that no code ships without its license.
// - The components, srcts/index.ts, and the build preset, srcts/build, go into inst/types,
//   which a package's build resolves as `@blockr/ui` and `@blockr/ui/build`:
//   their declarations, each bundled, and the package.json that names them.
//
// The build owns inst/js and inst/types and empties them first,
// so that nothing is left of a file that is gone.

import { execFileSync } from "node:child_process";
import { createHash } from "node:crypto";
import { existsSync } from "node:fs";
import { mkdir, readdir, readFile, rm, writeFile } from "node:fs/promises";
import { dirname, join, relative, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import * as esbuild from "esbuild";
import ts from "typescript";
import { COMPONENTS, IMPORTS, importFile, REACT_ARIA, REACT_ENTRIES } from "../import-map/index.ts";
import { staticImports } from "./preload.ts";
import { installedVersion, leaveReact, preset, VERSIONS } from "./preset.ts";
import { vendorCheck } from "./vendor-check.ts";

// The repo of blockr.ui,
// whose node_modules holds the React and the React Aria that the build writes into inst/js.
const repo = resolve(dirname(fileURLToPath(import.meta.url)), "../..");

// The options of esbuild for the code that blockr.ui ships from other projects:
// resolved from blockr.ui's repo, for the browsers that the preset builds for,
// minified, and with its license comments.
function vendorOptions(out: string) {
  return {
    absWorkingDir: repo,
    outdir: out,
    bundle: true,
    format: "esm",
    platform: "browser",
    target: preset.target,
    define: preset.define,
    minify: true,
    legalComments: "inline",
    metafile: true,
    logLevel: "warning",
  } satisfies esbuild.BuildOptions;
}

// Fails where esbuild warned, which it prints.
function failOnWarnings(result: esbuild.BuildResult): void {
  if (result.warnings.length > 0) throw new Error("esbuild warned, as printed above");
}

// The files that a build wrote, by their paths.
// The metafile gives each from `dir`, the working directory of the build.
function outputs(metafile: esbuild.Metafile, dir: string): string[] {
  return Object.keys(metafile.outputs).map((file) => resolve(dir, file));
}

// Fails where the node_modules of `dir`, as blockr.ui's repo,
// hold another version of React Aria's packages than VERSIONS names.
// The build writes React Aria from there into inst/js,
// and the preset checks every package's build against VERSIONS, so the two must agree.
export function checkVersions(dir: string): void {
  const wrong = Object.entries(VERSIONS).flatMap(([name, version]) => {
    const installed = installedVersion(name, dir);
    if (installed === version) return [];
    const held = installed === undefined ? `no ${name}` : `${name} ${installed}`;
    return [`${held}, where VERSIONS names ${version}`];
  });
  if (wrong.length > 0) {
    throw new Error(
      `The node_modules of blockr.ui hold ${listing(wrong)}. Install the versions that VERSIONS in srcts/build/preset.ts names, or change VERSIONS together with the pins in package.json.`,
    );
  }
}

// Bundles React and ReactDOM, minified, into React's file in `out`, as react-3f2a9c1d.js,
// and returns its name and the directories of the packages that it holds.
// The name holds a hash of the content, so that no browser runs an old copy from its cache,
// since React's entry points import the file by that name.
// The file exports each of React's entry points as a namespace of its own,
// entry0 for `react` and so on, which the ES modules of reactModules() read.
async function reactFile(out: string): Promise<{ file: string; sources: string[] }> {
  const result = await esbuild.build({
    ...vendorOptions(out),
    write: false,
    stdin: {
      contents: REACT_ENTRIES.map((name, i) => `export * as entry${String(i)} from ${JSON.stringify(name)};`).join("\n"),
      resolveDir: repo,
      loader: "js",
    },
  });
  failOnWarnings(result);

  const text = result.outputFiles[0]?.text ?? "";
  const file = `react-${createHash("sha256").update(text).digest("hex").slice(0, 8)}.js`;
  await writeFile(join(out, file), text);
  return { file, sources: [...licensedSources(result.metafile, repo).values()].flat() };
}

// The names that each of React's entry points exports in production,
// which its ES module exports as well.
// A child process requires them,
// since this process may hold React's development build, which exports more.
function reactNames(): Record<string, string[]> {
  const script = `console.log(JSON.stringify(Object.fromEntries(${JSON.stringify(REACT_ENTRIES)}.map((name) => [name, Object.keys(require(name))]))));`;
  const out = execFileSync(process.execPath, ["-e", script], {
    cwd: repo,
    env: { ...process.env, NODE_ENV: "production" },
    encoding: "utf8",
  });
  return JSON.parse(out) as Record<string, string[]>;
}

// Writes React's entry points into `out` as ES modules of their own,
// each exporting the names that the entry point exports, from React's file, `bundle`,
// so that all of them read one React.
async function reactModules(out: string, bundle: string): Promise<string[]> {
  const names = reactNames();
  const written: string[] = [];
  for (const [i, name] of REACT_ENTRIES.entries()) {
    const exported = (names[name] ?? []).filter((key) => key !== "default" && /^[A-Za-z_$][\w$]*$/.test(key));
    const file = importFile(name);
    await writeFile(
      join(out, file),
      [
        `// ${name}, as an ES module, from the one copy of React that blockr.ui ships.`,
        `import { entry${String(i)} as m } from "./${bundle}";`,
        "export default m.default;",
        `export const { ${exported.join(", ")} } = m;`,
        "",
      ].join("\n"),
    );
    written.push(file);
  }
  return written;
}

// Bundles the whole of React Aria's three packages, minified, into `out`,
// each as an entry of its own, named as the import map names it, such as react-aria.js.
// The code that the three share goes into files of its own, such as react-aria-3f2a9c1d.js.
// Their names hold a hash of the content, since the entries import them by those names.
// The files leave their imports of React's entry points imports,
// through the preset's handling of React alone.
// React Aria's own code imports the entry points inside its packages,
// which the preset refuses in a module.
async function reactAria(out: string): Promise<esbuild.Metafile> {
  const result = await esbuild.build({
    ...vendorOptions(out),
    entryPoints: REACT_ARIA.map((name) => ({ in: name, out: name })),
    splitting: true,
    chunkNames: "react-aria-[hash]",
    plugins: [{ name: "blockr.ui-react-imports", setup: leaveReact }],
  });
  failOnWarnings(result);
  return result.metafile;
}

// Bundles blockr.ui's components, srcts/index.ts, into blockr-ui.js in `out`, unminified,
// with the build preset and the vendor check, as a package bundles its modules.
// It writes nothing where srcts holds no such file.
async function components(root: string, out: string): Promise<esbuild.Metafile | undefined> {
  const entry = join(root, "srcts", "index.ts");
  if (!existsSync(entry)) return undefined;
  const result = await esbuild.build({
    ...preset,
    plugins: [...preset.plugins, vendorCheck],
    absWorkingDir: root,
    outdir: out,
    entryPoints: [{ in: entry, out: importFile(COMPONENTS).replace(/\.js$/, "") }],
    metafile: true,
    logLevel: "warning",
  });
  failOnWarnings(result);
  return result.metafile;
}

export async function build(root: string): Promise<string[]> {
  const out = join(root, "inst", "js");

  checkVersions(repo);
  await rm(out, { recursive: true, force: true });
  await mkdir(out, { recursive: true });

  const own = await components(root, out);
  const react = await reactFile(out);
  const modules = await reactModules(out, react.file);
  const aria = await reactAria(out);
  const metafiles = [aria, ...(own === undefined ? [] : [own])];

  const written = [
    ...[react.file, ...modules].map((file) => join(out, file)),
    ...outputs(aria, repo),
    ...(own === undefined ? [] : outputs(own, root)),
  ].map((file) => relative(root, file));

  // The entries of the import map that the page R serves holds, as R reads them:
  // each import by the file in inst/js that it resolves to.
  // R serves the files that these import by their names, such as React's file, as files beside them.
  const imports = Object.fromEntries(
    IMPORTS.filter((name) => existsSync(join(out, importFile(name)))).map((name) => [name, importFile(name)]),
  );
  const map = join(out, "import-map.json");
  await writeJson(map, { imports });
  written.push(relative(root, map));

  // What each file imports up front, React's entry points included, which import React's file.
  const preload = join(out, "preload.json");
  const imported: Record<string, readonly string[]> = {
    ...staticImports(metafiles),
    ...Object.fromEntries(modules.map((file) => [file, [`./${react.file}`]])),
  };
  await writeJson(preload, Object.fromEntries(Object.entries(imported).sort(([a], [b]) => (a < b ? -1 : a > b ? 1 : 0))));
  written.push(relative(root, preload));

  // What each file holds from other projects, by the file's path.
  const bundled = new Map([
    [join(out, react.file), react.sources],
    ...licensedSources(aria, repo),
    ...(own === undefined ? [] : licensedSources(own, root)),
  ]);
  written.push(...(await licenses(root, out, bundled)));
  return written.sort();
}

// What each file of a build holds from other projects, by the file's path:
// the directory of each package from node_modules whose code it holds.
// The metafile gives each path from `dir`, the working directory of the build.
// An input whose code the build left out counts for none,
// such as the empty module of client-only, which React Aria imports.
function licensedSources(metafile: esbuild.Metafile, dir: string): Map<string, string[]> {
  const bundled = new Map<string, string[]>();
  for (const [output, { inputs }] of Object.entries(metafile.outputs)) {
    const sources = new Set(
      Object.entries(inputs).flatMap(([input, { bytesInOutput }]) => {
        const found = /^(.*node_modules\/(?:@[^/]+\/)?[^/]+)\//.exec(input)?.[1];
        return found === undefined || bytesInOutput === 0 ? [] : [resolve(dir, found)];
      }),
    );
    if (sources.size > 0) bundled.set(resolve(dir, output), [...sources]);
  }
  return bundled;
}

// The parts of a package's package.json that LICENSES names.
interface Manifest {
  name: string;
  version: string;
  author?: string | { name?: string };
}

// The text of each license file in a package's directory, such as LICENSE or LICENSE.md,
// or none where the directory holds none.
async function licenseTexts(dir: string): Promise<string[]> {
  const files = (await readdir(dir)).filter((file) => /^(licen[cs]e|copying)/i.test(file)).sort();
  return Promise.all(files.map(async (file) => (await readFile(join(dir, file), "utf8")).replace(/\r\n/g, "\n").trimEnd()));
}

// The author that a package's package.json names, without an email or a URL.
function authorName(manifest: Manifest): string | undefined {
  const name = typeof manifest.author === "string" ? manifest.author.replace(/\s*[<(].*$/, "") : manifest.author?.name;
  return name === undefined || name === "" ? undefined : name;
}

// A list in prose: "a", "a and b", "a, b and c".
function listing(items: readonly string[]): string {
  if (items.length <= 1) return items.join("");
  return `${items.slice(0, -1).join(", ")} and ${items.at(-1) ?? ""}`;
}

// Writes the licenses of what the files in `dir` hold from other projects into `dir`, as LICENSES,
// and returns its path from `root`.
// For each file, `bundled` gives the directories of the packages from node_modules that it holds,
// by the file's path.
// It fails for a package whose directory holds no license file.
// Where the files hold nothing from other projects, it writes nothing.
export async function licenses(
  root: string,
  dir: string,
  bundled: ReadonlyMap<string, readonly string[]>,
): Promise<string[]> {
  const holders = new Map<string, string[]>();
  for (const [file, sources] of bundled) {
    for (const source of sources) holders.set(source, [...(holders.get(source) ?? []), relative(dir, file)]);
  }
  if (holders.size === 0) return [];

  const entries = await Promise.all(
    [...holders].map(async ([source, files]) => {
      const held = listing(files.sort());
      const manifest = JSON.parse(await readFile(join(source, "package.json"), "utf8")) as Manifest;
      const texts = await licenseTexts(source);
      if (texts.length === 0) {
        throw new Error(`The code of ${manifest.name} in ${held} comes with no license: its directory in node_modules holds no license file.`);
      }
      const author = authorName(manifest);
      return {
        line: `${manifest.name} ${manifest.version}${author === undefined ? "" : `, by ${author}`}, in ${held}`,
        text: texts.join("\n\n"),
      };
    }),
  );

  // Each license once, after the lines of the packages that share it,
  // in the order of the packages' names.
  const byLicense = new Map<string, string[]>();
  for (const { line, text } of entries.sort((a, b) => (a.line < b.line ? -1 : 1))) {
    byLicense.set(text, [...(byLicense.get(text) ?? []), line]);
  }
  const file = join(dir, "LICENSES");
  await writeFile(
    file,
    [...byLicense].map(([text, lines]) => `${lines.join("\n")}\n\n${text}\n`).join(`\n${"-".repeat(78)}\n\n`),
  );
  return [relative(root, file)];
}

async function writeJson(file: string, value: unknown): Promise<void> {
  await writeFile(file, `${JSON.stringify(value, null, 2)}\n`);
}

// The version of a package that blockr.ui's build resolves,
// from the package.json in blockr.ui's node_modules.
function packageVersion(name: string): string {
  const version = installedVersion(name, repo);
  if (version === undefined) throw new Error(`The node_modules of blockr.ui hold no ${name}.`);
  return version;
}

// The package that inst/types is: blockr.ui's components, as `@blockr/ui`,
// and the build preset, as `@blockr/ui/build`, each with its declarations and bundled.
// The components bundle leaves React and React Aria imports, as blockr-ui.js does.
// So the package names React and React Aria's three packages as its peers,
// at the versions that blockr.ui's files hold.
// Then npm installs them with `@blockr/ui` where a package has none,
// and refuses a package that asks for other versions.
// A package's tests then run blockr.ui's components on one copy of React and of React Aria,
// the copy that its own code imports as well.
// A package's build leaves its imports of all of them to the page's import map, through the preset.
export const TYPES_PACKAGE = {
  name: "@blockr/ui",
  private: true,
  description:
    "The components of blockr.ui's client, and the build preset with which a package builds its modules against them, as the R package blockr.ui ships them.",
  type: "module",
  exports: {
    ".": { types: "./index.d.ts", default: "./index.js" },
    "./build": { types: "./build/index.d.ts", default: "./build/index.js" },
  },
  peerDependencies: {
    react: packageVersion("react"),
    "react-dom": packageVersion("react-dom"),
    ...Object.fromEntries(REACT_ARIA.map((name) => [name, VERSIONS[name]])),
  },
};

// The sources in srcts that the types package bundles, each as the entry that it names:
// the components as `@blockr/ui` and the build preset as `@blockr/ui/build`.
const BUNDLED = [
  { name: "index", source: "index.ts" },
  { name: "build/index", source: join("build", "index.ts") },
];

// Writes inst/types: the declarations of the sources that BUNDLED names,
// each file's beside where its source sits in srcts, those sources bundled,
// and the package.json that names them.
// The declarations follow tsconfig.node.json, since the build preset runs in Node.
export async function types(root: string): Promise<string[]> {
  const src = join(root, "srcts");
  const bundled = BUNDLED.map(({ name, source }) => ({ name, file: join(src, source) })).filter(({ file }) => existsSync(file));
  if (bundled.length === 0) return [];

  const dir = join(root, "inst", "types");
  await rm(dir, { recursive: true, force: true });
  await mkdir(dir, { recursive: true });

  const config = ts.readConfigFile(join(root, "tsconfig.node.json"), (file) => ts.sys.readFile(file));
  const { options } = ts.parseJsonConfigFileContent(config.config, ts.sys, root);
  const program = ts.createProgram(
    bundled.map(({ file }) => file),
    {
      ...options,
      noEmit: false,
      declaration: true,
      emitDeclarationOnly: true,
      rootDir: src,
      outDir: dir,
      incremental: false,
      tsBuildInfoFile: undefined,
    },
  );

  const written: string[] = [];
  const emitted = program.emit(undefined, (file, text) => {
    ts.sys.writeFile(file, text);
    written.push(relative(root, file));
  });
  const problems = ts.getPreEmitDiagnostics(program).concat(emitted.diagnostics);
  if (problems.length > 0) {
    throw new Error(
      ts.formatDiagnostics(problems, {
        getCanonicalFileName: (file) => file,
        getCurrentDirectory: () => root,
        getNewLine: () => "\n",
      }),
    );
  }

  const peers = Object.keys(TYPES_PACKAGE.peerDependencies);
  const result = await esbuild.build({
    absWorkingDir: root,
    entryPoints: bundled.map(({ name, file }) => ({ in: file, out: name })),
    outdir: dir,
    bundle: true,
    format: "esm",
    platform: "neutral",
    target: "es2022",
    jsx: "automatic",
    // The peers that the package.json names stay imports, with their subpaths, such as react/jsx-runtime.
    // So do Node's own modules, which the build preset reads.
    external: [...peers, ...peers.map((name) => `${name}/*`), "node:*"],
    logLevel: "warning",
    metafile: true,
  });
  failOnWarnings(result);
  written.push(...outputs(result.metafile, root).map((file) => relative(root, file)));
  written.push(...(await licenses(root, dir, licensedSources(result.metafile, root))));

  const manifest = join(dir, "package.json");
  await writeJson(manifest, TYPES_PACKAGE);
  written.push(relative(root, manifest));

  return written.sort();
}

if (process.argv[1] === fileURLToPath(import.meta.url)) {
  for (const file of [...(await build(repo)), ...(await types(repo))]) {
    console.log(file);
  }
}
