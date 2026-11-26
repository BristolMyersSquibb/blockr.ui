// @vitest-environment node

// What the build ships in inst/types, which a package's build resolves as `@blockr/ui`:
// the declarations of blockr.ui's components and of the build preset, each bundled,
// the entry of Lucide's icons, `@blockr/ui/icons`, the Tailwind half of the preset,
// and the package.json that names them, with React and React Aria as its peers and lucide as its dependency.
// How a package's stylesheet builds against the Tailwind half is in ./stylesheet.test.ts.

import { copyFile, cp, mkdir, mkdtemp, readdir, readFile, rm, symlink, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { dirname, join, relative, resolve } from "node:path";
import { fileURLToPath, pathToFileURL } from "node:url";
import * as esbuild from "esbuild";
import ts from "typescript";
import { afterEach, beforeEach, expect, test } from "vitest";
import type * as Build from "./index.ts";
import { iconNames, types, TYPES_PACKAGE } from "./build.ts";

const repo = resolve(dirname(fileURLToPath(import.meta.url)), "../..");

let root: string;

beforeEach(async () => {
  // The package sits under the repo, so that TypeScript finds Node's types in the repo's node_modules,
  // which tsconfig.node.json names.
  const scratch = join(repo, "node_modules", ".tmp");
  await mkdir(scratch, { recursive: true });
  root = await mkdtemp(join(scratch, "blockr-ui-types-"));
  await copyFile(join(repo, "tsconfig.client.json"), join(root, "tsconfig.client.json"));
  await copyFile(join(repo, "tsconfig.node.json"), join(root, "tsconfig.node.json"));
});

afterEach(async () => {
  await rm(root, { recursive: true, force: true });
});

async function write(dir: string, files: Record<string, string>): Promise<void> {
  for (const [path, content] of Object.entries(files)) {
    await mkdir(dirname(join(dir, path)), { recursive: true });
    await writeFile(join(dir, path), content);
  }
}

async function files(dir: string): Promise<string[]> {
  const entries = await readdir(dir, { recursive: true, withFileTypes: true });
  return entries
    .filter((entry) => entry.isFile())
    .map((entry) => relative(dir, join(entry.parentPath, entry.name)))
    .sort();
}

test("writes the declarations of the components and of the build preset, each bundled, the preset's stylesheet as it stands, and the package.json that names them", async () => {
  const presetCss = "@theme inline reference {\n  --spacing: var(--bk-spacing);\n}\n";
  await write(root, {
    "srcts/index.ts": "export function greet(name: string): string {\n  return `Hello, ${name}`;\n}\n",
    "srcts/build/index.ts": 'import { join } from "node:path";\n\nexport function where(dir: string): string {\n  return join(dir, "inst");\n}\n',
    "srcts/build/preset.css": presetCss,
    "inst/types/gone.d.ts": "",
  });

  const written = await types(root);

  expect(written).toEqual([
    "inst/types/build/index.d.ts",
    "inst/types/build/index.js",
    "inst/types/icons/index.d.ts",
    "inst/types/icons/index.js",
    "inst/types/index.d.ts",
    "inst/types/index.js",
    "inst/types/package.json",
    "inst/types/preset.css",
  ]);
  expect(await readFile(join(root, "inst/types/preset.css"), "utf8")).toBe(presetCss);
  expect(await files(join(root, "inst", "types"))).toEqual(written.map((file) => relative("inst/types", file)));
  expect(JSON.parse(await readFile(join(root, "inst/types/package.json"), "utf8"))).toEqual(TYPES_PACKAGE);
  expect(await readFile(join(root, "inst/types/index.d.ts"), "utf8")).toContain("export declare function greet(name: string): string;");

  // The bundle of the build preset leaves Node's own modules imports, since it runs in Node.
  const components = (await import(pathToFileURL(join(root, "inst/types/index.js")).href)) as { greet: (name: string) => string };
  const build = (await import(pathToFileURL(join(root, "inst/types/build/index.js")).href)) as { where: (dir: string) => string };
  expect(components.greet("you")).toBe("Hello, you");
  expect(build.where("pkg")).toBe(join("pkg", "inst"));
}, 60_000);

test("names React and React Aria's three packages as the peers of the package, at the versions that blockr.ui's files hold", async () => {
  const manifest = JSON.parse(await readFile(join(repo, "inst/types/package.json"), "utf8")) as typeof TYPES_PACKAGE;
  const versions = await Promise.all(
    ["react", "react-dom", "react-aria-components", "react-aria", "react-stately"].map(async (name) => {
      const own = JSON.parse(await readFile(join(repo, "node_modules", name, "package.json"), "utf8")) as { version: string };
      return [name, own.version];
    }),
  );

  expect(manifest.peerDependencies).toEqual(Object.fromEntries(versions));
});

test("names lucide and @internationalized/date as the dependencies of the package, at the versions that blockr.ui pins", async () => {
  const manifest = JSON.parse(await readFile(join(repo, "inst/types/package.json"), "utf8")) as typeof TYPES_PACKAGE;
  const own = JSON.parse(await readFile(join(repo, "package.json"), "utf8")) as { dependencies: Record<string, string> };
  const names = ["@internationalized/date", "lucide"];
  const installed = await Promise.all(
    names.map(async (name) => (JSON.parse(await readFile(join(repo, "node_modules", name, "package.json"), "utf8")) as { version: string }).version),
  );

  expect(manifest.dependencies).toEqual(Object.fromEntries(names.map((name) => [name, own.dependencies[name]])));
  expect(installed).toEqual(names.map((name) => own.dependencies[name]));
});

test("offers each icon of lucide as @blockr/ui/icons, under the names that lucide exports, with their aliases, but neither `icons` nor its functions", async () => {
  const lucide = (await import("lucide")) as Record<string, unknown>;
  const entry = (await import(pathToFileURL(join(repo, "inst/types/icons/index.js")).href)) as Record<string, unknown>;
  const names = await iconNames();

  // The names that lucide's declarations export.
  // Where they export a value under another name, as for an alias, the name is the one after `as`.
  const declared = await readFile(join(repo, "node_modules/lucide/dist/lucide.d.ts"), "utf8");
  const exported = (/^export \{ (.*) \};$/m.exec(declared)?.[1] ?? "").split(", ").map((name) => name.split(" as ").at(-1) ?? "");
  expect(names).toEqual(exported.filter((name) => !["createElement", "createIcons", "icons"].includes(name)).sort());
  expect(names.length).toBeGreaterThan(2000);

  expect(Object.keys(entry).sort()).toEqual(names);
  for (const name of names) expect(entry[name]).toBe(lucide[name]);
  // An alias, which lucide keeps for the old name of an icon that it renamed, names the same shape.
  expect(entry.TriangleAlert).toBe(entry.TriangleExclamationPoint);
  for (const name of ["icons", "createIcons", "createElement"]) {
    expect(lucide).toHaveProperty(name);
    expect(entry).not.toHaveProperty(name);
  }

  // The declarations name the same icons, and add the type IconNode.
  const declarations = await readFile(join(repo, "inst/types/icons/index.d.ts"), "utf8");
  const listed = /^export \{\n([^}]*)\} from "lucide";$/m.exec(declarations)?.[1] ?? "";
  expect(listed.split(",\n").map((name) => name.trim()).filter((name) => name !== "")).toEqual(names);
  expect(declarations).toContain('export type { IconNode } from "lucide";');
});

test("refuses types that do not check", async () => {
  await write(root, { "srcts/index.ts": "export const n: number = 'one';\n" });

  await expect(types(root)).rejects.toThrow("Type 'string' is not assignable to type 'number'.");
}, 60_000);

// A package's module, which draws with React and React Aria, and imports blockr.ui's components.
const view = `import * as ui from "@blockr/ui";
import { useState } from "react";
import { Button } from "react-aria-components";

export const components: object = ui;

export default function Count() {
  const [count, setCount] = useState(0);
  return <Button onPress={() => { setCount(count + 1); }}>{count}</Button>;
}
`;

// A package's module that draws icons of Lucide's, with blockr.ui's Icon, and writes one as markup.
const pinned = `import { Icon, iconMarkup } from "@blockr/ui";
import { Pin, TriangleAlert } from "@blockr/ui/icons";
import type { IconNode } from "@blockr/ui/icons";

const shapes: IconNode[] = [Pin, TriangleAlert];

export const markup: string[] = shapes.map((shape) => iconMarkup(shape, 16));

export default function Pinned() {
  return <Icon icon={Pin} size={16} label="Pinned" />;
}
`;

// A package's build, which bundles the module with the preset and the vendor check,
// and lists what each file imports up front.
const build = `import * as esbuild from "esbuild";
import { preset, staticImports, vendorCheck } from "@blockr/ui/build";

const result = await esbuild.build({
  ...preset,
  plugins: [...preset.plugins, vendorCheck],
  entryPoints: ["src/count.tsx"],
  outdir: "inst/js",
  metafile: true,
});

export const imported: Record<string, string[]> = staticImports([result.metafile]);
`;

// Installs blockr.ui's types as a package installs them, with `npm install --no-save --install-links`:
// as a copy, outside blockr.ui's repo.
// TypeScript resolves a file's imports from the file's real path,
// so the types find React and React Aria in the package's node_modules.
// With them come React and React Aria's three packages, which npm installs as their peers,
// lucide, which npm installs as their dependency,
// and esbuild and React's types, which a package that builds with the preset installs itself.
async function installTypes(dir: string): Promise<void> {
  await cp(join(repo, "inst", "types"), join(dir, "node_modules", "@blockr", "ui"), { recursive: true });
  for (const name of ["react", "react-dom", "react-aria-components", "react-aria", "react-stately", "lucide", "esbuild", "@types/react", "@types/react-dom"]) {
    await mkdir(dirname(join(dir, "node_modules", name)), { recursive: true });
    await symlink(join(repo, "node_modules", name), join(dir, "node_modules", name), "dir");
  }
}

// What TypeScript reports for the files of a package's src, as the resolution given finds their imports.
function typeCheck(pkg: string, files: readonly string[], resolution: "bundler" | "nodenext"): string[] {
  const program = ts.createProgram(
    files.map((file) => join(pkg, "src", file)),
    {
      target: ts.ScriptTarget.ES2022,
      lib: ["lib.es2022.d.ts", "lib.dom.d.ts"],
      module: resolution === "bundler" ? ts.ModuleKind.Preserve : ts.ModuleKind.NodeNext,
      moduleResolution: resolution === "bundler" ? ts.ModuleResolutionKind.Bundler : ts.ModuleResolutionKind.NodeNext,
      jsx: ts.JsxEmit.ReactJSX,
      strict: true,
      verbatimModuleSyntax: true,
      allowImportingTsExtensions: true,
      noEmit: true,
      skipLibCheck: false,
      types: [],
    },
  );
  return ts.getPreEmitDiagnostics(program).map((diagnostic) => ts.flattenDiagnosticMessageText(diagnostic.messageText, "\n"));
}

test.each(["bundler", "nodenext"] as const)(
  "a package's modules and its build type-check against what blockr.ui ships, as %s resolution finds it",
  async (resolution) => {
    const pkg = await mkdtemp(join(tmpdir(), "blockr-ui-package-"));
    try {
      await installTypes(pkg);
      await write(pkg, {
        "package.json": JSON.stringify({ name: "blockr.test", private: true, type: "module" }),
        "src/count.tsx": view,
        "src/pinned.tsx": pinned,
        "src/build.ts": build,
      });

      expect(typeCheck(pkg, ["count.tsx", "pinned.tsx", "build.ts"], resolution)).toEqual([]);
    } finally {
      await rm(pkg, { recursive: true, force: true });
    }
  },
  60_000,
);

test("TypeScript refuses a package's import of `icons` or of lucide's functions from @blockr/ui/icons", async () => {
  const pkg = await mkdtemp(join(tmpdir(), "blockr-ui-package-"));
  try {
    await installTypes(pkg);
    await write(pkg, {
      "package.json": JSON.stringify({ name: "blockr.test", private: true, type: "module" }),
      "src/all.ts": 'import { createElement, createIcons, icons } from "@blockr/ui/icons";\n\nexport default [createElement, createIcons, icons];\n',
    });

    // TypeScript reports each name that the entry lacks, and suggests an icon for `icons`, as Coins.
    const reported = typeCheck(pkg, ["all.ts"], "bundler");
    expect(reported).toHaveLength(3);
    expect(reported.map((text) => /^.*"@blockr\/ui\/icons"' has no exported member(?: named)? '(\w+)'/.exec(text)?.[1]).sort()).toEqual([
      "createElement",
      "createIcons",
      "icons",
    ]);
  } finally {
    await rm(pkg, { recursive: true, force: true });
  }
}, 60_000);

test("a package's build bundles its module with the preset of its copy of blockr.ui's types, leaving React, React Aria and the components to the import map", async () => {
  const pkg = await mkdtemp(join(tmpdir(), "blockr-ui-package-"));
  try {
    await installTypes(pkg);
    await write(pkg, { "src/count.tsx": view });

    const { preset, staticImports, vendorCheck } = (await import(
      pathToFileURL(join(pkg, "node_modules/@blockr/ui/build/index.js")).href
    )) as typeof Build;
    const result = await esbuild.build({
      ...preset,
      plugins: [...preset.plugins, vendorCheck],
      absWorkingDir: pkg,
      entryPoints: ["src/count.tsx"],
      outdir: "inst/js",
      metafile: true,
      logLevel: "silent",
    });

    expect(staticImports([result.metafile])["count.js"]?.sort()).toEqual(["@blockr/ui", "react", "react-aria-components", "react/jsx-runtime"]);
  } finally {
    await rm(pkg, { recursive: true, force: true });
  }
});

test("a package's build bundles into its vendor file the icons that the package imports from @blockr/ui/icons, and no others", async () => {
  const pkg = await mkdtemp(join(tmpdir(), "blockr-ui-package-"));
  try {
    await installTypes(pkg);

    // A package's build writes its vendor file with the preset, minified,
    // from lines that export what the package's modules import.
    const { preset } = (await import(pathToFileURL(join(pkg, "node_modules/@blockr/ui/build/index.js")).href)) as typeof Build;
    const result = await esbuild.build({
      ...preset,
      absWorkingDir: pkg,
      stdin: { contents: 'export { Pin, TriangleAlert } from "@blockr/ui/icons";\n', resolveDir: pkg, loader: "js" },
      write: false,
      minify: true,
      metafile: true,
      logLevel: "silent",
    });

    // The file holds the code of the two icons' modules in lucide alone, and imports nothing.
    const [output] = Object.values(result.metafile.outputs);
    const held = Object.entries(output?.inputs ?? {}).filter(([, { bytesInOutput }]) => bytesInOutput > 0);
    expect(held.map(([input]) => relative(join(repo, "node_modules"), resolve(pkg, input))).sort()).toEqual([
      "lucide/dist/esm/icons/pin.mjs",
      "lucide/dist/esm/icons/triangle-exclamation-point.mjs",
    ]);
    expect(output?.imports).toEqual([]);
    expect(output?.exports.sort()).toEqual(["Pin", "TriangleAlert"]);
    expect(result.outputFiles[0]?.text.length).toBeLessThan(2_000);
  } finally {
    await rm(pkg, { recursive: true, force: true });
  }
});

test("the vendor check fails a package's build that puts icons of @blockr/ui/icons into one of its own files, naming the entry", async () => {
  const pkg = await mkdtemp(join(tmpdir(), "blockr-ui-package-"));
  try {
    await installTypes(pkg);
    await write(pkg, { "src/pinned.tsx": pinned });

    const { preset, vendorCheck } = (await import(pathToFileURL(join(pkg, "node_modules/@blockr/ui/build/index.js")).href)) as typeof Build;
    const failure = await esbuild
      .build({
        ...preset,
        plugins: [...preset.plugins, vendorCheck],
        absWorkingDir: pkg,
        entryPoints: ["src/pinned.tsx"],
        outdir: "inst/js",
        logLevel: "silent",
      })
      .then(
        () => undefined,
        (error: unknown) => error as esbuild.BuildFailure,
      );

    expect(failure?.errors.map(({ text }) => text)).toEqual([
      "The build put icons of @blockr/ui/icons into inst/js/pinned.js. Bundle the icons that the package imports into its vendor file instead, and import them from there.",
    ]);
  } finally {
    await rm(pkg, { recursive: true, force: true });
  }
});
