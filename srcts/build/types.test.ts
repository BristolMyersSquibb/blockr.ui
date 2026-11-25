// @vitest-environment node

// What the build ships in inst/types, which a package's build resolves as `@blockr/ui`:
// the declarations of blockr.ui's components and of the build preset, each bundled,
// and the package.json that names them, with React and React Aria as its peers.

import { copyFile, cp, mkdir, mkdtemp, readdir, readFile, rm, symlink, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { dirname, join, relative, resolve } from "node:path";
import { fileURLToPath, pathToFileURL } from "node:url";
import * as esbuild from "esbuild";
import ts from "typescript";
import { afterEach, beforeEach, expect, test } from "vitest";
import type * as Build from "./index.ts";
import { types, TYPES_PACKAGE } from "./build.ts";

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

test("writes the declarations of the components and of the build preset, each bundled, and the package.json that names them", async () => {
  await write(root, {
    "srcts/index.ts": "export function greet(name: string): string {\n  return `Hello, ${name}`;\n}\n",
    "srcts/build/index.ts": 'import { join } from "node:path";\n\nexport function where(dir: string): string {\n  return join(dir, "inst");\n}\n',
    "inst/types/gone.d.ts": "",
  });

  const written = await types(root);

  expect(written).toEqual([
    "inst/types/build/index.d.ts",
    "inst/types/build/index.js",
    "inst/types/index.d.ts",
    "inst/types/index.js",
    "inst/types/package.json",
  ]);
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
// and esbuild and React's types, which a package that builds with the preset installs itself.
async function installTypes(dir: string): Promise<void> {
  await cp(join(repo, "inst", "types"), join(dir, "node_modules", "@blockr", "ui"), { recursive: true });
  for (const name of ["react", "react-dom", "react-aria-components", "react-aria", "react-stately", "esbuild", "@types/react", "@types/react-dom"]) {
    await mkdir(dirname(join(dir, "node_modules", name)), { recursive: true });
    await symlink(join(repo, "node_modules", name), join(dir, "node_modules", name), "dir");
  }
}

test.each(["bundler", "nodenext"] as const)(
  "a package's module and its build type-check against what blockr.ui ships, as %s resolution finds it",
  async (resolution) => {
    const pkg = await mkdtemp(join(tmpdir(), "blockr-ui-package-"));
    try {
      await installTypes(pkg);
      await write(pkg, {
        "package.json": JSON.stringify({ name: "blockr.test", private: true, type: "module" }),
        "src/count.tsx": view,
        "src/build.ts": build,
      });

      const program = ts.createProgram(
        ["count.tsx", "build.ts"].map((file) => join(pkg, "src", file)),
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

      const diagnostics = ts.getPreEmitDiagnostics(program).map((diagnostic) => ts.flattenDiagnosticMessageText(diagnostic.messageText, "\n"));
      expect(diagnostics).toEqual([]);
    } finally {
      await rm(pkg, { recursive: true, force: true });
    }
  },
  60_000,
);

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
