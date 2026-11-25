// @vitest-environment node

// What blockr.ui's build writes into inst/js: React's file and React's entry points,
// React Aria's files, the file of blockr.ui's components, the entries of the page's import map,
// what each file imports up front, and the licenses of what the files hold from other projects.
// The tests build a package of their own, whose srcts holds the components.

import { createRequire } from "node:module";
import { mkdir, mkdtemp, readdir, readFile, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath, pathToFileURL } from "node:url";
import { afterAll, beforeAll, expect, test } from "vitest";
import { importFile, IMPORTS, REACT_ARIA, REACT_ENTRIES } from "../import-map/index.ts";
import { build, checkVersions, licenses } from "./build.ts";
import { VERSIONS } from "./preset.ts";

const repo = resolve(dirname(fileURLToPath(import.meta.url)), "../..");

async function write(root: string, files: Record<string, string>): Promise<void> {
  for (const [path, content] of Object.entries(files)) {
    await mkdir(dirname(join(root, path)), { recursive: true });
    await writeFile(join(root, path), content);
  }
}

// The packages that the code of a file imports up front, by the specifier of each.
function importedPackages(code: string): string[] {
  return [...new Set([...code.matchAll(/(?:^|[;}])\s*(?:import|export)\b[^;]*?\bfrom\s*"([^"]+)"/gm)].map((m) => m[1] ?? ""))].sort();
}

// Whether code is minified, which puts it on lines of thousands of characters.
// Unminified code takes a line for each statement.
function minified(code: string): boolean {
  return code.length / code.split("\n").length > 500;
}

// The package that the tests build, and what the build wrote.
// It sits under the repo,
// so that Node resolves the imports of React and of React Aria in its files to the repo's node_modules,
// as the tests load them.
let root: string;
let out: string;
let written: string[];

beforeAll(async () => {
  const scratch = join(repo, "node_modules", ".tmp");
  await mkdir(scratch, { recursive: true });
  root = await mkdtemp(join(scratch, "blockr-ui-build-"));
  out = join(root, "inst", "js");
  await write(root, {
    "srcts/index.ts": [
      'import { createElement } from "react";',
      'import { Button } from "react-aria-components";',
      'import { label } from "./label.ts";',
      "",
      "export function Hello() {",
      "  return createElement(Button, null, label);",
      "}",
      "",
    ].join("\n"),
    "srcts/label.ts": 'export const label = "Hello";\n',
    "inst/js/gone.js": "",
  });
  written = await build(root);
});

afterAll(async () => {
  await rm(root, { recursive: true, force: true });
});

// The files in inst/js whose names match the pattern.
async function named(pattern: RegExp): Promise<string[]> {
  return (await readdir(out)).filter((file) => pattern.test(file)).sort();
}

test("writes React's file, React's entry points, React Aria's files and the file of blockr.ui's components, with the import map, what each imports up front and the licenses", async () => {
  const react = await named(/^react-[0-9a-f]{8}\.js$/);
  const shared = await named(/^react-aria-[0-9A-Z]{8}\.js$/);
  expect(react).toHaveLength(1);
  expect(shared.length).toBeGreaterThan(0);

  // The build empties inst/js first.
  const files = [
    "LICENSES",
    "blockr-ui.js",
    "import-map.json",
    "preload.json",
    ...react,
    ...shared,
    ...REACT_ENTRIES.map(importFile),
    ...REACT_ARIA.map(importFile),
  ].sort();
  expect(await readdir(out).then((found) => found.sort())).toEqual(files);
  expect(written).toEqual(files.map((file) => `inst/js/${file}`).sort());
});

test("writes React and ReactDOM into React's file, minified, with React's license comments, and each of React's entry points as an ES module that exports what the entry point exports", async () => {
  const [react = ""] = await named(/^react-[0-9a-f]{8}\.js$/);
  const code = await readFile(join(out, react), "utf8");
  expect(code).toContain("@license React");
  expect(minified(code)).toBe(true);
  expect(importedPackages(code)).toEqual([]);

  // Each entry point exports what React's does in production,
  // bar the two names that its development build alone exports.
  const installed = createRequire(import.meta.url);
  for (const name of REACT_ENTRIES) {
    const file = join(out, importFile(name));
    expect(importedPackages(await readFile(file, "utf8"))).toEqual([`./${react}`]);
    const module = (await import(pathToFileURL(file).href)) as Record<string, unknown>;
    const exported = Object.keys(installed(name) as object).filter((key) => !["act", "captureOwnerStack"].includes(key));
    expect(Object.keys(module).sort()).toEqual([...exported, "default"].sort());
  }
});

test("writes the whole of React Aria's three packages into React Aria's files, minified, which import React through the import map alone", async () => {
  const shared = await named(/^react-aria-[0-9A-Z]{8}\.js$/);
  for (const file of [...REACT_ARIA.map(importFile), ...shared]) {
    const code = await readFile(join(out, file), "utf8");
    expect(minified(code)).toBe(true);
    // React marks each element that it makes with this symbol, so a file that held React would name it.
    expect(code).not.toContain("react.transitional.element");
    expect(code).not.toContain("require(");
    for (const name of importedPackages(code)) {
      expect(["react", "react-dom", ...shared.map((chunk) => `./${chunk}`)]).toContain(name);
    }
  }

  // Each of React Aria's files exports what its package exports.
  for (const name of REACT_ARIA) {
    const module = (await import(pathToFileURL(join(out, importFile(name))).href)) as Record<string, unknown>;
    const own = (await import(name)) as Record<string, unknown>;
    expect(Object.keys(module).sort()).toEqual(Object.keys(own).sort());
  }
});

test("writes blockr.ui's own code into the file of its components, unminified, which imports React and React Aria through the import map", async () => {
  const code = await readFile(join(out, "blockr-ui.js"), "utf8");

  expect(minified(code)).toBe(false);
  expect(code).toContain('var label = "Hello";');
  expect(code).toMatch(/^function Hello\(\) \{\n {2}return createElement\(Button, null, label\);\n\}$/m);
  expect(importedPackages(code)).toEqual(["react", "react-aria-components"]);
  expect(code).not.toContain("react-aria-Button");
});

test("writes the entries of the page's import map, each import by the file in inst/js that it resolves to", async () => {
  const map = JSON.parse(await readFile(join(out, "import-map.json"), "utf8")) as unknown;

  expect(map).toEqual({
    imports: {
      react: "react.js",
      "react/jsx-runtime": "react-jsx-runtime.js",
      "react-dom": "react-dom.js",
      "react-dom/client": "react-dom-client.js",
      "@blockr/ui": "blockr-ui.js",
      "react-aria-components": "react-aria-components.js",
      "react-aria": "react-aria.js",
      "react-stately": "react-stately.js",
    },
  });
  expect(Object.keys((map as { imports: object }).imports)).toEqual([...IMPORTS]);
});

test("lists what each file imports up front, so that a page that imports React alone loads none of React Aria", async () => {
  const preload = JSON.parse(await readFile(join(out, "preload.json"), "utf8")) as Record<string, string[]>;
  const [react = ""] = await named(/^react-[0-9a-f]{8}\.js$/);
  const shared = await named(/^react-aria-[0-9A-Z]{8}\.js$/);

  // React's entry points import React's file, which imports nothing.
  for (const name of REACT_ENTRIES) expect(preload[importFile(name)]).toEqual([`./${react}`]);
  expect(preload[react]).toBeUndefined();

  // The file of the components imports React and React Aria by their names in the import map.
  expect(preload["blockr-ui.js"]?.sort()).toEqual(["react", "react-aria-components"]);

  // React Aria's files import each other and React, and nothing beyond.
  for (const file of [...REACT_ARIA.map(importFile), ...shared]) {
    for (const name of preload[file] ?? []) {
      expect(["react", "react-dom", ...shared.map((chunk) => `./${chunk}`)]).toContain(name);
    }
  }
  expect(preload["react-aria-components.js"]).toContain("react");
});

test("writes the licenses of the code from other projects in its files into LICENSES beside them, each license once", async () => {
  const text = await readFile(join(out, "LICENSES"), "utf8");
  const [react = ""] = await named(/^react-[0-9a-f]{8}\.js$/);

  for (const name of ["react", "react-dom", "scheduler"]) {
    const { version } = createRequire(import.meta.url)(`${name}/package.json`) as { version: string };
    expect(text).toContain(`${name} ${version}, in ${react}\n`);
  }
  for (const name of REACT_ARIA) expect(text).toMatch(new RegExp(`^${name} ${VERSIONS[name]}, in .*${importFile(name)}`, "m"));
  // React Aria imports client-only, whose module is empty, so no file holds its code.
  expect(text).not.toContain("client-only");
  // React's license once, after the lines of React, ReactDOM and the scheduler, which share it.
  expect(text.match(/^MIT License$/gm)?.length).toBeGreaterThanOrEqual(1);
  expect(text.match(/Copyright \(c\) Meta Platforms, Inc\. and affiliates\./g)).toHaveLength(1);
});

test("fails where the file of blockr.ui's components would bundle a package from node_modules, through the vendor check", async () => {
  const other = await mkdtemp(join(tmpdir(), "blockr-ui-build-"));
  try {
    await write(other, {
      "node_modules/left-pad/package.json": '{ "name": "left-pad", "main": "index.js" }\n',
      "node_modules/left-pad/index.js": "module.exports = (s, n) => String(s).padStart(n);\n",
      "srcts/index.ts": 'import leftPad from "left-pad";\nexport const padded = leftPad("x", 3);\n',
    });
    await expect(build(other)).rejects.toThrow("The build put code of left-pad, from node_modules, into inst/js/blockr-ui.js.");
  } finally {
    await rm(other, { recursive: true, force: true });
  }
});

test("fails for a package that comes with no license, naming the files that hold its code", async () => {
  const other = await mkdtemp(join(tmpdir(), "blockr-ui-licenses-"));
  try {
    await write(other, {
      "node_modules/quiet/package.json": '{ "name": "quiet", "version": "1.0.0" }\n',
      "node_modules/quiet/index.js": "export const quiet = 1;\n",
    });
    const dir = join(other, "inst", "js");
    const bundled = new Map([
      [join(dir, "b.js"), [join(other, "node_modules", "quiet")]],
      [join(dir, "a.js"), [join(other, "node_modules", "quiet")]],
    ]);
    await expect(licenses(other, dir, bundled)).rejects.toThrow(
      "The code of quiet in a.js and b.js comes with no license: its directory in node_modules holds no license file.",
    );
  } finally {
    await rm(other, { recursive: true, force: true });
  }
});

test("fails where blockr.ui's node_modules hold other versions of React Aria or of Tailwind CSS than the preset checks", async () => {
  const other = await mkdtemp(join(tmpdir(), "blockr-ui-versions-"));
  try {
    await write(
      other,
      Object.fromEntries(
        Object.entries(VERSIONS).map(([name, version]) => [`node_modules/${name}/package.json`, JSON.stringify({ name, version })]),
      ),
    );
    expect(() => {
      checkVersions(other);
    }).not.toThrow();

    await write(other, { "node_modules/react-stately/package.json": '{ "name": "react-stately", "version": "3.50.0" }\n' });
    await rm(join(other, "node_modules", "tailwindcss"), { recursive: true });
    expect(() => {
      checkVersions(other);
    }).toThrow(
      `The node_modules of blockr.ui hold react-stately 3.50.0, where VERSIONS names ${VERSIONS["react-stately"]} and tailwindcss is missing, where VERSIONS names ${VERSIONS.tailwindcss}.`,
    );
  } finally {
    await rm(other, { recursive: true, force: true });
  }
});

test("the versions that the preset checks are those that blockr.ui's package.json pins", async () => {
  const manifest = JSON.parse(await readFile(join(repo, "package.json"), "utf8")) as Record<
    "dependencies" | "devDependencies",
    Record<string, string>
  >;

  for (const [name, version] of Object.entries(VERSIONS)) {
    expect(manifest.dependencies[name] ?? manifest.devDependencies[name]).toBe(version);
  }
});
