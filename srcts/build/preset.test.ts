// What a package's build makes of its module with the build preset that blockr.ui ships as `@blockr/ui/build`:
// a module that holds none of React, of blockr.ui's components and of React Aria,
// but imports them, as the page that R serves resolves them through its import map.
// It holds the icons that it imports from `@blockr/ui/icons`, for which the import map names no entry.
// Here the tests resolve those imports to React and React Aria in node_modules,
// and to the source of the components, as vitest.config.ts sets out.

import { cp, mkdir, mkdtemp, readFile, rm, writeFile } from "node:fs/promises";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath, pathToFileURL } from "node:url";
import * as esbuild from "esbuild";
import * as lucide from "lucide";
import { act, createElement } from "react";
import type { ReactElement } from "react";
import { createRoot } from "react-dom/client";
import { afterEach, beforeEach, expect, test } from "vitest";
import { IMPORTS } from "../import-map/index.ts";
import * as components from "../index.ts";
import type * as Build from "./index.ts";
import { VERSIONS } from "./preset.ts";

// React warns where a test draws outside act() unless this is set.
(globalThis as { IS_REACT_ACT_ENVIRONMENT?: boolean }).IS_REACT_ACT_ENVIRONMENT = true;

const repo = resolve(dirname(fileURLToPath(import.meta.url)), "../..");

// The build preset and the vendor check as blockr.ui ships them.
const { preset, vendorCheck } = (await import(pathToFileURL(join(repo, "inst/types/build/index.js")).href)) as typeof Build;

let root: string;

beforeEach(async () => {
  // The package sits under the repo,
  // since vitest imports no module from outside it into a test that runs in a page.
  // Its node_modules holds what a test writes there, and the repo's node_modules the rest,
  // as Node resolves a package from the nearest node_modules that holds it.
  const scratch = join(repo, "node_modules", ".tmp");
  await mkdir(scratch, { recursive: true });
  root = await mkdtemp(join(scratch, "blockr-ui-preset-"));
});

afterEach(async () => {
  await rm(root, { recursive: true, force: true });
});

async function write(files: Record<string, string>): Promise<void> {
  for (const [path, content] of Object.entries(files)) {
    await mkdir(dirname(join(root, path)), { recursive: true });
    await writeFile(join(root, path), content);
  }
}

// Bundles a package's module from its source, as its build does with the preset,
// and gives the file it bundled it into.
async function bundle(source: string, plugins: esbuild.Plugin[] = preset.plugins): Promise<string> {
  await write({ "srcts/view.tsx": source });
  await esbuild.build({
    ...preset,
    plugins,
    absWorkingDir: root,
    entryPoints: [join(root, "srcts", "view.tsx")],
    outdir: join(root, "inst", "js"),
    logLevel: "silent",
  });
  return join(root, "inst", "js", "view.js");
}

// The packages that the code of a bundle imports, by the specifier of each.
function importedPackages(code: string): string[] {
  return [...new Set([...code.matchAll(/^(?:import|export)\b[^;]*?\bfrom\s*"([^"]+)"/gm)].map((m) => m[1] ?? ""))].sort();
}

// Draws a React element into an element of the page, in act(), so that React has drawn it on return.
function draw(node: ReactElement): { el: HTMLElement; redraw: (next: ReactElement) => void; unmount: () => void } {
  const el = document.createElement("div");
  document.body.append(el);
  const view = createRoot(el);
  const render = (next: ReactElement): void => {
    act(() => {
      view.render(next);
    });
  };
  render(node);
  return {
    el,
    redraw: render,
    unmount: () => {
      act(() => {
        view.unmount();
      });
      el.remove();
    },
  };
}

const view = `import { useState } from "react";
import * as ui from "@blockr/ui";
import { Button } from "react-aria-components";
import { useFocusRing } from "react-aria";
import { useToggleState } from "react-stately";

export const components = ui;

export default function Toggle() {
  const state = useToggleState();
  const { focusProps } = useFocusRing();
  const [presses, setPresses] = useState(0);
  return (
    <Button {...focusProps} onPress={() => { state.toggle(); setPresses(presses + 1); }}>
      {state.isSelected ? "on" : "off"} {presses}
    </Button>
  );
}
`;

test("leaves a package's module importing React, blockr.ui's components and React Aria's three packages, which the import map names", async () => {
  const code = await readFile(await bundle(view), "utf8");

  expect(importedPackages(code)).toEqual(["@blockr/ui", "react", "react-aria", "react-aria-components", "react-stately", "react/jsx-runtime"]);
  expect(importedPackages(code).every((name) => (IMPORTS as readonly string[]).includes(name))).toBe(true);
  // React marks each element it makes with this symbol.
  expect(code).not.toContain("react.transitional.element");
  // React Aria gives each of its components a class of this kind by default.
  expect(code).not.toContain("react-aria-Button");
});

test("the module draws with the React and the React Aria that it imports", async () => {
  const module = (await import(pathToFileURL(await bundle(view)).href)) as { default: () => ReactElement; components: object };
  const drawn = draw(createElement(module.default));

  const button = drawn.el.querySelector("button");
  expect(button?.className).toBe("react-aria-Button");
  expect(button?.textContent).toBe("off 0");

  // The hooks of React Aria's packages and of React keep their state in the one React.
  act(() => {
    button?.click();
  });
  expect(button?.textContent).toBe("on 1");
  drawn.unmount();

  // The module's import of the components reads their source here.
  expect(module.components).toBe(components);
});

test("refuses an entry point of React that the import map does not name", async () => {
  await expect(bundle(`import { renderToString } from "react-dom/server";\n\nexport const html = renderToString(null);\n`)).rejects.toThrow(
    "A module imports react-dom/server, which the page's import map does not name.",
  );
});

test("refuses an entry of blockr.ui that the import map does not name", async () => {
  await expect(bundle(`import * as stubs from "@blockr/ui/stubs";\n\nexport default stubs;\n`)).rejects.toThrow(
    `A module imports @blockr/ui/stubs, which the page's import map does not name. It names ${IMPORTS.join(", ")}.`,
  );
});

// Installs blockr.ui's types into the package's node_modules as `@blockr/ui`, as core's install_js_types() does.
// The package's build resolves `@blockr/ui/icons` there,
// and the lucide whose icons the entry re-exports in the repo's node_modules.
async function installTypes(): Promise<void> {
  await cp(join(repo, "inst", "types"), join(root, "node_modules", "@blockr", "ui"), { recursive: true });
}

// The modules of lucide whose code a bundle holds, which esbuild names in a comment above the code of each,
// by their paths from the package.
// Here lucide sits in the repo's node_modules, as ../../lucide from the package.
function lucideModules(code: string): string[] {
  return [...code.matchAll(/^\/\/ .*\blucide\/dist\/esm\/(.*)$/gm)].map((m) => m[1] ?? "").sort();
}

test("bundles the icons that a module imports from @blockr/ui/icons, for which the import map names no entry, and no others", async () => {
  await installTypes();
  const file = await bundle(`import { Pin, TriangleAlert } from "@blockr/ui/icons";\n\nexport const shapes = [Pin, TriangleAlert];\n`);
  const code = await readFile(file, "utf8");

  expect(IMPORTS).not.toContain("@blockr/ui/icons");
  expect(importedPackages(code)).toEqual([]);
  expect(lucideModules(code)).toEqual(["icons/pin.mjs", "icons/triangle-exclamation-point.mjs"]);
  const { shapes } = (await import(pathToFileURL(file).href)) as { shapes: unknown[] };
  expect(shapes).toEqual([lucide.Pin, lucide.TriangleAlert]);
});

// A package of Lucide's, a file inside one, and a package of icons beyond lucide's.
// The preset refuses each before esbuild resolves it,
// so lucide-react and @lucide/lab fail alike, though npm does not install them here.
test.each(["lucide", "lucide-react", "lucide/dist/esm/icons/pin.mjs", "@lucide/lab"])(
  "refuses an import of Lucide's packages, %s, naming @blockr/ui/icons instead",
  async (name) => {
    await expect(bundle(`import * as icons from "${name}";\n\nexport default icons;\n`)).rejects.toThrow(
      `A module imports ${name}. Import Lucide's icons from @blockr/ui/icons instead, which offers each icon of the Lucide that blockr.ui pins, and draw them with Icon from @blockr/ui.`,
    );
  },
);

test("refuses a module's import of lucide beside its import of @blockr/ui/icons, whose own import of lucide passes", async () => {
  await installTypes();

  await expect(
    bundle(`import { Pin } from "@blockr/ui/icons";\nimport { X } from "lucide";\n\nexport const shapes = [Pin, X];\n`),
  ).rejects.toThrow("A module imports lucide. Import Lucide's icons from @blockr/ui/icons instead");
});

// The React Compiler's runtime as npm installs it,
// beside a library that the compiler built for React 17 and 18 as well, such as React Flow 13.
// Like the real package, it calls `require("react")`.
const compilerRuntime: Record<string, string> = {
  "node_modules/react-compiler-runtime/package.json": '{ "name": "react-compiler-runtime", "main": "index.js" }\n',
  "node_modules/react-compiler-runtime/index.js": 'const React = require("react");\nexports.c = (size) => React.__COMPILER_RUNTIME.c(size);\n',
};

// A component as the React Compiler writes it, which imports the runtime from `runtime`.
// It keeps the element that it draws in the cache that the runtime's `c` gives it,
// and makes the element again only when its text changes.
// It counts in `made` how often it made one.
const compiledLabel = (runtime: string) => `import { c as _c } from "${runtime}";
import { jsx } from "react/jsx-runtime";

export let made = 0;

export default function Label({ text }: { text: string }) {
  const $ = _c(2);
  let t0;
  if ($[0] !== text) {
    made += 1;
    t0 = jsx("span", { children: text });
    $[0] = text;
    $[1] = t0;
  } else {
    t0 = $[1];
  }
  return t0;
}
`;

// A library that the compiler built for React 17 and 18 as well imports the package react-compiler-runtime.
// One built for React 19 alone imports React's entry point react/compiler-runtime.
test.each(["react-compiler-runtime", "react/compiler-runtime"])(
  "resolves the React Compiler's runtime, %s, to that of the React that the module imports, and bundles none of it",
  async (runtime) => {
    await write(compilerRuntime);
    const file = await bundle(compiledLabel(runtime));
    const code = await readFile(file, "utf8");

    // The module imports the runtime from React, which the import map names.
    // It holds neither the runtime's package nor React.
    // So it holds no call of `require("react")`, which fails as the page loads.
    expect(importedPackages(code)).toEqual(["react", "react/jsx-runtime"]);
    expect(code).not.toContain("node_modules/react-compiler-runtime");
    expect(code).not.toContain('require("react")');
    expect(code).not.toContain("react.transitional.element");

    // Drawn again with the same text, the component keeps the element that it made,
    // in the cache of the React that draws it.
    const module = (await import(pathToFileURL(file).href)) as { default: (props: { text: string }) => ReactElement; made: number };
    const drawn = draw(createElement(module.default, { text: "a" }));
    drawn.redraw(createElement(module.default, { text: "a" }));
    expect(drawn.el.textContent).toBe("a");
    expect(module.made).toBe(1);

    drawn.redraw(createElement(module.default, { text: "b" }));
    expect(drawn.el.textContent).toBe("b");
    expect(module.made).toBe(2);
    drawn.unmount();
  },
);

test("gives a CommonJS module that a module bundles the React that the module imports, where it calls require()", async () => {
  // A library in CommonJS, as the shim of use-sync-external-store is, which React Aria bundles.
  await write({
    "node_modules/legacy-store/package.json": '{ "name": "legacy-store", "main": "index.js" }\n',
    "node_modules/legacy-store/index.js": 'const React = require("react");\nexports.hooks = [React.useState, React.default.useState];\n',
  });
  const file = await bundle(`import { hooks } from "legacy-store";\nimport { useState } from "react";\n\nexport const same = hooks.every((hook) => hook === useState);\n`);
  const code = await readFile(file, "utf8");

  // The module imports React through the import map,
  // and calls no require(), which would fail as the page loads it.
  expect(importedPackages(code)).toEqual(["react"]);
  expect(code).not.toContain('require("react")');
  expect(code).not.toContain('__require("react")');
  const module = (await import(pathToFileURL(file).href)) as { same: boolean };
  expect(module.same).toBe(true);
});

// The preset refuses each import before esbuild resolves it,
// so the packages under @react-aria/ and @react-stately/ fail alike,
// though npm does not install them with this version of React Aria.
test.each([
  ["react-aria-components/Button", "react-aria-components"],
  ["react-aria/useTable", "react-aria"],
  ["react-stately/useListState", "react-stately"],
  ["@react-aria/button", "react-aria"],
  ["@react-aria/utils/src/mergeProps", "react-aria"],
  ["@react-stately/utils", "react-stately"],
])("refuses an import of what lies inside React Aria's packages, %s, naming the package to import instead", async (name, instead) => {
  await expect(bundle(`import * as aria from "${name}";\n\nexport default aria;\n`)).rejects.toThrow(
    `A module imports ${name}, which would bundle a second copy of React Aria. Import it from ${instead} instead, which the page's import map names.`,
  );
});

test("leaves a package's module bundling @internationalized/date and @internationalized/number, which hold no React context, and importing React Aria's types", async () => {
  const file = await bundle(`import type { Key } from "react-aria-components/Table";
import type { AriaButtonProps } from "@react-aria/button";
import { CalendarDate } from "@internationalized/date";
import { NumberFormatter } from "@internationalized/number";

export const props: AriaButtonProps = {};
export const keys: Key[] = [new CalendarDate(2026, 11, 19).toString(), new NumberFormatter("en-US").format(1234.5)];
`);

  expect(importedPackages(await readFile(file, "utf8"))).toEqual([]);
  const { keys } = (await import(pathToFileURL(file).href)) as { keys: string[] };
  expect(keys).toEqual(["2026-11-19", "1,234.5"]);
});

const ariaImport = `import { Button } from "react-aria-components";\n\nexport default Button;\n`;

test("fails a build whose React Aria packages are at another version than blockr.ui's files hold, naming both", async () => {
  await write({ "node_modules/react-aria-components/package.json": '{ "name": "react-aria-components", "version": "1.21.0" }\n' });

  await expect(bundle(ariaImport)).rejects.toThrow(
    `A module imports react-aria-components 1.21.0, from node_modules, but the page runs react-aria-components ${VERSIONS["react-aria-components"]}, from blockr.ui's files. Install react-aria-components@${VERSIONS["react-aria-components"]}, so that the module builds against the React Aria that runs on the page.`,
  );
});

test("builds a module against React Aria at the versions that blockr.ui's files hold, or with none in node_modules", async () => {
  // The repo's node_modules holds React Aria at blockr.ui's versions.
  await expect(bundle(ariaImport)).resolves.toBeDefined();

  // A package's own node_modules holds them.
  await write({
    "node_modules/react-aria-components/package.json": JSON.stringify({ name: "react-aria-components", version: VERSIONS["react-aria-components"] }),
  });
  await expect(bundle(ariaImport)).resolves.toBeDefined();
});

test("the vendor check fails a package's build that puts code from node_modules into one of its own files, naming each npm package and the file", async () => {
  await write({
    "node_modules/@acme/pad/package.json": '{ "name": "@acme/pad", "type": "module", "main": "index.js" }\n',
    "node_modules/@acme/pad/index.js": "export const pad = (s, n) => String(s).padStart(n);\n",
    "node_modules/left-pad/package.json": '{ "name": "left-pad", "main": "index.js" }\n',
    "node_modules/left-pad/index.js": "module.exports = (s, n) => String(s).padStart(n);\n",
    "srcts/dock.ts": 'import { pad } from "@acme/pad";\nimport leftPad from "left-pad";\nexport const padded = [pad("x", 3), leftPad("y", 3)];\n',
    "srcts/util.ts": 'import { pad } from "@acme/pad";\nexport const padded = (s: string) => pad(s, 3);\nexport const label = "plain";\n',
    "srcts/plain.ts": 'import { label } from "./util.ts";\nexport const ui = label;\n',
  });
  const options = (entry: string, plugins: esbuild.Plugin[]) => ({
    ...preset,
    plugins,
    absWorkingDir: root,
    entryPoints: [join(root, "srcts", entry)],
    outdir: join(root, "inst", "js"),
    logLevel: "silent" as const,
  });

  // Without the check, as a package builds its vendor file, the build bundles both npm packages.
  await expect(esbuild.build(options("dock.ts", preset.plugins))).resolves.toBeDefined();

  // With it, the build fails, with an error for each npm package.
  const failure = await esbuild.build(options("dock.ts", [...preset.plugins, vendorCheck])).then(
    () => undefined,
    (error: unknown) => error as esbuild.BuildFailure,
  );
  expect(failure?.errors.map(({ text }) => text).sort()).toEqual([
    "The build put code of @acme/pad, from node_modules, into inst/js/dock.js. Bundle @acme/pad into the package's vendor file instead, and import it from there.",
    "The build put code of left-pad, from node_modules, into inst/js/dock.js. Bundle left-pad into the package's vendor file instead, and import it from there.",
  ]);

  // A module can bundle a file of its own,
  // which uses an npm package only in a function that the module leaves out.
  // Its file then holds none of the npm package's code, so it passes the check.
  await expect(esbuild.build(options("plain.ts", [...preset.plugins, vendorCheck]))).resolves.toBeDefined();
});
