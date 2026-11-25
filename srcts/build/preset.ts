// The build preset, with which blockr.ui's build bundles its components,
// and which blockr.ui ships to packages as `@blockr/ui/build`,
// so that every package builds its modules against blockr.ui alike
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.7.4#the-build-preset).
// It holds esbuild's options for a module, which is an ES module for the browser.
// The module leaves its imports of React's entry points, of blockr.ui's components and of React Aria's three packages imports,
// which the page that R serves resolves through its import map (../import-map/index.ts).
// So the module's bundle holds none of them.
// The icons of @blockr/ui/icons are the exception, since the import map names no entry for them:
// the preset bundles those that the module imports (ICONS).
// A package's build adds its entry points and where they go,
// as in `esbuild.build({ ...preset, entryPoints: ["srcts/head-block.tsx"], outdir: "inst/js" })`.
// To the builds of its own files, it adds the vendor check of ./vendor-check.ts as well,
// which the preset's plugins leave out.
// The file imports no npm package, only Node's own modules and the list of the import map,
// so that its bundle in inst/types holds blockr.ui's own code alone.

import { existsSync, readFileSync } from "node:fs";
import { dirname, join } from "node:path";
import type { BuildOptions, OnResolveResult, Plugin, PluginBuild } from "esbuild";
import { IMPORTS } from "../import-map/index.ts";
import type { ReactAria } from "../import-map/index.ts";

// The versions of React Aria's three packages that blockr.ui is written for,
// which React Aria's files in inst/js hold.
// A package's build imports React Aria at these versions alone,
// so that the package builds against the React Aria that runs on the page.
// The package.json of blockr.ui pins the same versions, which its build checks (./build.ts).
export const VERSIONS = {
  "react-aria-components": "1.22.1",
  "react-aria": "3.53.1",
  "react-stately": "3.51.0",
} as const satisfies Record<ReactAria, string>;

// The imports of React's entry points, such as react/jsx-runtime and react-dom/client.
const REACT = /^react(-dom)?(\/.*)?$/;

// The imports of the React Compiler's runtime.
// The code that the compiler writes keeps a component's values between renders in a cache,
// which the runtime's `c` gives it.
// A library that the compiler built for React 17 and 18 as well, such as React Flow 13,
// imports the runtime's own package, react-compiler-runtime.
// A library built for React 19 alone imports react/compiler-runtime instead,
// an entry point of React that the page's import map does not name.
// The preset resolves both to COMPILER_RUNTIME_MODULE,
// so that a package's bundle holds no copy of the runtime,
// and the import map needs no entry for it.
const COMPILER_RUNTIME = /^react[-/]compiler-runtime$/;

// What the preset bundles in place of the React Compiler's runtime:
// a module that reads the runtime that React exports as __COMPILER_RUNTIME.
// Its import of React stays an import, as every other does.
// So the page runs the runtime of its one copy of React.
// React's runtime holds `c` alone.
// The compiler's code imports nothing else, unless the compiler ran with an option for debugging it.
// A build of such code fails, with an error that names the import.
const COMPILER_RUNTIME_MODULE = `import { __COMPILER_RUNTIME } from "react";
export const { c } = __COMPILER_RUNTIME;
`;

// The imports of blockr.ui's components, `@blockr/ui`, and of the other entries of blockr.ui.
const BLOCKR_UI = /^@blockr\/ui(\/.*)?$/;

// The entry of blockr.ui that offers Lucide's icons, from which the preset bundles the icons that a module imports
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.7.3#the-icons).
// The page's import map names no entry for it.
// So the build of a package's file of libraries, its vendor file, holds the icons that the package draws, and no others.
// The entry re-exports the icons of Lucide's package lucide, which npm installs with blockr.ui's types (./build.ts).
const ICONS = "@blockr/ui/icons";

// The imports of Lucide's packages, such as lucide, lucide-react and @lucide/lab, and of what lies inside one.
// The preset fails a build that imports one, unless the import is the entry's, ICONS.
// So every package draws the icons of the Lucide that blockr.ui pins,
// and imports them through that one entry.
// That keeps open serving each icon to a page once, however many packages import it
// (https://github.com/BristolMyersSquibb/blockr/wiki/Backlog#each-icon-once-on-the-page).
const LUCIDE = /^(?:lucide(?:-[^/]+)?|@lucide\/[^/]+)(?:\/.*)?$/;

// Marks the call with which the preset has esbuild resolve ICONS.
const RESOLVING_ICONS = Symbol("resolving @blockr/ui/icons");

// The imports of React Aria's three packages, each by its name.
const REACT_ARIA_PACKAGE = /^(react-aria-components|react-aria|react-stately)$/;

// The imports of what lies inside React Aria's packages,
// each of which would bundle a second copy of React Aria.
// They are the entry points inside its three packages, such as react-aria-components/Table,
// and every package under @react-aria/ or @react-stately/.
// React Aria 1.22 gives each of its components such an entry point,
// and the packages under @react-aria/ and @react-stately/ only pass on what those entry points hold.
// A second copy would hold what blockr.ui's copy holds inside itself:
// its React contexts, the focus modality and the stack of open overlays.
// An item of the second copy inside blockr.ui's ComboBox would then find no combo box.
// The match names the package to import instead,
// as react-aria for react-aria/useTable and for @react-aria/table.
// The packages under @internationalized/, such as @internationalized/date, hold no React context,
// so a module bundles them itself.
// A type that a module imports with `import type` passes,
// since esbuild drops that import before it resolves anything.
const INSIDE_REACT_ARIA = /^(?:(react-aria-components|react-aria|react-stately)|@(react-aria|react-stately))\//;

// Leaves an import that the page's import map names an import.
// Any other, such as react-dom/server, fails the build,
// rather than bundle a second copy of what the page holds once.
function leaveNamed(path: string): OnResolveResult {
  if ((IMPORTS as readonly string[]).includes(path)) return { path, external: true };
  return {
    errors: [{ text: `A module imports ${path}, which the page's import map does not name. It names ${IMPORTS.join(", ")}.` }],
  };
}

// Leaves each import of one of React's entry points that the page's import map names an import,
// and fails the build on any other entry point of React (leaveNamed()).
// An import of the React Compiler's runtime gets COMPILER_RUNTIME_MODULE instead (COMPILER_RUNTIME).
// It resolves that import before React's entry points, since react/compiler-runtime is one of them.
// A CommonJS module that a build bundles reads React with `require("react")`,
// as the shim of use-sync-external-store does, which React Aria bundles.
// The ES module that esbuild writes cannot run that call where React stays an import,
// so the module would fail as the page loads it.
// So such a call gets an ES module that exports what the entry point exports,
// through an import of the entry point, which stays an import as every other does.
// The build of React Aria's files leaves its imports of React to the import map alone (./build.ts).
export function leaveReact(build: PluginBuild): void {
  build.onResolve({ filter: COMPILER_RUNTIME }, () => ({ path: "react-compiler-runtime", namespace: "blockr.ui" }));
  build.onLoad({ filter: /^react-compiler-runtime$/, namespace: "blockr.ui" }, () => ({
    contents: COMPILER_RUNTIME_MODULE,
    loader: "js",
  }));
  build.onResolve({ filter: REACT }, (args) => {
    const left = leaveNamed(args.path);
    return args.kind === "require-call" && left.external === true ? { path: args.path, namespace: "blockr.ui-require" } : left;
  });
  build.onLoad({ filter: REACT, namespace: "blockr.ui-require" }, (args) => {
    const name = JSON.stringify(args.path);
    return { contents: `export * from ${name};\nexport { default } from ${name};\n`, loader: "js" };
  });
}

// The version of the npm package that Node resolves by its name from `dir`,
// from the package.json in the nearest node_modules that holds it,
// or undefined where no node_modules above `dir` holds it.
export function installedVersion(name: string, dir: string): string | undefined {
  for (let at = dir; ; at = dirname(at)) {
    const manifest = join(at, "node_modules", name, "package.json");
    if (existsSync(manifest)) return (JSON.parse(readFileSync(manifest, "utf8")) as { version: string }).version;
    if (dirname(at) === at) return undefined;
  }
}

// The directory that a build resolves from, by its options.
function workingDir(build: PluginBuild): string {
  return build.initialOptions.absWorkingDir ?? process.cwd();
}

// Leaves each import that the page's import map names an import:
// React's entry points (leaveReact()), blockr.ui's components and React Aria's three packages.
// It fails the build where a module imports one of React Aria's packages,
// and node_modules holds that package at another version than VERSIONS names.
// An import of what lies inside React Aria's packages fails the build too,
// with an error that names the package to import instead (INSIDE_REACT_ARIA).
// It bundles the icons that a module imports from ICONS,
// and fails the build where a module imports one of Lucide's packages itself (LUCIDE).
const imports: Plugin = {
  name: "blockr.ui-imports",
  setup(build) {
    // The files that ICONS resolved to in this build, which alone may import Lucide's packages.
    // Esbuild resolves what a file imports only after it has resolved the file,
    // so the set holds the file by then.
    const icons = new Set<string>();

    leaveReact(build);
    // Resolves an import of ICONS as esbuild does, from node_modules, and keeps the file in `icons`.
    // To learn the file, the handler calls build.resolve(), which runs the handlers again.
    // In that call, marked by RESOLVING_ICONS, every handler leaves the import to esbuild.
    build.onResolve({ filter: /^@blockr\/ui\/icons$/ }, async (args) => {
      if (args.pluginData === RESOLVING_ICONS) return undefined;
      const resolved = await build.resolve(args.path, {
        kind: args.kind,
        importer: args.importer,
        resolveDir: args.resolveDir,
        pluginData: RESOLVING_ICONS,
      });
      if (resolved.errors.length > 0) return { errors: resolved.errors };
      icons.add(resolved.path);
      return { path: resolved.path, external: resolved.external, namespace: resolved.namespace, sideEffects: resolved.sideEffects };
    });
    build.onResolve({ filter: BLOCKR_UI }, (args) => (args.path === ICONS ? undefined : leaveNamed(args.path)));
    build.onResolve({ filter: LUCIDE }, (args) => {
      if (icons.has(args.importer)) return undefined;
      return {
        errors: [
          {
            text: `A module imports ${args.path}. Import Lucide's icons from ${ICONS} instead, which offers each icon of the Lucide that blockr.ui pins, and draw them with Icon from @blockr/ui.`,
          },
        ],
      };
    });
    build.onResolve({ filter: REACT_ARIA_PACKAGE }, (args) => {
      const name = args.path as ReactAria;
      const installed = installedVersion(name, args.resolveDir || workingDir(build));
      if (installed === undefined || installed === VERSIONS[name]) return { path: name, external: true };
      return {
        errors: [
          {
            text: `A module imports ${name} ${installed}, from node_modules, but the page runs ${name} ${VERSIONS[name]}, from blockr.ui's files. Install ${name}@${VERSIONS[name]}, so that the module builds against the React Aria that runs on the page.`,
          },
        ],
      };
    });
    build.onResolve({ filter: INSIDE_REACT_ARIA }, (args) => {
      const [, own, scope] = INSIDE_REACT_ARIA.exec(args.path) ?? [];
      return {
        errors: [
          {
            text: `A module imports ${args.path}, which would bundle a second copy of React Aria. Import it from ${own ?? scope ?? ""} instead, which the page's import map names.`,
          },
        ],
      };
    });
  },
};

export const preset = {
  bundle: true,
  format: "esm",
  platform: "browser",
  target: "es2022",
  jsx: "automatic",
  // React and the libraries built on it read this,
  // to leave out what they check only while an app is developed.
  define: { "process.env.NODE_ENV": JSON.stringify("production") },
  plugins: [imports],
} satisfies BuildOptions;
