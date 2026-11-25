// srcts/build/preset.ts
import { existsSync, readFileSync } from "node:fs";
import { dirname, join } from "node:path";

// srcts/import-map/index.ts
var REACT_ENTRIES = ["react", "react/jsx-runtime", "react-dom", "react-dom/client"];
var COMPONENTS = "@blockr/ui";
var REACT_ARIA = ["react-aria-components", "react-aria", "react-stately"];
var IMPORTS = [...REACT_ENTRIES, COMPONENTS, ...REACT_ARIA];

// srcts/build/preset.ts
var VERSIONS = {
  "react-aria-components": "1.22.1",
  "react-aria": "3.53.1",
  "react-stately": "3.51.0"
};
var REACT = /^react(-dom)?(\/.*)?$/;
var COMPILER_RUNTIME = /^react[-/]compiler-runtime$/;
var COMPILER_RUNTIME_MODULE = `import { __COMPILER_RUNTIME } from "react";
export const { c } = __COMPILER_RUNTIME;
`;
var BLOCKR_UI = /^@blockr\/ui(\/.*)?$/;
var REACT_ARIA_PACKAGE = /^(react-aria-components|react-aria|react-stately)$/;
var INSIDE_REACT_ARIA = /^(?:(react-aria-components|react-aria|react-stately)|@(react-aria|react-stately))\//;
function leaveNamed(path) {
  if (IMPORTS.includes(path)) return { path, external: true };
  return {
    errors: [{ text: `A module imports ${path}, which the page's import map does not name. It names ${IMPORTS.join(", ")}.` }]
  };
}
function leaveReact(build) {
  build.onResolve({ filter: COMPILER_RUNTIME }, () => ({ path: "react-compiler-runtime", namespace: "blockr.ui" }));
  build.onLoad({ filter: /^react-compiler-runtime$/, namespace: "blockr.ui" }, () => ({
    contents: COMPILER_RUNTIME_MODULE,
    loader: "js"
  }));
  build.onResolve({ filter: REACT }, (args) => {
    const left = leaveNamed(args.path);
    return args.kind === "require-call" && left.external === true ? { path: args.path, namespace: "blockr.ui-require" } : left;
  });
  build.onLoad({ filter: REACT, namespace: "blockr.ui-require" }, (args) => {
    const name = JSON.stringify(args.path);
    return { contents: `export * from ${name};
export { default } from ${name};
`, loader: "js" };
  });
}
function installedVersion(name, dir) {
  for (let at = dir; ; at = dirname(at)) {
    const manifest = join(at, "node_modules", name, "package.json");
    if (existsSync(manifest)) return JSON.parse(readFileSync(manifest, "utf8")).version;
    if (dirname(at) === at) return void 0;
  }
}
function workingDir(build) {
  return build.initialOptions.absWorkingDir ?? process.cwd();
}
var imports = {
  name: "blockr.ui-imports",
  setup(build) {
    leaveReact(build);
    build.onResolve({ filter: BLOCKR_UI }, (args) => leaveNamed(args.path));
    build.onResolve({ filter: REACT_ARIA_PACKAGE }, (args) => {
      const name = args.path;
      const installed = installedVersion(name, args.resolveDir || workingDir(build));
      if (installed === void 0 || installed === VERSIONS[name]) return { path: name, external: true };
      return {
        errors: [
          {
            text: `A module imports ${name} ${installed}, from node_modules, but the page runs ${name} ${VERSIONS[name]}, from blockr.ui's files. Install ${name}@${VERSIONS[name]}, so that the module builds against the React Aria that runs on the page.`
          }
        ]
      };
    });
    build.onResolve({ filter: INSIDE_REACT_ARIA }, (args) => {
      const [, own, scope] = INSIDE_REACT_ARIA.exec(args.path) ?? [];
      return {
        errors: [
          {
            text: `A module imports ${args.path}, which would bundle a second copy of React Aria. Import it from ${own ?? scope ?? ""} instead, which the page's import map names.`
          }
        ]
      };
    });
  }
};
var preset = {
  bundle: true,
  format: "esm",
  platform: "browser",
  target: "es2022",
  jsx: "automatic",
  // React and the libraries built on it read this,
  // to leave out what they check only while an app is developed.
  define: { "process.env.NODE_ENV": JSON.stringify("production") },
  plugins: [imports]
};

// srcts/build/preload.ts
import { dirname as dirname2, relative } from "node:path/posix";
function staticImports(metafiles) {
  const files = [];
  for (const metafile of metafiles) {
    for (const [path, output] of Object.entries(metafile?.outputs ?? {})) {
      const imports2 = output.imports.flatMap((entry) => {
        if (entry.kind !== "import-statement" && entry.kind !== "import-rule") return [];
        return [entry.external === true ? entry.path : `./${relative(dirname2(path), entry.path)}`];
      });
      if (imports2.length > 0) files.push([path.slice(path.lastIndexOf("/") + 1), [...new Set(imports2)]]);
    }
  }
  return Object.fromEntries(files.sort(([a], [b]) => a < b ? -1 : a > b ? 1 : 0));
}

// srcts/build/vendor-check.ts
var vendorCheck = {
  name: "blockr.ui-vendor-check",
  setup(build) {
    build.initialOptions.metafile = true;
    build.onEnd((result) => ({
      errors: bundledPackages(result.metafile).map(({ name, file }) => ({
        text: `The build put code of ${name}, from node_modules, into ${file}. Bundle ${name} into the package's vendor file instead, and import it from there.`
      }))
    }));
  }
};
function bundledPackages(metafile) {
  return Object.entries(metafile?.outputs ?? {}).flatMap(([file, output]) => {
    const names = new Set(
      Object.entries(output.inputs).flatMap(([input, { bytesInOutput }]) => {
        const name = inputPackage(input);
        return name === void 0 || bytesInOutput === 0 ? [] : [name];
      })
    );
    return [...names].map((name) => ({ name, file }));
  });
}
function inputPackage(input) {
  const at = input.lastIndexOf("node_modules/");
  if (at === -1) return void 0;
  const [first = "", second = ""] = input.slice(at + "node_modules/".length).split("/");
  return first.startsWith("@") ? `${first}/${second}` : first;
}
export {
  preset,
  staticImports,
  vendorCheck
};
