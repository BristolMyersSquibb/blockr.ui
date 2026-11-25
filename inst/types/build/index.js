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
  "react-stately": "3.51.0",
  tailwindcss: "4.3.3"
};
var REACT = /^react(-dom)?(\/.*)?$/;
var COMPILER_RUNTIME = /^react[-/]compiler-runtime$/;
var COMPILER_RUNTIME_MODULE = `import { __COMPILER_RUNTIME } from "react";
export const { c } = __COMPILER_RUNTIME;
`;
var BLOCKR_UI = /^@blockr\/ui(\/.*)?$/;
var ICONS = "@blockr/ui/icons";
var LUCIDE = /^(?:lucide(?:-[^/]+)?|@lucide\/[^/]+)(?:\/.*)?$/;
var RESOLVING_ICONS = /* @__PURE__ */ Symbol("resolving @blockr/ui/icons");
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
    const icons = /* @__PURE__ */ new Set();
    leaveReact(build);
    build.onResolve({ filter: /^@blockr\/ui\/icons$/ }, async (args) => {
      if (args.pluginData === RESOLVING_ICONS) return void 0;
      const resolved = await build.resolve(args.path, {
        kind: args.kind,
        importer: args.importer,
        resolveDir: args.resolveDir,
        pluginData: RESOLVING_ICONS
      });
      if (resolved.errors.length > 0) return { errors: resolved.errors };
      icons.add(resolved.path);
      return { path: resolved.path, external: resolved.external, namespace: resolved.namespace, sideEffects: resolved.sideEffects };
    });
    build.onResolve({ filter: BLOCKR_UI }, (args) => args.path === ICONS ? void 0 : leaveNamed(args.path));
    build.onResolve({ filter: LUCIDE }, (args) => {
      if (icons.has(args.importer)) return void 0;
      return {
        errors: [
          {
            text: `A module imports ${args.path}. Import Lucide's icons from ${ICONS} instead, which offers each icon of the Lucide that blockr.ui pins, and draw them with Icon from @blockr/ui.`
          }
        ]
      };
    });
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

// srcts/build/theme-check.ts
import { readFile } from "node:fs/promises";

// srcts/build/theme-variables.ts
var themeVariables = {
  // The theme variables, as The theme variables lists them
  // (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.7.2#which-theme-variables-blockrui-declares):
  // the colours by job, the faces, the sizes of text, the radius, the shadows, the spacing and the layer of the overlays.
  variables: [
    "--bk-color-background",
    "--bk-color-foreground",
    "--bk-color-card",
    "--bk-color-card-foreground",
    "--bk-color-popover",
    "--bk-color-popover-foreground",
    "--bk-color-primary",
    "--bk-color-primary-foreground",
    "--bk-color-primary-text",
    "--bk-color-secondary",
    "--bk-color-secondary-foreground",
    "--bk-color-muted",
    "--bk-color-muted-foreground",
    "--bk-color-accent",
    "--bk-color-accent-foreground",
    "--bk-color-destructive",
    "--bk-color-destructive-foreground",
    "--bk-color-destructive-text",
    "--bk-color-warning",
    "--bk-color-warning-foreground",
    "--bk-color-warning-text",
    "--bk-color-success",
    "--bk-color-success-foreground",
    "--bk-color-success-text",
    "--bk-color-border",
    "--bk-color-input",
    "--bk-color-ring",
    "--bk-font-sans",
    "--bk-font-mono",
    "--bk-text-xs",
    "--bk-text-sm",
    "--bk-text-base",
    "--bk-text-lg",
    "--bk-radius",
    "--bk-shadow-sm",
    "--bk-shadow-md",
    "--bk-shadow-lg",
    "--bk-spacing",
    "--bk-z-index-overlay"
  ],
  // The fonts whose faces blockr.ui's stylesheet writes, by their families.
  // Open Sans is the default of --bk-font-sans, and the package ships its files.
  fonts: ["Open Sans"],
  // The attribute on the page's root that holds the mode, `light` or `dark`.
  // Core's board option dark_mode sets it, and the dark variant of the preset keys on it
  // (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.7.4#prefixes).
  modeAttribute: "data-bk-mode"
};

// srcts/build/theme-check.ts
var LISTED = new Set(themeVariables.variables);
var READ = /var\(\s*(--bk-[\w-]+)/g;
var DECLARED = /(?<![\w-])(--bk-[\w-]+)\s*:/g;
var TAILWIND = /\/\*!\s*tailwindcss v([^\s*|]+)/g;
var CSS_COMMENT = /\/\*[\s\S]*?\*\//g;
var PALETTE = [
  "red",
  "orange",
  "amber",
  "yellow",
  "lime",
  "green",
  "emerald",
  "teal",
  "cyan",
  "sky",
  "blue",
  "indigo",
  "violet",
  "purple",
  "fuchsia",
  "pink",
  "rose",
  "slate",
  "gray",
  "zinc",
  "neutral",
  "stone",
  "mauve",
  "olive",
  "mist",
  "taupe"
];
var COLOUR = new RegExp(String.raw`^[a-z][a-z0-9-]*?-(${PALETTE.join("|")})-(50|[1-9]00|950)(?:/.+)?$`);
var DROPPED = [
  { scale: "faces", utility: /^font-serif$/, held: "font-sans or font-mono" },
  { scale: "sizes of text", utility: /^text-[2-9]?xl(?:\/.+)?$/, held: "text-xs, text-sm, text-base or text-lg" },
  {
    scale: "radii",
    utility: /^rounded(?:-(?:[setrbl]|ss|se|ee|es|tl|tr|br|bl))?(?:-xs)?$/,
    held: "rounded-sm to rounded-4xl, rounded-full or rounded-none"
  },
  { scale: "shadows", utility: /^shadow(?:-(?:2?xs|2?xl|inner))?(?:\/.+)?$/, held: "shadow-sm, shadow-md, shadow-lg or shadow-none" }
];
var PART = String.raw`(?:[^\s"'\x60:()[\]{}<>;,=]|\[(?:[^\s[\]]|\[[^\s[\]]*\])*\]|\((?:[^\s()]|\([^\s()]*\))*\))+`;
var CLASS = new RegExp(String.raw`(?<![\w:/.\\-])([a-z]+):((?:${PART}:)*)(${PART})`, "g");
function lineOf(text, at) {
  return text.slice(0, at).split("\n").length;
}
function stylesheetProblems(file, own) {
  const rules = file.text.replace(CSS_COMMENT, (comment) => comment.replace(/[^\n]/g, " "));
  const declared = own ? new Set([...rules.matchAll(DECLARED)].map(([, name = ""]) => name)) : /* @__PURE__ */ new Set();
  const versions = [...file.text.matchAll(TAILWIND)].flatMap(
    ({ 1: version = "", index }) => version === VERSIONS.tailwindcss ? [] : [
      {
        file: file.name,
        line: lineOf(file.text, index),
        message: `Tailwind CSS ${version} wrote the stylesheet, but blockr.ui is written for Tailwind CSS ${VERSIONS.tailwindcss}, which every package's build runs. Install tailwindcss@${VERSIONS.tailwindcss}.`
      }
    ]
  );
  const reads = [...rules.matchAll(READ)].flatMap(
    ({ 1: name = "", index }) => LISTED.has(name) || declared.has(name) ? [] : [
      {
        file: file.name,
        line: lineOf(rules, index),
        message: `The stylesheet reads ${name}, which the list of theme variables lacks. Read one of the variables that themeVariables in @blockr/ui/build lists.`
      }
    ]
  );
  return [...versions, ...reads];
}
function codeProblems(file, own) {
  return [...file.text.matchAll(CLASS)].flatMap(({ 0: name, 1: prefix = "", 3: part = "", index }) => {
    const at = { file: file.name, line: lineOf(file.text, index) };
    const utility = part.replace(/^!?-?/, "").replace(/!$/, "");
    const colour = COLOUR.exec(utility);
    const dropped = DROPPED.find((step) => step.utility.test(utility));
    return [
      ...!own && prefix === "bk" ? [{ ...at, message: `${name} is a class under bk:, which blockr.ui alone writes, for its own components. Write it under the package's own prefix.` }] : [],
      ...colour === null ? [] : [
        {
          ...at,
          message: `${name} names ${colour[1] ?? ""}-${colour[2] ?? ""} of Tailwind's palette, which the build preset drops, so the class writes nothing. Draw with the colour of a job, such as primary or muted-foreground, which a theme and the mode reach.`
        }
      ],
      ...dropped === void 0 ? [] : [
        {
          ...at,
          message: `${name} names a step of the ${dropped.scale} that the theme variables lack, which the build preset drops, so the class writes nothing. Draw with a step that they hold: ${dropped.held}.`
        }
      ]
    ];
  });
}
function themeProblems(stylesheets, code, own) {
  return [...stylesheets.map((file) => stylesheetProblems(file, own)), ...code.map((file) => codeProblems(file, own))].flatMap(
    (found) => found.sort((a, b) => a.line - b.line)
  );
}
function themeError(problems) {
  const lines = problems.map(({ file, line, message }) => `${file}:${String(line)}: ${message}`);
  return new Error(`The build fails themeCheck() of @blockr/ui/build:
${lines.join("\n")}`);
}
async function themeCheck(files) {
  const read = (paths = []) => Promise.all(paths.map(async (name) => ({ name, text: await readFile(name, "utf8") })));
  const problems = themeProblems(await read(files.stylesheets), await read(files.code), false);
  if (problems.length > 0) throw themeError(problems);
}

// srcts/build/vendor-check.ts
var vendorCheck = {
  name: "blockr.ui-vendor-check",
  setup(build) {
    build.initialOptions.metafile = true;
    build.onEnd((result) => ({
      errors: bundledPackages(result.metafile).map(({ name, file }) => ({
        text: name === "lucide" ? `The build put icons of @blockr/ui/icons into ${file}. Bundle the icons that the package imports into its vendor file instead, and import them from there.` : `The build put code of ${name}, from node_modules, into ${file}. Bundle ${name} into the package's vendor file instead, and import it from there.`
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
  themeCheck,
  themeVariables,
  vendorCheck
};
