// @vitest-environment node

// What blockr.ui's stylesheet holds, as the build writes it into inst/js/blockr-ui.css:
// the theme variables with their defaults for light mode and for dark mode, and the faces of Open Sans
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.7.2#the-defaults).
// And how the classes of a package read the theme variables through the Tailwind half of the build preset,
// which blockr.ui's own stylesheet imports, and which blockr.ui ships in inst/types as `@blockr/ui/preset.css`
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.7.4#the-build-preset).

import { cp, mkdir, mkdtemp, readdir, readFile, rm, symlink, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { basename, dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import tailwindcss from "@tailwindcss/postcss";
import postcss from "postcss";
import type { Container, Root, Rule } from "postcss";
import { afterAll, beforeAll, expect, test } from "vitest";

const repo = resolve(dirname(fileURLToPath(import.meta.url)), "../..");
const shipped = join(repo, "inst", "js", "blockr-ui.css");

// The theme variables, which The theme variables lists
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.7.2#which-theme-variables-blockrui-declares):
// the colours by job and the shadows, which take a value for each mode,
// and the faces, the sizes of text, the radius, the spacing and the layer of the overlays, which both modes share.
const COLOURS = [
  "background",
  "foreground",
  "card",
  "card-foreground",
  "popover",
  "popover-foreground",
  "primary",
  "primary-foreground",
  "primary-text",
  "secondary",
  "secondary-foreground",
  "muted",
  "muted-foreground",
  "accent",
  "accent-foreground",
  "destructive",
  "destructive-foreground",
  "destructive-text",
  "warning",
  "warning-foreground",
  "warning-text",
  "success",
  "success-foreground",
  "success-text",
  "border",
  "input",
  "ring",
];
const SHADOWS = ["sm", "md", "lg"];
const TEXT = ["xs", "sm", "base", "lg"];
const BY_MODE = [...COLOURS.map((job) => `--bk-color-${job}`), ...SHADOWS.map((step) => `--bk-shadow-${step}`)];
const SHARED = [
  "--bk-font-sans",
  "--bk-font-mono",
  ...TEXT.map((step) => `--bk-text-${step}`),
  "--bk-radius",
  "--bk-spacing",
  "--bk-z-index-overlay",
];

const ROOTS = [":root", ':root[data-bk-mode="light"]', ':root[data-bk-mode="dark"]'];

// A rule of a stylesheet: the name of the cascade layer that it sits in, if any,
// its selectors, and its declarations, each value by its property.
interface StyleRule {
  layer: string | undefined;
  selectors: string[];
  declarations: Record<string, string>;
}

function declarations(node: Container): Record<string, string> {
  const found: Record<string, string> = {};
  node.each((child) => {
    if (child.type === "decl") found[child.prop] = child.value;
  });
  return found;
}

// The rules of a stylesheet, wherever they sit.
// Its layers are walked from the outside in, so that the innermost layer around a rule names it last.
function styleRules(root: Root): StyleRule[] {
  const layers = new Map<Rule, string>();
  root.walkAtRules("layer", (layer) => {
    layer.walkRules((rule) => {
      layers.set(rule, layer.params);
    });
  });
  const rules: StyleRule[] = [];
  root.walkRules((rule) => {
    rules.push({ layer: layers.get(rule), selectors: rule.selectors, declarations: declarations(rule) });
  });
  return rules;
}

// Whether a rule has the one selector given.
function selects(rule: StyleRule, selector: string): boolean {
  return rule.selectors.length === 1 && rule.selectors[0] === selector;
}

// The rule with the one selector given, which a test expects once.
function ruleFor(rules: StyleRule[], selector: string): StyleRule {
  const found = rules.filter((rule) => selects(rule, selector));
  expect(found, selector).toHaveLength(1);
  const [rule = { layer: undefined, selectors: [], declarations: {} }] = found;
  return rule;
}

// Builds a stylesheet with Tailwind CSS, as the build does, from its text and the path that it resolves from.
async function compile(text: string, from: string): Promise<Root> {
  const result = await postcss([tailwindcss({ base: dirname(from), optimize: { minify: false } })]).process(text, { from });
  return postcss.parse(result.css);
}

test("sets the defaults that both modes share under :root, and the colours and the shadows of each mode under its attribute, all inside Tailwind's theme layer", async () => {
  const rules = styleRules(postcss.parse(await readFile(shipped, "utf8")));
  const [shared, light, dark] = ROOTS.map((selector) => ruleFor(rules, selector));

  for (const rule of [shared, light, dark]) expect(rule?.layer).toBe("theme");
  expect(Object.keys(shared?.declarations ?? {}).sort()).toEqual([...SHARED].sort());
  expect(Object.keys(light?.declarations ?? {}).sort()).toEqual(["color-scheme", ...BY_MODE].sort());
  expect(Object.keys(dark?.declarations ?? {}).sort()).toEqual(["color-scheme", ...BY_MODE].sort());
  expect(light?.declarations["color-scheme"]).toBe("light");
  expect(dark?.declarations["color-scheme"]).toBe("dark");

  // No rule outside the layer sets a theme variable,
  // so that a frontend's value, which it sets outside any layer, wins over each default, whichever stylesheet loads first.
  const outside = rules.filter((rule) => rule.layer !== "theme").flatMap((rule) => Object.keys(rule.declarations));
  expect(outside.filter((property) => property.startsWith("--bk-"))).toEqual([]);
});

test("writes a face for each file of Open Sans that blockr.ui ships, at its URL from inst/js, with Open Sans first in the sans face", async () => {
  const css = postcss.parse(await readFile(shipped, "utf8"));
  const fonts = join(repo, "inst", "assets", "fonts");
  const files = (await readdir(fonts)).filter((file) => file.endsWith(".woff2"));
  const faces: Record<string, string>[] = [];
  css.walkAtRules("font-face", (face) => {
    faces.push(declarations(face));
  });

  const urls = faces.map((face) => /url\("?([^")]+)"?\)/.exec(face.src ?? "")?.[1] ?? "");
  expect(urls.map((url) => resolve(dirname(shipped), url)).sort()).toEqual(files.map((file) => join(fonts, file)).sort());
  for (const [i, face] of faces.entries()) {
    expect(face["font-family"]?.replaceAll('"', "")).toBe("Open Sans");
    expect(face["font-style"]).toBe(urls[i]?.endsWith("-italic.woff2") ? "italic" : "normal");
    expect(face["font-weight"]).toBe("300 800");
  }

  // Where the ranges of two faces overlap, the browser tries the face declared last first,
  // so each style lists math first, as Google Fonts' stylesheet does.
  for (const style of ["normal", "italic"]) {
    const scripts = urls.filter((url) => url.endsWith(`-${style}.woff2`)).map((url) => basename(url).replace(/^open-sans-|-(normal|italic)\.woff2$/g, ""));
    expect(scripts).toEqual(["math", "greek", "latin-ext", "latin"]);
  }

  const shared = ruleFor(styleRules(css), ":root");
  expect(shared.declarations["--bk-font-sans"]).toMatch(/^"Open Sans", /);
});

test("writes blockr.ui's classes outside any cascade layer, reading the theme variables, and no rule for an element of the page, such as Tailwind's reset", async () => {
  // The stylesheet of blockr.ui, as if a component drew with these classes.
  const source = join(repo, "srcts", "index.css");
  const text = `${await readFile(source, "utf8")}\n@source inline("bk:bg-popover bk:shadow-md bk:rounded-lg bk:font-bold");\n`;
  const rules = styleRules(await compile(text, source));

  expect(ruleFor(rules, ".bk\\:bg-popover")).toEqual({
    layer: undefined,
    selectors: [".bk\\:bg-popover"],
    declarations: { "background-color": "var(--bk-color-popover)" },
  });
  expect(ruleFor(rules, ".bk\\:rounded-lg").declarations).toEqual({ "border-radius": "var(--bk-radius)" });
  expect(ruleFor(rules, ".bk\\:shadow-md").declarations["--tw-shadow"]).toBe("var(--bk-shadow-md)");

  // Tailwind writes no theme variable of its own,
  // as `--bk-color-popover: var(--bk-color-popover)`, a variable that refers to itself.
  // It writes those of its theme that the theme variables do not cover, such as the weights of the faces.
  const written = rules.filter((rule) => !ROOTS.some((root) => selects(rule, root)));
  const variables = written.flatMap((rule) => Object.keys(rule.declarations)).filter((property) => property.startsWith("--bk-"));
  expect(variables).toEqual(["--bk-font-weight-bold"]);

  // Every rule outside the theme variables' is a class of blockr.ui's, under bk:,
  // or Tailwind's own, which registers the properties of its utilities.
  for (const rule of written) {
    if (rule.layer === "theme" || rule.layer === "properties") continue;
    for (const selector of rule.selectors) expect(selector).toMatch(/^\.bk\\:/);
  }
});

// A package that builds its stylesheet against its copy of blockr.ui's types,
// installed as core's `install_js_types()` installs them, with the Tailwind CSS that it runs itself.
let pkg: string;

beforeAll(async () => {
  pkg = await mkdtemp(join(tmpdir(), "blockr-ui-stylesheet-"));
  await cp(join(repo, "inst", "types"), join(pkg, "node_modules", "@blockr", "ui"), { recursive: true });
  await symlink(join(repo, "node_modules", "tailwindcss"), join(pkg, "node_modules", "tailwindcss"), "dir");
});

afterAll(async () => {
  await rm(pkg, { recursive: true, force: true });
});

// Builds the package's stylesheet, as the build preset gives it under core's prefix,
// with the classes given in place of the files that it scans.
async function packageRules(classes: string[]): Promise<StyleRule[]> {
  const file = join(pkg, "src", "index.css");
  const text = [
    "@layer theme;",
    '@import "tailwindcss/theme.css" layer(theme) prefix(bc);',
    '@import "tailwindcss/utilities.css" source(none);',
    '@import "@blockr/ui/preset.css";',
    `@source inline("${classes.join(" ")}");`,
    "",
  ].join("\n");
  await mkdir(dirname(file), { recursive: true });
  await writeFile(file, text);
  return styleRules(await compile(text, file));
}

// The selector of a class, as Tailwind writes it.
function selector(name: string): string {
  return `.${name.replaceAll(":", "\\:").replaceAll("/", "\\/")}`;
}

test("a package's classes read the theme variables under the package's own prefix, through its copy of the preset", async () => {
  const radii = { sm: "calc(var(--bk-radius) * .6)", md: "calc(var(--bk-radius) * .8)", lg: "var(--bk-radius)", xl: "calc(var(--bk-radius) * 1.4)", "2xl": "calc(var(--bk-radius) * 1.8)", "3xl": "calc(var(--bk-radius) * 2.2)", "4xl": "calc(var(--bk-radius) * 2.6)" };
  const lineHeights = { xs: "calc(1 / .75)", sm: "calc(1.25 / .875)", base: "calc(1.5 / 1)", lg: "calc(1.75 / 1.125)" };
  const rules = await packageRules([
    ...COLOURS.map((job) => `bc:bg-${job}`),
    "bc:font-sans",
    "bc:font-mono",
    ...TEXT.map((step) => `bc:text-${step}`),
    ...Object.keys(radii).map((step) => `bc:rounded-${step}`),
    ...SHADOWS.map((step) => `bc:shadow-${step}`),
    "bc:px-2",
    "bc:z-overlay",
  ]);
  const read = (name: string) => ruleFor(rules, selector(name)).declarations;

  for (const job of COLOURS) expect(read(`bc:bg-${job}`)).toEqual({ "background-color": `var(--bk-color-${job})` });
  expect(read("bc:font-sans")).toEqual({ "font-family": "var(--bk-font-sans)" });
  expect(read("bc:font-mono")).toEqual({ "font-family": "var(--bk-font-mono)" });
  // Each size of text keeps Tailwind's line height, a ratio to the size, which follows the size.
  for (const [step, ratio] of Object.entries(lineHeights)) {
    expect(read(`bc:text-${step}`)).toEqual({ "font-size": `var(--bk-text-${step})`, "line-height": `var(--tw-leading, ${ratio})` });
  }
  for (const [step, radius] of Object.entries(radii)) expect(read(`bc:rounded-${step}`)).toEqual({ "border-radius": radius });
  for (const step of SHADOWS) expect(read(`bc:shadow-${step}`)["--tw-shadow"]).toBe(`var(--bk-shadow-${step})`);
  expect(read("bc:px-2")).toEqual({ "padding-inline": "calc(var(--bk-spacing) * 2)" });
  expect(read("bc:z-overlay")).toEqual({ "z-index": "var(--bk-z-index-overlay)" });

  // With `reference`, the package's stylesheet writes none of the theme variables, which blockr.ui's stylesheet alone sets.
  const variables = rules.flatMap((rule) => Object.keys(rule.declarations)).filter((property) => property.startsWith("--bk-"));
  expect(variables).toEqual([]);
});

test("a class of a step that the theme variables lack writes nothing, while black, white, a pill's radius and no shadow stay", async () => {
  const lacking = [
    "bc:bg-blue-600",
    "bc:text-gray-900",
    "bc:bg-red-500/10",
    "bc:font-serif",
    "bc:text-xl",
    "bc:rounded",
    "bc:rounded-xs",
    "bc:shadow",
    "bc:shadow-xs",
    "bc:shadow-2xl",
    "bc:shadow-inner",
  ];
  const kept = ["bc:bg-black/40", "bc:ring-white", "bc:rounded-full", "bc:rounded-none", "bc:shadow-none", "bc:bg-transparent"];
  const rules = await packageRules([...lacking, ...kept]);
  const written = rules.flatMap((rule) => rule.selectors);

  for (const name of lacking) expect(written).not.toContain(selector(name));
  for (const name of kept) expect(written).toContain(selector(name));
  expect(ruleFor(rules, selector("bc:ring-white")).declarations).toEqual({ "--tw-ring-color": "#fff" });
  expect(ruleFor(rules, selector("bc:rounded-full")).declarations["border-radius"]).not.toContain("var(");
});

test("a package's dark variant applies under data-bk-mode=\"dark\" on the page's root", async () => {
  const rules = await packageRules(["bc:dark:bg-card"]);

  expect(ruleFor(rules, '.bc\\:dark\\:bg-card:where(:root[data-bk-mode="dark"], :root[data-bk-mode="dark"] *)').declarations).toEqual({
    "background-color": "var(--bk-color-card)",
  });
});
