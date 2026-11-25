// @vitest-environment node

// What blockr.ui's stylesheet holds, as the build writes it into inst/js/blockr-ui.css:
// the theme variables with their defaults for light mode and for dark mode, and the faces of Open Sans
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.7.2#the-defaults).
// The tests compare it with the list of theme variables, which blockr.ui declares in ./theme-variables.ts
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.7.4#the-list-of-theme-variables).
// And how the classes of a package read the theme variables through the Tailwind half of the build preset,
// which blockr.ui's own stylesheet imports, and which blockr.ui ships in inst/types as `@blockr/ui/preset.css`
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.7.4#the-build-preset).

import { existsSync } from "node:fs";
import { cp, mkdir, mkdtemp, readdir, readFile, rm, symlink, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { basename, dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import tailwindcss from "@tailwindcss/postcss";
import postcss from "postcss";
import type { Container, Root, Rule } from "postcss";
import { afterAll, beforeAll, expect, test } from "vitest";
import { themeProblems } from "./theme-check.ts";
import { themeVariables } from "./theme-variables.ts";

const repo = resolve(dirname(fileURLToPath(import.meta.url)), "../..");
const shipped = join(repo, "inst", "js", "blockr-ui.css");

// The list of theme variables, which blockr.ui's stylesheet holds whole:
// the variables, the families of the fonts whose faces it writes, and the attribute on the page's root that holds the mode.
const { variables, fonts, modeAttribute } = themeVariables;

// The variables that take a value for each mode, the colours by job and the shadows,
// and those that both modes share: the faces, the sizes of text, the radius, the spacing and the layer of the overlays
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.7.2#which-theme-variables-blockrui-declares).
const BY_MODE: readonly string[] = variables.filter((name) => /^--bk-(color|shadow)-/.test(name));
const SHARED: readonly string[] = variables.filter((name) => !BY_MODE.includes(name));

// The names that the variables give the jobs of the colours, the sizes of text and the shadows, which their classes name,
// as `primary` for --bk-color-primary, which `bc:bg-primary` reads.
function steps(prefix: string): string[] {
  return variables.filter((name) => name.startsWith(prefix)).map((name) => name.slice(prefix.length));
}
const COLOURS = steps("--bk-color-");
const TEXT = steps("--bk-text-");
const SHADOWS = steps("--bk-shadow-");

const ROOTS = [":root", `:root[${modeAttribute}="light"]`, `:root[${modeAttribute}="dark"]`];

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

test("sets each variable of the list, those that both modes share under :root, and the colours and the shadows of each mode under the mode attribute, all inside Tailwind's theme layer", async () => {
  const rules = styleRules(postcss.parse(await readFile(shipped, "utf8")));
  const [shared, light, dark] = ROOTS.map((selector) => ruleFor(rules, selector));

  for (const rule of [shared, light, dark]) expect(rule?.layer).toBe("theme");
  expect(Object.keys(shared?.declarations ?? {}).sort()).toEqual([...SHARED].sort());
  expect(Object.keys(light?.declarations ?? {}).sort()).toEqual(["color-scheme", ...BY_MODE].sort());
  expect(Object.keys(dark?.declarations ?? {}).sort()).toEqual(["color-scheme", ...BY_MODE].sort());
  expect(light?.declarations["color-scheme"]).toBe("light");
  expect(dark?.declarations["color-scheme"]).toBe("dark");

  // The three rules set the whole list, and no other variable of blockr.ui's.
  const set = new Set([shared, light, dark].flatMap((rule) => Object.keys(rule?.declarations ?? {})).filter((property) => property.startsWith("--bk-")));
  expect([...set].sort()).toEqual([...variables].sort());

  // No rule outside the layer sets a theme variable,
  // so that a frontend's value, which it sets outside any layer, wins over each default, whichever stylesheet loads first.
  const outside = rules.filter((rule) => rule.layer !== "theme").flatMap((rule) => Object.keys(rule.declarations));
  expect(outside.filter((property) => property.startsWith("--bk-"))).toEqual([]);
});

test("writes a face for each file of the fonts of the list, Open Sans, which reads the file beside the stylesheet, with Open Sans first in the sans face", async () => {
  const css = postcss.parse(await readFile(shipped, "utf8"));
  const files = (await readdir(join(repo, "srcts", "fonts"))).filter((file) => file.endsWith(".woff2"));
  const faces: Record<string, string>[] = [];
  css.walkAtRules("font-face", (face) => {
    faces.push(declarations(face));
  });

  // R serves the files beside the stylesheet with it, where the build copies the files of Open Sans.
  const urls = faces.map((face) => /url\("?([^")]+)"?\)/.exec(face.src ?? "")?.[1] ?? "");
  expect([...urls].sort()).toEqual([...files].sort());
  for (const url of urls) expect(existsSync(resolve(dirname(shipped), url)), url).toBe(true);
  expect([...new Set(faces.map((face) => face["font-family"]?.replaceAll('"', "")))]).toEqual([...fonts]);
  for (const [i, face] of faces.entries()) {
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

// The forms of a radius: for the whole box, for each side and for each corner, as rounded, rounded-t and rounded-tl.
const SIDES = ["", "-s", "-e", "-t", "-r", "-b", "-l", "-ss", "-se", "-ee", "-es", "-tl", "-tr", "-br", "-bl"];

// The utilities that read a colour, one of which each colour of the palette takes in turn below.
const COLOURED = [
  "bg",
  "text",
  "border",
  "border-x",
  "ring",
  "ring-offset",
  "inset-ring",
  "outline",
  "decoration",
  "fill",
  "stroke",
  "accent",
  "caret",
  "from",
  "via",
  "to",
  "shadow",
  "inset-shadow",
  "drop-shadow",
  "text-shadow",
];

// A class under core's prefix for each colour of Tailwind's palette and each step of its faces, sizes of text, radii and shadows,
// as the theme of the Tailwind in node_modules holds them.
async function tailwindClasses(): Promise<string[]> {
  const theme = await readFile(join(repo, "node_modules", "tailwindcss", "theme.css"), "utf8");
  const names = (pattern: RegExp): string[] => [...theme.matchAll(pattern)].map(([, name = ""]) => name);
  return [
    ...names(/--color-([a-z]+-\d+):/g).map((colour, i) => `bc:${COLOURED[i % COLOURED.length] ?? "bg"}-${colour}`),
    ...names(/--font-([a-z]+):/g).map((face) => `bc:font-${face}`),
    ...names(/--text-([a-z0-9]+):/g).map((size) => `bc:text-${size}`),
    ...names(/--radius(-[a-z0-9]+)?:/g).flatMap((step) => SIDES.map((side) => `bc:rounded${side}${step}`)),
    ...names(/--shadow(-[a-z0-9]+)?:/g).map((step) => `bc:shadow${step}`),
  ];
}

test("themeCheck() fails exactly the classes that write nothing, of each colour of Tailwind's palette and each step of its faces, sizes of text, radii and shadows", async () => {
  // Black and white stay, for what must stand out on any colour, and so do the classes that read no variable.
  const kept = ["bc:bg-black/40", "bc:ring-white", "bc:bg-transparent", "bc:bg-current", "bc:rounded-full", "bc:rounded-none", "bc:shadow-none"];
  const classes = [...(await tailwindClasses()), ...kept];
  const rules = await packageRules(classes);
  const written = new Set(rules.flatMap((rule) => rule.selectors));
  // Each class on a line of its own, so that the line of a problem gives the class.
  const code = { name: "view.tsx", text: classes.map((name) => `"${name}"`).join("\n") };
  const failed = new Set(themeProblems([], [code], false).map(({ line }) => classes[line - 1]));

  // A class that writes a rule and that the check fails, or one that writes none and that the check passes.
  expect(classes.filter((name) => written.has(selector(name)) === failed.has(name))).toEqual([]);
  expect(classes.filter((name) => /-\d+$/.test(name)).every((name) => failed.has(name))).toBe(true);
  for (const name of kept) expect(written).toContain(selector(name));
  expect(ruleFor(rules, selector("bc:ring-white")).declarations).toEqual({ "--tw-ring-color": "#fff" });
  expect(ruleFor(rules, selector("bc:rounded-full")).declarations["border-radius"]).not.toContain("var(");
});

test("the preset reads each variable of the list, and no other variable of blockr.ui's", async () => {
  const preset = (await readFile(join(repo, "srcts", "build", "preset.css"), "utf8")).replace(/\/\*[\s\S]*?\*\//g, "");
  const read = new Set([...preset.matchAll(/var\((--bk-[\w-]+)/g)].map(([, name = ""]) => name));

  expect([...read].sort()).toEqual([...variables].sort());
});

test("a package's dark variant applies under the mode attribute of the list, data-bk-mode=\"dark\", on the page's root", async () => {
  const rules = await packageRules(["bc:dark:bg-card"]);
  const dark = `:root[${modeAttribute}="dark"]`;

  expect(ruleFor(rules, `.bc\\:dark\\:bg-card:where(${dark}, ${dark} *)`).declarations).toEqual({
    "background-color": "var(--bk-color-card)",
  });
});
