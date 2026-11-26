// @vitest-environment node

// What the check of the Tailwind half of the build preset, themeCheck() of `@blockr/ui/build`, fails a build for:
// a stylesheet that reads a variable of blockr.ui's that the list of theme variables lacks,
// a stylesheet that another version of Tailwind CSS wrote,
// another package's stylesheet that sets a variable under --bk-,
// code that names a class of Tailwind's palette or of a step that the theme variables lack, under any prefix,
// and another package's code that names a class under bk:.
// That the check fails exactly the classes that write nothing under the preset is in ./stylesheet.test.ts,
// which compiles them with Tailwind.

import { mkdtemp, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath, pathToFileURL } from "node:url";
import { afterEach, beforeEach, expect, test } from "vitest";
import type * as Build from "./index.ts";
import { VERSIONS } from "./preset.ts";
import { themeProblems } from "./theme-check.ts";
import type { Problem } from "./theme-check.ts";

const repo = resolve(dirname(fileURLToPath(import.meta.url)), "../..");

// The check as blockr.ui ships it.
const { themeCheck } = (await import(pathToFileURL(join(repo, "inst/types/build/index.js")).href)) as typeof Build;

// Where each problem is, as its file and its line.
function where(problems: readonly Problem[]): string[] {
  return problems.map(({ file, line }) => `${file}:${String(line)}`);
}

test("fails a stylesheet that reads a variable of blockr.ui's that the list of theme variables lacks, in plain CSS or in an arbitrary value", () => {
  const css = {
    name: "view.css",
    text: [
      "/* A read in a comment, var(--bk-color-blue), is no read. */",
      ".a { color: var(--bk-color-primary); }",
      ".b { color: var(--bk-color-blue); }",
      ".bc\\:bg-\\[var\\(--bk-color-x\\)\\] { background-color: var( --bk-color-x, #fff); }",
      ":root { --bk-own: 1px; }",
      ".c { width: var(--bk-own); }",
      "",
    ].join("\n"),
  };
  const problems = themeProblems([css], [], false);

  // Line 5 fails as well, since another package's stylesheet sets no variable under --bk- (below).
  expect(where(problems)).toEqual(["view.css:3", "view.css:4", "view.css:5", "view.css:6"]);
  expect(problems[0]?.message).toBe(
    "The stylesheet reads --bk-color-blue, which the list of theme variables lacks. Read one of the variables that themeVariables in @blockr/ui/build lists.",
  );
  expect(problems.flatMap(({ message }) => /reads (\S+),/.exec(message)?.[1] ?? [])).toEqual(["--bk-color-blue", "--bk-color-x", "--bk-own"]);

  // In blockr.ui's own build, the stylesheet may also read a variable that it sets itself,
  // as it reads those of Tailwind's theme that its classes use, such as --bk-font-weight-bold.
  expect(where(themeProblems([css], [], true))).toEqual(["view.css:3", "view.css:4"]);
});

test("fails another package's stylesheet that sets a variable under --bk-, in plain CSS or through an arbitrary property, but not blockr.ui's own", () => {
  const css = {
    name: "frontend.css",
    text: [
      "/* A frontend gives its values in its theme, where --bk-color-card: #fff would set one. */",
      ':root[data-bk-mode="light"] {',
      "  --bk-color-card: #fafafa;",
      "  color: var(--bk-color-card-foreground);",
      "}",
      ".dock-panel { --bk-radius: 0.75rem; --dock-radius: calc(var(--bk-radius) * 1.5); }",
      ".bc\\:\\[--bk-spacing\\:2px\\] { --bk-spacing: 2px; }",
      "",
    ].join("\n"),
  };
  const problems = themeProblems([css], [], false);

  // A read of a theme variable and a variable of the package's own pass, as does the selector of the arbitrary property.
  expect(where(problems)).toEqual(["frontend.css:3", "frontend.css:6", "frontend.css:7"]);
  expect(problems.map(({ message }) => /sets (\S+),/.exec(message)?.[1])).toEqual(["--bk-color-card", "--bk-radius", "--bk-spacing"]);
  expect(problems[0]?.message).toBe(
    "The stylesheet sets --bk-color-card, but only blockr.ui's stylesheet sets variables under --bk-. A frontend gives its values in a theme, which its method of frontend_theme() returns in R.",
  );

  // The stylesheet of blockr.ui sets the theme variables, with their defaults.
  expect(themeProblems([css], [], true)).toEqual([]);
});

test("fails a stylesheet that another version of Tailwind CSS wrote, naming both versions, and passes plain CSS", () => {
  const banner = (version: string) => `/*! tailwindcss v${version} | MIT License | https://tailwindcss.com */`;
  const plain = { name: "dock.css", text: ".dock { color: var(--bk-color-foreground); }\n" };
  const current = { name: "core.css", text: `${banner(VERSIONS.tailwindcss)}\n.a { display: flex; }\n` };
  // A stylesheet that bundles another, which an older Tailwind wrote, as a vendor stylesheet might.
  const bundled = { name: "vendor.css", text: `${banner(VERSIONS.tailwindcss)}\n.a { display: flex; }\n${banner("4.2.0")}\n.b { display: grid; }\n` };

  expect(themeProblems([plain, current, bundled], [], false)).toEqual([
    {
      file: "vendor.css",
      line: 3,
      message: `Tailwind CSS 4.2.0 wrote the stylesheet, but blockr.ui is written for Tailwind CSS ${VERSIONS.tailwindcss}, which every package's build runs. Install tailwindcss@${VERSIONS.tailwindcss}.`,
    },
  ]);
});

test("fails code that names a colour of Tailwind's palette under any prefix, with variants, an opacity or the mark that makes it important", () => {
  const code = {
    name: "view.tsx",
    text: [
      'export const a = "bc:bg-blue-600";',
      'export const b = cn("bc:flex", "bc:hover:text-gray-900");',
      "export const c = `bc:data-[state=open]:border-x-red-500/50 bc:bg-primary`;",
      'export const d = "xy:[&>svg:hover]:fill-taupe-950!";',
      'export const e = "bc:bg-black/40 bc:ring-white bc:text-muted-foreground bc:bg-primary/10 bc:bg-[#0072b2]";',
      // A class without a prefix is no class of any package's, since each package writes its classes under its own.
      'export const f = "bg-blue-600";',
      "// The colours of Tailwind's palette are at https://tailwindcss.com/docs/colors#blue-500.",
      "",
    ].join("\n"),
  };
  const problems = themeProblems([], [code], false);

  expect(where(problems)).toEqual(["view.tsx:1", "view.tsx:2", "view.tsx:3", "view.tsx:4"]);
  expect(problems.map(({ message }) => message.split(" names ")[0])).toEqual([
    "bc:bg-blue-600",
    "bc:hover:text-gray-900",
    "bc:data-[state=open]:border-x-red-500/50",
    "xy:[&>svg:hover]:fill-taupe-950!",
  ]);
  expect(problems[2]?.message).toBe(
    "bc:data-[state=open]:border-x-red-500/50 names red-500 of Tailwind's palette, which the build preset drops, so the class writes nothing. Draw with the colour of a job, such as primary or muted-foreground, which a theme and the mode reach.",
  );
});

test("fails code that names a step of the faces, the sizes of text, the radii or the shadows that the theme variables lack, and passes the steps that they hold", () => {
  const code = {
    name: "view.tsx",
    text: [
      '"bc:font-serif"',
      '"bc:text-xl" "bc:lg:text-9xl/7"',
      '"bc:rounded"',
      '"bc:rounded-tl-xs" "bc:rounded-s"',
      '"bc:shadow-2xl"',
      '"bc:hover:shadow-inner"',
      [
        '"bc:font-sans bc:font-mono bc:font-bold bc:text-xs bc:text-lg bc:text-sm/6 bc:rounded-lg bc:rounded-t-sm bc:rounded-full bc:rounded-none',
        'bc:shadow-md bc:shadow-none bc:inset-shadow-xs bc:drop-shadow-xl bc:text-shadow-lg"',
      ].join(" "),
      "",
    ].join("\n"),
  };
  const problems = themeProblems([], [code], false);

  expect(where(problems)).toEqual(["view.tsx:1", "view.tsx:2", "view.tsx:2", "view.tsx:3", "view.tsx:4", "view.tsx:4", "view.tsx:5", "view.tsx:6"]);
  expect(problems.at(-1)?.message).toBe(
    "bc:hover:shadow-inner names a step of the shadows that the theme variables lack, which the build preset drops, so the class writes nothing. Draw with a step that they hold: shadow-sm, shadow-md, shadow-lg or shadow-none.",
  );
  expect(problems[0]?.message).toContain("Draw with a step that they hold: font-sans or font-mono.");
  expect(problems[1]?.message).toContain("Draw with a step that they hold: text-xs, text-sm, text-base or text-lg.");
  expect(problems[3]?.message).toContain("Draw with a step that they hold: rounded-sm to rounded-4xl, rounded-full or rounded-none.");
});

test("fails another package's code that names a class under bk:, but not blockr.ui's own", () => {
  const code = {
    name: "view.tsx",
    text: [
      'export const a = "bk:flex";',
      // An object's key and a type are no classes.
      "export const b = { bk: 1 };",
      "export function c(bk: string): string {",
      "  return bk;",
      "}",
      'export const d = "bk:bg-blue-600";',
      "",
    ].join("\n"),
  };

  const problems = themeProblems([], [code], false);

  expect(where(problems)).toEqual(["view.tsx:1", "view.tsx:6", "view.tsx:6"]);
  expect(problems[0]?.message).toBe(
    "bk:flex is a class under bk:, which blockr.ui alone writes, for its own components. Write it under the package's own prefix.",
  );
  expect(problems.slice(1).map(({ message }) => message.split(", ")[0])).toEqual([
    "bk:bg-blue-600 is a class under bk:",
    "bk:bg-blue-600 names blue-600 of Tailwind's palette",
  ]);
  expect(where(themeProblems([], [code], true))).toEqual(["view.tsx:6"]);
});

let dir: string;

beforeEach(async () => {
  dir = await mkdtemp(join(tmpdir(), "blockr-ui-theme-check-"));
});

afterEach(async () => {
  await rm(dir, { recursive: true, force: true });
});

test("as blockr.ui ships it, reads the stylesheets and the code of a package's build by their paths, and fails it with a line for each problem", async () => {
  const css = join(dir, "dock.css");
  const tsx = join(dir, "dock.tsx");
  await writeFile(css, ".dock { color: var(--bk-color-foreground); }\n.pin { color: var(--bk-color-pin); }\n");
  await writeFile(tsx, 'export const pin = "bd:text-amber-500";\n');

  await expect(themeCheck({ stylesheets: [css], code: [tsx] })).rejects.toThrow(
    new Error(
      [
        "The build fails themeCheck() of @blockr/ui/build:",
        `${css}:2: The stylesheet reads --bk-color-pin, which the list of theme variables lacks. Read one of the variables that themeVariables in @blockr/ui/build lists.`,
        `${tsx}:1: bd:text-amber-500 names amber-500 of Tailwind's palette, which the build preset drops, so the class writes nothing. Draw with the colour of a job, such as primary or muted-foreground, which a theme and the mode reach.`,
      ].join("\n"),
    ),
  );

  await writeFile(css, ".dock { color: var(--bk-color-foreground); }\n");
  await writeFile(tsx, 'export const pin = "bd:text-warning-text";\n');
  await expect(themeCheck({ stylesheets: [css], code: [tsx] })).resolves.toBeUndefined();
  await expect(themeCheck({})).resolves.toBeUndefined();
});
