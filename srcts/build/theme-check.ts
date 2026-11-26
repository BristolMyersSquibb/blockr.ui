// The check of the Tailwind half of the build preset, which blockr.ui ships to packages as themeCheck() in `@blockr/ui/build`
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.7.4#the-build-preset).
// A package's build runs it on the stylesheets that it writes and on its code,
// the files in which it names classes, such as those that Tailwind CSS scans.
// The check fails the build where:
//
// 1. A stylesheet reads a variable of blockr.ui's, under --bk-, that the list of theme variables lacks (./theme-variables.ts),
//    in plain CSS or in an arbitrary value, such as bc:bg-[var(--bk-color-blue)].
//    Only the variables of the list are what blockr.ui's stylesheet provides other packages
//    (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.7.4#the-list-of-theme-variables).
// 2. A stylesheet carries the comment in which Tailwind writes its version, with another version than VERSIONS in ./preset.ts names.
//    The properties that Tailwind registers for its utilities, such as --tw-blur, carry no prefix,
//    so stylesheets that two versions of Tailwind wrote could register them differently.
//    Plain CSS carries no such comment and passes.
// 3. Another package's stylesheet sets a variable under --bk-, as `--bk-color-card: #fff`,
//    in plain CSS or through an arbitrary property, such as bc:[--bk-radius:0].
//    Only blockr.ui's stylesheet sets them, with their defaults.
//    A frontend gives its values in a theme, which its method of frontend_theme() returns in R,
//    and R writes that theme into the page's head
//    (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.7.2#where-the-values-are-set).
//    A value in a stylesheet would escape the frontend's tests of the defaults that it keeps,
//    and a value on an element below :root would win there over the themes of the frontend and of the app.
// 4. The code names a class of a colour of Tailwind's palette, such as bc:bg-blue-600,
//    or of a step of the faces, the sizes of text, the radii or the shadows that the theme variables lack, such as bc:shadow-2xl,
//    under any prefix.
//    The preset drops them (./preset.css), and Tailwind skips a class that it does not know without a word,
//    so such a class writes nothing (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.7.2#colours).
// 5. Another package's code names a class under bk:, which blockr.ui alone writes, for its own components.
//    So those classes come and go with blockr.ui's components
//    (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.7.4#prefixes).
//
// A package that runs no Tailwind, as the dock writes plain CSS, runs the check on its CSS and on its code.
// The build of blockr.ui runs the check on its own stylesheet and code too (./build.ts),
// so that a component of blockr.ui that names a colour of the palette fails it.
// The file imports no npm package, only Node's own modules, as ./preset.ts does.

import { readFile } from "node:fs/promises";
import { VERSIONS } from "./preset.ts";
import { themeVariables } from "./theme-variables.ts";

// A file that the check reads: its name, as an error names it, and its text.
export interface Checked {
  name: string;
  text: string;
}

// What the check finds wrong in a file, at a line.
export interface Problem {
  file: string;
  line: number;
  message: string;
}

// The variables of the list, by their full names.
const LISTED = new Set<string>(themeVariables.variables);

// A read of a variable of blockr.ui's, as var(--bk-color-popover).
const READ = /var\(\s*(--bk-[\w-]+)/g;

// A declaration of a variable of blockr.ui's, as --bk-color-popover: #fff.
const DECLARED = /(?<![\w-])(--bk-[\w-]+)\s*:/g;

// The comment in which Tailwind writes its version at the top of each stylesheet that it compiles,
// as `/*! tailwindcss v4.3.3 | MIT License | https://tailwindcss.com */`.
const TAILWIND = /\/\*!\s*tailwindcss v([^\s*|]+)/g;

// A comment of a stylesheet.
const CSS_COMMENT = /\/\*[\s\S]*?\*\//g;

// The colours of Tailwind's palette, in the order of Tailwind's theme.
// Each comes in the steps from 50 to 950.
// The preset drops all of them, while black and white stay (./preset.css).
const PALETTE = [
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
  "taupe",
];

// A utility that names a colour of the palette, as bg-blue-600, border-x-red-500/50 or inset-shadow-taupe-950,
// with the colour, its step and any modifier after a slash, such as an opacity.
const COLOUR = new RegExp(String.raw`^[a-z][a-z0-9-]*?-(${PALETTE.join("|")})-(50|[1-9]00|950)(?:/.+)?$`);

// The steps of Tailwind's theme that the theme variables lack, by their scale, which the preset drops (./preset.css).
// For each scale, the utilities that name such a step, with any modifier after a slash,
// and the steps that the theme variables hold, which an error names instead.
// The radii come for the whole box, a side or a corner, as rounded, rounded-t and rounded-tl-xs.
const DROPPED = [
  { scale: "faces", utility: /^font-serif$/, held: "font-sans or font-mono" },
  { scale: "sizes of text", utility: /^text-[2-9]?xl(?:\/.+)?$/, held: "text-xs, text-sm, text-base or text-lg" },
  {
    scale: "radii",
    utility: /^rounded(?:-(?:[setrbl]|ss|se|ee|es|tl|tr|br|bl))?(?:-xs)?$/,
    held: "rounded-sm to rounded-4xl, rounded-full or rounded-none",
  },
  { scale: "shadows", utility: /^shadow(?:-(?:2?xs|2?xl|inner))?(?:\/.+)?$/, held: "shadow-sm, shadow-md, shadow-lg or shadow-none" },
];

// A part of a class between two colons: a variant, such as hover or data-[state=open], or the utility.
// What lies in brackets or in parentheses may hold colons and one level of nesting, as [&:hover] and bg-(--x) do.
const PART = String.raw`(?:[^\s"'\x60:()[\]{}<>;,=]|\[(?:[^\s[\]]|\[[^\s[\]]*\])*\]|\((?:[^\s()]|\([^\s()]*\))*\))+`;

// A class under a prefix, as bc:hover:bg-blue-600/50,
// with the prefix, which Tailwind takes in lowercase letters alone, its variants and its utility.
// It starts where nothing of a word, a path or another class comes before it.
const CLASS = new RegExp(String.raw`(?<![\w:/.\\-])([a-z]+):((?:${PART}:)*)(${PART})`, "g");

// The line of a text that a position in it falls on, from 1.
function lineOf(text: string, at: number): number {
  return text.slice(0, at).split("\n").length;
}

// What the check finds wrong in a stylesheet (1 to 3 above).
// In blockr.ui's own build, a stylesheet sets variables of blockr.ui's,
// and may also read those that it sets itself,
// such as those of Tailwind's theme that blockr.ui's classes read under its prefix, as --bk-font-weight-bold.
function stylesheetProblems(file: Checked, own: boolean): Problem[] {
  // The stylesheet with each comment blanked out, all but its line breaks, so that each line keeps its number.
  const rules = file.text.replace(CSS_COMMENT, (comment) => comment.replace(/[^\n]/g, " "));
  const declarations = [...rules.matchAll(DECLARED)];
  const declared = new Set(own ? declarations.map(([, name = ""]) => name) : []);

  const versions = [...file.text.matchAll(TAILWIND)].flatMap(({ 1: version = "", index }) =>
    version === VERSIONS.tailwindcss
      ? []
      : [
          {
            file: file.name,
            line: lineOf(file.text, index),
            message: `Tailwind CSS ${version} wrote the stylesheet, but blockr.ui is written for Tailwind CSS ${VERSIONS.tailwindcss}, which every package's build runs. Install tailwindcss@${VERSIONS.tailwindcss}.`,
          },
        ],
  );
  const reads = [...rules.matchAll(READ)].flatMap(({ 1: name = "", index }) =>
    LISTED.has(name) || declared.has(name)
      ? []
      : [
          {
            file: file.name,
            line: lineOf(rules, index),
            message: `The stylesheet reads ${name}, which the list of theme variables lacks. Read one of the variables that themeVariables in @blockr/ui/build lists.`,
          },
        ],
  );
  const sets = own
    ? []
    : declarations.map(({ 1: name = "", index }) => ({
        file: file.name,
        line: lineOf(rules, index),
        message: `The stylesheet sets ${name}, but only blockr.ui's stylesheet sets variables under --bk-. A frontend gives its values in a theme, which its method of frontend_theme() returns in R.`,
      }));
  return [...versions, ...reads, ...sets];
}

// What the check finds wrong in a file of code (4 and 5 above).
// In blockr.ui's own build, the code may name classes under bk:.
function codeProblems(file: Checked, own: boolean): Problem[] {
  return [...file.text.matchAll(CLASS)].flatMap(({ 0: name, 1: prefix = "", 3: part = "", index }) => {
    const at = { file: file.name, line: lineOf(file.text, index) };
    // The utility without the marks that make it important or negative, as in `bg-red-500!` and `-mt-2`.
    const utility = part.replace(/^!?-?/, "").replace(/!$/, "");
    const colour = COLOUR.exec(utility);
    const dropped = DROPPED.find((step) => step.utility.test(utility));
    return [
      ...(!own && prefix === "bk"
        ? [{ ...at, message: `${name} is a class under bk:, which blockr.ui alone writes, for its own components. Write it under the package's own prefix.` }]
        : []),
      ...(colour === null
        ? []
        : [
            {
              ...at,
              message: `${name} names ${colour[1] ?? ""}-${colour[2] ?? ""} of Tailwind's palette, which the build preset drops, so the class writes nothing. Draw with the colour of a job, such as primary or muted-foreground, which a theme and the mode reach.`,
            },
          ]),
      ...(dropped === undefined
        ? []
        : [
            {
              ...at,
              message: `${name} names a step of the ${dropped.scale} that the theme variables lack, which the build preset drops, so the class writes nothing. Draw with a step that they hold: ${dropped.held}.`,
            },
          ]),
    ];
  });
}

// What the check finds wrong in a build's stylesheets and code, by file in the order given, and by line in each file.
// Here `own` marks the build of blockr.ui itself, which writes the classes under bk: and sets the variables that its classes read.
export function themeProblems(stylesheets: readonly Checked[], code: readonly Checked[], own: boolean): Problem[] {
  return [...stylesheets.map((file) => stylesheetProblems(file, own)), ...code.map((file) => codeProblems(file, own))].flatMap((found) =>
    found.sort((a, b) => a.line - b.line),
  );
}

// The error that fails a build, with a line for each problem, which names its file and line.
export function themeError(problems: readonly Problem[]): Error {
  const lines = problems.map(({ file, line, message }) => `${file}:${String(line)}: ${message}`);
  return new Error(`The build fails themeCheck() of @blockr/ui/build:\n${lines.join("\n")}`);
}

// Fails a package's build where its stylesheets or its code fail one of the checks that the top of this file lists.
// It takes the paths of the stylesheets that the build wrote and of its code, such as the files that Tailwind scanned for classes.
// An error names each file by its path as given.
export async function themeCheck(files: { stylesheets?: readonly string[]; code?: readonly string[] }): Promise<void> {
  const read = (paths: readonly string[] = []): Promise<Checked[]> =>
    Promise.all(paths.map(async (name) => ({ name, text: await readFile(name, "utf8") })));
  const problems = themeProblems(await read(files.stylesheets), await read(files.code), false);
  if (problems.length > 0) throw themeError(problems);
}
