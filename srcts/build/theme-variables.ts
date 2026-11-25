// The list of theme variables: what blockr.ui's stylesheet provides every other package
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.7.4#the-list-of-theme-variables).
// Every package's stylesheet reads blockr.ui's variables.
// Tailwind CSS leaves out of a stylesheet whatever the files that it scans do not use.
// So what blockr.ui provides is this list, rather than whatever its components use.
// The list holds the theme variables by their full names, blockr.ui's fonts and the mode attribute.
// It holds neither blockr.ui's classes, which no other package uses, nor another package's names.
//
// The build preset exports the list as `themeVariables`, for builds.
// Its check, themeCheck() in ./theme-check.ts, fails a build whose stylesheet reads a variable of blockr.ui's that the list lacks.
// The build writes the list into inst/js/theme-variables.json (./build.ts).
// The R function theme_variables() reads that file, so that the lists in R and in JS never disagree.
// The tests compare blockr.ui's compiled stylesheet with the list (./stylesheet.test.ts).
//
// Adding to the list takes a minor release of blockr.ui, and removing or renaming takes a major.
// Each major names one version of Tailwind CSS, which VERSIONS in ./preset.ts holds.
// The file imports nothing, so that its bundle in inst/types holds the list alone.

export const themeVariables = {
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
    "--bk-z-index-overlay",
  ],
  // The fonts whose faces blockr.ui's stylesheet writes, by their families.
  // Open Sans is the default of --bk-font-sans, and the package ships its files.
  fonts: ["Open Sans"],
  // The attribute on the page's root that holds the mode, `light` or `dark`.
  // Core's board option dark_mode sets it, and the dark variant of the preset keys on it
  // (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.7.4#prefixes).
  modeAttribute: "data-bk-mode",
} as const;
