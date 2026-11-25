// The build preset that blockr.ui ships to packages, as `@blockr/ui/build`,
// the vendor check beside it, and what each file of a build imports up front,
// which R names in the head of the page that it serves.
// Beside them, the list of theme variables, and the check that a build's stylesheets and code keep to it.

export { preset } from "./preset.ts";
export { staticImports } from "./preload.ts";
export { themeCheck } from "./theme-check.ts";
export { themeVariables } from "./theme-variables.ts";
export { vendorCheck } from "./vendor-check.ts";
