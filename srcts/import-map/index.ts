// The imports that the page R serves resolves through its import map,
// each to a file of its own in inst/js that blockr.ui ships:
// React's entry points, blockr.ui's components and React Aria's three packages
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.7.4#the-bundle).
// A module's build leaves these imports as they are, through the build preset,
// so that its bundle holds none of them.
// Every module on the page then imports the one copy of each.
// Hooks fail on a second copy of React (https://react.dev/warnings/invalid-hook-call-warning).
// A second copy of React Aria holds React contexts of its own, which blockr.ui's components do not read.

export const REACT_ENTRIES = ["react", "react/jsx-runtime", "react-dom", "react-dom/client"] as const;

// The entry of blockr.ui's components.
export const COMPONENTS = "@blockr/ui";

// React Aria's three packages.
// The package react-aria-components holds React Aria's components,
// react-aria the hooks that the components are built from,
// and react-stately the state that the hooks keep.
export const REACT_ARIA = ["react-aria-components", "react-aria", "react-stately"] as const;

export type ReactAria = (typeof REACT_ARIA)[number];

export const IMPORTS = [...REACT_ENTRIES, COMPONENTS, ...REACT_ARIA] as const;

export type Import = (typeof IMPORTS)[number];

// The file in inst/js that an import resolves to, named after the import,
// as react-dom-client.js for `react-dom/client`, blockr-ui.js for `@blockr/ui`,
// and react-aria.js for `react-aria`.
export function importFile(name: Import): string {
  return `${name.replace(/^@/, "").replaceAll("/", "-")}.js`;
}
