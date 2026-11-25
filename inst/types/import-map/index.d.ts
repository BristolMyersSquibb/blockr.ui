export declare const REACT_ENTRIES: readonly ["react", "react/jsx-runtime", "react-dom", "react-dom/client"];
export declare const COMPONENTS = "@blockr/ui";
export declare const REACT_ARIA: readonly ["react-aria-components", "react-aria", "react-stately"];
export type ReactAria = (typeof REACT_ARIA)[number];
export declare const IMPORTS: readonly ["react", "react/jsx-runtime", "react-dom", "react-dom/client", "@blockr/ui", "react-aria-components", "react-aria", "react-stately"];
export type Import = (typeof IMPORTS)[number];
export declare function importFile(name: Import): string;
