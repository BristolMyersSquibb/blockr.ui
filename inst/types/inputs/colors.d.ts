import type { Color } from "react-aria-components";
import type { Bound } from "./bound.ts";
import type { Json } from "./bound.ts";
export declare const FALLBACK = "#000000";
export declare function hex(color: Color): string;
export declare function valueColor(value: Json | undefined): string | undefined;
export declare const Ended: import("react").Context<((color: Color) => void) | null>;
export declare function useDragged(bound: Bound): {
    readonly shown: Color | string;
    readonly change: (color: Color) => void;
    readonly end: (color: Color) => void;
};
