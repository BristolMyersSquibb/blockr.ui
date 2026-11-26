import type { ReactElement } from "react";
import type { ColorSwatchPickerItemProps, ColorSwatchPickerProps as AriaColorSwatchPickerProps, ColorSwatchProps } from "react-aria-components";
import type { Bound } from "./bound.ts";
export declare function ColorSwatch({ className, style, ...props }: ColorSwatchProps): ReactElement;
export interface ColorSwatchPickerProps extends AriaColorSwatchPickerProps {
    readonly bound?: Bound;
}
export declare function ColorSwatchPicker({ bound, ...props }: ColorSwatchPickerProps): ReactElement;
export declare function ColorSwatchPickerItem({ className, ...props }: ColorSwatchPickerItemProps): ReactElement;
