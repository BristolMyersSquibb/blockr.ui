import type { ReactElement } from "react";
import type { DialogProps, SheetOverlayProps } from "react-aria-components";
export interface SheetProps extends Omit<SheetOverlayProps, "children"> {
    readonly children?: DialogProps["children"];
    readonly overscrollPadding?: boolean;
}
export declare function Sheet({ children, className, overscrollPadding, ...props }: SheetProps): ReactElement;
