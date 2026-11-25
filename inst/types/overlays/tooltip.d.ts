import type { ReactElement, ReactNode } from "react";
import type { TooltipProps as AriaTooltipProps } from "react-aria-components";
export interface TooltipProps extends Omit<AriaTooltipProps, "children"> {
    readonly children: ReactNode;
}
export declare function Tooltip({ children, className, style, offset, ...props }: TooltipProps): ReactElement;
