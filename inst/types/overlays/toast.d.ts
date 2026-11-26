import type { ReactElement } from "react";
import type { ToastProps, ToastRegionProps } from "react-aria-components";
export { UNSTABLE_ToastContent as ToastContent, UNSTABLE_ToastQueue as ToastQueue } from "react-aria-components";
export type { QueuedToast } from "react-aria-components";
export declare function ToastRegion<T>({ className, ...props }: ToastRegionProps<T>): ReactElement;
export declare function Toast<T>({ className, style, ...props }: ToastProps<T>): ReactElement;
