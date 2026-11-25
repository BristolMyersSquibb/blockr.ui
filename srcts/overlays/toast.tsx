// The toasts of blockr.ui: React Aria's ToastQueue, ToastRegion, Toast
// and ToastContent (https://react-aria.adobe.com/Toast), drawn with the theme variables,
// after the Toast of React Aria's Tailwind starter kit
// (https://github.com/adobe/react-spectrum/blob/react-aria-components%401.22.1/starters/tailwind/src/Toast.tsx).
// React Aria 1.22 still names them UNSTABLE_, so a minor release may change
// them, and blockr.ui names them without the prefix, as the starter kit does.
// Each takes the props of its part of React Aria.
//
// A UI keeps its toasts in a ToastQueue, adds one with `add()`, which takes
// what the toast shows and returns the toast's key, and closes one with
// `close()`, given the key.
// A toast added with a `timeout`, in milliseconds, closes once it is up, and
// fades in its last 300 milliseconds, but not while the user prefers reduced
// motion. One added without a timeout stays until the user or the UI closes
// it.
// Its `onClose` runs whichever way it closes.
//
// The region draws the toasts of a queue in the window's bottom right corner,
// one above the other, the latest lowest, each as the function that it is given
// draws the toast.
// It is a landmark, which F6 moves the keyboard's focus to, and it lets clicks
// through between the toasts, so that the page below takes them.
// The column scrolls where the toasts outgrow the window, and its padding keeps
// their shadows from being cut off there.
// While the pointer rests on the region or the focus is in it, React Aria
// pauses the timeout of every toast, and the fade with it.
// React Aria marks the region as a layer of its own, which a modal leaves
// within the user's reach, so that the user can still close a toast while a
// dialog is open.
// The region sits one above the overlays, at --bk-z-index-overlay and one
// (./stacking.ts).
// React Aria draws it in a portal at the end of the page's body only while it
// holds a toast, so it can come before a modal on the page, and would otherwise
// show beneath the modal's backdrop.
//
// A toast draws its ToastContent, which holds what it says, with a title and a
// description in React Aria's Text of the slots "title" and "description",
// beside a Button of blockr.ui of the slot "close", which closes the toast.
// React Aria makes the content an alert, which a screen reader reads out at
// once, unless its `role` says otherwise, as "status" does.
// A toast draws in the colours of an overlay, `popover` and
// `popover-foreground`, with the edge of an outline, `border`, and the shadow
// of what floats over the page, `shadow-md`.

import type { CSSProperties, ReactElement } from "react";
import { composeRenderProps, UNSTABLE_Toast as AriaToast, UNSTABLE_ToastRegion as AriaToastRegion } from "react-aria-components";
import type { ToastProps, ToastRegionProps } from "react-aria-components";

export { UNSTABLE_ToastContent as ToastContent, UNSTABLE_ToastQueue as ToastQueue } from "react-aria-components";
export type { QueuedToast } from "react-aria-components";

// How long a toast takes to fade, in milliseconds, at the end of its timeout.
const FADES_FOR = 300;

export function ToastRegion<T>({ className, ...props }: ToastRegionProps<T>): ReactElement {
  return <AriaToastRegion {...props} className={composeRenderProps(className, (given) => `${REGION} ${given ?? ""}`)} />;
}

export function Toast<T>({ className, style, ...props }: ToastProps<T>): ReactElement {
  const { timeout } = props.toast;
  const fade: CSSProperties | undefined =
    timeout === undefined ? undefined : { animationDelay: `${String(Math.max(0, timeout - FADES_FOR))}ms` };
  return (
    <AriaToast
      {...props}
      style={composeRenderProps(style, (given: CSSProperties | undefined) => ({ ...fade, ...given }))}
      className={composeRenderProps(className, (given) => `${TOAST} ${timeout === undefined ? "" : FADE} ${given ?? ""}`)}
    />
  );
}

const REGION =
  "bk:group/toasts bk:pointer-events-none bk:fixed bk:right-2 bk:bottom-2 bk:z-[calc(var(--bk-z-index-overlay)+1)] bk:m-0 bk:box-border bk:flex bk:max-h-[calc(100dvh-(--spacing(4)))] bk:w-84 bk:max-w-[calc(100vw-(--spacing(4)))] bk:flex-col-reverse bk:gap-2 bk:overflow-y-auto bk:p-2 bk:font-sans bk:outline-none bk:data-focus-visible:outline-2 bk:data-focus-visible:outline-ring";
const TOAST =
  "bk:pointer-events-auto bk:box-border bk:flex bk:items-start bk:gap-2 bk:rounded-md bk:border bk:border-border bk:bg-popover bk:px-3 bk:py-2 bk:text-sm bk:text-popover-foreground bk:shadow-md bk:outline-none bk:data-focus-visible:outline-2 bk:data-focus-visible:outline-offset-2 bk:data-focus-visible:outline-ring";
// The fade of a toast with a timeout, in its last 300 milliseconds, which
// pauses as React Aria pauses the timeout: while the pointer rests on the
// region or the focus is in it (../index.css).
// The toast's style gives the fade its delay, the timeout less the fade.
const FADE =
  "bk:toast-fade bk:group-data-hovered/toasts:[animation-play-state:paused] bk:group-focus-within/toasts:[animation-play-state:paused]";
