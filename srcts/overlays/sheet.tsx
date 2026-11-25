// The sheet of blockr.ui:
// React Aria's SheetOverlay, Sheet, SheetBackdrop and SheetContent
// (https://react-aria.adobe.com/Sheet), drawn as one, drawn with the theme
// variables, after the Sheet of React Aria's Tailwind starter kit
// (https://github.com/adobe/react-spectrum/blob/react-aria-components%401.22.1/starters/tailwind/src/Sheet.tsx).
// It takes the props of React Aria's SheetOverlay, such as `position`, the edge
// of the window that the sheet slides in from, the bottom by default, and
// `snapPoints`, and the starter kit's `overscrollPadding`, true by default,
// which makes the sheet seem to go on past that edge as the user drags it
// beyond.
// It is a modal dialog, as blockr.ui's Modal is, which opens from a
// DialogTrigger, holds its children in a dialog, keeps the focus inside it and
// makes the rest of the page inert while it is open.
// The user closes it by dragging it back out, by Escape, or by a press on the
// backdrop.
// Only the sheet at the bottom of a stack of sheets darkens the page, and a
// sheet that opens over another moves the other back.
// React Aria draws it in a portal at the end of the page's body.
// It stacks at the z-index of the overlays, --bk-z-index-overlay
// (./stacking.ts).
// It draws in the colours of an overlay, `popover` and `popover-foreground`,
// with the edge of an outline, `border`, and the shadow of a dialog,
// `shadow-lg`.
// The backdrop fades in as the sheet slides in, and a sheet that another
// opens over moves back, through the motions bk-sheet-backdrop and
// bk-sheet-scale-back of blockr.ui's stylesheet (../index.css).

import type { ReactElement } from "react";
import {
  composeRenderProps,
  Sheet as AriaSheet,
  SheetBackdrop,
  SheetContent,
  SheetOverlay,
} from "react-aria-components";
import type { DialogProps, SheetOverlayProps } from "react-aria-components";

export interface SheetProps extends Omit<SheetOverlayProps, "children"> {
  // What the sheet holds, which it draws in a dialog.
  readonly children?: DialogProps["children"];
  readonly overscrollPadding?: boolean;
}

export function Sheet({ children, className, overscrollPadding = true, ...props }: SheetProps): ReactElement {
  const { snapPoints } = props;
  return (
    <SheetOverlay {...props} className={composeRenderProps(className, (given) => `bk:group/sheet bk:z-overlay ${given ?? ""}`)}>
      <SheetBackdrop
        className={BACKDROP}
        swipeAnimation="bk-sheet-backdrop"
        swipeAnimationRange={snapPoints === undefined ? undefined : { start: snapPoints.length - 1 }}
      />
      <AriaSheet
        overscrollPadding={overscrollPadding}
        stackAnimation="bk-sheet-scale-back"
        className={({ position }) => `${SHEET} ${POSITIONS[position]}`}
      >
        <SheetContent className={CONTENT}>{children}</SheetContent>
      </AriaSheet>
    </SheetOverlay>
  );
}

// The backdrop darkens the page under the first sheet of a stack alone.
const BACKDROP = "bk:data-[stack-index=0]:bg-black/40";
// The sheet sets up a space in three dimensions, in which a sheet that opens
// over it moves it back.
const SHEET =
  "bk:z-[1] bk:overflow-clip bk:rounded-2xl bk:border bk:border-border bk:bg-popover bk:font-sans bk:text-popover-foreground bk:shadow-lg bk:outline-hidden bk:will-change-transform bk:[transform:perspective(1000px)] bk:forced-colors:bg-[Canvas]";
const POSITIONS: Readonly<Record<"bottom" | "top" | "center" | "left" | "right", string>> = {
  bottom: "bk:max-h-[calc(100%-(--spacing(8)))] bk:w-[calc(100%-(--spacing(4)))] bk:max-w-200 bk:origin-[center_-150px]",
  top: "bk:max-h-[calc(100%-(--spacing(8)))] bk:w-[calc(100%-(--spacing(4)))] bk:max-w-200 bk:origin-[center_150px]",
  center: "bk:max-h-[calc(100%-(--spacing(8)))] bk:w-[calc(100%-(--spacing(4)))] bk:max-w-200 bk:origin-[center_-150px]",
  left: "bk:h-[calc(100%-(--spacing(4)))] bk:w-75 bk:origin-[150px_center]",
  right: "bk:h-[calc(100%-(--spacing(4)))] bk:w-75 bk:origin-[-150px_center]",
};
// The content scrolls once the sheet is open in full. Before then, a swipe on
// it moves the sheet, as on iOS.
const CONTENT =
  "bk:box-border bk:h-full bk:overflow-hidden bk:overscroll-auto bk:p-6 bk:pb-[calc(--spacing(6)+var(--sheet-scroll-padding-y))] bk:outline-hidden bk:[scrollbar-gutter:stable] bk:group-data-[expanded]/sheet:overflow-auto";
