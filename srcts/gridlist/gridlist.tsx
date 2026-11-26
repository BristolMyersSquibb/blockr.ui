// The grid list of blockr.ui: React Aria's GridList, GridListItem,
// GridListSection and GridListHeader (https://react-aria.adobe.com/GridList),
// drawn with the theme variables, after the GridList of React Aria's Tailwind
// starter kit
// (https://github.com/adobe/react-spectrum/blob/react-aria-components%401.22.1/starters/tailwind/src/GridList.tsx).
// Each takes the props of its part of React Aria.
// A grid list shows its items one below the other, as core's lists of
// entries looked before React Aria drew them in core's React SDK, from which
// it came to blockr.ui.
// It draws no frame, so that it adds no frame inside the card or the panel
// that holds it.
// Where its `orientation` is "horizontal", it shows its items side by side.
// Where it has no item, it shows what `renderEmptyState` gives.
// The user moves the focus through its items by the arrow keys.
// A press or Enter runs the item's `onAction`, or the list's.
// Where the list's `selectionMode` is "single" or "multiple", each item
// starts with a checkbox that selects it.
// Where the list's `dragAndDropHooks` let its items drag, each item starts
// with a button that drags it.
// A keyboard user drags the item from that button too.
// A section groups items under the header that it holds.

import type { HTMLAttributes, ReactElement } from "react";
import {
  composeRenderProps,
  GridList as AriaGridList,
  GridListHeader as AriaGridListHeader,
  GridListItem as AriaGridListItem,
  GridListSection,
} from "react-aria-components";
import type { GridListItemProps, GridListProps } from "react-aria-components";
import { DragButton } from "../dnd/index.ts";
import { Checkbox } from "../inputs/index.ts";

export { GridListSection };

export function GridList<T extends object>(props: GridListProps<T>): ReactElement {
  return <AriaGridList {...props} className={composeRenderProps(props.className, (given) => `${LIST} ${given ?? ""}`)} />;
}

export function GridListItem({ children, ...props }: GridListItemProps): ReactElement {
  const textValue = props.textValue ?? (typeof children === "string" ? children : undefined);
  return (
    <AriaGridListItem {...props} textValue={textValue} className={composeRenderProps(props.className, (given) => `${ITEM} ${given ?? ""}`)}>
      {composeRenderProps(children, (children, { selectionMode, selectionBehavior, allowsDragging }) => (
        <>
          {allowsDragging && <DragButton />}
          {selectionMode !== "none" && selectionBehavior === "toggle" && <Checkbox slot="selection" />}
          {children}
        </>
      ))}
    </AriaGridListItem>
  );
}

export function GridListHeader({ className, ...props }: HTMLAttributes<HTMLElement>): ReactElement {
  return <AriaGridListHeader {...props} className={`${HEADER} ${className ?? ""}`} />;
}

// The classes of the list and its parts.
// A list without items says why in muted text, where an item would stand.
const LIST =
  "bk:grid bk:content-start bk:gap-0.5 bk:overflow-auto bk:font-sans bk:outline-none bk:data-[orientation=horizontal]:flex bk:data-[orientation=horizontal]:flex-row bk:data-[orientation=horizontal]:flex-nowrap bk:data-empty:px-2 bk:data-empty:py-0.5 bk:data-empty:text-sm bk:data-empty:text-muted-foreground bk:data-focus-visible:outline-2 bk:data-focus-visible:outline-ring";
// The item under the pointer shows on the fill of a hover, and an item that
// the user selects in a tint of the brand colour.
// A press on an item selects it, runs its action or moves the focus to it, so
// the item's text cannot be selected.
const ITEM =
  "bk:flex bk:shrink-0 bk:cursor-pointer bk:select-none bk:items-center bk:gap-2 bk:rounded-sm bk:px-2 bk:py-0.5 bk:text-sm bk:outline-none bk:-outline-offset-2 bk:data-focus-visible:outline-2 bk:data-focus-visible:outline-ring bk:data-hovered:bg-accent bk:data-hovered:text-accent-foreground bk:data-pressed:bg-accent bk:data-selected:bg-primary/10 bk:data-disabled:cursor-default bk:data-disabled:opacity-50";
// The header of a section, in the muted heading of a section of blockr.ui's
// menu.
const HEADER = "bk:block bk:px-2 bk:pt-1 bk:text-xs bk:font-semibold bk:text-muted-foreground";
