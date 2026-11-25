// The tree of blockr.ui: React Aria's Tree and TreeItem
// (https://react-aria.adobe.com/Tree), drawn with the theme variables, after
// the Tree of React Aria's Tailwind starter kit
// (https://github.com/adobe/react-spectrum/blob/react-aria-components%401.22.1/starters/tailwind/src/Tree.tsx).
// Each takes the props of its part of React Aria.
// An item also takes the starter kit's `title`, the text that the item shows.
// The tree draws no frame, so that it adds no frame inside the card, the
// panel or the popover that holds it.
// An item holds the items below it as its children.
// It shows them while it is expanded, each indented by its level.
// An item with items below it starts with a button that expands it and
// collapses it again, as the right and left arrow keys do.
// Where the tree's `selectionMode` is "single" or "multiple", each item
// starts with a checkbox that selects it.
// Where the tree's `dragAndDropHooks` let its items drag, each item starts
// with a button that drags it.
// A keyboard user drags the item from that button too.
// The starter kit's GridList and Table draw that button, while its Tree does
// not.

import type { ReactElement } from "react";
import {
  Button,
  composeRenderProps,
  Tree as AriaTree,
  TreeItem as AriaTreeItem,
  TreeItemContent,
} from "react-aria-components";
import type { TreeItemProps as AriaTreeItemProps, TreeProps } from "react-aria-components";
import { DragButton } from "../dnd/index.ts";
import { Icon } from "../icon/index.ts";
import { Checkbox } from "../inputs/index.ts";
import { ChevronRight } from "../vendor/index.ts";

export function Tree<T extends object>(props: TreeProps<T>): ReactElement {
  return <AriaTree {...props} className={composeRenderProps(props.className, (given) => `${TREE} ${given ?? ""}`)} />;
}

export interface TreeItemProps extends Partial<AriaTreeItemProps> {
  readonly title: string;
}

export function TreeItem({ title, children, ...props }: TreeItemProps): ReactElement {
  return (
    <AriaTreeItem textValue={title} {...props} className={composeRenderProps(props.className, (given) => `${ITEM} ${given ?? ""}`)}>
      <TreeItemContent>
        {({ selectionMode, selectionBehavior, hasChildItems, isExpanded, allowsDragging }) => (
          <div className="bk:flex bk:min-w-0 bk:flex-1 bk:items-center bk:gap-1">
            {allowsDragging && <DragButton />}
            {selectionMode !== "none" && selectionBehavior === "toggle" && <Checkbox slot="selection" />}
            <div className={INDENT} />
            {hasChildItems ? (
              <Button slot="chevron" className={EXPAND}>
                <Chevron expanded={isExpanded} />
              </Button>
            ) : (
              <div className="bk:size-6 bk:shrink-0" />
            )}
            <span className="bk:min-w-0 bk:truncate">{title}</span>
          </div>
        )}
      </TreeItemContent>
      {children}
    </AriaTreeItem>
  );
}

// The chevron of an item that holds other items, Lucide's ChevronRight, as in
// the starter kit.
// It points at the item's title while the item is collapsed, and down while
// the item is expanded.
export function Chevron({ expanded }: { expanded: boolean }): ReactElement {
  return (
    <span
      className={`bk:flex bk:text-muted-foreground bk:transition-transform bk:motion-reduce:transition-none ${expanded ? "bk:rotate-90" : ""}`}
    >
      <Icon icon={ChevronRight} />
    </span>
  );
}

// The classes of the tree and its parts.
const TREE =
  "bk:grid bk:content-start bk:gap-0.5 bk:overflow-auto bk:font-sans bk:outline-none bk:data-focus-visible:outline-2 bk:data-focus-visible:outline-ring";
// The item under the pointer shows on the fill of a hover, and an item that
// the user selects in a tint of the brand colour.
// A press on an item selects it or moves the focus to it, so the item's text
// cannot be selected.
const ITEM =
  "bk:group bk:relative bk:flex bk:cursor-default bk:items-center bk:rounded-sm bk:px-1 bk:font-sans bk:text-sm bk:select-none bk:outline-none bk:-outline-offset-2 bk:data-focus-visible:outline-2 bk:data-focus-visible:outline-ring bk:data-hovered:bg-accent bk:data-hovered:text-accent-foreground bk:data-pressed:bg-accent bk:data-selected:bg-primary/10 bk:data-disabled:opacity-50";
// The space that indents an item by its level, which React Aria sets in
// --tree-item-level, from 1 at the top.
const INDENT = "bk:w-[calc((var(--tree-item-level)_-_1)_*_var(--bk-spacing)_*_3)] bk:shrink-0";
const EXPAND =
  "bk:flex bk:size-6 bk:shrink-0 bk:cursor-default bk:items-center bk:justify-center bk:rounded-sm bk:border-0 bk:bg-transparent bk:p-0 bk:text-inherit bk:outline-none bk:[-webkit-tap-highlight-color:transparent] bk:data-focus-visible:outline-2 bk:data-focus-visible:outline-ring bk:data-disabled:opacity-50";
