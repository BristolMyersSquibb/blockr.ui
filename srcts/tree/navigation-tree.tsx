// The navigation tree of blockr.ui: React Aria's NavigationTree,
// NavigationTreeItem, NavigationTreeItemContent, NavigationTreeSection and
// NavigationTreeHeader (https://react-aria.adobe.com/NavigationTree), drawn
// with the theme variables, after the NavigationTree of React Aria's Tailwind
// starter kit
// (https://github.com/adobe/react-spectrum/blob/react-aria-components%401.22.1/starters/tailwind/src/NavigationTree.tsx),
// with its NavigationTreeItemLink.
// Each takes the props of its part of React Aria.
// The tree also takes the starter kit's `renderLink`, and the link takes the
// props of React Aria's Link.
// It is a tree of links, such as to the pages of an app.
// It draws no frame, so that it adds no frame inside the panel or the popover
// that holds it.
// It marks the item of the current page.
// An item holds its content and the items below it.
// The content holds the item's link.
// The item shows the items below it while it is expanded, each indented by
// its level.
// An item with items below it ends with a button that expands it and
// collapses it again.
// A line at the left edge of the current item takes the brand colour.
// A collapsed item above the current one shows a dot there, and the item
// under the pointer shows a muted line.
// A section groups items under its header.
// A tree whose links lead within the page, as a router's links do, draws each
// link through `renderLink`, which React Aria's Link takes as its `render`.

import { createContext, useContext } from "react";
import type { ReactElement, ReactNode } from "react";
import {
  Button,
  composeRenderProps,
  Link,
  NavigationTree as AriaNavigationTree,
  NavigationTreeHeader as AriaNavigationTreeHeader,
  NavigationTreeItem as AriaNavigationTreeItem,
  NavigationTreeItemContent as AriaNavigationTreeItemContent,
  NavigationTreeSection as AriaNavigationTreeSection,
} from "react-aria-components";
import type {
  LinkProps,
  NavigationTreeHeaderProps,
  NavigationTreeItemProps,
  NavigationTreeProps as AriaNavigationTreeProps,
  NavigationTreeSectionProps,
} from "react-aria-components";
import { Chevron } from "./tree.tsx";

export interface NavigationTreeProps<T> extends AriaNavigationTreeProps<T> {
  readonly renderLink?: LinkProps["render"];
}

// What an item's link shows at its left edge: the current item, or a
// collapsed item above it.
type Indicator = "current" | "ancestor" | undefined;

const IndicatorContext = createContext<Indicator>(undefined);
const RenderLinkContext = createContext<LinkProps["render"]>(undefined);

export function NavigationTree<T extends object>({ renderLink, ...props }: NavigationTreeProps<T>): ReactElement {
  return (
    <RenderLinkContext.Provider value={renderLink}>
      <AriaNavigationTree {...props} className={composeRenderProps(props.className, (given) => `${TREE} ${given ?? ""}`)} />
    </RenderLinkContext.Provider>
  );
}

export function NavigationTreeItem(props: NavigationTreeItemProps): ReactElement {
  return <AriaNavigationTreeItem {...props} className={composeRenderProps(props.className, (given) => `${ITEM} ${given ?? ""}`)} />;
}

export function NavigationTreeItemContent({ children }: { children?: ReactNode }): ReactElement {
  return (
    <AriaNavigationTreeItemContent>
      {({ level, hasChildItems, isExpanded, isCurrent, isCurrentAncestor }) => (
        <>
          {level > 1 && <div className="bk:shrink-0" style={{ width: `calc((${String(level)} - 1) * var(--bk-spacing) * 4)` }} />}
          <IndicatorContext.Provider value={isCurrent ? "current" : isCurrentAncestor && !isExpanded ? "ancestor" : undefined}>
            {children}
          </IndicatorContext.Provider>
          <Button slot="chevron" isDisabled={!hasChildItems} className={`${EXPAND} ${hasChildItems ? "" : "bk:invisible"}`}>
            <Chevron expanded={isExpanded} />
          </Button>
        </>
      )}
    </AriaNavigationTreeItemContent>
  );
}

export function NavigationTreeSection<T extends object>(props: NavigationTreeSectionProps<T>): ReactElement {
  return <AriaNavigationTreeSection {...props} className="bk:not-first:mt-4" />;
}

export function NavigationTreeHeader(props: NavigationTreeHeaderProps): ReactElement {
  return <AriaNavigationTreeHeader {...props} className="bk:px-2 bk:py-1 bk:text-sm bk:font-semibold bk:text-muted-foreground" />;
}

export interface NavigationTreeItemLinkProps extends Omit<LinkProps, "children"> {
  readonly children?: ReactNode;
}

export function NavigationTreeItemLink({ render, children, ...props }: NavigationTreeItemLinkProps): ReactElement {
  const indicator = useContext(IndicatorContext);
  const given = useContext(RenderLinkContext);
  return (
    <Link render={render ?? given} {...props} className={LINK}>
      {({ isHovered }) => (
        <>
          <span aria-hidden="true" data-indicator={indicator ?? (isHovered ? "hover" : undefined)} className={MARK} />
          {children}
        </>
      )}
    </Link>
  );
}

// The classes of the tree and its parts.
const TREE = "bk:flex bk:flex-col bk:gap-0.5 bk:overflow-auto bk:font-sans bk:outline-none";
// The item draws the outline of the focus across the whole row, since its
// link takes the focus.
const ITEM =
  "bk:group bk:relative bk:flex bk:cursor-default bk:items-center bk:rounded-md bk:font-sans bk:select-none bk:outline-none bk:-outline-offset-2 bk:[-webkit-tap-highlight-color:transparent] bk:data-focus-visible:outline-2 bk:data-focus-visible:outline-ring bk:data-disabled:opacity-50";
// An item without a page has no link, so its title shows no pointer.
const LINK =
  "bk:relative bk:flex bk:min-w-0 bk:flex-1 bk:cursor-pointer bk:items-center bk:gap-2 bk:px-2 bk:py-1.5 bk:text-sm bk:text-current bk:no-underline bk:outline-none bk:group-[:not([data-href])]:cursor-default bk:data-disabled:cursor-default";
// The line at the left edge of an item's link: the brand colour on the
// current item, even under the pointer, a muted dot on a collapsed item above
// it, and a muted line on an item with a page under the pointer.
const MARK =
  "bk:absolute bk:start-0.5 bk:top-1/2 bk:h-[1lh] bk:w-0.5 bk:-translate-y-1/2 bk:rounded-full bk:forced-color-adjust-none bk:group-data-href:data-[indicator=hover]:bg-muted-foreground bk:data-[indicator=current]:bg-primary bk:data-[indicator=ancestor]:size-1 bk:data-[indicator=ancestor]:bg-muted-foreground bk:forced-colors:data-indicator:bg-[Highlight]";
const EXPAND =
  "bk:flex bk:size-6 bk:shrink-0 bk:cursor-default bk:items-center bk:justify-center bk:rounded-md bk:border-0 bk:bg-transparent bk:p-0 bk:text-inherit bk:outline-none bk:-outline-offset-2 bk:[-webkit-tap-highlight-color:transparent] bk:data-focus-visible:outline-2 bk:data-focus-visible:outline-ring";
