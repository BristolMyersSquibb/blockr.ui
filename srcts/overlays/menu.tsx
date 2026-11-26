// The menu of blockr.ui: React Aria's MenuTrigger, Menu, MenuItem,
// MenuSection and SubmenuTrigger (https://react-aria.adobe.com/Menu), drawn
// with the theme variables, after the Menu of React Aria's Tailwind starter kit
// (https://github.com/adobe/react-spectrum/blob/react-aria-components%401.22.1/starters/tailwind/src/Menu.tsx).
// Each takes the props of its part of React Aria, and the trigger the
// starter kit's `placement` too.
// The trigger holds the button that opens the menu, blockr.ui's Button, and
// the menu, which it opens in blockr.ui's Popover beside the button.
// The submenu's trigger holds an item of a menu and the menu that opens from
// it, in blockr.ui's Popover beside the item's row, as the pointer rests on the
// item, as the user presses it, or on the arrow key that points away from the
// menu. The item shows a chevron, Lucide's ChevronRight, at its right.
// React Aria closes the menu as the user picks an item, on Escape and on a
// press outside it, and moves the focus through its items by the arrow keys.
// An item shows a check, Lucide's Check, where the menu picks, and the item
// is picked.
// An item holds its text, or its label in a Text of the slot "label", with
// its description, in a Text of the slot "description", in a muted line below
// it, and its shortcut, in React Aria's Keyboard, at its right.
// React Aria names both the description and the shortcut to assistive
// technology, in the item's aria-describedby.
// A section groups items under its title.

import { Children } from "react";
import type { ReactElement, ReactNode } from "react";
import {
  Collection,
  composeRenderProps,
  Header,
  Menu as AriaMenu,
  MenuItem as AriaMenuItem,
  MenuSection as AriaMenuSection,
  MenuTrigger as AriaMenuTrigger,
  SubmenuTrigger as AriaSubmenuTrigger,
} from "react-aria-components";
import type {
  MenuItemProps,
  MenuProps,
  MenuSectionProps as AriaMenuSectionProps,
  MenuTriggerProps as AriaMenuTriggerProps,
  PopoverProps,
  SubmenuTriggerProps,
} from "react-aria-components";
import { Icon } from "../icon/index.ts";
import { Check, ChevronRight } from "../vendor/index.ts";
import { Popover } from "./popover.tsx";

export interface MenuTriggerProps extends AriaMenuTriggerProps {
  readonly placement?: PopoverProps["placement"];
}

export function MenuTrigger({ placement, children, ...props }: MenuTriggerProps): ReactElement {
  const [trigger, menu] = Children.toArray(children);
  return (
    <AriaMenuTrigger {...props}>
      {trigger}
      <Popover placement={placement} className="bk:min-w-44">
        {menu}
      </Popover>
    </AriaMenuTrigger>
  );
}

export function SubmenuTrigger({ children, ...props }: SubmenuTriggerProps): ReactElement {
  // React Aria's SubmenuTrigger takes its item and its menu as elements.
  const [trigger, menu] = Children.toArray(children) as [ReactElement, ReactElement];
  return (
    <AriaSubmenuTrigger {...props}>
      {trigger}
      <Popover offset={-2} crossOffset={-4} className="bk:min-w-44">
        {menu}
      </Popover>
    </AriaSubmenuTrigger>
  );
}

export function Menu<T extends object>({ className, ...props }: MenuProps<T>): ReactElement {
  return <AriaMenu {...props} className={composeRenderProps(className, (given) => `${MENU} ${given ?? ""}`)} />;
}

export function MenuItem(props: MenuItemProps): ReactElement {
  const textValue = props.textValue ?? (typeof props.children === "string" ? props.children : undefined);
  return (
    <AriaMenuItem {...props} textValue={textValue} className={composeRenderProps(props.className, (given) => `${ITEM} ${given ?? ""}`)}>
      {composeRenderProps(props.children, (children, { selectionMode, isSelected, hasSubmenu }) => (
        <>
          {selectionMode === "none" ? null : (
            <span className="bk:flex bk:size-3.5 bk:shrink-0 bk:text-primary">{isSelected ? <Icon icon={Check} /> : null}</span>
          )}
          <span className={CONTENT}>{children}</span>
          {hasSubmenu ? (
            <span className="bk:flex bk:shrink-0 bk:text-muted-foreground">
              <Icon icon={ChevronRight} />
            </span>
          ) : null}
        </>
      ))}
    </AriaMenuItem>
  );
}

export interface MenuSectionProps<T> extends AriaMenuSectionProps<T> {
  readonly title?: string;
  readonly items?: Iterable<T>;
  readonly children?: ReactNode | ((item: T) => ReactElement);
}

export function MenuSection<T extends object>({ title, items, children, ...props }: MenuSectionProps<T>): ReactElement {
  return (
    <AriaMenuSection {...props}>
      {title === undefined ? null : <Header className={HEADING}>{title}</Header>}
      <Collection items={items}>{children}</Collection>
    </AriaMenuSection>
  );
}

const MENU = "bk:grid bk:max-h-[inherit] bk:gap-0.5 bk:overflow-auto bk:p-1 bk:outline-none";
const ITEM =
  "bk:box-border bk:flex bk:cursor-pointer bk:items-center bk:gap-2 bk:rounded-sm bk:px-2 bk:py-1 bk:text-left bk:font-sans bk:text-sm bk:outline-none bk:data-hovered:bg-accent bk:data-hovered:text-accent-foreground bk:data-focused:bg-accent bk:data-focused:text-accent-foreground bk:data-hovered:[&_[slot=description]]:text-accent-foreground bk:data-hovered:[&_kbd]:text-accent-foreground bk:data-focused:[&_[slot=description]]:text-accent-foreground bk:data-focused:[&_kbd]:text-accent-foreground bk:data-disabled:cursor-default bk:data-disabled:opacity-50";
const HEADING = "bk:block bk:px-2 bk:pt-1 bk:font-sans bk:text-xs bk:font-semibold bk:text-muted-foreground";
// The item's text or label, its description in a second row, and its
// shortcut in a column of its own at its right, each cut to one line.
const CONTENT =
  "bk:grid bk:min-w-0 bk:flex-1 bk:grid-cols-[minmax(0,1fr)_auto] bk:items-center bk:gap-x-4 bk:[&>*]:truncate bk:[&>[slot=description]]:col-start-1 bk:[&>[slot=description]]:text-xs bk:[&>[slot=description]]:text-muted-foreground bk:[&>kbd]:col-start-2 bk:[&>kbd]:row-start-1 bk:[&>kbd]:font-sans bk:[&>kbd]:text-xs bk:[&>kbd]:text-muted-foreground";
