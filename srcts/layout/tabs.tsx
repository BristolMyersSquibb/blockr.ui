// The tabs of blockr.ui: React Aria's Tabs, TabList, Tab and TabPanel
// (https://react-aria.adobe.com/Tabs), drawn with the theme variables, after the Tabs of
// React Aria's Tailwind starter kit
// (https://github.com/adobe/react-spectrum/blob/react-aria-components%401.22.1/starters/tailwind/src/Tabs.tsx).
// Each takes the props of its part of React Aria.
// The tabs hold the list of tabs and a panel for each tab, of the tab's `id`,
// which shows while its tab is the one picked.
// The list lays the tabs out in a row, or in a column where the tabs'
// `orientation` is "vertical", and scrolls where they outgrow it.
// The picked tab shows on a pill in the accent colour, which slides to the
// tab that the user picks, but not while the user prefers reduced motion.
// The arrow keys move between the tabs, and pick the tab that they move to,
// unless the tabs' `keyboardActivation` is "manual".

import type { ReactElement } from "react";
import {
  composeRenderProps,
  SelectionIndicator,
  Tab as AriaTab,
  TabList as AriaTabList,
  TabPanel as AriaTabPanel,
  Tabs as AriaTabs,
} from "react-aria-components";
import type { TabListProps, TabPanelProps, TabProps, TabsProps } from "react-aria-components";

export function Tabs({ className, ...props }: TabsProps): ReactElement {
  return (
    <AriaTabs
      {...props}
      className={composeRenderProps(className, (given, { orientation }) => `${TABS} ${ORIENTED[orientation]} ${given ?? ""}`)}
    />
  );
}

export function TabList<T extends object>({ className, ...props }: TabListProps<T>): ReactElement {
  return (
    <AriaTabList
      {...props}
      className={composeRenderProps(className, (given, { orientation }) => `${LIST} ${LISTED[orientation]} ${given ?? ""}`)}
    />
  );
}

export function Tab({ className, children, ...props }: TabProps): ReactElement {
  return (
    <AriaTab {...props} className={composeRenderProps(className, (given) => `${TAB} ${given ?? ""}`)}>
      {composeRenderProps(children, (children) => (
        <>
          {children}
          <SelectionIndicator className={PILL} />
        </>
      ))}
    </AriaTab>
  );
}

export function TabPanel({ className, ...props }: TabPanelProps): ReactElement {
  return <AriaTabPanel {...props} className={composeRenderProps(className, (given) => `${PANEL} ${given ?? ""}`)} />;
}

const TABS = "bk:flex bk:max-w-full bk:gap-4 bk:font-sans";
const ORIENTED = { horizontal: "bk:flex-col", vertical: "bk:flex-row" } as const;
const LIST = "bk:-m-1 bk:flex bk:max-w-full bk:overflow-x-auto bk:overflow-y-clip bk:p-1 bk:[scrollbar-width:none]";
const LISTED = { horizontal: "bk:flex-row", vertical: "bk:flex-col bk:items-start" } as const;
// A tab sets up a stacking context of its own, so that the pill sits behind
// its text and above what the tab sits on.
const TAB =
  "bk:relative bk:isolate bk:flex bk:cursor-default bk:items-center bk:rounded-full bk:px-3 bk:py-1.5 bk:font-sans bk:text-sm bk:font-medium bk:text-muted-foreground bk:outline-none bk:transition-colors bk:forced-color-adjust-none bk:[-webkit-tap-highlight-color:transparent] bk:data-hovered:text-inherit bk:data-selected:text-primary-foreground bk:data-focus-visible:outline-2 bk:data-focus-visible:outline-offset-2 bk:data-focus-visible:outline-ring bk:data-disabled:cursor-default bk:data-disabled:opacity-50 bk:forced-colors:data-selected:text-[HighlightText]";
const PILL =
  "bk:absolute bk:inset-0 bk:-z-1 bk:rounded-full bk:bg-primary bk:motion-safe:transition-[translate,width,height] bk:forced-colors:bg-[Highlight]";
const PANEL =
  "bk:box-border bk:flex-1 bk:rounded-sm bk:p-4 bk:font-sans bk:text-sm bk:outline-none bk:data-focus-visible:outline-2 bk:data-focus-visible:outline-ring";
