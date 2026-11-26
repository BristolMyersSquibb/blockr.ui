// The toolbar of blockr.ui: React Aria's Toolbar and Separator
// (https://react-aria.adobe.com/Toolbar), drawn with the theme variables, after the Toolbar
// and the Separator of React Aria's Tailwind starter kit
// (https://github.com/adobe/react-spectrum/blob/react-aria-components%401.22.1/starters/tailwind/src/Toolbar.tsx,
// https://github.com/adobe/react-spectrum/blob/react-aria-components%401.22.1/starters/tailwind/src/Separator.tsx).
// Each takes the props of its part of React Aria.
// The toolbar lays out the controls that it holds in a row, which wraps, or
// in a column where its `orientation` is "vertical".
// Tab moves the focus into the toolbar and out of it again, and the arrow
// keys move it between the controls.
// A toggle button group in the toolbar lays out its buttons as the toolbar
// lays out its controls, as the starter kit has it.
// The separator is a line between them, across where its `orientation` is
// "horizontal", the default, and upright, as tall as the row, where it is
// "vertical", as between the groups of a toolbar's row.

import type { ReactElement } from "react";
import {
  composeRenderProps,
  Separator as AriaSeparator,
  ToggleButtonGroupContext,
  Toolbar as AriaToolbar,
} from "react-aria-components";
import type { SeparatorProps, ToolbarProps } from "react-aria-components";

export function Toolbar({ className, ...props }: ToolbarProps): ReactElement {
  return (
    <ToggleButtonGroupContext value={{ orientation: props.orientation }}>
      <AriaToolbar
        {...props}
        className={composeRenderProps(className, (given, { orientation }) => `${TOOLBAR} ${ORIENTED[orientation]} ${given ?? ""}`)}
      />
    </ToggleButtonGroupContext>
  );
}

export function Separator({ className, ...props }: SeparatorProps): ReactElement {
  return <AriaSeparator {...props} className={`${SEPARATOR} ${ACROSS[props.orientation ?? "horizontal"]} ${className ?? ""}`} />;
}

const TOOLBAR = "bk:flex bk:flex-wrap bk:gap-2";
const ORIENTED = { horizontal: "bk:flex-row bk:items-center", vertical: "bk:flex-col bk:items-start" } as const;
const SEPARATOR = "bk:m-0 bk:border-none bk:bg-border bk:forced-colors:bg-[ButtonBorder]";
const ACROSS = { horizontal: "bk:h-px bk:w-full", vertical: "bk:w-px bk:self-stretch" } as const;
