// The popover of blockr.ui:
// React Aria's Popover (https://react-aria.adobe.com/Popover), drawn with the
// theme variables, after the Popover of React Aria's Tailwind starter kit
// (https://github.com/adobe/react-spectrum/blob/react-aria-components%401.22.1/starters/tailwind/src/Popover.tsx).
// It takes the props of React Aria's Popover.
// React Aria draws it in a portal at the end of the page's body, beside the
// element that it hangs from, and above a modal that is open.
// It stacks at the z-index of the overlays, --bk-z-index-overlay, in place of
// the one that React Aria writes on it (./stacking.ts).
// It draws in the colours of an overlay, `popover` and `popover-foreground`,
// with the edge of an outline, `border`, and the shadow of what floats over
// the page, `shadow-md`.

import type { ReactElement } from "react";
import { composeRenderProps, Popover as AriaPopover } from "react-aria-components";
import type { PopoverProps } from "react-aria-components";
import { overlayStyle } from "./stacking.ts";

export function Popover({ className, style, offset = 4, ...props }: PopoverProps): ReactElement {
  return (
    <AriaPopover
      {...props}
      offset={offset}
      style={overlayStyle(style)}
      className={composeRenderProps(className, (given) => `${POPOVER} ${given ?? ""}`)}
    />
  );
}

const POPOVER =
  "bk:overflow-auto bk:rounded-md bk:border bk:border-border bk:bg-popover bk:font-sans bk:text-sm bk:text-popover-foreground bk:shadow-md bk:outline-none";
