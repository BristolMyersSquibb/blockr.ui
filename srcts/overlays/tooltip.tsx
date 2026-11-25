// The tooltip of blockr.ui:
// React Aria's TooltipTrigger and Tooltip
// (https://react-aria.adobe.com/Tooltip), drawn with the theme variables, after
// the Tooltip of React Aria's Tailwind starter kit
// (https://github.com/adobe/react-spectrum/blob/react-aria-components%401.22.1/starters/tailwind/src/Tooltip.tsx).
// Each takes the props of its part of React Aria.
// The trigger holds the element that the tooltip names and the tooltip.
// That element takes the focus, as a Button or a Link of blockr.ui does, or a
// child of React Aria's Focusable, which has a role such as "button", so that
// the user reaches the tooltip with the keyboard as well.
// React Aria shows the tooltip as the pointer rests on the element, after 1.5
// seconds, at once while another tooltip shows or has just gone, and at once as
// the element takes the focus from the keyboard.
// It hides the tooltip as the pointer or the focus leaves, on Escape and as the
// user presses the element.
// It shows no tooltip for a disabled element, which takes neither the pointer
// nor the focus, nor for a touch, which has no hover, so a UI says what the
// user must know in another way as well.
// React Aria draws the tooltip in a portal at the end of the page's body, above
// the element where the window has room, with an arrow that points at the
// element.
// It stacks at the z-index of the overlays, --bk-z-index-overlay, in place of
// the one that React Aria writes on it (./stacking.ts).
// It draws in the colours of an overlay, `popover` and `popover-foreground`,
// with the edge of an outline, `border`, and the shadow of what floats over
// the page, `shadow-md`.

import type { ReactElement, ReactNode } from "react";
import { composeRenderProps, OverlayArrow, Tooltip as AriaTooltip } from "react-aria-components";
import type { TooltipProps as AriaTooltipProps } from "react-aria-components";
import { overlayStyle } from "./stacking.ts";

export interface TooltipProps extends Omit<AriaTooltipProps, "children"> {
  readonly children: ReactNode;
}

export function Tooltip({ children, className, style, offset = 8, ...props }: TooltipProps): ReactElement {
  return (
    <AriaTooltip
      {...props}
      offset={offset}
      style={overlayStyle(style)}
      className={composeRenderProps(className, (given) => `${TOOLTIP} ${given ?? ""}`)}
    >
      <OverlayArrow>
        <svg width={8} height={8} viewBox="0 0 8 8" className={ARROW} aria-hidden="true">
          <path d="M0 0 L4 4 L8 0" />
        </svg>
      </OverlayArrow>
      {children}
    </AriaTooltip>
  );
}

const TOOLTIP =
  "bk:group bk:box-border bk:max-w-80 bk:rounded-md bk:border bk:border-border bk:bg-popover bk:px-2 bk:py-1 bk:font-sans bk:text-xs bk:text-popover-foreground bk:shadow-md bk:outline-none bk:will-change-transform";
// The arrow points down from a tooltip above its element, and turns to point
// at the element wherever else the tooltip sits.
const ARROW =
  "bk:block bk:fill-popover bk:stroke-border bk:forced-colors:fill-[Canvas] bk:forced-colors:stroke-[ButtonBorder] bk:group-data-[placement=bottom]:rotate-180 bk:group-data-[placement=left]:-rotate-90 bk:group-data-[placement=right]:rotate-90";
