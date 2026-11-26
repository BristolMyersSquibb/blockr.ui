// The link of blockr.ui: React Aria's Link
// (https://react-aria.adobe.com/Link), drawn with the theme variables, after the Link of
// React Aria's Tailwind starter kit
// (https://github.com/adobe/react-spectrum/blob/react-aria-components%401.22.1/starters/tailwind/src/Link.tsx).
// It takes the props of React Aria's Link, and the starter kit's `variant`:
// "primary", the default, in the blue of a link, or "secondary", in the
// colour of the text around it.
// It is underlined, more faintly until the pointer rests on it.
// Given an `href`, it is an <a> that goes there, and otherwise an element of
// the role link, whose `onPress` acts.
// A disabled link is not underlined, and does nothing.

import type { ReactElement } from "react";
import { composeRenderProps, Link as AriaLink } from "react-aria-components";
import type { LinkProps as AriaLinkProps } from "react-aria-components";

export interface LinkProps extends AriaLinkProps {
  readonly variant?: "primary" | "secondary";
}

export function Link({ variant = "primary", className, ...props }: LinkProps): ReactElement {
  return <AriaLink {...props} className={composeRenderProps(className, (given) => `${LINK} ${VARIANTS[variant]} ${given ?? ""}`)} />;
}

const LINK =
  "bk:cursor-pointer bk:rounded-sm bk:underline bk:outline-none bk:transition bk:[-webkit-tap-highlight-color:transparent] bk:data-focus-visible:outline-2 bk:data-focus-visible:outline-offset-2 bk:data-focus-visible:outline-ring bk:data-disabled:cursor-default bk:data-disabled:no-underline bk:forced-colors:data-disabled:text-[GrayText]";
// A primary link draws in the text of the brand colour, `primary-text`, the
// colour of a link, which reaches the 4.5:1 that WCAG's level AA asks of text
// on every surface (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.7.2#colours).
// A secondary link draws in the colour of the text around it.
const VARIANTS = {
  primary:
    "bk:text-primary-text bk:decoration-primary-text/60 bk:data-hovered:decoration-primary-text",
  secondary: "bk:text-inherit bk:decoration-current/50 bk:data-hovered:decoration-current",
} as const;
