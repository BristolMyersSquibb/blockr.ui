// The buttons of blockr.ui, React Aria's Button
// (https://react-aria.adobe.com/Button), drawn with the theme variables: the
// button, and the button inside a field below.
// The button is drawn after the Button of React Aria's Tailwind starter kit
// (https://github.com/adobe/react-spectrum/blob/react-aria-components%401.22.1/starters/tailwind/src/Button.tsx).
// It takes the props of React Aria's Button, and the starter kit's `variant`,
// one of the four looks of a button: "primary", in the brand colour,
// "secondary", with a border, "destructive", in the colour of what removes,
// and "quiet", with no border (../styles/styles.ts).
// It takes `aria-keyshortcuts` too, which names the button's shortcut to
// assistive technology, and which React Aria's Button passes on to no
// element.
// The menus and dialogs of blockr.ui open from it, since React Aria opens them
// only from a button of its own, of the one copy of React Aria on the page.
// The button also takes a ref, which holds the button's element.
// React hands a ref to a component as one of its props, so the ref reaches
// React Aria's Button with the other props.

import type { ReactElement, RefAttributes } from "react";
import { Button as AriaButton, composeRenderProps } from "react-aria-components";
import type { ButtonProps as AriaButtonProps } from "react-aria-components";
import { buttons } from "../styles/index.ts";

export interface ButtonProps extends AriaButtonProps, RefAttributes<HTMLButtonElement> {
  readonly variant?: keyof typeof buttons;
  readonly "aria-keyshortcuts"?: string;
}

export function Button({ variant = "primary", className, "aria-keyshortcuts": shortcuts, ...props }: ButtonProps): ReactElement {
  return (
    <AriaButton
      {...props}
      render={
        shortcuts === undefined || props.render !== undefined
          ? props.render
          : (dom) => <button {...dom} aria-keyshortcuts={shortcuts} />
      }
      className={composeRenderProps(className, (given) => `${buttons[variant]} ${given ?? ""}`)}
    />
  );
}

// A button inside a field, such as the button of the select's field that
// clears the pick, or of the search field that clears the search: React Aria's
// Button drawn with the theme variables, after the FieldButton of the starter kit
// (https://github.com/adobe/react-spectrum/blob/react-aria-components%401.22.1/starters/tailwind/src/FieldButton.tsx).
// It takes the props of React Aria's Button.
// A field of React Aria takes a button inside it for its own, as the select
// opens its list with it, unless the button sets `slot={null}`.
export function FieldButton({ className, ...props }: AriaButtonProps): ReactElement {
  return <AriaButton {...props} className={composeRenderProps(className, (given) => `${FIELD_BUTTON} ${given ?? ""}`)} />;
}

const FIELD_BUTTON =
  "bk:flex bk:size-5 bk:shrink-0 bk:cursor-pointer bk:items-center bk:justify-center bk:rounded-sm bk:border-0 bk:bg-transparent bk:p-0 bk:text-muted-foreground bk:outline-none bk:data-hovered:bg-accent bk:data-hovered:text-accent-foreground bk:data-focus-visible:outline-2 bk:data-focus-visible:outline-ring bk:data-disabled:cursor-default bk:data-disabled:opacity-50";
