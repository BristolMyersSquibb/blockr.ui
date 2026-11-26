// The field of tokens of blockr.ui: React Aria's TokenField,
// TokenInput and Token (https://react-aria.adobe.com/TokenField), drawn with the theme
// variables, after the TokenField and the Token of React Aria's Tailwind starter
// kit
// (https://github.com/adobe/react-spectrum/blob/react-aria-components%401.22.1/starters/tailwind/src/TokenField.tsx).
// They take what those take: the props of React Aria's parts, and the label
// and the description that the starter kit adds.
// The field holds text in which tokens sit, such as the names of columns in
// a formula, in the box of a text field, and draws each token through its
// children, a function of the token, as a Token.
// A field of tokens takes no `bound`: React Aria keeps its value as the text
// and the tokens it holds, which a caller turns into what it submits itself.

import type { ReactElement, Ref } from "react";
import { composeRenderProps, Token as AriaToken, TokenField as AriaTokenField, TokenInput } from "react-aria-components";
import type { TokenFieldProps as AriaTokenFieldProps, TokenFieldValue, TokenInputProps, TokenProps } from "react-aria-components";
import { styles } from "../styles/index.ts";
import { Description, Label } from "./inputs.tsx";

export interface TokenFieldProps<T extends TokenFieldValue = TokenFieldValue> extends Omit<AriaTokenFieldProps<T>, "children"> {
  // What the starter kit adds.
  readonly label?: string;
  readonly description?: string;
  readonly inputRef?: Ref<HTMLDivElement>;
  readonly children: TokenInputProps["children"];
}

export function TokenField<T extends TokenFieldValue = TokenFieldValue>({
  label,
  description,
  className,
  inputRef,
  children,
  ...props
}: TokenFieldProps<T>): ReactElement {
  return (
    <AriaTokenField {...props} className={composeRenderProps(className, (given) => `${styles.field} ${given ?? ""}`)}>
      {label === undefined ? null : <Label>{label}</Label>}
      <TokenInput ref={inputRef} className={INPUT}>
        {children}
      </TokenInput>
      {description === undefined ? null : <Description>{description}</Description>}
    </AriaTokenField>
  );
}

// A token, drawn as a chip with an edge in the brand colour on a quiet fill,
// which takes the brand colour itself where the user selects it.
export function Token({ className, ...props }: TokenProps): ReactElement {
  return <AriaToken {...props} className={composeRenderProps(className, (given) => `${TOKEN} ${given ?? ""}`)} />;
}

// The classes of the box, a text field's, and of a token.
const INPUT =
  "bk:box-border bk:min-h-7.5 bk:w-full bk:rounded-md bk:border bk:border-input bk:bg-transparent bk:px-2 bk:py-1 bk:font-sans bk:text-sm bk:outline-none bk:data-focused:outline-2 bk:data-focused:outline-offset-1 bk:data-focused:outline-ring bk:data-disabled:opacity-50";
const TOKEN =
  "bk:mx-0.5 bk:inline-flex bk:h-5 bk:cursor-default bk:items-center bk:whitespace-nowrap bk:rounded-sm bk:border bk:border-primary bk:bg-muted bk:px-1.5 bk:text-xs bk:data-selected:bg-primary bk:data-selected:text-primary-foreground";
