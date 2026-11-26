// The search field of blockr.ui: React Aria's SearchField
// (https://react-aria.adobe.com/SearchField), drawn with the theme variables, after the
// SearchField of React Aria's Tailwind starter kit
// (https://github.com/adobe/react-spectrum/blob/react-aria-components%401.22.1/starters/tailwind/src/SearchField.tsx).
// It takes what that takes: the props of React Aria's SearchField, and the
// label, the description, the error message and the placeholder that the
// starter kit adds.
// Its box shows a magnifier before the text, and after it a FieldButton that
// clears the search, while there is one.
// Escape clears the search too, and goes on to the elements around the field
// where there is none, as a dialog that closes on Escape needs.
// The button is outside the order of Tab, and keeps the focus in the field.
//
// Given `bound`, it shows and edits the string that its `bound` gives, as
// blockr.ui's TextField does (./textfield.tsx).
// It hands what the user types to the binding, which submits it once the user
// pauses, and at once as the user leaves the field, presses Enter or clears the
// search.
// It keeps what the user typed until the binding shows the value again.
// It says below itself why its last submit was refused, the `error` of its
// `bound`.

import type { ReactElement } from "react";
import { composeRenderProps, Group, Input, SearchField as AriaSearchField } from "react-aria-components";
import type { SearchFieldProps as AriaSearchFieldProps, ValidationResult } from "react-aria-components";
import type { Bound } from "./bound.ts";
import { styles } from "../styles/index.ts";
import { FieldButton } from "./button.tsx";
import { Icon } from "../icon/index.ts";
import { Search, X } from "../vendor/index.ts";
import { Description, FieldError, Label, Refusal, useTyped } from "./inputs.tsx";

export interface SearchFieldProps extends AriaSearchFieldProps {
  // What the starter kit adds.
  readonly label?: string;
  readonly description?: string | null;
  readonly errorMessage?: string | ((validation: ValidationResult) => string);
  readonly placeholder?: string;
  // The value that it shows and submits.
  readonly bound?: Bound;
}

export function SearchField({ bound, ...props }: SearchFieldProps): ReactElement {
  return bound === undefined ? <Drawn {...props} /> : <BoundSearch {...props} bound={bound} />;
}

type DrawnProps = Omit<SearchFieldProps, "bound">;

function BoundSearch({ bound, onChange, onKeyDown, onBlur, onClear, ...props }: DrawnProps & { readonly bound: Bound }): ReactElement {
  const [text, typed] = useTyped(bound);
  return (
    <>
      <Drawn
        {...props}
        value={text ?? (typeof bound.value === "string" ? bound.value : "")}
        onChange={(value) => {
          typed(value);
          bound.type(value);
          onChange?.(value);
        }}
        onKeyDown={(event) => {
          if (event.key === "Enter" && !event.nativeEvent.isComposing) bound.flush();
          event.continuePropagation();
          onKeyDown?.(event);
        }}
        onBlur={(event) => {
          bound.flush();
          onBlur?.(event);
        }}
        onClear={() => {
          bound.flush();
          onClear?.();
        }}
      />
      <Refusal error={bound.error} />
    </>
  );
}

// The label, the box with the magnifier, the text and the button that clears
// it, the description and the error message.
function Drawn({ label, description, errorMessage, placeholder, className, ...props }: DrawnProps): ReactElement {
  return (
    <AriaSearchField {...props} className={composeRenderProps(className, (given) => `bk:group ${styles.field} ${given ?? ""}`)}>
      {label === undefined ? null : <Label>{label}</Label>}
      <Group className={FIELD}>
        <span className="bk:flex bk:shrink-0 bk:text-muted-foreground">
          <Icon icon={Search} />
        </span>
        <Input placeholder={placeholder} className={INPUT} />
        <FieldButton className="bk:group-data-empty:invisible">
          <Icon icon={X} size={10} />
        </FieldButton>
      </Group>
      {/* React Aria names the description of the field only where the
          element is there from the start, so it is there while empty too. */}
      <Description>{description}</Description>
      <FieldError>{errorMessage}</FieldError>
    </AriaSearchField>
  );
}

// The classes of the box, which draws the outline of the focus while the
// focus is in it, and of the text, which hides the browser's own button that
// clears a search.
const FIELD =
  "bk:flex bk:min-h-7.5 bk:min-w-0 bk:items-center bk:gap-1.5 bk:justify-self-stretch bk:rounded-md bk:border bk:border-input bk:bg-transparent bk:py-0.5 bk:pr-1 bk:pl-2 bk:font-sans bk:text-sm bk:data-focus-within:outline-2 bk:data-focus-within:outline-offset-1 bk:data-focus-within:outline-ring bk:group-data-invalid:border-destructive bk:data-disabled:opacity-50";
const INPUT =
  "bk:min-w-0 bk:flex-1 bk:border-0 bk:bg-transparent bk:p-0 bk:font-sans bk:text-sm bk:text-inherit bk:outline-none bk:placeholder:text-muted-foreground bk:[&::-webkit-search-cancel-button]:hidden";
