// The checkbox and the checkbox group of blockr.ui: React Aria's
// CheckboxField and CheckboxButton, drawn as one, and its CheckboxGroup
// (https://react-aria.adobe.com/Checkbox, https://react-aria.adobe.com/CheckboxGroup),
// drawn with the theme variables.
// They are drawn after the Checkbox and the CheckboxGroup of React Aria's
// Tailwind starter kit
// (https://github.com/adobe/react-spectrum/blob/react-aria-components%401.22.1/starters/tailwind/src/Checkbox.tsx,
// https://github.com/adobe/react-spectrum/blob/react-aria-components%401.22.1/starters/tailwind/src/CheckboxGroup.tsx),
// and take what those take: the props of React Aria's parts, and the
// description and the error message that the starter kit adds, and the label
// of the group.
// A checkbox holds its label as its children, and a group its checkboxes,
// each with the value that it picks.
//
// Given `bound`, a checkbox shows and edits the boolean that its `bound` gives,
// as blockr.ui's other inputs do (./inputs.tsx), and a group an array of
// strings, by the values of its checkboxes.
// Each submits the user's pick at once, a group built from the value that the
// host holds then, through the get() of its `bound`, so that it keeps a string
// that another user added in the meantime.
// A group submits its picks in the order the user picked them, after those the
// value holds, or where `ordered`, in the order of its checkboxes, as for
// severities.
// A group draws no checkbox for a value that none of its checkboxes holds.
// It marks itself invalid and says below itself which values are not available,
// as the select does (./combobox.tsx), beside a button, Remove, which submits
// the value without them, but not while it is `outdated`.
// Each says below itself why its last submit was refused, the `error` of its
// `bound`.

import { useRef } from "react";
import type { ReactElement, ReactNode } from "react";
import { strings } from "./json.ts";
import {
  CheckboxButton,
  CheckboxField,
  CheckboxGroup as AriaCheckboxGroup,
  composeRenderProps,
  FieldError,
  Label,
  Text,
} from "react-aria-components";
import type { CheckboxFieldProps, CheckboxGroupProps as AriaCheckboxGroupProps, ValidationResult } from "react-aria-components";
import type { Bound } from "./bound.ts";
import { styles } from "../styles/index.ts";
import { Icon } from "../icon/index.ts";
import { Check, Minus } from "../vendor/index.ts";
import { NotAvailable, Refusal } from "./inputs.tsx";
import { held, inOrder, ItemsContext, lacking, merged, useItem, useItems } from "./items.ts";

export interface CheckboxProps extends Omit<CheckboxFieldProps, "children"> {
  // Its label.
  readonly children?: ReactNode;
  // What the starter kit adds.
  readonly description?: string;
  readonly errorMessage?: string | ((validation: ValidationResult) => string);
  // The value that it shows and submits.
  readonly bound?: Bound;
}

export function Checkbox({ bound, ...props }: CheckboxProps): ReactElement {
  return bound === undefined ? <DrawnCheckbox {...props} /> : <BoundCheckbox {...props} bound={bound} />;
}

type DrawnCheckboxProps = Omit<CheckboxProps, "bound">;

function BoundCheckbox({ bound, onChange, ...props }: DrawnCheckboxProps & { readonly bound: Bound }): ReactElement {
  return (
    <>
      <DrawnCheckbox
        {...props}
        isSelected={bound.value === true}
        onChange={(selected) => {
          void bound.commit(selected);
          onChange?.(selected);
        }}
      />
      <Refusal error={bound.error} />
    </>
  );
}

// The box and the label, the description and the error message.
// Inside a group, the checkbox tells the group its value.
function DrawnCheckbox({ children, description, errorMessage, className, ...props }: DrawnCheckboxProps): ReactElement {
  const ref = useRef<HTMLDivElement>(null);
  useItem(props.value, ref);
  return (
    <CheckboxField {...props} ref={ref} className="bk:grid bk:gap-1 bk:font-sans">
      <CheckboxButton className={composeRenderProps(className, (given) => `bk:group ${CHECK} ${given ?? ""}`)}>
        {composeRenderProps(children, (children, { isSelected, isIndeterminate }) => (
          <>
            <span className={BOX}>{isIndeterminate ? <Icon icon={Minus} size={12} /> : isSelected ? <Icon icon={Check} size={12} /> : null}</span>
            {children}
          </>
        ))}
      </CheckboxButton>
      {/* React Aria names the description of the checkbox only where the
          element is there from the start, so it is there while empty too. */}
      <Text slot="description" className={`${styles.hint} bk:ml-6 bk:empty:hidden`}>
        {description}
      </Text>
      <FieldError className={`${styles.alert} bk:ml-6`}>{errorMessage}</FieldError>
    </CheckboxField>
  );
}

export interface CheckboxGroupProps extends Omit<AriaCheckboxGroupProps, "children"> {
  // What the starter kit adds.
  readonly label?: string;
  readonly description?: string;
  readonly errorMessage?: string | ((validation: ValidationResult) => string);
  // Its checkboxes.
  readonly children?: ReactNode;
  // The value that it shows and submits.
  readonly bound?: Bound;
  readonly outdated?: boolean;
  readonly ordered?: boolean;
}

export function CheckboxGroup({ bound, outdated = false, ordered = false, ...props }: CheckboxGroupProps): ReactElement {
  return bound === undefined ? (
    <DrawnGroup {...props} />
  ) : (
    <BoundGroup {...props} bound={bound} outdated={outdated} ordered={ordered} />
  );
}

type DrawnGroupProps = Omit<CheckboxGroupProps, "bound" | "outdated" | "ordered">;

function BoundGroup({
  bound,
  outdated,
  ordered,
  onChange,
  children,
  ...props
}: DrawnGroupProps & { readonly bound: Bound; readonly outdated: boolean; readonly ordered: boolean }): ReactElement {
  const { known, items } = useItems();
  const shown = strings(bound.value);
  const gone = lacking(shown, known);
  const marking = !outdated && gone.length > 0;
  return (
    <>
      <DrawnGroup
        {...props}
        value={[...shown]}
        isInvalid={props.isInvalid === true || marking}
        onChange={(picks) => {
          const value = merged(strings(bound.get()), shown, picks);
          if (value !== null) void bound.commit(ordered && known !== null ? inOrder(value, known) : value);
          onChange?.(picks);
        }}
      >
        <ItemsContext.Provider value={items}>{children}</ItemsContext.Provider>
      </DrawnGroup>
      {marking ? (
        <NotAvailable
          values={gone}
          onRemove={() => {
            void bound.commit(held(strings(bound.get()), known));
          }}
        />
      ) : null}
      <Refusal error={bound.error} />
    </>
  );
}

// The label, the checkboxes, the description and the error message.
function DrawnGroup({ label, description, errorMessage, children, className, ...props }: DrawnGroupProps): ReactElement {
  return (
    <AriaCheckboxGroup {...props} className={composeRenderProps(className, (given) => `${styles.field} ${given ?? ""}`)}>
      {label === undefined ? null : <Label className={styles.label}>{label}</Label>}
      <div className="bk:grid bk:gap-1.5">{children}</div>
      <Text slot="description" className={`${styles.hint} bk:empty:hidden`}>
        {description}
      </Text>
      <FieldError className={styles.alert}>{errorMessage}</FieldError>
    </AriaCheckboxGroup>
  );
}

// The classes of the checkbox and of its box.
// The box is outlined as a field is, and takes the brand colour where the
// checkbox is checked or neither, with its check or its dash in the colour of
// what sits on the brand colour.
// It takes the colour of an error where it is invalid, as where its group
// holds a value that none of its checkboxes holds.
const CHECK =
  "bk:flex bk:w-fit bk:cursor-pointer bk:items-center bk:gap-2 bk:text-sm bk:data-disabled:cursor-default bk:data-disabled:opacity-50";
const BOX =
  "bk:box-border bk:flex bk:size-4 bk:shrink-0 bk:items-center bk:justify-center bk:rounded-sm bk:border bk:border-input bk:bg-transparent bk:outline-offset-2 bk:outline-ring bk:group-data-focus-visible:outline-2 bk:group-data-selected:border-primary bk:group-data-selected:bg-primary bk:group-data-selected:text-primary-foreground bk:group-data-indeterminate:border-primary bk:group-data-indeterminate:bg-primary bk:group-data-indeterminate:text-primary-foreground bk:group-data-invalid:border-destructive bk:group-data-selected:group-data-invalid:bg-destructive";
