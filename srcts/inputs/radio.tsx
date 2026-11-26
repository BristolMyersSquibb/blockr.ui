// The radio group of blockr.ui: React Aria's RadioGroup, and its
// RadioField and RadioButton drawn as one radio
// (https://react-aria.adobe.com/RadioGroup), drawn with the theme variables, after the
// RadioGroup and the Radio of React Aria's Tailwind starter kit
// (https://github.com/adobe/react-spectrum/blob/react-aria-components%401.22.1/starters/tailwind/src/RadioGroup.tsx).
// They take what those take: the props of React Aria's parts, and the label,
// the description and the error message that the starter kit adds.
// A group holds its radios, each with the value that it picks and its label
// as its children, one below the other, or side by side where its
// orientation is "horizontal".
//
// Given `bound`, a group shows and edits the string that its `bound` gives, as
// blockr.ui's other inputs do (./inputs.tsx), by the values of its radios.
// It submits the user's pick at once.
// Where none of its radios holds its value, it picks no radio, marks itself
// invalid and says below itself that the value is not available, as the select
// does (./combobox.tsx), but not while it is `outdated`.
// The user drops the value by picking a radio.
// It says below itself why its last submit was refused, the `error` of its
// `bound`.

import { useRef } from "react";
import type { ReactElement, ReactNode } from "react";
import { composeRenderProps, RadioButton, RadioField, RadioGroup as AriaRadioGroup } from "react-aria-components";
import type { RadioFieldProps, RadioGroupProps as AriaRadioGroupProps, ValidationResult } from "react-aria-components";
import type { Bound } from "./bound.ts";
import { styles } from "../styles/index.ts";
import { Description, FieldError, Label, NotAvailable, Refusal } from "./inputs.tsx";
import { ItemsContext, lacking, useItem, useItems } from "./items.ts";

export interface RadioGroupProps extends Omit<AriaRadioGroupProps, "children"> {
  // What the starter kit adds.
  readonly label?: string;
  readonly description?: string;
  readonly errorMessage?: string | ((validation: ValidationResult) => string);
  // Its radios.
  readonly children?: ReactNode;
  // The value that it shows and submits.
  readonly bound?: Bound;
  readonly outdated?: boolean;
}

export function RadioGroup({ bound, outdated = false, ...props }: RadioGroupProps): ReactElement {
  return bound === undefined ? <DrawnGroup {...props} /> : <BoundGroup {...props} bound={bound} outdated={outdated} />;
}

type DrawnGroupProps = Omit<RadioGroupProps, "bound" | "outdated">;

function BoundGroup({
  bound,
  outdated,
  onChange,
  children,
  ...props
}: DrawnGroupProps & { readonly bound: Bound; readonly outdated: boolean }): ReactElement {
  const { known, items } = useItems();
  const held = typeof bound.value === "string" ? bound.value : null;
  const gone = lacking(held === null ? [] : [held], known);
  const marking = !outdated && gone.length > 0;
  return (
    <>
      <DrawnGroup
        {...props}
        value={held}
        isInvalid={props.isInvalid === true || marking}
        onChange={(value) => {
          void bound.commit(value);
          onChange?.(value);
        }}
      >
        <ItemsContext.Provider value={items}>{children}</ItemsContext.Provider>
      </DrawnGroup>
      {marking ? <NotAvailable values={gone} /> : null}
      <Refusal error={bound.error} />
    </>
  );
}

// The label, the radios, the description and the error message.
function DrawnGroup({ label, description, errorMessage, children, className, ...props }: DrawnGroupProps): ReactElement {
  return (
    <AriaRadioGroup {...props} className={composeRenderProps(className, (given) => `bk:group ${styles.field} ${given ?? ""}`)}>
      {label === undefined ? null : <Label>{label}</Label>}
      <div className="bk:flex bk:gap-1.5 bk:group-data-[orientation=horizontal]:flex-row bk:group-data-[orientation=horizontal]:flex-wrap bk:group-data-[orientation=horizontal]:gap-4 bk:group-data-[orientation=vertical]:flex-col">
        {children}
      </div>
      <Description>{description}</Description>
      <FieldError>{errorMessage}</FieldError>
    </AriaRadioGroup>
  );
}

export interface RadioProps extends Omit<RadioFieldProps, "children"> {
  // Its label.
  readonly children?: ReactNode;
  // What the starter kit adds.
  readonly description?: string;
}

// The circle and the label, and the description, of a radio that tells its
// group its value.
export function Radio({ children, description, className, ...props }: RadioProps): ReactElement {
  const ref = useRef<HTMLDivElement>(null);
  useItem(props.value, ref);
  return (
    <RadioField {...props} ref={ref} className="bk:grid bk:gap-1 bk:font-sans">
      <RadioButton className={composeRenderProps(className, (given) => `bk:group ${RADIO} ${given ?? ""}`)}>
        <span className={CIRCLE} />
        {children}
      </RadioButton>
      <Description className="bk:ml-6">{description}</Description>
    </RadioField>
  );
}

// The classes of the radio and of its circle, whose border thickens in the
// accent colour where the radio is picked, and takes the colour of danger
// where the group is invalid, as where it holds a value that the radios lack.
const RADIO =
  "bk:flex bk:w-fit bk:cursor-pointer bk:items-center bk:gap-2 bk:text-sm bk:data-disabled:cursor-default bk:data-disabled:opacity-50";
const CIRCLE =
  "bk:box-border bk:size-4 bk:shrink-0 bk:rounded-full bk:border bk:border-input bk:bg-transparent bk:outline-offset-2 bk:outline-ring bk:transition-[border-width] bk:group-data-focus-visible:outline-2 bk:group-data-selected:border-[5px] bk:group-data-selected:border-primary bk:group-data-invalid:border-destructive";
