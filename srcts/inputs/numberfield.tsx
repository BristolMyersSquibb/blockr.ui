// The number field of blockr.ui, which core's own UIs, frontends, the
// UIs of plugins and those of blocks all draw with: React Aria's NumberField
// (https://react-aria.adobe.com/NumberField), drawn with the theme variables.
// It is drawn after the NumberField of React Aria's Tailwind starter kit
// (https://github.com/adobe/react-spectrum/blob/react-aria-components%401.22.1/starters/tailwind/src/NumberField.tsx),
// with its buttons that step the number up and down, and takes what that
// takes: the props of React Aria's NumberField, such as minValue, maxValue,
// step and formatOptions, and the label, the description, the error message
// and the placeholder that the starter kit adds.
// While it is `outdated`, as while the request that gives its maxValue asks
// again, its maxValue may be for older data, so it takes none.
//
// Given `bound`, it shows and edits the number that its `bound` gives, as
// blockr.ui's other inputs do (./inputs.tsx).
// It submits the number as React Aria reports it: as the user presses Enter,
// leaves the field or steps the number.
// So it submits nothing as the user pauses, unlike blockr.ui's text field,
// since the number passes through wrong ones as the user types, as 15 on the
// way to 150.
// A step of the number, by a button of the field or an arrow key, is a pick,
// which gives way to the value of its `bound` again where the host refuses it.
// It submits only a number in its range, and on its step where it has one.
// A bound field takes React Aria's commitBehavior "validate" by default, so
// that it keeps a number outside them as it is, rather than pull it in.
// So it shows the value as the host holds it, outside the range too, as once
// the largest number shrank, and marks such a number, as the user types it too.
// It says why in the words of the browser's own number input.
// On Enter and as the user leaves the field, a number that does not fit gives
// way to the value of its `bound` again.
// It says below itself why its last submit was refused, the `error` of its
// `bound`.
//
// React Aria writes the number anew, in its format, whenever the value that
// the field shows changes, which would move the text under the user's cursor.
// So while the user types, the field keeps showing the value that it showed
// as the user started, whatever the host announces, as a number that another
// user set.
// Once the user ends typing, the field submits the number that the user
// typed, which replaces the number that the host announced meanwhile, as the
// later of two edits.
// Where the field unmounts while the user types, the edit goes with it, as
// the picks of the select of several go with it where it unmounts while its
// list is open (./combobox.tsx).

import { useState } from "react";
import type { ReactElement } from "react";
import { Button, composeRenderProps, Group, Input, NumberField as AriaNumberField } from "react-aria-components";
import type { NumberFieldProps as AriaNumberFieldProps, ValidationResult } from "react-aria-components";
import { Icon } from "../icon/index.ts";
import { styles } from "../styles/index.ts";
import { ChevronDown, ChevronUp } from "../vendor/index.ts";
import type { Bound } from "./bound.ts";
import { Description, FieldError, Label, Refusal } from "./inputs.tsx";

export interface NumberFieldProps extends AriaNumberFieldProps {
  // What the starter kit adds.
  readonly label?: string;
  readonly description?: string | null;
  readonly errorMessage?: string | ((validation: ValidationResult) => string);
  readonly placeholder?: string;
  // The value that it shows and submits.
  readonly bound?: Bound;
  readonly outdated?: boolean;
}

export function NumberField({ bound, outdated = false, maxValue, ...props }: NumberFieldProps): ReactElement {
  const largest = outdated ? undefined : maxValue;
  return bound === undefined ? (
    <Drawn {...props} maxValue={largest} />
  ) : (
    <BoundNumber {...props} maxValue={largest} bound={bound} />
  );
}

type DrawnProps = Omit<NumberFieldProps, "bound" | "outdated">;

// The value that the field shows in place of the value of its `bound`: the
// value it showed as the user started to type, until the user ends typing, or
// the number that the user ended typing with or stepped to, until the binding
// shows a value again.
interface Kept {
  readonly value: number;
  readonly typing: boolean;
}

function BoundNumber({
  bound,
  onChange,
  onInput,
  onBlur,
  commitBehavior = "validate",
  validationBehavior = "aria",
  ...props
}: DrawnProps & { readonly bound: Bound }): ReactElement {
  const held = typeof bound.value === "number" ? bound.value : NaN;
  const [kept, setKept] = useState<Kept | null>(null);
  // A value that the binding shows ends the number that the field kept once
  // the user ended typing or stepped, but leaves what the user is typing.
  const [seen, setSeen] = useState(held);
  if (!Object.is(seen, held)) {
    setSeen(held);
    if (kept !== null && !kept.typing) setKept(null);
  }
  const fits = (value: number): boolean => inRange(value, props.minValue, props.maxValue, props.step);

  return (
    <>
      <Drawn
        {...props}
        commitBehavior={commitBehavior}
        validationBehavior={validationBehavior}
        value={kept?.value ?? held}
        onInput={(event) => {
          setKept((now) => (now?.typing === true ? now : { value: now?.value ?? held, typing: true }));
          onInput?.(event);
        }}
        onChange={(value) => {
          // React Aria commits: on Enter or as the user leaves the field,
          // which ends typing, or as the user steps the number.
          if (!fits(value)) {
            setKept(null);
          } else if (kept?.typing === true) {
            bound.type(value);
            bound.flush();
            setKept(Object.is(value, held) ? null : { value, typing: false });
          } else {
            setKept({ value, typing: false });
            void bound.commit(value).then((took) => {
              if (!took) setKept(null);
            });
          }
          onChange?.(value);
        }}
        onBlur={(event) => {
          // Where the user typed the number that the field showed, React Aria
          // commits nothing, and the user ends typing as the field loses the
          // focus.
          setKept((now) => (now?.typing === true ? null : now));
          onBlur?.(event);
        }}
      />
      <Refusal error={bound.error} />
    </>
  );
}

// Whether the number fits the range, and the step where there is one, which
// counts from the smallest number, or from 0, as the browser's own number
// input counts it.
// The slider takes a number so too (./slider.tsx).
export function inRange(value: number, min: number | undefined, max: number | undefined, step: number | undefined): boolean {
  if (Number.isNaN(value)) return false;
  if (min !== undefined && value < min) return false;
  if (max !== undefined && value > max) return false;
  if (step === undefined || !(step > 0)) return true;
  const steps = (value - (min ?? 0)) / step;
  return Math.abs(steps - Math.round(steps)) < 1e-9;
}

// The label, the field with its buttons, the description and the error
// message.
function Drawn({ label, description, errorMessage, placeholder, className, onInput, ...props }: DrawnProps): ReactElement {
  return (
    <AriaNumberField {...props} className={composeRenderProps(className, (given) => `bk:group ${styles.field} ${given ?? ""}`)}>
      {label === undefined ? null : <Label>{label}</Label>}
      <Group className={FIELD}>
        <Input placeholder={placeholder} onInput={onInput} className={INPUT} />
        <div className={STEPPERS}>
          <Button slot="increment" className={STEPPER}>
            <Icon icon={ChevronUp} size={10} />
          </Button>
          <Button slot="decrement" className={STEPPER}>
            <Icon icon={ChevronDown} size={10} />
          </Button>
        </div>
      </Group>
      {/* React Aria names the description of the field only where the
          element is there from the start, so it is there while empty too. */}
      <Description>{description}</Description>
      <FieldError>{errorMessage}</FieldError>
    </AriaNumberField>
  );
}

// The classes of the field and its parts.
// The field is a box like blockr.ui's other inputs, which holds the text field and
// the buttons, and draws the outline of the focus while the focus is in it.
const FIELD =
  "bk:flex bk:min-w-0 bk:items-stretch bk:justify-self-stretch bk:overflow-hidden bk:rounded-md bk:border bk:border-input bk:bg-transparent bk:font-sans bk:text-sm bk:data-focus-within:outline-2 bk:data-focus-within:outline-offset-1 bk:data-focus-within:outline-ring bk:data-invalid:border-destructive bk:data-disabled:opacity-50";
const INPUT =
  "bk:min-w-0 bk:flex-1 bk:border-0 bk:bg-transparent bk:px-2 bk:py-1 bk:font-sans bk:text-sm bk:text-inherit bk:outline-none bk:placeholder:text-muted-foreground";
const STEPPERS = "bk:flex bk:flex-col bk:border-l bk:border-input bk:group-data-invalid:border-destructive";
const STEPPER =
  "bk:flex bk:flex-1 bk:cursor-pointer bk:items-center bk:justify-center bk:border-0 bk:bg-transparent bk:px-1 bk:py-0 bk:text-muted-foreground bk:data-hovered:bg-accent bk:data-hovered:text-accent-foreground bk:data-pressed:bg-accent bk:data-disabled:cursor-default bk:data-disabled:opacity-50";
