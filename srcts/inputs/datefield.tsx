// The date field and the time field of blockr.ui: React Aria's
// DateField and TimeField (https://react-aria.adobe.com/DateField,
// https://react-aria.adobe.com/TimeField), drawn with the theme variables.
// They are drawn after the DateField and the TimeField of React Aria's
// Tailwind starter kit
// (https://github.com/adobe/react-spectrum/blob/react-aria-components%401.22.1/starters/tailwind/src/DateField.tsx,
// https://github.com/adobe/react-spectrum/blob/react-aria-components%401.22.1/starters/tailwind/src/TimeField.tsx),
// and take what those take: the props of their part of React Aria, and the
// label, the description and the error message that the starter kit adds.
// The user types the date or the time segment by segment, as its month, its
// day and its year, and the arrow keys step a segment up and down.
//
// Given `bound`, the date field shows and edits the date that its `bound`
// gives, as blockr.ui's other inputs do (./inputs.tsx).
// The date is a string, as "2026-11-19", or as "2026-11-19 14:30:00" where the
// field's granularity takes a time (./dates.ts).
// The time field shows and edits a time in the same way, as "14:30:00".
// Each submits what the user typed as the user leaves the field or presses
// Enter, and a step of a segment by a key at once, as a pick (./bounddate.ts).
// It submits only a value from minValue to maxValue, and the date field only a
// date that isDateUnavailable leaves.
// A bound field takes React Aria's validationBehavior "aria" by default, so
// that it marks a value that it does not take as the user types it.
// It says below itself why its last submit was refused, the `error` of its
// `bound`.

import type { ReactElement, ReactNode } from "react";
import {
  composeRenderProps,
  DateField as AriaDateField,
  DateInput as AriaDateInput,
  DateSegment,
  TimeField as AriaTimeField,
} from "react-aria-components";
import type {
  DateFieldProps as AriaDateFieldProps,
  DateValue,
  TimeFieldProps as AriaTimeFieldProps,
  TimeValue,
  ValidationResult,
} from "react-aria-components";
import type { Bound } from "./bound.ts";
import { styles } from "../styles/index.ts";
import { useBoundDate } from "./bounddate.ts";
import { dateFits, readDate, readTime, takesTime, timeFits, writeDate, writeTime } from "./dates.ts";
import { Description, FieldError, Label, NotAvailable, Refusal } from "./inputs.tsx";

export interface DateFieldProps<T extends DateValue> extends AriaDateFieldProps<T> {
  // What the starter kit adds.
  readonly label?: string;
  readonly description?: string | null;
  readonly errorMessage?: string | ((validation: ValidationResult) => string);
  // The value that it shows and submits.
  readonly bound?: Bound;
}

export interface TimeFieldProps<T extends TimeValue> extends AriaTimeFieldProps<T> {
  // What the starter kit adds.
  readonly label?: string;
  readonly description?: string | null;
  readonly errorMessage?: string | ((validation: ValidationResult) => string);
  // The value that it shows and submits.
  readonly bound?: Bound;
}

export function DateField<T extends DateValue>({ bound, ...props }: DateFieldProps<T>): ReactElement {
  return bound === undefined ? <DrawnDate {...props} /> : <BoundDate {...(props as DrawnDateProps<DateValue>)} bound={bound} />;
}

export function TimeField<T extends TimeValue>({ bound, ...props }: TimeFieldProps<T>): ReactElement {
  return bound === undefined ? <DrawnTime {...props} /> : <BoundTime {...(props as DrawnTimeProps<TimeValue>)} bound={bound} />;
}

// What a field draws with, the value of its `bound` that it shows as missing, where
// it shows one, and what hears each key that the user presses in its segments,
// where it is bound.
interface Bindable {
  readonly missing?: string | null;
  readonly onKey?: (key: string) => void;
}
type DrawnDateProps<T extends DateValue> = Omit<DateFieldProps<T>, "bound"> & Bindable;
type DrawnTimeProps<T extends TimeValue> = Omit<TimeFieldProps<T>, "bound"> & Bindable;

function BoundDate({
  bound,
  onChange,
  onBlur,
  onKeyDown,
  validationBehavior = "aria",
  ...props
}: DrawnDateProps<DateValue> & { readonly bound: Bound }): ReactElement {
  const time = takesTime(props.granularity);
  const date = useBoundDate<DateValue>(bound, {
    read: (value) => readDate(value, time),
    write: (value) => writeDate(value, time),
    fits: (value) => dateFits(value, props),
    required: props.isRequired === true,
  });
  return (
    <>
      <DrawnDate
        {...props}
        validationBehavior={validationBehavior}
        value={date.value}
        missing={date.missing}
        onKey={date.heard}
        onChange={(value) => {
          date.change(value);
          onChange?.(value);
        }}
        onBlur={(event) => {
          date.flush();
          onBlur?.(event);
        }}
        onKeyDown={(event) => {
          if (event.key === "Enter") date.flush();
          onKeyDown?.(event);
        }}
      />
      <Refusal error={bound.error} />
    </>
  );
}

function BoundTime({
  bound,
  onChange,
  onBlur,
  onKeyDown,
  validationBehavior = "aria",
  ...props
}: DrawnTimeProps<TimeValue> & { readonly bound: Bound }): ReactElement {
  const time = useBoundDate<TimeValue>(bound, {
    read: readTime,
    write: writeTime,
    fits: (value) => timeFits(value, props),
    required: props.isRequired === true,
  });
  return (
    <>
      <DrawnTime
        {...props}
        validationBehavior={validationBehavior}
        value={time.value}
        missing={time.missing}
        onKey={time.heard}
        onChange={(value) => {
          time.change(value);
          onChange?.(value);
        }}
        onBlur={(event) => {
          time.flush();
          onBlur?.(event);
        }}
        onKeyDown={(event) => {
          if (event.key === "Enter") time.flush();
          onKeyDown?.(event);
        }}
      />
      <Refusal error={bound.error} />
    </>
  );
}

// The label, the field, the description, the error message, and the line that
// names a value of its `bound` that the field shows as missing, as blockr.ui's
// other inputs name a value that they lack.
function DrawnDate<T extends DateValue>({
  label,
  description,
  errorMessage,
  missing = null,
  onKey,
  isInvalid,
  className,
  ...props
}: DrawnDateProps<T>): ReactElement {
  return (
    <AriaDateField
      {...props}
      isInvalid={missing === null ? isInvalid : true}
      className={composeRenderProps(className, (given) => `${styles.field} ${given ?? ""}`)}
    >
      <FieldParts label={label} description={description} errorMessage={errorMessage} missing={missing} onKey={onKey} />
    </AriaDateField>
  );
}

function DrawnTime<T extends TimeValue>({
  label,
  description,
  errorMessage,
  missing = null,
  onKey,
  isInvalid,
  className,
  ...props
}: DrawnTimeProps<T>): ReactElement {
  return (
    <AriaTimeField
      {...props}
      isInvalid={missing === null ? isInvalid : true}
      className={composeRenderProps(className, (given) => `${styles.field} ${given ?? ""}`)}
    >
      <FieldParts label={label} description={description} errorMessage={errorMessage} missing={missing} onKey={onKey} />
    </AriaTimeField>
  );
}

function FieldParts({
  label,
  description,
  errorMessage,
  missing,
  onKey,
}: Pick<DrawnDateProps<DateValue>, "label" | "description" | "errorMessage" | "missing" | "onKey">): ReactElement {
  return (
    <>
      {label === undefined ? null : <Label>{label}</Label>}
      <Segments onKey={onKey}>
        <DateInput className={BOX} />
      </Segments>
      {/* React Aria names the description of the field only where the
          element is there from the start, so it is there while empty too. */}
      <Description>{description}</Description>
      <FieldError>{errorMessage}</FieldError>
      {missing === null || missing === undefined ? null : <NotAvailable values={[missing]} />}
    </>
  );
}

// The segments inside it, in an element that hears each key that the user
// presses in them before React Aria does, through `onKey`, as React Aria stops
// a key that steps a segment.
// The element draws no box, so the segments sit where they would without it.
// Where no `onKey` is given, as in an input that is not bound, there is no
// such element.
export function Segments({ onKey, children }: { readonly onKey: ((key: string) => void) | undefined; readonly children: ReactNode }): ReactElement {
  return onKey === undefined ? (
    <>{children}</>
  ) : (
    <div
      className="bk:contents"
      onKeyDownCapture={(event) => {
        onKey(event.key);
      }}
    >
      {children}
    </div>
  );
}

// The segments of a date or a time, which the user types into one by one, in
// the box of a field, or without one inside the box of a picker, which
// `className` gives.
// A segment that the user is in shows in the accent colour, and one without
// a value, as before the user typed it, shows its placeholder muted.
// The slot of a range's field, "start" or "end", picks its half of the range.
export function DateInput({ slot, className }: { readonly slot?: "start" | "end"; readonly className: string }): ReactElement {
  return (
    <AriaDateInput slot={slot} className={className}>
      {(segment) => <DateSegment segment={segment} className={SEGMENT} />}
    </AriaDateInput>
  );
}

// The classes of the field and its parts.
// The field is a box like blockr.ui's other inputs, which scrolls the segments
// sideways where they are wider than the box, and draws the outline of the
// focus while the focus is in it.
const BOX =
  "bk:flex bk:min-w-0 bk:items-center bk:justify-self-stretch bk:overflow-x-auto bk:whitespace-nowrap bk:rounded-md bk:border bk:border-input bk:bg-transparent bk:px-2 bk:py-1 bk:font-sans bk:text-sm bk:cursor-text bk:[scrollbar-width:none] bk:data-focus-within:outline-2 bk:data-focus-within:outline-offset-1 bk:data-focus-within:outline-ring bk:data-invalid:border-destructive bk:data-disabled:cursor-default bk:data-disabled:opacity-50";
// The segments of a picker sit in its box, which draws the border and the
// outline of the focus.
export const INLINE =
  "bk:flex bk:min-w-0 bk:items-center bk:whitespace-nowrap bk:py-1 bk:font-sans bk:text-sm bk:cursor-text bk:data-disabled:cursor-default";
const SEGMENT =
  "bk:rounded-sm bk:px-0.5 bk:tabular-nums bk:caret-transparent bk:outline-none bk:data-[type=literal]:px-0 bk:data-placeholder:text-muted-foreground bk:data-focused:bg-primary bk:data-focused:text-primary-foreground";
