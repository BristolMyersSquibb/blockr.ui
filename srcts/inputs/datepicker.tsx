// The date picker and the date range picker of blockr.ui: React Aria's
// DatePicker and DateRangePicker (https://react-aria.adobe.com/DatePicker,
// https://react-aria.adobe.com/DateRangePicker), drawn with the theme variables.
// They are drawn after the DatePicker and the DateRangePicker of React Aria's
// Tailwind starter kit
// (https://github.com/adobe/react-spectrum/blob/react-aria-components%401.22.1/starters/tailwind/src/DatePicker.tsx,
// https://github.com/adobe/react-spectrum/blob/react-aria-components%401.22.1/starters/tailwind/src/DateRangePicker.tsx),
// and take what those take: the props of their part of React Aria, and the
// label, the description and the error message that the starter kit adds.
// The date picker holds the field of a date, which the user types into as
// into the date field (./datefield.tsx), and the range picker one such field for
// each end of the range.
// The button at the field's right opens blockr.ui's calendar, or its range
// calendar (./calendar.tsx), in a popover of blockr.ui, in which the user picks
// the date, or the first and the last day of the range.
// React Aria draws the popover in a portal at the end of the page's body, as
// every overlay of blockr.ui (../overlays/index.ts).
//
// Given `bound`, the date picker shows and edits the date that its `bound`
// gives, as blockr.ui's other inputs do (./inputs.tsx): a string such as
// "2026-11-19", or "2026-11-19 14:30:00" where its granularity takes a time
// (./dates.ts).
// The range picker shows and edits a range in the same way, as { "start":
// "2026-11-19", "end": "2026-11-26" }.
// Each submits a pick in its calendar at once, and so a step of a segment of
// its field by a key, and what the user typed into its field as the user leaves
// the picker or presses Enter (./bounddate.ts).
// The date picker submits only a date from minValue to maxValue that
// isDateUnavailable leaves, and the range picker only a range whose ends lie
// from minValue to maxValue, and whose start comes no later than its end.
// A bound picker takes React Aria's validationBehavior "aria" by default, so
// that it marks a value that it does not take as the user types it.
// It says below itself why its last submit was refused, the `error` of its
// `bound`.

import { useRef } from "react";
import type { KeyboardEvent, ReactElement } from "react";
import {
  composeRenderProps,
  DatePicker as AriaDatePicker,
  DateRangePicker as AriaDateRangePicker,
  FieldError,
  Group,
  Label,
  Text,
} from "react-aria-components";
import type {
  DatePickerProps as AriaDatePickerProps,
  DateRange,
  DateRangePickerProps as AriaDateRangePickerProps,
  DateValue,
  ValidationResult,
} from "react-aria-components";
import { Icon } from "../icon/index.ts";
import { Popover } from "../overlays/index.ts";
import { styles } from "../styles/index.ts";
import { Calendar as CalendarIcon } from "../vendor/index.ts";
import type { Bound } from "./bound.ts";
import { useBoundDate } from "./bounddate.ts";
import type { BoundDate } from "./bounddate.ts";
import { FieldButton } from "./button.tsx";
import { Calendar, RangeCalendar } from "./calendar.tsx";
import { DateInput, INLINE, Segments } from "./datefield.tsx";
import { dateFits, rangeFits, readDate, readRange, takesTime, writeDate, writeRange } from "./dates.ts";
import { NotAvailable, Refusal } from "./inputs.tsx";

export interface DatePickerProps<T extends DateValue> extends AriaDatePickerProps<T> {
  // What the starter kit adds.
  readonly label?: string;
  readonly description?: string | null;
  readonly errorMessage?: string | ((validation: ValidationResult) => string);
  // The value that it shows and submits.
  readonly bound?: Bound;
}

export interface DateRangePickerProps<T extends DateValue> extends AriaDateRangePickerProps<T> {
  // What the starter kit adds.
  readonly label?: string;
  readonly description?: string | null;
  readonly errorMessage?: string | ((validation: ValidationResult) => string);
  // The value that it shows and submits.
  readonly bound?: Bound;
}

export function DatePicker<T extends DateValue>({ bound, ...props }: DatePickerProps<T>): ReactElement {
  return bound === undefined ? <DrawnPicker {...props} /> : <BoundPicker {...(props as DrawnProps<DateValue>)} bound={bound} />;
}

export function DateRangePicker<T extends DateValue>({ bound, ...props }: DateRangePickerProps<T>): ReactElement {
  return bound === undefined ? (
    <DrawnRangePicker {...props} />
  ) : (
    <BoundRangePicker {...(props as DrawnRangeProps<DateValue>)} bound={bound} />
  );
}

// What a picker draws with, the value of its `bound` that it shows as missing, where
// it shows one, and what hears each key that the user presses in the segments
// of its field, where it is bound.
interface Bindable {
  readonly missing?: string | null;
  readonly onKey?: (key: string) => void;
}
type DrawnProps<T extends DateValue> = Omit<DatePickerProps<T>, "bound"> & Bindable;
type DrawnRangeProps<T extends DateValue> = Omit<DateRangePickerProps<T>, "bound"> & Bindable;

// The handlers through which a bound picker hands its binding what the user
// does: a change while its calendar is open, which is a pick, and any other
// change, which is a change of its field's segments, a step or an edit, which
// it submits as the focus leaves the picker or as the user presses Enter in
// its field.
// React passes on a key that the user presses in the calendar to the picker,
// whose popover React Aria draws in a portal, so a key counts only where it
// comes from the picker's own element.
function usePicking<V>(value: BoundDate<V>) {
  const open = useRef(false);
  return {
    onOpenChange: (isOpen: boolean): void => {
      open.current = isOpen;
    },
    onChange: (next: V | null): void => {
      if (open.current) value.pick(next);
      else value.change(next);
    },
    onBlur: (): void => {
      value.flush();
    },
    onKeyDown: (event: KeyboardEvent<HTMLDivElement>): void => {
      if (event.key === "Enter" && event.currentTarget.contains(event.target as Node)) value.flush();
    },
  };
}

function BoundPicker({
  bound,
  onChange,
  onOpenChange,
  onBlur,
  onKeyDown,
  validationBehavior = "aria",
  ...props
}: DrawnProps<DateValue> & { readonly bound: Bound }): ReactElement {
  const time = takesTime(props.granularity);
  const date = useBoundDate<DateValue>(bound, {
    read: (value) => readDate(value, time),
    write: (value) => writeDate(value, time),
    fits: (value) => dateFits(value, props),
    required: props.isRequired === true,
  });
  const picking = usePicking(date);
  return (
    <>
      <DrawnPicker
        {...props}
        validationBehavior={validationBehavior}
        value={date.value}
        missing={date.missing}
        onKey={date.heard}
        onOpenChange={(isOpen) => {
          picking.onOpenChange(isOpen);
          onOpenChange?.(isOpen);
        }}
        onChange={(value) => {
          picking.onChange(value);
          onChange?.(value);
        }}
        onBlur={(event) => {
          picking.onBlur();
          onBlur?.(event);
        }}
        onKeyDown={(event) => {
          picking.onKeyDown(event);
          onKeyDown?.(event);
        }}
      />
      <Refusal error={bound.error} />
    </>
  );
}

function BoundRangePicker({
  bound,
  onChange,
  onOpenChange,
  onBlur,
  onKeyDown,
  validationBehavior = "aria",
  ...props
}: DrawnRangeProps<DateValue> & { readonly bound: Bound }): ReactElement {
  const time = takesTime(props.granularity);
  const range = useBoundDate<DateRange>(bound, {
    read: (value) => readRange(value, time),
    write: (value) => writeRange(value, time),
    fits: (value) => rangeFits(value, { minValue: props.minValue, maxValue: props.maxValue }),
    required: props.isRequired === true,
  });
  const picking = usePicking(range);
  return (
    <>
      <DrawnRangePicker
        {...props}
        validationBehavior={validationBehavior}
        value={range.value}
        missing={range.missing}
        onKey={range.heard}
        onOpenChange={(isOpen) => {
          picking.onOpenChange(isOpen);
          onOpenChange?.(isOpen);
        }}
        onChange={(value) => {
          picking.onChange(value);
          onChange?.(value);
        }}
        onBlur={(event) => {
          picking.onBlur();
          onBlur?.(event);
        }}
        onKeyDown={(event) => {
          picking.onKeyDown(event);
          onKeyDown?.(event);
        }}
      />
      <Refusal error={bound.error} />
    </>
  );
}

// The label, the field with its button, the description, the error message,
// which says why the picker is invalid, or which value of its `bound` it shows
// as missing, and the popover with the calendar.
function DrawnPicker<T extends DateValue>({
  label,
  description,
  errorMessage,
  missing = null,
  onKey,
  isInvalid,
  className,
  ...props
}: DrawnProps<T>): ReactElement {
  return (
    <AriaDatePicker
      {...props}
      isInvalid={missing === null ? isInvalid : true}
      className={composeRenderProps(className, (given) => `bk:group ${styles.field} ${given ?? ""}`)}
    >
      {label === undefined ? null : <Label className={styles.label}>{label}</Label>}
      <Group className={FIELD}>
        <Segments onKey={onKey}>
          <DateInput className={`${INLINE} bk:flex-1`} />
        </Segments>
        <Opener />
      </Group>
      <PickerParts description={description} errorMessage={errorMessage} missing={missing} />
      <Popover className={POPOVER}>
        <Calendar />
      </Popover>
    </AriaDatePicker>
  );
}

// The field holds the two fields of the range, with a dash between them.
function DrawnRangePicker<T extends DateValue>({
  label,
  description,
  errorMessage,
  missing = null,
  onKey,
  isInvalid,
  className,
  ...props
}: DrawnRangeProps<T>): ReactElement {
  return (
    <AriaDateRangePicker
      {...props}
      isInvalid={missing === null ? isInvalid : true}
      className={composeRenderProps(className, (given) => `bk:group ${styles.field} ${given ?? ""}`)}
    >
      {label === undefined ? null : <Label className={styles.label}>{label}</Label>}
      <Group className={FIELD}>
        <Segments onKey={onKey}>
          <div className={ENDS}>
            <DateInput slot="start" className={INLINE} />
            <span aria-hidden="true" className="bk:px-1 bk:text-muted-foreground">
              –
            </span>
            <DateInput slot="end" className={`${INLINE} bk:flex-1`} />
          </div>
        </Segments>
        <Opener />
      </Group>
      <PickerParts description={description} errorMessage={errorMessage} missing={missing} />
      <Popover className={POPOVER}>
        <RangeCalendar />
      </Popover>
    </AriaDateRangePicker>
  );
}

function PickerParts({
  description,
  errorMessage,
  missing,
}: Pick<DrawnProps<DateValue>, "description" | "errorMessage" | "missing">): ReactElement {
  return (
    <>
      {/* React Aria names the description of the picker only where the
          element is there from the start, so it is there while empty too. */}
      <Text slot="description" className={`${styles.hint} bk:empty:hidden`}>
        {description}
      </Text>
      <FieldError className={styles.alert}>{errorMessage}</FieldError>
      {missing === null || missing === undefined ? null : <NotAvailable values={[missing]} />}
    </>
  );
}

// The button at the field's right, which opens the calendar, blockr.ui's button
// inside a field, as in the starter kit.
function Opener(): ReactElement {
  return (
    <FieldButton>
      <Icon icon={CalendarIcon} />
    </FieldButton>
  );
}

// The classes of the picker and its parts.
// The field is a box like blockr.ui's other inputs, which holds the segments and
// the button, and draws the outline of the focus while the focus is in it.
// The fields of a range scroll sideways where they are wider than the box.
const FIELD =
  "bk:flex bk:min-w-0 bk:items-center bk:gap-1 bk:justify-self-stretch bk:rounded-md bk:border bk:border-input bk:bg-transparent bk:pr-0.5 bk:pl-2 bk:font-sans bk:text-sm bk:data-focus-within:outline-2 bk:data-focus-within:outline-offset-1 bk:data-focus-within:outline-ring bk:data-invalid:border-destructive bk:data-disabled:opacity-50";
const ENDS = "bk:flex bk:min-w-0 bk:flex-1 bk:items-center bk:overflow-x-auto bk:[scrollbar-width:none]";
const POPOVER = "bk:p-3";
