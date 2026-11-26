// The calendar and the range calendar of blockr.ui: React Aria's
// Calendar and RangeCalendar (https://react-aria.adobe.com/Calendar,
// https://react-aria.adobe.com/RangeCalendar), drawn with the theme variables.
// They are drawn after the Calendar and the RangeCalendar of React Aria's
// Tailwind starter kit
// (https://github.com/adobe/react-spectrum/blob/react-aria-components%401.22.1/starters/tailwind/src/Calendar.tsx,
// https://github.com/adobe/react-spectrum/blob/react-aria-components%401.22.1/starters/tailwind/src/RangeCalendar.tsx),
// and take what those take: the props of their part of React Aria, and the
// error message that the starter kit adds.
// Each shows the months that its visibleDuration gives, one by default, side
// by side, with buttons that page to the months before and after.
// The user picks a day, or the first and the last day of a range, by a press
// or by the arrow keys and Enter.
// The date pickers of blockr.ui open them (./datepicker.tsx).
//
// Given `bound`, the calendar shows and picks the date that its `bound` gives,
// as blockr.ui's other inputs do (./inputs.tsx), as a string such as
// "2026-11-19" (./dates.ts).
// The range calendar picks a range in the same way, as { "start": "2026-11-19",
// "end": "2026-11-26" }.
// Each submits a pick at once (./bounddate.ts).
// It says below itself why its last submit was refused, the `error` of its
// `bound`.

import type { ReactElement } from "react";
import {
  Calendar as AriaCalendar,
  CalendarCell,
  CalendarGrid,
  CalendarGridBody,
  CalendarGridHeader,
  CalendarHeaderCell,
  CalendarHeading,
  composeRenderProps,
  RangeCalendar as AriaRangeCalendar,
  Text,
  useLocale,
} from "react-aria-components";
import type { CalendarDate } from "@internationalized/date";
import type {
  CalendarProps as AriaCalendarProps,
  DateRange,
  DateValue,
  RangeCalendarProps as AriaRangeCalendarProps,
} from "react-aria-components";
import { Icon } from "../icon/index.ts";
import { styles } from "../styles/index.ts";
import { ChevronLeft, ChevronRight } from "../vendor/index.ts";
import type { Bound } from "./bound.ts";
import { useBoundDate } from "./bounddate.ts";
import { Button } from "./button.tsx";
import { dateFits, rangeFits, readDate, readRange, writeDate, writeRange } from "./dates.ts";
import { NotAvailable, Refusal } from "./inputs.tsx";

export interface CalendarProps<T extends DateValue> extends AriaCalendarProps<T> {
  // What the starter kit adds.
  readonly errorMessage?: string;
  // The value that it shows and submits.
  readonly bound?: Bound;
}

export interface RangeCalendarProps<T extends DateValue> extends AriaRangeCalendarProps<T> {
  // What the starter kit adds.
  readonly errorMessage?: string;
  // The value that it shows and submits.
  readonly bound?: Bound;
}

export function Calendar<T extends DateValue>({ bound, ...props }: CalendarProps<T>): ReactElement {
  return bound === undefined ? <DrawnCalendar {...props} /> : <BoundCalendar {...(props as DrawnProps<DateValue>)} bound={bound} />;
}

export function RangeCalendar<T extends DateValue>({ bound, ...props }: RangeCalendarProps<T>): ReactElement {
  return bound === undefined ? (
    <DrawnRange {...props} />
  ) : (
    <BoundRange {...(props as DrawnRangeProps<DateValue>)} bound={bound} />
  );
}

// What a calendar draws with, and the value of its `bound` that it shows as missing,
// where it shows one.
type DrawnProps<T extends DateValue> = Omit<CalendarProps<T>, "bound"> & { readonly missing?: string | null };
type DrawnRangeProps<T extends DateValue> = Omit<RangeCalendarProps<T>, "bound"> & { readonly missing?: string | null };

function BoundCalendar({ bound, onChange, ...props }: DrawnProps<DateValue> & { readonly bound: Bound }): ReactElement {
  const date = useBoundDate<DateValue>(bound, {
    read: (value) => readDate(value, false),
    write: (value) => writeDate(value, false),
    fits: (value) => dateFits(value, props),
    required: true,
  });
  return (
    <>
      <DrawnCalendar
        {...props}
        value={date.value}
        missing={date.missing}
        onChange={(value) => {
          date.pick(value);
          onChange?.(value);
        }}
      />
      <Refusal error={bound.error} />
    </>
  );
}

function BoundRange({ bound, onChange, ...props }: DrawnRangeProps<DateValue> & { readonly bound: Bound }): ReactElement {
  const range = useBoundDate<DateRange>(bound, {
    read: (value) => readRange(value, false),
    write: (value) => writeRange(value, false),
    fits: (value) => rangeFits(value, { minValue: props.minValue, maxValue: props.maxValue }),
    required: true,
  });
  return (
    <>
      <DrawnRange
        {...props}
        value={range.value}
        missing={range.missing}
        onChange={(value) => {
          range.pick(value);
          onChange?.(value);
        }}
      />
      <Refusal error={bound.error} />
    </>
  );
}

// The months, each under its heading, the error message, and the line that
// names a value of its `bound` that the calendar shows as missing, as blockr.ui's
// other inputs name a value that they lack.
function DrawnCalendar<T extends DateValue>({ errorMessage, missing = null, isInvalid, className, ...props }: DrawnProps<T>): ReactElement {
  return (
    <AriaCalendar
      {...props}
      isInvalid={missing === null ? isInvalid : true}
      className={composeRenderProps(className, (given) => `${CALENDAR} ${given ?? ""}`)}
    >
      <Months count={props.visibleDuration?.months ?? 1} cell={(date) => <CalendarCell date={date} className={CELL} />} />
      {errorMessage === undefined ? null : (
        <Text slot="errorMessage" className={styles.alert}>
          {errorMessage}
        </Text>
      )}
      {missing === null ? null : <NotAvailable values={[missing]} />}
    </AriaCalendar>
  );
}

function DrawnRange<T extends DateValue>({ errorMessage, missing = null, isInvalid, className, ...props }: DrawnRangeProps<T>): ReactElement {
  return (
    <AriaRangeCalendar
      {...props}
      isInvalid={missing === null ? isInvalid : true}
      className={composeRenderProps(className, (given) => `${CALENDAR} ${given ?? ""}`)}
    >
      <Months
        count={props.visibleDuration?.months ?? 1}
        cell={(date) => (
          <CalendarCell date={date} className={RANGE_CELL}>
            {({ formattedDate, isSelected, isSelectionStart, isSelectionEnd }) => (
              <span className={isSelected && (isSelectionStart || isSelectionEnd) ? `${DAY} ${END}` : `${DAY} ${isSelected ? "" : UNPICKED}`}>
                {formattedDate}
              </span>
            )}
          </CalendarCell>
        )}
      />
      {errorMessage === undefined ? null : (
        <Text slot="errorMessage" className={styles.alert}>
          {errorMessage}
        </Text>
      )}
      {missing === null ? null : <NotAvailable values={[missing]} />}
    </AriaRangeCalendar>
  );
}

// The months of a calendar, `count` of them side by side, each with its
// heading and its grid of days, which `cell` draws.
// The first month has the button to the months before, and the last the
// button to those after, each with the arrow that points that way in the
// direction of the user's language.
function Months({ count, cell }: { readonly count: number; readonly cell: (date: CalendarDate) => ReactElement }): ReactElement {
  const { direction } = useLocale();
  return (
    <>
      {Array.from({ length: count }, (_, i) => (
        <div key={i} className={MONTH}>
          <header className={HEADER}>
            {i === 0 ? (
              <Button variant="quiet" slot="previous" className={PAGE}>
                <Arrow back={direction !== "rtl"} />
              </Button>
            ) : null}
            <CalendarHeading offset={{ months: i }} className={HEADING} />
            {i === count - 1 ? (
              <Button variant="quiet" slot="next" className={PAGE}>
                <Arrow back={direction === "rtl"} />
              </Button>
            ) : null}
          </header>
          <CalendarGrid offset={{ months: i }} className={GRID}>
            <CalendarGridHeader>{(day) => <CalendarHeaderCell className={WEEKDAY}>{day}</CalendarHeaderCell>}</CalendarGridHeader>
            <CalendarGridBody>{cell}</CalendarGridBody>
          </CalendarGrid>
        </div>
      ))}
    </>
  );
}

// The chevron of a button that pages to the months before, where `back`, or
// to those after.
function Arrow({ back }: { readonly back: boolean }): ReactElement {
  return <Icon icon={back ? ChevronLeft : ChevronRight} size={12} />;
}

// The classes of the calendar and its parts.
// A day is a circle, which shows in the brand colour where it is picked, as
// does each end of a range, while the days between the ends of a range show
// on a band in a tint of it.
// A day that is not picked takes the fill of a hover under the pointer.
// A day that the calendar does not take, as one before minValue or one of the
// months around the month shown, shows faded, and one that
// isDateUnavailable takes away shows struck through.
// A picked day of an invalid calendar shows in the colour of an error.
const CALENDAR = "bk:flex bk:w-fit bk:max-w-full bk:gap-3 bk:overflow-auto bk:font-sans bk:text-sm";
const MONTH = "bk:flex bk:flex-col bk:gap-2";
const HEADER = "bk:flex bk:items-center bk:gap-1";
const HEADING = "bk:m-0 bk:flex-1 bk:text-center bk:text-sm bk:font-semibold";
const PAGE = "bk:size-7 bk:justify-center bk:px-0 bk:text-inherit";
const GRID = "bk:border-collapse bk:border-spacing-0";
const WEEKDAY = "bk:size-8 bk:p-0 bk:text-xs bk:font-medium bk:text-muted-foreground";
const CELL =
  "bk:flex bk:size-8 bk:cursor-pointer bk:items-center bk:justify-center bk:rounded-full bk:text-sm bk:tabular-nums bk:outline-none bk:data-hovered:bg-accent bk:data-hovered:text-accent-foreground bk:data-pressed:bg-accent bk:data-selected:bg-primary bk:data-selected:text-primary-foreground bk:data-today:font-semibold bk:data-focus-visible:outline-2 bk:data-focus-visible:outline-offset-1 bk:data-focus-visible:outline-ring bk:data-disabled:cursor-default bk:data-disabled:opacity-40 bk:data-unavailable:cursor-default bk:data-unavailable:line-through bk:data-unavailable:opacity-40 bk:data-invalid:data-selected:bg-destructive";
const RANGE_CELL =
  "bk:group bk:size-8 bk:cursor-pointer bk:text-sm bk:tabular-nums bk:outline-none bk:data-selected:bg-primary/15 bk:data-selection-start:rounded-s-full bk:data-selection-end:rounded-e-full bk:[td:first-child_&]:rounded-s-full bk:[td:last-child_&]:rounded-e-full bk:data-disabled:cursor-default bk:data-disabled:opacity-40 bk:data-unavailable:cursor-default bk:data-unavailable:line-through bk:data-unavailable:opacity-40 bk:data-invalid:data-selected:bg-destructive/15";
const DAY =
  "bk:flex bk:size-full bk:items-center bk:justify-center bk:rounded-full bk:group-data-today:font-semibold bk:group-data-focus-visible:outline-2 bk:group-data-focus-visible:outline-offset-1 bk:group-data-focus-visible:outline-ring";
const END = "bk:bg-primary bk:text-primary-foreground bk:group-data-invalid:bg-destructive";
const UNPICKED = "bk:group-data-hovered:bg-accent bk:group-data-hovered:text-accent-foreground bk:group-data-pressed:bg-accent";
