// srcts/icon/icon.tsx
import { createElement } from "react";
import { jsx } from "react/jsx-runtime";
var SIZE = 14;
var STROKE_WIDTH = 1.875;
var VIEW_BOX = "0 0 24 24";
function Icon({ icon, size = SIZE, label }) {
  return /* @__PURE__ */ jsx(
    "svg",
    {
      width: size,
      height: size,
      viewBox: VIEW_BOX,
      fill: "none",
      stroke: "currentColor",
      strokeWidth: STROKE_WIDTH,
      strokeLinecap: "round",
      strokeLinejoin: "round",
      ...label === void 0 ? { "aria-hidden": true } : { role: "img", "aria-label": label },
      children: icon.map(([tag, attributes], i) => createElement(tag, { key: i, ...attributes }))
    }
  );
}
function iconMarkup(icon, size = SIZE) {
  const svg = attributeList({
    width: size,
    height: size,
    viewBox: VIEW_BOX,
    fill: "none",
    stroke: "currentColor",
    "stroke-width": STROKE_WIDTH,
    "stroke-linecap": "round",
    "stroke-linejoin": "round",
    "aria-hidden": "true"
  });
  return `<svg${svg}>${icon.map(([tag, attributes]) => `<${tag}${attributeList(attributes)}/>`).join("")}</svg>`;
}
function attributeList(attributes) {
  return Object.entries(attributes).flatMap(([name, value]) => value === void 0 ? [] : [` ${name}="${escaped(String(value))}"`]).join("");
}
function escaped(value) {
  return value.replace(/[&<>"']/g, (char) => `&#${String(char.charCodeAt(0))};`);
}

// srcts/inputs/button.tsx
import { Button as AriaButton, composeRenderProps } from "react-aria-components";

// srcts/styles/styles.ts
var styles = {
  formLayout: "bk:grid bk:gap-3 bk:font-sans",
  field: "bk:grid bk:gap-1 bk:font-sans",
  label: "bk:text-sm bk:font-medium",
  box: "bk:min-w-0 bk:justify-self-stretch bk:rounded-md bk:border bk:border-input bk:bg-transparent bk:px-2 bk:py-1 bk:font-sans bk:text-sm bk:text-inherit bk:placeholder:text-muted-foreground bk:focus:outline-2 bk:focus:outline-offset-1 bk:focus:outline-ring bk:aria-invalid:border-destructive",
  hint: "bk:m-0 bk:text-sm bk:text-muted-foreground",
  alert: "bk:m-0 bk:text-sm bk:text-destructive-text bk:whitespace-pre-wrap"
};
var buttons = {
  primary: "bk:inline-flex bk:items-center bk:gap-1 bk:rounded-md bk:border bk:border-primary bk:bg-primary bk:px-2 bk:py-1 bk:font-sans bk:text-sm bk:text-primary-foreground bk:cursor-pointer bk:disabled:cursor-default bk:disabled:opacity-50",
  secondary: "bk:inline-flex bk:items-center bk:gap-1 bk:rounded-md bk:border bk:border-border bk:bg-transparent bk:px-2 bk:py-1 bk:font-sans bk:text-sm bk:text-inherit bk:cursor-pointer bk:hover:bg-accent bk:hover:text-accent-foreground bk:disabled:cursor-default bk:disabled:opacity-50",
  destructive: "bk:inline-flex bk:items-center bk:gap-1 bk:rounded-md bk:border bk:border-destructive bk:bg-destructive bk:px-2 bk:py-1 bk:font-sans bk:text-sm bk:text-destructive-foreground bk:cursor-pointer bk:hover:bg-destructive/90 bk:disabled:cursor-default bk:disabled:opacity-50",
  quiet: "bk:inline-flex bk:items-center bk:gap-1 bk:rounded-md bk:border-0 bk:bg-transparent bk:px-1.5 bk:py-0.5 bk:font-sans bk:text-xs bk:text-muted-foreground bk:cursor-pointer bk:hover:bg-accent bk:hover:text-accent-foreground"
};

// srcts/inputs/button.tsx
import { jsx as jsx2 } from "react/jsx-runtime";
function Button({ variant = "primary", className, "aria-keyshortcuts": shortcuts, ...props }) {
  return /* @__PURE__ */ jsx2(
    AriaButton,
    {
      ...props,
      render: shortcuts === void 0 || props.render !== void 0 ? props.render : (dom) => /* @__PURE__ */ jsx2("button", { ...dom, "aria-keyshortcuts": shortcuts }),
      className: composeRenderProps(className, (given) => `${buttons[variant]} ${given ?? ""}`)
    }
  );
}
function FieldButton({ className, ...props }) {
  return /* @__PURE__ */ jsx2(AriaButton, { ...props, className: composeRenderProps(className, (given) => `${FIELD_BUTTON} ${given ?? ""}`) });
}
var FIELD_BUTTON = "bk:flex bk:size-5 bk:shrink-0 bk:cursor-pointer bk:items-center bk:justify-center bk:rounded-sm bk:border-0 bk:bg-transparent bk:p-0 bk:text-muted-foreground bk:outline-none bk:data-hovered:bg-accent bk:data-hovered:text-accent-foreground bk:data-focus-visible:outline-2 bk:data-focus-visible:outline-ring bk:data-disabled:cursor-default bk:data-disabled:opacity-50";

// srcts/inputs/calendar.tsx
import {
  Calendar as AriaCalendar,
  CalendarCell,
  CalendarGrid,
  CalendarGridBody,
  CalendarGridHeader,
  CalendarHeaderCell,
  CalendarHeading,
  composeRenderProps as composeRenderProps3,
  RangeCalendar as AriaRangeCalendar,
  Text as Text2,
  useLocale
} from "react-aria-components";
import { ChevronLeft, ChevronRight } from "./vendor-723651e1.js";

// srcts/inputs/bounddate.ts
import { useRef, useState } from "react";

// srcts/inputs/json.ts
function own(record, key) {
  return Object.hasOwn(record, key) ? record[key] : void 0;
}
function isObject(x) {
  return typeof x === "object" && x !== null && !Array.isArray(x);
}
function jsonEqual(a, b) {
  if (a === b) return true;
  if (Array.isArray(a)) {
    return Array.isArray(b) && a.length === b.length && a.every((x, i) => jsonEqual(x, b[i]));
  }
  if (isObject(a) && isObject(b)) {
    const keys = Object.keys(a);
    return keys.length === Object.keys(b).length && keys.every((key) => Object.hasOwn(b, key) && jsonEqual(a[key], b[key]));
  }
  return false;
}
function strings(value) {
  return Array.isArray(value) ? value.filter((x) => typeof x === "string") : [];
}

// srcts/inputs/dates.ts
import { parseDate, parseDateTime, parseTime, toCalendarDate, toCalendarDateTime, toTime } from "./vendor-723651e1.js";
var DATE = /^\d{4}-\d{2}-\d{2}$/;
var DATE_TIME = /^(\d{4}-\d{2}-\d{2})(?: (\d{2}:\d{2}(?::\d{2})?))?$/;
var TIME = /^\d{2}:\d{2}(?::\d{2})?$/;
function takesTime(granularity) {
  return granularity !== void 0 && granularity !== "day";
}
function readDate(value, time) {
  if (typeof value !== "string") return void 0;
  const parts = (time ? DATE_TIME : DATE).exec(value);
  if (parts === null) return void 0;
  try {
    return time ? parseDateTime(parts[2] === void 0 ? value : `${parts[1] ?? ""}T${parts[2]}`) : parseDate(value);
  } catch {
    return void 0;
  }
}
function writeDate(value, time) {
  return time ? toCalendarDateTime(value).toString().replace("T", " ") : toCalendarDate(value).toString();
}
function readTime(value) {
  if (typeof value !== "string" || !TIME.test(value)) return void 0;
  try {
    return parseTime(value);
  } catch {
    return void 0;
  }
}
function writeTime(value) {
  return ("day" in value ? toTime(value) : value).toString();
}
function readRange(value, time) {
  if (typeof value !== "object" || value === null || Array.isArray(value)) return void 0;
  const record = value;
  const start = readDate(own(record, "start") ?? null, time);
  const end = readDate(own(record, "end") ?? null, time);
  return start === void 0 || end === void 0 ? void 0 : { start, end };
}
function writeRange(value, time) {
  return { start: writeDate(value.start, time), end: writeDate(value.end, time) };
}
function dateFits(value, { minValue, maxValue, isDateUnavailable }) {
  return (minValue === void 0 || minValue === null || value.compare(minValue) >= 0) && (maxValue === void 0 || maxValue === null || value.compare(maxValue) <= 0) && isDateUnavailable?.(value) !== true;
}
function rangeFits(value, bounds) {
  return value.start.compare(value.end) <= 0 && dateFits(value.start, bounds) && dateFits(value.end, bounds);
}
function timeFits(value, { minValue, maxValue }) {
  const time = "day" in value ? toTime(value) : value;
  const at = (bound) => time.compare("day" in bound ? toTime(bound) : bound);
  return (minValue === void 0 || minValue === null || at(minValue) >= 0) && (maxValue === void 0 || maxValue === null || at(maxValue) <= 0);
}
function shownValue(value) {
  return typeof value === "string" ? value : JSON.stringify(value);
}

// srcts/inputs/bounddate.ts
var STEPS = /* @__PURE__ */ new Set(["ArrowUp", "ArrowDown", "PageUp", "PageDown", "Home", "End"]);
function useBoundDate(bound, { read, write, fits, required }) {
  const held2 = bound.value === void 0 ? null : read(bound.value);
  const [kept, setKept] = useState(null);
  const latest = useRef(null);
  const keep = (next) => {
    latest.current = next;
    setKept(next);
  };
  const stepped = useRef(false);
  const [seen, setSeen] = useState(bound.value);
  if (!Object.is(seen, bound.value)) {
    setSeen(bound.value);
    if (kept?.submitted === true) setKept(null);
  }
  const submit = (value) => {
    const next = value === null ? void 0 : write(value);
    if (value === null ? required : !fits(value)) {
      keep(null);
      return;
    }
    if (jsonEqual(next, bound.value)) {
      keep(null);
      return;
    }
    keep({ value, submitted: true });
    void (next === void 0 ? bound.unset() : bound.commit(next)).then((took) => {
      if (!took) keep(null);
    });
  };
  return {
    value: kept === null ? held2 ?? null : kept.value,
    missing: kept === null && bound.value !== void 0 && held2 === void 0 ? shownValue(bound.value) : null,
    heard: (key) => {
      stepped.current = STEPS.has(key);
    },
    change: (value) => {
      if (stepped.current) {
        stepped.current = false;
        submit(value);
      } else {
        keep({ value, submitted: false });
      }
    },
    pick: submit,
    flush: () => {
      const edit = latest.current;
      if (edit !== null && !edit.submitted) submit(edit.value);
    }
  };
}

// srcts/inputs/inputs.tsx
import { useState as useState2 } from "react";
import { composeRenderProps as composeRenderProps2, FieldError as AriaFieldError, Label as AriaLabel, Text } from "react-aria-components";
import { jsx as jsx3, jsxs } from "react/jsx-runtime";
function FormLayout({ children }) {
  return /* @__PURE__ */ jsx3("div", { className: styles.formLayout, children });
}
function Field({ label, children }) {
  return /* @__PURE__ */ jsxs("label", { className: styles.field, children: [
    /* @__PURE__ */ jsx3("span", { className: styles.label, children: label }),
    children
  ] });
}
function Label({ className, ...props }) {
  return /* @__PURE__ */ jsx3(AriaLabel, { ...props, className: `${styles.label} ${className ?? ""}` });
}
function Description({ className, ...props }) {
  return /* @__PURE__ */ jsx3(Text, { ...props, slot: "description", className: `${styles.hint} bk:empty:hidden ${className ?? ""}` });
}
function FieldError({ className, ...props }) {
  return /* @__PURE__ */ jsx3(AriaFieldError, { ...props, className: composeRenderProps2(className, (given) => `${styles.alert} ${given ?? ""}`) });
}
function NotAvailable({
  values,
  onRemove
}) {
  if (values.length === 0) return null;
  const named = values.join(", ");
  return /* @__PURE__ */ jsxs("div", { className: "bk:flex bk:flex-wrap bk:items-center bk:gap-x-2", children: [
    /* @__PURE__ */ jsx3("p", { role: "alert", className: styles.alert, children: `Not available: ${named}` }),
    onRemove === void 0 ? null : /* @__PURE__ */ jsx3(Button, { variant: "quiet", "aria-label": `Remove ${named}`, onPress: onRemove, children: "Remove" })
  ] });
}
function Refusal({ error }) {
  return error === null ? null : /* @__PURE__ */ jsx3("p", { role: "alert", className: styles.alert, children: error });
}
function useTyped(bound) {
  const [text, setText] = useState2(null);
  const [seen, setSeen] = useState2(bound.value);
  if (!Object.is(seen, bound.value)) {
    setSeen(bound.value);
    setText(null);
  }
  return [text, setText];
}

// srcts/inputs/calendar.tsx
import { Fragment, jsx as jsx4, jsxs as jsxs2 } from "react/jsx-runtime";
function Calendar({ bound, ...props }) {
  return bound === void 0 ? /* @__PURE__ */ jsx4(DrawnCalendar, { ...props }) : /* @__PURE__ */ jsx4(BoundCalendar, { ...props, bound });
}
function RangeCalendar({ bound, ...props }) {
  return bound === void 0 ? /* @__PURE__ */ jsx4(DrawnRange, { ...props }) : /* @__PURE__ */ jsx4(BoundRange, { ...props, bound });
}
function BoundCalendar({ bound, onChange, ...props }) {
  const date = useBoundDate(bound, {
    read: (value) => readDate(value, false),
    write: (value) => writeDate(value, false),
    fits: (value) => dateFits(value, props),
    required: true
  });
  return /* @__PURE__ */ jsxs2(Fragment, { children: [
    /* @__PURE__ */ jsx4(
      DrawnCalendar,
      {
        ...props,
        value: date.value,
        missing: date.missing,
        onChange: (value) => {
          date.pick(value);
          onChange?.(value);
        }
      }
    ),
    /* @__PURE__ */ jsx4(Refusal, { error: bound.error })
  ] });
}
function BoundRange({ bound, onChange, ...props }) {
  const range = useBoundDate(bound, {
    read: (value) => readRange(value, false),
    write: (value) => writeRange(value, false),
    fits: (value) => rangeFits(value, { minValue: props.minValue, maxValue: props.maxValue }),
    required: true
  });
  return /* @__PURE__ */ jsxs2(Fragment, { children: [
    /* @__PURE__ */ jsx4(
      DrawnRange,
      {
        ...props,
        value: range.value,
        missing: range.missing,
        onChange: (value) => {
          range.pick(value);
          onChange?.(value);
        }
      }
    ),
    /* @__PURE__ */ jsx4(Refusal, { error: bound.error })
  ] });
}
function DrawnCalendar({ errorMessage, missing = null, isInvalid, className, ...props }) {
  return /* @__PURE__ */ jsxs2(
    AriaCalendar,
    {
      ...props,
      isInvalid: missing === null ? isInvalid : true,
      className: composeRenderProps3(className, (given) => `${CALENDAR} ${given ?? ""}`),
      children: [
        /* @__PURE__ */ jsx4(Months, { count: props.visibleDuration?.months ?? 1, cell: (date) => /* @__PURE__ */ jsx4(CalendarCell, { date, className: CELL }) }),
        errorMessage === void 0 ? null : /* @__PURE__ */ jsx4(Text2, { slot: "errorMessage", className: styles.alert, children: errorMessage }),
        missing === null ? null : /* @__PURE__ */ jsx4(NotAvailable, { values: [missing] })
      ]
    }
  );
}
function DrawnRange({ errorMessage, missing = null, isInvalid, className, ...props }) {
  return /* @__PURE__ */ jsxs2(
    AriaRangeCalendar,
    {
      ...props,
      isInvalid: missing === null ? isInvalid : true,
      className: composeRenderProps3(className, (given) => `${CALENDAR} ${given ?? ""}`),
      children: [
        /* @__PURE__ */ jsx4(
          Months,
          {
            count: props.visibleDuration?.months ?? 1,
            cell: (date) => /* @__PURE__ */ jsx4(CalendarCell, { date, className: RANGE_CELL, children: ({ formattedDate, isSelected, isSelectionStart, isSelectionEnd }) => /* @__PURE__ */ jsx4("span", { className: isSelected && (isSelectionStart || isSelectionEnd) ? `${DAY} ${END}` : `${DAY} ${isSelected ? "" : UNPICKED}`, children: formattedDate }) })
          }
        ),
        errorMessage === void 0 ? null : /* @__PURE__ */ jsx4(Text2, { slot: "errorMessage", className: styles.alert, children: errorMessage }),
        missing === null ? null : /* @__PURE__ */ jsx4(NotAvailable, { values: [missing] })
      ]
    }
  );
}
function Months({ count, cell }) {
  const { direction } = useLocale();
  return /* @__PURE__ */ jsx4(Fragment, { children: Array.from({ length: count }, (_, i) => /* @__PURE__ */ jsxs2("div", { className: MONTH, children: [
    /* @__PURE__ */ jsxs2("header", { className: HEADER, children: [
      i === 0 ? /* @__PURE__ */ jsx4(Button, { variant: "quiet", slot: "previous", className: PAGE, children: /* @__PURE__ */ jsx4(Arrow, { back: direction !== "rtl" }) }) : null,
      /* @__PURE__ */ jsx4(CalendarHeading, { offset: { months: i }, className: HEADING }),
      i === count - 1 ? /* @__PURE__ */ jsx4(Button, { variant: "quiet", slot: "next", className: PAGE, children: /* @__PURE__ */ jsx4(Arrow, { back: direction === "rtl" }) }) : null
    ] }),
    /* @__PURE__ */ jsxs2(CalendarGrid, { offset: { months: i }, className: GRID, children: [
      /* @__PURE__ */ jsx4(CalendarGridHeader, { children: (day) => /* @__PURE__ */ jsx4(CalendarHeaderCell, { className: WEEKDAY, children: day }) }),
      /* @__PURE__ */ jsx4(CalendarGridBody, { children: cell })
    ] })
  ] }, i)) });
}
function Arrow({ back }) {
  return /* @__PURE__ */ jsx4(Icon, { icon: back ? ChevronLeft : ChevronRight, size: 12 });
}
var CALENDAR = "bk:flex bk:w-fit bk:max-w-full bk:gap-3 bk:overflow-auto bk:font-sans bk:text-sm";
var MONTH = "bk:flex bk:flex-col bk:gap-2";
var HEADER = "bk:flex bk:items-center bk:gap-1";
var HEADING = "bk:m-0 bk:flex-1 bk:text-center bk:text-sm bk:font-semibold";
var PAGE = "bk:size-7 bk:justify-center bk:px-0 bk:text-inherit";
var GRID = "bk:border-collapse bk:border-spacing-0";
var WEEKDAY = "bk:size-8 bk:p-0 bk:text-xs bk:font-medium bk:text-muted-foreground";
var CELL = "bk:flex bk:size-8 bk:cursor-pointer bk:items-center bk:justify-center bk:rounded-full bk:text-sm bk:tabular-nums bk:outline-none bk:data-hovered:bg-accent bk:data-hovered:text-accent-foreground bk:data-pressed:bg-accent bk:data-selected:bg-primary bk:data-selected:text-primary-foreground bk:data-today:font-semibold bk:data-focus-visible:outline-2 bk:data-focus-visible:outline-offset-1 bk:data-focus-visible:outline-ring bk:data-disabled:cursor-default bk:data-disabled:opacity-40 bk:data-unavailable:cursor-default bk:data-unavailable:line-through bk:data-unavailable:opacity-40 bk:data-invalid:data-selected:bg-destructive";
var RANGE_CELL = "bk:group bk:size-8 bk:cursor-pointer bk:text-sm bk:tabular-nums bk:outline-none bk:data-selected:bg-primary/15 bk:data-selection-start:rounded-s-full bk:data-selection-end:rounded-e-full bk:[td:first-child_&]:rounded-s-full bk:[td:last-child_&]:rounded-e-full bk:data-disabled:cursor-default bk:data-disabled:opacity-40 bk:data-unavailable:cursor-default bk:data-unavailable:line-through bk:data-unavailable:opacity-40 bk:data-invalid:data-selected:bg-destructive/15";
var DAY = "bk:flex bk:size-full bk:items-center bk:justify-center bk:rounded-full bk:group-data-today:font-semibold bk:group-data-focus-visible:outline-2 bk:group-data-focus-visible:outline-offset-1 bk:group-data-focus-visible:outline-ring";
var END = "bk:bg-primary bk:text-primary-foreground bk:group-data-invalid:bg-destructive";
var UNPICKED = "bk:group-data-hovered:bg-accent bk:group-data-hovered:text-accent-foreground bk:group-data-pressed:bg-accent";

// srcts/inputs/checkbox.tsx
import { useRef as useRef3 } from "react";
import {
  CheckboxButton,
  CheckboxField,
  CheckboxGroup as AriaCheckboxGroup,
  composeRenderProps as composeRenderProps4
} from "react-aria-components";
import { Check, Minus } from "./vendor-723651e1.js";

// srcts/inputs/items.ts
import { createContext, useCallback, useContext, useLayoutEffect, useMemo, useRef as useRef2, useState as useState3 } from "react";
var ItemsContext = createContext(null);
function useItems() {
  const entries = useRef2(/* @__PURE__ */ new Map());
  const [known2, setKnown] = useState3(null);
  const sync = useCallback(() => {
    const next = inPageOrder([...entries.current.values()]);
    setKnown((last) => last !== null && sameValues(last, next) ? last : next);
  }, []);
  const items = useMemo(
    () => ({
      add(value, element) {
        const key = Symbol(value);
        entries.current.set(key, { value, element });
        sync();
        return () => {
          entries.current.delete(key);
          sync();
        };
      }
    }),
    [sync]
  );
  useLayoutEffect(() => {
    sync();
  });
  return { known: known2, items };
}
function useItem(value, ref) {
  const items = useContext(ItemsContext);
  useLayoutEffect(() => {
    if (items === null || value === void 0) return void 0;
    return items.add(value, () => ref.current);
  }, [items, value, ref]);
}
function inPageOrder(entries) {
  const placed = entries.map((entry) => ({ value: entry.value, element: entry.element() }));
  placed.sort((a, b) => {
    if (a.element === null || b.element === null) return a.element === null ? b.element === null ? 0 : 1 : -1;
    return a.element.compareDocumentPosition(b.element) & Node.DOCUMENT_POSITION_FOLLOWING ? -1 : 1;
  });
  return [...new Set(placed.map((entry) => entry.value))];
}
function sameValues(a, b) {
  return a.length === b.length && a.every((value, i) => value === b[i]);
}
function inOrder(values, order) {
  return [...order.filter((id) => values.includes(id)), ...values.filter((value) => !order.includes(value))];
}
function merged(now, before, picks) {
  const added = picks.filter((value) => !before.includes(value));
  const removed = before.filter((value) => !picks.includes(value));
  if (added.length === 0 && removed.length === 0) return null;
  return [...now.filter((x) => !removed.includes(x)), ...added.filter((x) => !now.includes(x))];
}
function lacking(values, known2) {
  return known2 === null ? [] : values.filter((value) => !known2.includes(value));
}
function held(values, known2) {
  return known2 === null ? [...values] : values.filter((value) => known2.includes(value));
}

// srcts/inputs/checkbox.tsx
import { Fragment as Fragment2, jsx as jsx5, jsxs as jsxs3 } from "react/jsx-runtime";
function Checkbox({ bound, ...props }) {
  return bound === void 0 ? /* @__PURE__ */ jsx5(DrawnCheckbox, { ...props }) : /* @__PURE__ */ jsx5(BoundCheckbox, { ...props, bound });
}
function BoundCheckbox({ bound, onChange, ...props }) {
  return /* @__PURE__ */ jsxs3(Fragment2, { children: [
    /* @__PURE__ */ jsx5(
      DrawnCheckbox,
      {
        ...props,
        isSelected: bound.value === true,
        onChange: (selected) => {
          void bound.commit(selected);
          onChange?.(selected);
        }
      }
    ),
    /* @__PURE__ */ jsx5(Refusal, { error: bound.error })
  ] });
}
function DrawnCheckbox({ children, description, errorMessage, className, ...props }) {
  const ref = useRef3(null);
  useItem(props.value, ref);
  return /* @__PURE__ */ jsxs3(CheckboxField, { ...props, ref, className: "bk:grid bk:gap-1 bk:font-sans", children: [
    /* @__PURE__ */ jsx5(CheckboxButton, { className: composeRenderProps4(className, (given) => `bk:group ${CHECK} ${given ?? ""}`), children: composeRenderProps4(children, (children2, { isSelected, isIndeterminate }) => /* @__PURE__ */ jsxs3(Fragment2, { children: [
      /* @__PURE__ */ jsx5("span", { className: BOX, children: isIndeterminate ? /* @__PURE__ */ jsx5(Icon, { icon: Minus, size: 12 }) : isSelected ? /* @__PURE__ */ jsx5(Icon, { icon: Check, size: 12 }) : null }),
      children2
    ] })) }),
    /* @__PURE__ */ jsx5(Description, { className: "bk:ml-6", children: description }),
    /* @__PURE__ */ jsx5(FieldError, { className: "bk:ml-6", children: errorMessage })
  ] });
}
function CheckboxGroup({ bound, outdated = false, ordered = false, ...props }) {
  return bound === void 0 ? /* @__PURE__ */ jsx5(DrawnGroup, { ...props }) : /* @__PURE__ */ jsx5(BoundGroup, { ...props, bound, outdated, ordered });
}
function BoundGroup({
  bound,
  outdated,
  ordered,
  onChange,
  children,
  ...props
}) {
  const { known: known2, items } = useItems();
  const shown = strings(bound.value);
  const gone = lacking(shown, known2);
  const marking = !outdated && gone.length > 0;
  return /* @__PURE__ */ jsxs3(Fragment2, { children: [
    /* @__PURE__ */ jsx5(
      DrawnGroup,
      {
        ...props,
        value: [...shown],
        isInvalid: props.isInvalid === true || marking,
        onChange: (picks) => {
          const value = merged(strings(bound.get()), shown, picks);
          if (value !== null) void bound.commit(ordered && known2 !== null ? inOrder(value, known2) : value);
          onChange?.(picks);
        },
        children: /* @__PURE__ */ jsx5(ItemsContext.Provider, { value: items, children })
      }
    ),
    marking ? /* @__PURE__ */ jsx5(
      NotAvailable,
      {
        values: gone,
        onRemove: () => {
          void bound.commit(held(strings(bound.get()), known2));
        }
      }
    ) : null,
    /* @__PURE__ */ jsx5(Refusal, { error: bound.error })
  ] });
}
function DrawnGroup({ label, description, errorMessage, children, className, ...props }) {
  return /* @__PURE__ */ jsxs3(AriaCheckboxGroup, { ...props, className: composeRenderProps4(className, (given) => `${styles.field} ${given ?? ""}`), children: [
    label === void 0 ? null : /* @__PURE__ */ jsx5(Label, { children: label }),
    /* @__PURE__ */ jsx5("div", { className: "bk:grid bk:gap-1.5", children }),
    /* @__PURE__ */ jsx5(Description, { children: description }),
    /* @__PURE__ */ jsx5(FieldError, { children: errorMessage })
  ] });
}
var CHECK = "bk:flex bk:w-fit bk:cursor-pointer bk:items-center bk:gap-2 bk:text-sm bk:data-disabled:cursor-default bk:data-disabled:opacity-50";
var BOX = "bk:box-border bk:flex bk:size-4 bk:shrink-0 bk:items-center bk:justify-center bk:rounded-sm bk:border bk:border-input bk:bg-transparent bk:outline-offset-2 bk:outline-ring bk:group-data-focus-visible:outline-2 bk:group-data-selected:border-primary bk:group-data-selected:bg-primary bk:group-data-selected:text-primary-foreground bk:group-data-indeterminate:border-primary bk:group-data-indeterminate:bg-primary bk:group-data-indeterminate:text-primary-foreground bk:group-data-invalid:border-destructive bk:group-data-selected:group-data-invalid:bg-destructive";

// srcts/inputs/colorfield.tsx
import { useContext as useContext2, useState as useState5 } from "react";
import { ColorField as AriaColorField, composeRenderProps as composeRenderProps5, Input, parseColor as parseColor2 } from "react-aria-components";

// srcts/inputs/colors.ts
import { createContext as createContext2, useState as useState4 } from "react";
import { parseColor } from "react-aria-components";
var FALLBACK = "#000000";
function hex(color) {
  return color.getChannelValue("alpha") < 1 ? color.toString("hexa") : color.toString("hex");
}
function valueColor(value) {
  if (typeof value !== "string") return void 0;
  try {
    parseColor(value);
  } catch {
    return void 0;
  }
  return value;
}
var Ended = createContext2(null);
function useDragged(bound) {
  const value = valueColor(bound.value);
  const [kept, setKept] = useState4(null);
  const [seen, setSeen] = useState4(value);
  if (seen !== value) {
    setSeen(value);
    if (kept !== null && !kept.dragging && (value === void 0 || hex(parseColor(value)) !== hex(kept.color))) setKept(null);
  }
  return {
    shown: kept?.color ?? value ?? FALLBACK,
    change: (color) => {
      setKept({ color, dragging: true });
    },
    end: (color) => {
      setKept({ color, dragging: false });
      void bound.commit(hex(color)).then((took) => {
        if (!took) setKept((now) => now?.dragging === true ? now : null);
      });
    }
  };
}

// srcts/inputs/colorfield.tsx
import { Fragment as Fragment3, jsx as jsx6, jsxs as jsxs4 } from "react/jsx-runtime";
function ColorField({ bound, ...props }) {
  return bound === void 0 ? /* @__PURE__ */ jsx6(Drawn, { ...props }) : /* @__PURE__ */ jsx6(BoundField, { ...props, bound });
}
function BoundField({ bound, onChange, onBlur, ...props }) {
  const value = valueColor(bound.value);
  const [typing, setTyping] = useState5(null);
  const shown = typing === null ? value : typing.value;
  return /* @__PURE__ */ jsxs4(Fragment3, { children: [
    /* @__PURE__ */ jsx6(
      Drawn,
      {
        ...props,
        value: shown ?? null,
        onInput: () => {
          setTyping((now) => now ?? { value: shown });
        },
        onChange: (color) => {
          setTyping(null);
          if (color !== null) {
            const alpha = shown === void 0 ? 1 : parseColor2(shown).getChannelValue("alpha");
            void bound.commit(hex(color.withChannelValue("alpha", alpha)));
          }
          onChange?.(color);
        },
        onBlur: (event) => {
          setTyping(null);
          onBlur?.(event);
        }
      }
    ),
    /* @__PURE__ */ jsx6(Refusal, { error: bound.error })
  ] });
}
function Drawn({ label, description, errorMessage, className, onChange, onInput, ...props }) {
  const ended = useContext2(Ended);
  return /* @__PURE__ */ jsxs4(
    AriaColorField,
    {
      ...props,
      onChange: (color) => {
        onChange?.(color);
        if (color !== null) ended?.(color);
      },
      className: composeRenderProps5(className, (given) => `${styles.field} ${given ?? ""}`),
      children: [
        label === void 0 ? null : /* @__PURE__ */ jsx6(Label, { children: label }),
        /* @__PURE__ */ jsx6(Input, { onInput, className: styles.box }),
        /* @__PURE__ */ jsx6(Description, { children: description }),
        /* @__PURE__ */ jsx6(FieldError, { children: errorMessage })
      ]
    }
  );
}

// srcts/inputs/colorparts.tsx
import { useContext as useContext3 } from "react";
import {
  ColorArea as AriaColorArea,
  ColorSlider as AriaColorSlider,
  ColorThumb as AriaColorThumb,
  ColorWheel as AriaColorWheel,
  ColorWheelTrack,
  composeRenderProps as composeRenderProps6,
  SliderOutput,
  SliderTrack
} from "react-aria-components";
import { Fragment as Fragment4, jsx as jsx7, jsxs as jsxs5 } from "react/jsx-runtime";
function ColorThumb({ className, style, ...props }) {
  return /* @__PURE__ */ jsx7(
    AriaColorThumb,
    {
      ...props,
      className: composeRenderProps6(className, (given) => `${THUMB} ${given ?? ""}`),
      style: composeRenderProps6(style, (given, { defaultStyle, isDisabled }) => ({
        ...defaultStyle,
        ...given,
        backgroundColor: isDisabled ? void 0 : defaultStyle.backgroundColor,
        boxShadow: "0 0 0 1px black, inset 0 0 0 1px black"
      }))
    }
  );
}
function ColorArea({ bound, ...props }) {
  return bound === void 0 ? /* @__PURE__ */ jsx7(Area, { ...props }) : /* @__PURE__ */ jsx7(BoundPart, { Part: Area, props, bound });
}
function ColorSlider({ bound, ...props }) {
  return bound === void 0 ? /* @__PURE__ */ jsx7(Slider, { ...props }) : /* @__PURE__ */ jsx7(BoundPart, { Part: Slider, props, bound });
}
function ColorWheel({ bound, ...props }) {
  return bound === void 0 ? /* @__PURE__ */ jsx7(Wheel, { ...props }) : /* @__PURE__ */ jsx7(BoundPart, { Part: Wheel, props, bound });
}
function BoundPart({
  Part,
  props,
  bound
}) {
  const dragged = useDragged(bound);
  return /* @__PURE__ */ jsxs5(Fragment4, { children: [
    /* @__PURE__ */ jsx7(
      Part,
      {
        ...props,
        value: dragged.shown,
        onChange: (color) => {
          dragged.change(color);
          props.onChange?.(color);
        },
        onChangeEnd: (color) => {
          dragged.end(color);
          props.onChangeEnd?.(color);
        }
      }
    ),
    /* @__PURE__ */ jsx7(Refusal, { error: bound.error })
  ] });
}
function useEnd(onChangeEnd) {
  const ended = useContext3(Ended);
  return (color) => {
    onChangeEnd?.(color);
    ended?.(color);
  };
}
function Area({ className, style, onChangeEnd, ...props }) {
  return /* @__PURE__ */ jsx7(
    AriaColorArea,
    {
      ...props,
      onChangeEnd: useEnd(onChangeEnd),
      className: composeRenderProps6(className, (given) => `${AREA} ${given ?? ""}`),
      style: composeRenderProps6(style, (given, { defaultStyle, isDisabled }) => ({
        ...defaultStyle,
        ...given,
        background: isDisabled ? void 0 : defaultStyle.background
      })),
      children: /* @__PURE__ */ jsx7(ColorThumb, {})
    }
  );
}
function Slider({ label, className, onChangeEnd, ...props }) {
  return /* @__PURE__ */ jsxs5(
    AriaColorSlider,
    {
      ...props,
      onChangeEnd: useEnd(onChangeEnd),
      className: composeRenderProps6(className, (given) => `${SLIDER} ${given ?? ""}`),
      children: [
        /* @__PURE__ */ jsx7(Label, { children: label }),
        /* @__PURE__ */ jsx7(SliderOutput, { className: OUTPUT }),
        /* @__PURE__ */ jsx7(
          SliderTrack,
          {
            className: ({ orientation, isDisabled }) => `${TRACK} ${orientation === "vertical" ? "bk:h-50 bk:w-6" : "bk:h-6 bk:w-full"} ${isDisabled ? DISABLED : ""}`,
            style: ({ defaultStyle, isDisabled }) => ({
              ...defaultStyle,
              background: isDisabled ? void 0 : `${String(defaultStyle.background)}, ${CHECKERBOARD}`
            }),
            children: /* @__PURE__ */ jsx7(ColorThumb, {})
          }
        )
      ]
    }
  );
}
function Wheel({ onChangeEnd, ...props }) {
  return /* @__PURE__ */ jsxs5(AriaColorWheel, { ...props, onChangeEnd: useEnd(onChangeEnd), outerRadius: 100, innerRadius: 74, children: [
    /* @__PURE__ */ jsx7(
      ColorWheelTrack,
      {
        className: WHEEL_TRACK,
        style: ({ defaultStyle, isDisabled }) => ({
          ...defaultStyle,
          background: isDisabled ? void 0 : `${String(defaultStyle.background)}, ${CHECKERBOARD}`
        })
      }
    ),
    /* @__PURE__ */ jsx7(ColorThumb, {})
  ] });
}
var THUMB = "bk:box-border bk:size-4.5 bk:top-[50%] bk:left-[50%] bk:rounded-full bk:border-2 bk:border-white bk:data-focus-visible:size-8 bk:data-dragging:bg-foreground bk:forced-colors:data-dragging:bg-[ButtonBorder] bk:data-disabled:border-border bk:data-disabled:bg-border bk:forced-colors:data-disabled:border-[GrayText] bk:forced-colors:data-disabled:bg-[GrayText]";
var AREA = "bk:aspect-square bk:w-full bk:max-w-56 bk:rounded-lg bk:bg-border bk:forced-colors:bg-[GrayText]";
var SLIDER = "bk:grid-cols-[1fr_auto] bk:flex-col bk:items-center bk:gap-2 bk:font-sans bk:data-[orientation=horizontal]:grid bk:data-[orientation=horizontal]:w-56 bk:data-[orientation=vertical]:flex";
var OUTPUT = "bk:text-sm bk:font-medium bk:text-muted-foreground bk:data-[orientation=vertical]:hidden";
var TRACK = "bk:group bk:col-span-2 bk:rounded-md";
var DISABLED = "bk:bg-border bk:forced-colors:bg-[GrayText]";
var WHEEL_TRACK = "bk:data-disabled:bg-border bk:forced-colors:data-disabled:bg-[GrayText]";
var CHECKERBOARD = "repeating-conic-gradient(#ccc 0% 25%, white 0% 50%) 50% / 16px 16px";

// srcts/inputs/colorpicker.tsx
import { ColorPicker as AriaColorPicker, Button as Button3 } from "react-aria-components";

// srcts/overlays/dialog.tsx
import { Button as Button2, composeRenderProps as composeRenderProps7, Dialog as AriaDialog, Heading, Modal as AriaModal, ModalOverlay } from "react-aria-components";
import { CircleExclamationPoint, Info } from "./vendor-723651e1.js";
import { Fragment as Fragment5, jsx as jsx8, jsxs as jsxs6 } from "react/jsx-runtime";
function Modal({ className, ...props }) {
  return /* @__PURE__ */ jsx8(ModalOverlay, { ...props, className: OVERLAY, children: /* @__PURE__ */ jsx8("div", { className: CENTER, children: /* @__PURE__ */ jsx8(AriaModal, { ...props, className: composeRenderProps7(className, (given) => `${MODAL} ${given ?? ""}`) }) }) });
}
function Dialog({ className, ...props }) {
  return /* @__PURE__ */ jsx8(AriaDialog, { ...props, className: `${DIALOG} ${className ?? ""}` });
}
function AlertDialog({
  title,
  variant = "info",
  actionLabel,
  cancelLabel = "Cancel",
  onAction,
  children,
  ...props
}) {
  const destructive = variant === "destructive";
  return /* @__PURE__ */ jsx8(Dialog, { role: "alertdialog", ...props, children: ({ close }) => /* @__PURE__ */ jsxs6(Fragment5, { children: [
    /* @__PURE__ */ jsx8(Heading, { slot: "title", className: TITLE, children: title }),
    /* @__PURE__ */ jsx8("span", { className: `bk:absolute bk:top-4 bk:right-4 bk:flex ${destructive ? "bk:text-destructive-text" : "bk:text-primary-text"}`, children: /* @__PURE__ */ jsx8(Icon, { icon: destructive ? CircleExclamationPoint : Info, size: 20 }) }),
    /* @__PURE__ */ jsx8("p", { className: "bk:mt-3 bk:mb-0 bk:font-sans bk:text-sm bk:text-muted-foreground", children }),
    /* @__PURE__ */ jsxs6("div", { className: "bk:mt-6 bk:flex bk:justify-end bk:gap-2", children: [
      /* @__PURE__ */ jsx8(Button2, { className: buttons.secondary, onPress: close, children: cancelLabel }),
      /* @__PURE__ */ jsx8(
        Button2,
        {
          className: destructive ? buttons.destructive : buttons.primary,
          autoFocus: true,
          onPress: () => {
            onAction?.();
            close();
          },
          children: actionLabel
        }
      )
    ] })
  ] }) });
}
var OVERLAY = "bk:absolute bk:top-0 bk:left-0 bk:isolate bk:z-overlay bk:h-(--page-height) bk:w-full bk:bg-black/40";
var CENTER = "bk:sticky bk:top-0 bk:left-0 bk:box-border bk:flex bk:h-(--visual-viewport-height) bk:w-full bk:items-center bk:justify-center bk:p-4";
var MODAL = "bk:box-border bk:max-h-[calc(var(--visual-viewport-height)*.9)] bk:w-full bk:max-w-144 bk:overflow-auto bk:rounded-lg bk:border bk:border-border bk:bg-popover bk:font-sans bk:text-popover-foreground bk:shadow-lg bk:outline-none";
var DIALOG = "bk:relative bk:box-border bk:max-h-[inherit] bk:p-4 bk:outline-none";
var TITLE = "bk:m-0 bk:pr-8 bk:font-sans bk:text-lg bk:font-semibold";

// srcts/overlays/menu.tsx
import { Children } from "react";
import {
  Collection,
  composeRenderProps as composeRenderProps10,
  Header,
  Menu as AriaMenu,
  MenuItem as AriaMenuItem,
  MenuSection as AriaMenuSection,
  MenuTrigger as AriaMenuTrigger,
  SubmenuTrigger as AriaSubmenuTrigger
} from "react-aria-components";
import { Check as Check2, ChevronRight as ChevronRight2 } from "./vendor-723651e1.js";

// srcts/overlays/popover.tsx
import { composeRenderProps as composeRenderProps9, Popover as AriaPopover } from "react-aria-components";

// srcts/overlays/stacking.ts
import { composeRenderProps as composeRenderProps8 } from "react-aria-components";
var OVERLAY_Z_INDEX = "var(--bk-z-index-overlay)";
function overlayStyle(style) {
  return composeRenderProps8(style, (given) => ({ ...given, zIndex: OVERLAY_Z_INDEX }));
}

// srcts/overlays/popover.tsx
import { jsx as jsx9 } from "react/jsx-runtime";
function Popover({ className, style, offset = 4, ...props }) {
  return /* @__PURE__ */ jsx9(
    AriaPopover,
    {
      ...props,
      offset,
      style: overlayStyle(style),
      className: composeRenderProps9(className, (given) => `${POPOVER} ${given ?? ""}`)
    }
  );
}
var POPOVER = "bk:overflow-auto bk:rounded-md bk:border bk:border-border bk:bg-popover bk:font-sans bk:text-sm bk:text-popover-foreground bk:shadow-md bk:outline-none";

// srcts/overlays/menu.tsx
import { Fragment as Fragment6, jsx as jsx10, jsxs as jsxs7 } from "react/jsx-runtime";
function MenuTrigger({ placement, children, ...props }) {
  const [trigger, menu] = Children.toArray(children);
  return /* @__PURE__ */ jsxs7(AriaMenuTrigger, { ...props, children: [
    trigger,
    /* @__PURE__ */ jsx10(Popover, { placement, className: "bk:min-w-44", children: menu })
  ] });
}
function SubmenuTrigger({ children, ...props }) {
  const [trigger, menu] = Children.toArray(children);
  return /* @__PURE__ */ jsxs7(AriaSubmenuTrigger, { ...props, children: [
    trigger,
    /* @__PURE__ */ jsx10(Popover, { offset: -2, crossOffset: -4, className: "bk:min-w-44", children: menu })
  ] });
}
function Menu({ className, ...props }) {
  return /* @__PURE__ */ jsx10(AriaMenu, { ...props, className: composeRenderProps10(className, (given) => `${MENU} ${given ?? ""}`) });
}
function MenuItem(props) {
  const textValue = props.textValue ?? (typeof props.children === "string" ? props.children : void 0);
  return /* @__PURE__ */ jsx10(AriaMenuItem, { ...props, textValue, className: composeRenderProps10(props.className, (given) => `${ITEM} ${given ?? ""}`), children: composeRenderProps10(props.children, (children, { selectionMode, isSelected, hasSubmenu }) => /* @__PURE__ */ jsxs7(Fragment6, { children: [
    selectionMode === "none" ? null : /* @__PURE__ */ jsx10("span", { className: "bk:flex bk:size-3.5 bk:shrink-0 bk:text-primary", children: isSelected ? /* @__PURE__ */ jsx10(Icon, { icon: Check2 }) : null }),
    /* @__PURE__ */ jsx10("span", { className: CONTENT, children }),
    hasSubmenu ? /* @__PURE__ */ jsx10("span", { className: "bk:flex bk:shrink-0 bk:text-muted-foreground", children: /* @__PURE__ */ jsx10(Icon, { icon: ChevronRight2 }) }) : null
  ] })) });
}
function MenuSection({ title, items, children, ...props }) {
  return /* @__PURE__ */ jsxs7(AriaMenuSection, { ...props, children: [
    title === void 0 ? null : /* @__PURE__ */ jsx10(Header, { className: HEADING2, children: title }),
    /* @__PURE__ */ jsx10(Collection, { items, children })
  ] });
}
var MENU = "bk:grid bk:max-h-[inherit] bk:gap-0.5 bk:overflow-auto bk:p-1 bk:outline-none";
var ITEM = "bk:box-border bk:flex bk:cursor-pointer bk:items-center bk:gap-2 bk:rounded-sm bk:px-2 bk:py-1 bk:text-left bk:font-sans bk:text-sm bk:outline-none bk:data-hovered:bg-accent bk:data-hovered:text-accent-foreground bk:data-focused:bg-accent bk:data-focused:text-accent-foreground bk:data-hovered:[&_[slot=description]]:text-accent-foreground bk:data-hovered:[&_kbd]:text-accent-foreground bk:data-focused:[&_[slot=description]]:text-accent-foreground bk:data-focused:[&_kbd]:text-accent-foreground bk:data-disabled:cursor-default bk:data-disabled:opacity-50";
var HEADING2 = "bk:block bk:px-2 bk:pt-1 bk:font-sans bk:text-xs bk:font-semibold bk:text-muted-foreground";
var CONTENT = "bk:grid bk:min-w-0 bk:flex-1 bk:grid-cols-[minmax(0,1fr)_auto] bk:items-center bk:gap-x-4 bk:[&>*]:truncate bk:[&>[slot=description]]:col-start-1 bk:[&>[slot=description]]:text-xs bk:[&>[slot=description]]:text-muted-foreground bk:[&>kbd]:col-start-2 bk:[&>kbd]:row-start-1 bk:[&>kbd]:font-sans bk:[&>kbd]:text-xs bk:[&>kbd]:text-muted-foreground";

// srcts/overlays/palette.tsx
import { Autocomplete, useFilter } from "react-aria-components";
import { jsx as jsx11, jsxs as jsxs8 } from "react/jsx-runtime";
function CommandPalette({
  isOpen,
  onOpenChange,
  placeholder = "Search commands",
  inputValue,
  defaultInputValue,
  onInputChange,
  filter,
  disableAutoFocusFirst,
  disableVirtualFocus,
  onAction,
  shouldCloseOnSelect = true,
  ...menu
}) {
  const { contains } = useFilter({ sensitivity: "base" });
  return /* @__PURE__ */ jsx11(Modal, { isDismissable: true, isOpen, onOpenChange, children: /* @__PURE__ */ jsx11(Dialog, { "aria-label": placeholder, className: "bk:flex bk:max-h-[inherit] bk:flex-col bk:gap-2 bk:p-2", children: /* @__PURE__ */ jsxs8(
    Autocomplete,
    {
      filter: filter ?? contains,
      inputValue,
      defaultInputValue,
      onInputChange,
      disableAutoFocusFirst,
      disableVirtualFocus,
      children: [
        /* @__PURE__ */ jsx11(SearchField, { autoFocus: true, "aria-label": placeholder, placeholder }),
        /* @__PURE__ */ jsx11(
          Menu,
          {
            renderEmptyState: () => /* @__PURE__ */ jsx11("p", { className: NOTE, children: "No results found." }),
            ...menu,
            onAction: (key, value) => {
              onAction?.(key, value);
              if (shouldCloseOnSelect) onOpenChange(false);
            },
            className: "bk:min-h-0 bk:flex-1"
          }
        )
      ]
    }
  ) }) });
}
var NOTE = "bk:m-0 bk:px-2.5 bk:py-2 bk:text-center bk:text-sm bk:italic bk:text-muted-foreground";

// srcts/overlays/sheet.tsx
import {
  composeRenderProps as composeRenderProps11,
  Sheet as AriaSheet,
  SheetBackdrop,
  SheetContent,
  SheetOverlay
} from "react-aria-components";
import { jsx as jsx12, jsxs as jsxs9 } from "react/jsx-runtime";
function Sheet({ children, className, overscrollPadding = true, ...props }) {
  const { snapPoints } = props;
  return /* @__PURE__ */ jsxs9(SheetOverlay, { ...props, className: composeRenderProps11(className, (given) => `bk:group/sheet bk:z-overlay ${given ?? ""}`), children: [
    /* @__PURE__ */ jsx12(
      SheetBackdrop,
      {
        className: BACKDROP,
        swipeAnimation: "bk-sheet-backdrop",
        swipeAnimationRange: snapPoints === void 0 ? void 0 : { start: snapPoints.length - 1 }
      }
    ),
    /* @__PURE__ */ jsx12(
      AriaSheet,
      {
        overscrollPadding,
        stackAnimation: "bk-sheet-scale-back",
        className: ({ position }) => `${SHEET} ${POSITIONS[position]}`,
        children: /* @__PURE__ */ jsx12(SheetContent, { className: CONTENT2, children })
      }
    )
  ] });
}
var BACKDROP = "bk:data-[stack-index=0]:bg-black/40";
var SHEET = "bk:z-[1] bk:overflow-clip bk:rounded-2xl bk:border bk:border-border bk:bg-popover bk:font-sans bk:text-popover-foreground bk:shadow-lg bk:outline-hidden bk:will-change-transform bk:[transform:perspective(1000px)] bk:forced-colors:bg-[Canvas]";
var POSITIONS = {
  bottom: "bk:max-h-[calc(100%-(--spacing(8)))] bk:w-[calc(100%-(--spacing(4)))] bk:max-w-200 bk:origin-[center_-150px]",
  top: "bk:max-h-[calc(100%-(--spacing(8)))] bk:w-[calc(100%-(--spacing(4)))] bk:max-w-200 bk:origin-[center_150px]",
  center: "bk:max-h-[calc(100%-(--spacing(8)))] bk:w-[calc(100%-(--spacing(4)))] bk:max-w-200 bk:origin-[center_-150px]",
  left: "bk:h-[calc(100%-(--spacing(4)))] bk:w-75 bk:origin-[150px_center]",
  right: "bk:h-[calc(100%-(--spacing(4)))] bk:w-75 bk:origin-[-150px_center]"
};
var CONTENT2 = "bk:box-border bk:h-full bk:overflow-hidden bk:overscroll-auto bk:p-6 bk:pb-[calc(--spacing(6)+var(--sheet-scroll-padding-y))] bk:outline-hidden bk:[scrollbar-gutter:stable] bk:group-data-[expanded]/sheet:overflow-auto";

// srcts/overlays/toast.tsx
import { composeRenderProps as composeRenderProps12, UNSTABLE_Toast as AriaToast, UNSTABLE_ToastRegion as AriaToastRegion } from "react-aria-components";
import { UNSTABLE_ToastContent, UNSTABLE_ToastQueue } from "react-aria-components";
import { jsx as jsx13 } from "react/jsx-runtime";
var FADES_FOR = 300;
function ToastRegion({ className, ...props }) {
  return /* @__PURE__ */ jsx13(AriaToastRegion, { ...props, className: composeRenderProps12(className, (given) => `${REGION} ${given ?? ""}`) });
}
function Toast({ className, style, ...props }) {
  const { timeout } = props.toast;
  const fade = timeout === void 0 ? void 0 : { animationDelay: `${String(Math.max(0, timeout - FADES_FOR))}ms` };
  return /* @__PURE__ */ jsx13(
    AriaToast,
    {
      ...props,
      style: composeRenderProps12(style, (given) => ({ ...fade, ...given })),
      className: composeRenderProps12(className, (given) => `${TOAST} ${timeout === void 0 ? "" : FADE} ${given ?? ""}`)
    }
  );
}
var REGION = "bk:group/toasts bk:pointer-events-none bk:fixed bk:right-2 bk:bottom-2 bk:z-[calc(var(--bk-z-index-overlay)+1)] bk:m-0 bk:box-border bk:flex bk:max-h-[calc(100dvh-(--spacing(4)))] bk:w-84 bk:max-w-[calc(100vw-(--spacing(4)))] bk:flex-col-reverse bk:gap-2 bk:overflow-y-auto bk:p-2 bk:font-sans bk:outline-none bk:data-focus-visible:outline-2 bk:data-focus-visible:outline-ring";
var TOAST = "bk:pointer-events-auto bk:box-border bk:flex bk:items-start bk:gap-2 bk:rounded-md bk:border bk:border-border bk:bg-popover bk:px-3 bk:py-2 bk:text-sm bk:text-popover-foreground bk:shadow-md bk:outline-none bk:data-focus-visible:outline-2 bk:data-focus-visible:outline-offset-2 bk:data-focus-visible:outline-ring";
var FADE = "bk:toast-fade bk:group-data-hovered/toasts:[animation-play-state:paused] bk:group-focus-within/toasts:[animation-play-state:paused]";

// srcts/overlays/tooltip.tsx
import { composeRenderProps as composeRenderProps13, OverlayArrow, Tooltip as AriaTooltip } from "react-aria-components";
import { jsx as jsx14, jsxs as jsxs10 } from "react/jsx-runtime";
function Tooltip({ children, className, style, offset = 8, ...props }) {
  return /* @__PURE__ */ jsxs10(
    AriaTooltip,
    {
      ...props,
      offset,
      style: overlayStyle(style),
      className: composeRenderProps13(className, (given) => `${TOOLTIP} ${given ?? ""}`),
      children: [
        /* @__PURE__ */ jsx14(OverlayArrow, { children: /* @__PURE__ */ jsx14("svg", { width: 8, height: 8, viewBox: "0 0 8 8", className: ARROW, "aria-hidden": "true", children: /* @__PURE__ */ jsx14("path", { d: "M0 0 L4 4 L8 0" }) }) }),
        children
      ]
    }
  );
}
var TOOLTIP = "bk:group bk:box-border bk:max-w-80 bk:rounded-md bk:border bk:border-border bk:bg-popover bk:px-2 bk:py-1 bk:font-sans bk:text-xs bk:text-popover-foreground bk:shadow-md bk:outline-none bk:will-change-transform";
var ARROW = "bk:block bk:fill-popover bk:stroke-border bk:forced-colors:fill-[Canvas] bk:forced-colors:stroke-[ButtonBorder] bk:group-data-[placement=bottom]:rotate-180 bk:group-data-[placement=left]:-rotate-90 bk:group-data-[placement=right]:rotate-90";

// srcts/overlays/index.ts
import { DialogTrigger, Focusable, Heading as Heading2, Keyboard, TooltipTrigger } from "react-aria-components";

// srcts/inputs/colorswatches.tsx
import { useContext as useContext4 } from "react";
import {
  ColorSwatch as AriaColorSwatch,
  ColorSwatchPicker as AriaColorSwatchPicker,
  ColorSwatchPickerItem as AriaColorSwatchPickerItem,
  composeRenderProps as composeRenderProps14
} from "react-aria-components";
import { Fragment as Fragment7, jsx as jsx15, jsxs as jsxs11 } from "react/jsx-runtime";
function ColorSwatch({ className, style, ...props }) {
  return /* @__PURE__ */ jsx15(
    AriaColorSwatch,
    {
      ...props,
      className: composeRenderProps14(className, (given) => `${SWATCH} ${given ?? ""}`),
      style: composeRenderProps14(style, (given, { color }) => ({
        ...given,
        background: `linear-gradient(${color.toString()}, ${color.toString()}), ${CHECKERBOARD2}`
      }))
    }
  );
}
function ColorSwatchPicker({ bound, ...props }) {
  return bound === void 0 ? /* @__PURE__ */ jsx15(Drawn2, { ...props }) : /* @__PURE__ */ jsx15(BoundSwatches, { ...props, bound });
}
function BoundSwatches({ bound, onChange, ...props }) {
  return /* @__PURE__ */ jsxs11(Fragment7, { children: [
    /* @__PURE__ */ jsx15(
      Drawn2,
      {
        ...props,
        value: valueColor(bound.value) ?? FALLBACK,
        onChange: (color) => {
          void bound.commit(hex(color));
          onChange?.(color);
        }
      }
    ),
    /* @__PURE__ */ jsx15(Refusal, { error: bound.error })
  ] });
}
function Drawn2({ className, onChange, ...props }) {
  const ended = useContext4(Ended);
  return /* @__PURE__ */ jsx15(
    AriaColorSwatchPicker,
    {
      ...props,
      onChange: (color) => {
        onChange?.(color);
        ended?.(color);
      },
      className: composeRenderProps14(className, (given, { layout }) => `${PICKER} ${layout === "stack" ? "bk:flex-col" : "bk:flex-wrap"} ${given ?? ""}`)
    }
  );
}
function ColorSwatchPickerItem({ className, ...props }) {
  return /* @__PURE__ */ jsx15(AriaColorSwatchPickerItem, { ...props, className: composeRenderProps14(className, (given) => `${ITEM2} ${given ?? ""}`), children: ({ isSelected }) => /* @__PURE__ */ jsxs11(Fragment7, { children: [
    /* @__PURE__ */ jsx15(ColorSwatch, {}),
    isSelected ? /* @__PURE__ */ jsx15("div", { className: SELECTED }) : null
  ] }) });
}
var SWATCH = "bk:box-border bk:size-8 bk:shrink-0 bk:rounded-md bk:border bk:border-border";
var CHECKERBOARD2 = "repeating-conic-gradient(#ccc 0% 25%, white 0% 50%) 50% / 16px 16px";
var PICKER = "bk:flex bk:gap-1";
var ITEM2 = "bk:relative bk:cursor-pointer bk:rounded-sm bk:outline-none bk:data-focus-visible:outline-2 bk:data-focus-visible:outline-offset-2 bk:data-focus-visible:outline-ring bk:data-disabled:cursor-default bk:data-disabled:opacity-50";
var SELECTED = "bk:absolute bk:top-0 bk:left-0 bk:box-border bk:size-full bk:rounded-md bk:border-2 bk:border-black bk:outline-2 bk:-outline-offset-4 bk:outline-white bk:dark:border-white bk:dark:outline-black bk:forced-color-adjust-none";

// srcts/inputs/colorpicker.tsx
import { Fragment as Fragment8, jsx as jsx16, jsxs as jsxs12 } from "react/jsx-runtime";
function ColorPicker({ bound, ...props }) {
  return bound === void 0 ? /* @__PURE__ */ jsx16(Drawn3, { ...props }) : /* @__PURE__ */ jsx16(BoundPicker, { ...props, bound });
}
function BoundPicker({ bound, onChange, ...props }) {
  const dragged = useDragged(bound);
  return /* @__PURE__ */ jsxs12(Fragment8, { children: [
    /* @__PURE__ */ jsx16(Ended, { value: dragged.end, children: /* @__PURE__ */ jsx16(
      Drawn3,
      {
        ...props,
        value: dragged.shown,
        onChange: (color) => {
          dragged.change(color);
          onChange?.(color);
        }
      }
    ) }),
    /* @__PURE__ */ jsx16(Refusal, { error: bound.error })
  ] });
}
function Drawn3({ label, children, isDisabled, ...props }) {
  return /* @__PURE__ */ jsx16(AriaColorPicker, { ...props, children: /* @__PURE__ */ jsxs12(DialogTrigger, { children: [
    /* @__PURE__ */ jsxs12(Button3, { isDisabled, className: BUTTON, children: [
      /* @__PURE__ */ jsx16(ColorSwatch, {}),
      label === void 0 ? null : /* @__PURE__ */ jsx16("span", { children: label })
    ] }),
    /* @__PURE__ */ jsx16(Popover, { placement: "bottom start", children: /* @__PURE__ */ jsx16(Dialog, { className: "bk:flex bk:flex-col bk:gap-2", children: children ?? /* @__PURE__ */ jsxs12(Fragment8, { children: [
      /* @__PURE__ */ jsx16(ColorArea, { colorSpace: "hsb", xChannel: "saturation", yChannel: "brightness" }),
      /* @__PURE__ */ jsx16(ColorSlider, { colorSpace: "hsb", channel: "hue" }),
      /* @__PURE__ */ jsx16(ColorField, { label: "Hex" })
    ] }) }) })
  ] }) });
}
var BUTTON = "bk:flex bk:cursor-pointer bk:items-center bk:gap-2 bk:rounded-sm bk:border-0 bk:bg-transparent bk:p-0 bk:font-sans bk:text-sm bk:text-inherit bk:outline-none bk:data-focus-visible:outline-2 bk:data-focus-visible:outline-offset-2 bk:data-focus-visible:outline-ring bk:data-disabled:cursor-default bk:data-disabled:opacity-50";

// srcts/inputs/combobox.tsx
import { useContext as useContext5, useEffect, useEffectEvent, useLayoutEffect as useLayoutEffect2, useMemo as useMemo2, useRef as useRef4, useState as useState6 } from "react";
import { Check as Check3, ChevronDown, X } from "./vendor-723651e1.js";
import {
  Button as Button4,
  Collection as Collection2,
  ComboBox as AriaComboBox,
  ComboBoxStateContext,
  composeRenderProps as composeRenderProps15,
  Group,
  Header as Header2,
  Input as Input2,
  ListBox,
  ListBoxItem,
  ListBoxSection,
  ListLayout,
  Popover as Popover2,
  Tag,
  TagGroup,
  TagList,
  TooltipTriggerStateContext,
  Virtualizer
} from "react-aria-components";
import { Fragment as Fragment9, jsx as jsx17, jsxs as jsxs13 } from "react/jsx-runtime";
function ComboBox(props) {
  const { bound, ordered = false, ...rest } = props;
  if (bound === void 0) return /* @__PURE__ */ jsx17(Drawn4, { ...rest });
  return props.selectionMode === "multiple" ? /* @__PURE__ */ jsx17(BoundSeveral, { ...rest, bound, ordered }) : /* @__PURE__ */ jsx17(BoundOne, { ...rest, bound });
}
function BoundOne({ bound, outdated = false, ...props }) {
  const current = typeof bound.value === "string" ? bound.value : null;
  return /* @__PURE__ */ jsxs13(Fragment9, { children: [
    /* @__PURE__ */ jsx17(
      Drawn4,
      {
        ...props,
        selectionMode: "single",
        outdated,
        marking: !outdated && known(props),
        onClear: () => {
          void bound.unset();
        },
        value: current,
        onChange: (key) => {
          if (key !== null && !Array.isArray(key)) void bound.commit(String(key));
        }
      }
    ),
    /* @__PURE__ */ jsx17(Refusal, { error: bound.error })
  ] });
}
function BoundSeveral({
  bound,
  outdated = false,
  ordered,
  onOpenChange,
  ...props
}) {
  const items = props.items ?? props.defaultItems;
  const held2 = strings(bound.value);
  const [picks, setPicks] = useState6(null);
  const pending = useRef4(null);
  const submit = (next, opened) => {
    const value = merged(strings(bound.get()), opened, next);
    if (value === null) return;
    void bound.commit(ordered && items !== void 0 ? inOrder(value, ids(items)) : value);
  };
  return /* @__PURE__ */ jsxs13(Fragment9, { children: [
    /* @__PURE__ */ jsx17(
      Drawn4,
      {
        ...props,
        selectionMode: "multiple",
        outdated,
        marking: !outdated && known(props),
        value: picks ?? held2,
        onOpenChange: (isOpen, trigger) => {
          if (isOpen) {
            pending.current ??= { picks: held2, opened: held2 };
            setPicks(pending.current.picks);
          } else {
            const left = pending.current;
            pending.current = null;
            setPicks(null);
            if (left !== null) submit(left.picks, left.opened);
          }
          onOpenChange?.(isOpen, trigger);
        },
        onChange: (keys) => {
          if (!Array.isArray(keys)) return;
          const next = keys.map(String);
          if (pending.current === null) {
            submit(next, held2);
            return;
          }
          pending.current = { ...pending.current, picks: next };
          setPicks(next);
        }
      }
    ),
    /* @__PURE__ */ jsx17(Refusal, { error: bound.error })
  ] });
}
function known(props) {
  return (props.items ?? props.defaultItems) !== void 0 || typeof props.children !== "function";
}
function ids(items) {
  return [...items].flatMap((item) => {
    const id = item.id;
    return typeof id === "string" || typeof id === "number" ? [String(id)] : [];
  });
}
function Drawn4({
  label,
  description,
  errorMessage,
  placeholder,
  children,
  renderEmptyState,
  outdated = false,
  marking = false,
  onClear,
  className,
  allowsEmptyCollection = true,
  ...props
}) {
  const multiple = props.selectionMode === "multiple";
  const isDisabled = props.isDisabled === true;
  const editable = !isDisabled && props.isReadOnly !== true;
  const given = props.items ?? props.defaultItems;
  const unknown = given === void 0 && typeof children === "function";
  const none = given === void 0 ? typeof children === "function" || !hasChildren(children) : [...given].length === 0;
  const empty = none && (outdated || unknown) ? "Loading\u2026" : none ? renderEmptyState?.() ?? "No options" : "No matches";
  const value = useSameValue(props.value);
  return /* @__PURE__ */ jsxs13(
    AriaComboBox,
    {
      ...props,
      value,
      defaultItems: unknown ? [] : props.defaultItems,
      allowsEmptyCollection,
      className: composeRenderProps15(className, (given2) => `bk:group ${styles.field} ${given2 ?? ""}`),
      children: [
        label === void 0 ? null : /* @__PURE__ */ jsx17(Label, { children: label }),
        /* @__PURE__ */ jsx17(
          Field2,
          {
            placeholder,
            multiple,
            marking,
            isDisabled,
            editable,
            clearable: props.isRequired !== true && editable,
            onClear,
            label
          }
        ),
        /* @__PURE__ */ jsx17(Description, { children: description }),
        /* @__PURE__ */ jsx17(FieldError, { children: errorMessage }),
        /* @__PURE__ */ jsx17(Lacking, { multiple, marking }),
        /* @__PURE__ */ jsx17(Popover2, { offset: 4, className: LIST, style: { zIndex: OVERLAY_Z_INDEX }, children: /* @__PURE__ */ jsx17(Rows, { empty, children }) })
      ]
    }
  );
}
function Rows({ empty, children }) {
  const probe = useRef4(null);
  const [unit, setUnit] = useState6(SPACING);
  useLayoutEffect2(() => {
    const measured = probe.current?.getBoundingClientRect().width ?? 0;
    if (measured > 0) setUnit(measured);
  }, []);
  const layout = useMemo2(() => ({ rowHeight: 8 * unit, headingHeight: 7 * unit, padding: unit }), [unit]);
  return /* @__PURE__ */ jsxs13(Fragment9, { children: [
    /* @__PURE__ */ jsx17("span", { ref: probe, "aria-hidden": "true", className: PROBE, style: { width: "var(--bk-spacing)" } }),
    /* @__PURE__ */ jsx17(Virtualizer, { layout: ListLayout, layoutOptions: layout, children: /* @__PURE__ */ jsx17(ListBox, { renderEmptyState: () => /* @__PURE__ */ jsx17("p", { className: NOTE2, children: empty }), className: LISTBOX, children }) })
  ] });
}
function useSameValue(value) {
  const [drawn, setDrawn] = useState6(value);
  const same = drawn === value || Array.isArray(drawn) && Array.isArray(value) && sameKeys(drawn, value);
  if (!same) setDrawn(value);
  return same ? drawn : value;
}
function sameKeys(a, b) {
  return a.length === b.length && a.every((key, i) => key === b[i]);
}
function hasChildren(children) {
  return Array.isArray(children) ? children.some((child) => hasChildren(child)) : children !== null && children !== void 0 && children !== false;
}
function Field2({
  placeholder,
  multiple,
  marking,
  isDisabled,
  editable,
  clearable,
  onClear,
  label
}) {
  const { state, keys, one, known: known2, lacked } = usePicks(multiple);
  const item = one === null ? void 0 : known2.get(one);
  const invalid = marking && lacked.length > 0;
  const remove = (gone) => {
    state?.setValue(keys.filter((key) => !gone.has(key)));
  };
  const chips = keys.map((key) => ({ key, text: known2.get(key) ?? String(key), gone: marking && !known2.has(key) }));
  const picks = label === void 0 ? "Picks" : `Picks of ${label}`;
  return /* @__PURE__ */ jsxs13(Group, { isInvalid: invalid, className: FIELD, children: [
    /* @__PURE__ */ jsxs13("div", { className: VALUES, children: [
      keys.length === 0 ? null : isDisabled ? (
        // The chips of a disabled select take no focus, as its text field
        // takes none.
        // React Aria's TagGroup keeps a tab stop on its list even where
        // every tag is disabled, so a disabled select draws its chips as
        // a plain list.
        /* @__PURE__ */ jsx17("div", { role: "list", "aria-label": picks, className: "bk:flex bk:min-w-0 bk:max-w-full bk:flex-wrap bk:gap-1", children: chips.map(({ key, text, gone }) => /* @__PURE__ */ jsx17(PlainChip, { text, gone }, key)) })
      ) : /* @__PURE__ */ jsx17(TagGroup, { "aria-label": picks, onRemove: editable ? remove : void 0, className: "bk:flex bk:min-w-0 bk:max-w-full", children: /* @__PURE__ */ jsx17(TagList, { className: "bk:flex bk:min-w-0 bk:flex-wrap bk:gap-1", children: chips.map(({ key, text, gone }) => /* @__PURE__ */ jsx17(Tag, { id: key, textValue: text, "data-missing": gone || void 0, className: CHIP, children: ({ isFocusVisible }) => /* @__PURE__ */ jsxs13(Fragment9, { children: [
        /* @__PURE__ */ jsx17(ChipText, { text, focused: isFocusVisible }),
        editable ? /* @__PURE__ */ jsx17(Button4, { slot: "remove", "aria-label": `Remove ${text}`, className: CHIP_REMOVE, children: /* @__PURE__ */ jsx17(Icon, { icon: X, size: 10 }) }) : null
      ] }) }, key)) }) }),
      /* @__PURE__ */ jsx17(
        Input2,
        {
          value: multiple || state === null || state.isFocused || state.isOpen ? void 0 : item ?? (one === null ? "" : String(one)),
          placeholder: one !== null && item === void 0 ? String(one) : keys.length > 0 ? void 0 : placeholder,
          "aria-invalid": invalid || void 0,
          className: INPUT,
          onClick: () => {
            if (editable && state !== null && !state.isOpen) state.open(null, "manual");
          },
          onKeyDown: (event) => {
            if (event.key === "Escape" && state?.isOpen === true) event.preventDefault();
            if (!editable || event.key !== "Backspace" || event.currentTarget.value !== "" || keys.length === 0) return;
            state?.setValue(keys.slice(0, -1));
          }
        }
      )
    ] }),
    clearable && (keys.length > 0 || one !== null) ? (
      // A FieldButton whose slot is null, which React Aria's ComboBox then
      // leaves alone, rather than take it for the button that opens the
      // list. It is outside the order of Tab, and keeps the focus in the
      // text field, as a press in the list does.
      // It is the trigger of its tooltip, as a button of React Aria is.
      /* @__PURE__ */ jsxs13(TooltipTrigger, { children: [
        /* @__PURE__ */ jsx17(
          FieldButton,
          {
            slot: null,
            excludeFromTabOrder: true,
            preventFocusOnPress: true,
            "aria-label": label === void 0 ? "Clear" : `Clear ${label}`,
            onPress: () => {
              if (onClear !== void 0) onClear();
              else state?.setValue(multiple ? [] : null);
            },
            children: /* @__PURE__ */ jsx17(Icon, { icon: X, size: 10 })
          }
        ),
        /* @__PURE__ */ jsx17(Tooltip, { children: multiple ? "Clear every pick" : "Clear the pick" })
      ] })
    ) : null,
    /* @__PURE__ */ jsx17(Button4, { className: OPENER, children: /* @__PURE__ */ jsx17("span", { className: CHEVRON, children: /* @__PURE__ */ jsx17(Icon, { icon: ChevronDown, size: 12 }) }) })
  ] });
}
function PlainChip({ text, gone }) {
  return /* @__PURE__ */ jsx17("div", { role: "listitem", "data-missing": gone || void 0, className: CHIP, children: /* @__PURE__ */ jsx17(ChipText, { text, focused: false }) });
}
function ChipText({ text, focused }) {
  return /* @__PURE__ */ jsx17(TooltipTrigger, { children: /* @__PURE__ */ jsx17(CutText, { text, focused }) });
}
function CutText({ text, focused }) {
  const tip = useContext5(TooltipTriggerStateContext);
  const ref = useRef4(null);
  const [pointed, setPointed] = useState6(false);
  const shown = useRef4({ pointed: false, focused: false });
  const follow = useEffectEvent((now) => {
    const before = shown.current;
    shown.current = now;
    const el = ref.current;
    const cut = el !== null && el.scrollWidth > el.clientWidth;
    if (tip === null) return;
    if (cut && now.focused) tip.open(true);
    else if (cut && now.pointed) tip.open();
    else if (before.pointed || before.focused) tip.close(before.focused);
  });
  useEffect(() => {
    follow({ pointed, focused });
  }, [pointed, focused]);
  return /* @__PURE__ */ jsxs13(Fragment9, { children: [
    /* @__PURE__ */ jsx17(
      "span",
      {
        ref,
        className: "bk:truncate",
        onPointerEnter: (event) => {
          if (event.pointerType !== "touch") setPointed(true);
        },
        onPointerLeave: () => {
          setPointed(false);
        },
        children: text
      }
    ),
    /* @__PURE__ */ jsx17(Tooltip, { triggerRef: ref, children: text })
  ] });
}
function usePicks(multiple) {
  const state = useContext5(ComboBoxStateContext);
  const value = state?.value ?? null;
  const keys = multiple && Array.isArray(value) ? value : [];
  const one = !multiple && value !== null && !Array.isArray(value) ? value : null;
  const known2 = new Map((state?.selectedItems ?? []).map((item) => [item.key, item.textValue]));
  const lacked = (one === null ? keys : [one]).filter((key) => !known2.has(key));
  return { state, keys, one, known: known2, lacked };
}
function Lacking({ multiple, marking }) {
  const { lacked } = usePicks(multiple);
  return marking ? /* @__PURE__ */ jsx17(NotAvailable, { values: lacked.map(String) }) : null;
}
function ComboBoxItem(props) {
  const textValue = props.textValue ?? (typeof props.children === "string" ? props.children : void 0);
  return /* @__PURE__ */ jsx17(ListBoxItem, { ...props, textValue, className: ROW, children: composeRenderProps15(props.children, (children, { isSelected, selectionMode }) => /* @__PURE__ */ jsxs13(Fragment9, { children: [
    selectionMode === "multiple" ? /* @__PURE__ */ jsx17("span", { className: "bk:flex bk:size-3.5 bk:shrink-0 bk:text-primary", children: isSelected ? /* @__PURE__ */ jsx17(Icon, { icon: Check3 }) : null }) : null,
    /* @__PURE__ */ jsx17(
      "span",
      {
        className: `bk:flex bk:min-w-0 bk:flex-1 bk:items-center bk:gap-2 bk:truncate ${selectionMode === "single" && isSelected ? "bk:font-semibold" : ""}`,
        children
      }
    )
  ] })) });
}
function ComboBoxSection({ title, items, children, ...props }) {
  return /* @__PURE__ */ jsxs13(ListBoxSection, { ...props, children: [
    /* @__PURE__ */ jsx17(Header2, { className: HEADING3, children: title }),
    /* @__PURE__ */ jsx17(Collection2, { items, children })
  ] });
}
var FIELD = "bk:flex bk:min-h-7.5 bk:min-w-0 bk:items-center bk:gap-1 bk:justify-self-stretch bk:rounded-md bk:border bk:border-input bk:bg-transparent bk:py-0.5 bk:pr-0.5 bk:pl-2 bk:font-sans bk:text-sm bk:data-focus-within:outline-2 bk:data-focus-within:outline-offset-1 bk:data-focus-within:outline-ring bk:data-invalid:border-destructive bk:data-disabled:opacity-50";
var VALUES = "bk:flex bk:min-w-0 bk:flex-1 bk:flex-wrap bk:items-center bk:gap-1";
var CHEVRON = "bk:flex bk:shrink-0 bk:text-muted-foreground bk:transition-transform bk:group-data-open:rotate-180";
var INPUT = "bk:min-w-16 bk:flex-1 bk:border-0 bk:bg-transparent bk:p-0 bk:font-sans bk:text-sm bk:text-inherit bk:outline-none bk:placeholder:text-muted-foreground bk:disabled:cursor-default";
var OPENER = "bk:ml-auto bk:flex bk:size-6 bk:shrink-0 bk:cursor-pointer bk:items-center bk:justify-center bk:rounded-sm bk:border-0 bk:bg-transparent bk:p-0 bk:text-inherit bk:data-disabled:cursor-default";
var CHIP = "bk:inline-flex bk:max-w-full bk:min-w-0 bk:items-center bk:gap-0.5 bk:rounded-sm bk:border bk:border-border bk:bg-muted bk:py-0 bk:pr-1.5 bk:pl-1.5 bk:data-allows-removing:pr-0.5 bk:text-xs bk:leading-5 bk:outline-none bk:data-focus-visible:outline-2 bk:data-focus-visible:outline-ring bk:data-missing:border-destructive bk:data-missing:text-destructive-text";
var CHIP_REMOVE = "bk:flex bk:size-4 bk:shrink-0 bk:cursor-pointer bk:items-center bk:justify-center bk:rounded-sm bk:border-0 bk:bg-transparent bk:p-0 bk:text-muted-foreground bk:data-hovered:bg-accent bk:data-hovered:text-accent-foreground";
var SPACING = 4;
var PROBE = "bk:pointer-events-none bk:invisible bk:absolute bk:h-0";
var LIST = "bk:box-border bk:w-(--trigger-width) bk:overflow-hidden bk:rounded-lg bk:bg-popover bk:font-sans bk:text-sm bk:text-popover-foreground bk:shadow-md bk:outline-none";
var LISTBOX = "bk:box-border bk:max-h-[inherit] bk:overflow-y-auto bk:rounded-lg bk:border bk:border-border bk:outline-none";
var HEADING3 = "bk:box-border bk:h-7 bk:bg-popover bk:px-2.5 bk:pt-2 bk:pb-1 bk:text-xs bk:font-semibold bk:uppercase bk:tracking-wider bk:text-muted-foreground";
var ROW = "bk:box-border bk:flex bk:h-8 bk:cursor-pointer bk:items-center bk:gap-2 bk:rounded-sm bk:px-2.5 bk:text-sm bk:outline-none bk:data-hovered:bg-accent bk:data-hovered:text-accent-foreground bk:data-focused:bg-accent bk:data-focused:text-accent-foreground bk:data-focus-visible:outline bk:data-focus-visible:outline-1 bk:data-focus-visible:outline-ring bk:data-disabled:cursor-default bk:data-disabled:opacity-50 bk:[&_[slot=description]]:min-w-0 bk:[&_[slot=description]]:flex-1 bk:[&_[slot=description]]:truncate bk:[&_[slot=description]]:text-sm bk:[&_[slot=description]]:text-muted-foreground bk:data-hovered:[&_[slot=description]]:text-accent-foreground bk:data-focused:[&_[slot=description]]:text-accent-foreground";
var NOTE2 = "bk:m-0 bk:px-2.5 bk:py-2 bk:text-center bk:text-sm bk:italic bk:text-muted-foreground";

// srcts/inputs/datefield.tsx
import {
  composeRenderProps as composeRenderProps16,
  DateField as AriaDateField,
  DateInput as AriaDateInput,
  DateSegment,
  TimeField as AriaTimeField
} from "react-aria-components";
import { Fragment as Fragment10, jsx as jsx18, jsxs as jsxs14 } from "react/jsx-runtime";
function DateField({ bound, ...props }) {
  return bound === void 0 ? /* @__PURE__ */ jsx18(DrawnDate, { ...props }) : /* @__PURE__ */ jsx18(BoundDate, { ...props, bound });
}
function TimeField({ bound, ...props }) {
  return bound === void 0 ? /* @__PURE__ */ jsx18(DrawnTime, { ...props }) : /* @__PURE__ */ jsx18(BoundTime, { ...props, bound });
}
function BoundDate({
  bound,
  onChange,
  onBlur,
  onKeyDown,
  validationBehavior = "aria",
  ...props
}) {
  const time = takesTime(props.granularity);
  const date = useBoundDate(bound, {
    read: (value) => readDate(value, time),
    write: (value) => writeDate(value, time),
    fits: (value) => dateFits(value, props),
    required: props.isRequired === true
  });
  return /* @__PURE__ */ jsxs14(Fragment10, { children: [
    /* @__PURE__ */ jsx18(
      DrawnDate,
      {
        ...props,
        validationBehavior,
        value: date.value,
        missing: date.missing,
        onKey: date.heard,
        onChange: (value) => {
          date.change(value);
          onChange?.(value);
        },
        onBlur: (event) => {
          date.flush();
          onBlur?.(event);
        },
        onKeyDown: (event) => {
          if (event.key === "Enter") date.flush();
          onKeyDown?.(event);
        }
      }
    ),
    /* @__PURE__ */ jsx18(Refusal, { error: bound.error })
  ] });
}
function BoundTime({
  bound,
  onChange,
  onBlur,
  onKeyDown,
  validationBehavior = "aria",
  ...props
}) {
  const time = useBoundDate(bound, {
    read: readTime,
    write: writeTime,
    fits: (value) => timeFits(value, props),
    required: props.isRequired === true
  });
  return /* @__PURE__ */ jsxs14(Fragment10, { children: [
    /* @__PURE__ */ jsx18(
      DrawnTime,
      {
        ...props,
        validationBehavior,
        value: time.value,
        missing: time.missing,
        onKey: time.heard,
        onChange: (value) => {
          time.change(value);
          onChange?.(value);
        },
        onBlur: (event) => {
          time.flush();
          onBlur?.(event);
        },
        onKeyDown: (event) => {
          if (event.key === "Enter") time.flush();
          onKeyDown?.(event);
        }
      }
    ),
    /* @__PURE__ */ jsx18(Refusal, { error: bound.error })
  ] });
}
function DrawnDate({
  label,
  description,
  errorMessage,
  missing = null,
  onKey,
  isInvalid,
  className,
  ...props
}) {
  return /* @__PURE__ */ jsx18(
    AriaDateField,
    {
      ...props,
      isInvalid: missing === null ? isInvalid : true,
      className: composeRenderProps16(className, (given) => `${styles.field} ${given ?? ""}`),
      children: /* @__PURE__ */ jsx18(FieldParts, { label, description, errorMessage, missing, onKey })
    }
  );
}
function DrawnTime({
  label,
  description,
  errorMessage,
  missing = null,
  onKey,
  isInvalid,
  className,
  ...props
}) {
  return /* @__PURE__ */ jsx18(
    AriaTimeField,
    {
      ...props,
      isInvalid: missing === null ? isInvalid : true,
      className: composeRenderProps16(className, (given) => `${styles.field} ${given ?? ""}`),
      children: /* @__PURE__ */ jsx18(FieldParts, { label, description, errorMessage, missing, onKey })
    }
  );
}
function FieldParts({
  label,
  description,
  errorMessage,
  missing,
  onKey
}) {
  return /* @__PURE__ */ jsxs14(Fragment10, { children: [
    label === void 0 ? null : /* @__PURE__ */ jsx18(Label, { children: label }),
    /* @__PURE__ */ jsx18(Segments, { onKey, children: /* @__PURE__ */ jsx18(DateInput, { className: BOX2 }) }),
    /* @__PURE__ */ jsx18(Description, { children: description }),
    /* @__PURE__ */ jsx18(FieldError, { children: errorMessage }),
    missing === null || missing === void 0 ? null : /* @__PURE__ */ jsx18(NotAvailable, { values: [missing] })
  ] });
}
function Segments({ onKey, children }) {
  return onKey === void 0 ? /* @__PURE__ */ jsx18(Fragment10, { children }) : /* @__PURE__ */ jsx18(
    "div",
    {
      className: "bk:contents",
      onKeyDownCapture: (event) => {
        onKey(event.key);
      },
      children
    }
  );
}
function DateInput({ slot, className }) {
  return /* @__PURE__ */ jsx18(AriaDateInput, { slot, className, children: (segment) => /* @__PURE__ */ jsx18(DateSegment, { segment, className: SEGMENT }) });
}
var BOX2 = "bk:flex bk:min-w-0 bk:items-center bk:justify-self-stretch bk:overflow-x-auto bk:whitespace-nowrap bk:rounded-md bk:border bk:border-input bk:bg-transparent bk:px-2 bk:py-1 bk:font-sans bk:text-sm bk:cursor-text bk:[scrollbar-width:none] bk:data-focus-within:outline-2 bk:data-focus-within:outline-offset-1 bk:data-focus-within:outline-ring bk:data-invalid:border-destructive bk:data-disabled:cursor-default bk:data-disabled:opacity-50";
var INLINE = "bk:flex bk:min-w-0 bk:items-center bk:whitespace-nowrap bk:py-1 bk:font-sans bk:text-sm bk:cursor-text bk:data-disabled:cursor-default";
var SEGMENT = "bk:rounded-sm bk:px-0.5 bk:tabular-nums bk:caret-transparent bk:outline-none bk:data-[type=literal]:px-0 bk:data-placeholder:text-muted-foreground bk:data-focused:bg-primary bk:data-focused:text-primary-foreground";

// srcts/inputs/datepicker.tsx
import { useRef as useRef5 } from "react";
import {
  composeRenderProps as composeRenderProps17,
  DatePicker as AriaDatePicker,
  DateRangePicker as AriaDateRangePicker,
  Group as Group2
} from "react-aria-components";
import { Calendar as CalendarIcon } from "./vendor-723651e1.js";
import { Fragment as Fragment11, jsx as jsx19, jsxs as jsxs15 } from "react/jsx-runtime";
function DatePicker({ bound, ...props }) {
  return bound === void 0 ? /* @__PURE__ */ jsx19(DrawnPicker, { ...props }) : /* @__PURE__ */ jsx19(BoundPicker2, { ...props, bound });
}
function DateRangePicker({ bound, ...props }) {
  return bound === void 0 ? /* @__PURE__ */ jsx19(DrawnRangePicker, { ...props }) : /* @__PURE__ */ jsx19(BoundRangePicker, { ...props, bound });
}
function usePicking(value) {
  const open = useRef5(false);
  return {
    onOpenChange: (isOpen) => {
      open.current = isOpen;
    },
    onChange: (next) => {
      if (open.current) value.pick(next);
      else value.change(next);
    },
    onBlur: () => {
      value.flush();
    },
    onKeyDown: (event) => {
      if (event.key === "Enter" && event.currentTarget.contains(event.target)) value.flush();
    }
  };
}
function BoundPicker2({
  bound,
  onChange,
  onOpenChange,
  onBlur,
  onKeyDown,
  validationBehavior = "aria",
  ...props
}) {
  const time = takesTime(props.granularity);
  const date = useBoundDate(bound, {
    read: (value) => readDate(value, time),
    write: (value) => writeDate(value, time),
    fits: (value) => dateFits(value, props),
    required: props.isRequired === true
  });
  const picking = usePicking(date);
  return /* @__PURE__ */ jsxs15(Fragment11, { children: [
    /* @__PURE__ */ jsx19(
      DrawnPicker,
      {
        ...props,
        validationBehavior,
        value: date.value,
        missing: date.missing,
        onKey: date.heard,
        onOpenChange: (isOpen) => {
          picking.onOpenChange(isOpen);
          onOpenChange?.(isOpen);
        },
        onChange: (value) => {
          picking.onChange(value);
          onChange?.(value);
        },
        onBlur: (event) => {
          picking.onBlur();
          onBlur?.(event);
        },
        onKeyDown: (event) => {
          picking.onKeyDown(event);
          onKeyDown?.(event);
        }
      }
    ),
    /* @__PURE__ */ jsx19(Refusal, { error: bound.error })
  ] });
}
function BoundRangePicker({
  bound,
  onChange,
  onOpenChange,
  onBlur,
  onKeyDown,
  validationBehavior = "aria",
  ...props
}) {
  const time = takesTime(props.granularity);
  const range = useBoundDate(bound, {
    read: (value) => readRange(value, time),
    write: (value) => writeRange(value, time),
    fits: (value) => rangeFits(value, { minValue: props.minValue, maxValue: props.maxValue }),
    required: props.isRequired === true
  });
  const picking = usePicking(range);
  return /* @__PURE__ */ jsxs15(Fragment11, { children: [
    /* @__PURE__ */ jsx19(
      DrawnRangePicker,
      {
        ...props,
        validationBehavior,
        value: range.value,
        missing: range.missing,
        onKey: range.heard,
        onOpenChange: (isOpen) => {
          picking.onOpenChange(isOpen);
          onOpenChange?.(isOpen);
        },
        onChange: (value) => {
          picking.onChange(value);
          onChange?.(value);
        },
        onBlur: (event) => {
          picking.onBlur();
          onBlur?.(event);
        },
        onKeyDown: (event) => {
          picking.onKeyDown(event);
          onKeyDown?.(event);
        }
      }
    ),
    /* @__PURE__ */ jsx19(Refusal, { error: bound.error })
  ] });
}
function DrawnPicker({
  label,
  description,
  errorMessage,
  missing = null,
  onKey,
  isInvalid,
  className,
  ...props
}) {
  return /* @__PURE__ */ jsxs15(
    AriaDatePicker,
    {
      ...props,
      isInvalid: missing === null ? isInvalid : true,
      className: composeRenderProps17(className, (given) => `bk:group ${styles.field} ${given ?? ""}`),
      children: [
        label === void 0 ? null : /* @__PURE__ */ jsx19(Label, { children: label }),
        /* @__PURE__ */ jsxs15(Group2, { className: FIELD2, children: [
          /* @__PURE__ */ jsx19(Segments, { onKey, children: /* @__PURE__ */ jsx19(DateInput, { className: `${INLINE} bk:flex-1` }) }),
          /* @__PURE__ */ jsx19(Opener, {})
        ] }),
        /* @__PURE__ */ jsx19(PickerParts, { description, errorMessage, missing }),
        /* @__PURE__ */ jsx19(Popover, { className: POPOVER2, children: /* @__PURE__ */ jsx19(Calendar, {}) })
      ]
    }
  );
}
function DrawnRangePicker({
  label,
  description,
  errorMessage,
  missing = null,
  onKey,
  isInvalid,
  className,
  ...props
}) {
  return /* @__PURE__ */ jsxs15(
    AriaDateRangePicker,
    {
      ...props,
      isInvalid: missing === null ? isInvalid : true,
      className: composeRenderProps17(className, (given) => `bk:group ${styles.field} ${given ?? ""}`),
      children: [
        label === void 0 ? null : /* @__PURE__ */ jsx19(Label, { children: label }),
        /* @__PURE__ */ jsxs15(Group2, { className: FIELD2, children: [
          /* @__PURE__ */ jsx19(Segments, { onKey, children: /* @__PURE__ */ jsxs15("div", { className: ENDS, children: [
            /* @__PURE__ */ jsx19(DateInput, { slot: "start", className: INLINE }),
            /* @__PURE__ */ jsx19("span", { "aria-hidden": "true", className: "bk:px-1 bk:text-muted-foreground", children: "\u2013" }),
            /* @__PURE__ */ jsx19(DateInput, { slot: "end", className: `${INLINE} bk:flex-1` })
          ] }) }),
          /* @__PURE__ */ jsx19(Opener, {})
        ] }),
        /* @__PURE__ */ jsx19(PickerParts, { description, errorMessage, missing }),
        /* @__PURE__ */ jsx19(Popover, { className: POPOVER2, children: /* @__PURE__ */ jsx19(RangeCalendar, {}) })
      ]
    }
  );
}
function PickerParts({
  description,
  errorMessage,
  missing
}) {
  return /* @__PURE__ */ jsxs15(Fragment11, { children: [
    /* @__PURE__ */ jsx19(Description, { children: description }),
    /* @__PURE__ */ jsx19(FieldError, { children: errorMessage }),
    missing === null || missing === void 0 ? null : /* @__PURE__ */ jsx19(NotAvailable, { values: [missing] })
  ] });
}
function Opener() {
  return /* @__PURE__ */ jsx19(FieldButton, { children: /* @__PURE__ */ jsx19(Icon, { icon: CalendarIcon }) });
}
var FIELD2 = "bk:flex bk:min-w-0 bk:items-center bk:gap-1 bk:justify-self-stretch bk:rounded-md bk:border bk:border-input bk:bg-transparent bk:pr-0.5 bk:pl-2 bk:font-sans bk:text-sm bk:data-focus-within:outline-2 bk:data-focus-within:outline-offset-1 bk:data-focus-within:outline-ring bk:data-invalid:border-destructive bk:data-disabled:opacity-50";
var ENDS = "bk:flex bk:min-w-0 bk:flex-1 bk:items-center bk:overflow-x-auto bk:[scrollbar-width:none]";
var POPOVER2 = "bk:p-3";

// srcts/inputs/dropzone.tsx
import { composeRenderProps as composeRenderProps18, DropZone as AriaDropZone, TextContext, useSlottedContext } from "react-aria-components";
import { jsx as jsx20 } from "react/jsx-runtime";
function DropZone({ className, children, ...props }) {
  return /* @__PURE__ */ jsx20(AriaDropZone, { ...props, className: composeRenderProps18(className, (given) => `${ZONE} ${given ?? ""}`), children: composeRenderProps18(children, (drawn) => /* @__PURE__ */ jsx20(Labelled, { children: drawn })) });
}
function Labelled({ children }) {
  const label = useSlottedContext(TextContext, "label");
  return /* @__PURE__ */ jsx20(TextContext, { value: { ...label, className: styles.label }, children });
}
var ZONE = "bk:box-border bk:flex bk:min-h-20 bk:flex-col bk:items-center bk:justify-center bk:gap-2 bk:rounded-lg bk:border bk:border-dashed bk:border-input bk:p-4 bk:text-center bk:font-sans bk:text-sm bk:outline-none bk:data-focus-visible:outline-2 bk:data-focus-visible:outline-offset-1 bk:data-focus-visible:outline-ring bk:data-drop-target:border-solid bk:data-drop-target:border-primary bk:data-drop-target:bg-accent bk:data-drop-target:text-accent-foreground bk:data-disabled:opacity-50";

// srcts/inputs/form.tsx
import { Form as AriaForm } from "react-aria-components";
import { jsx as jsx21 } from "react/jsx-runtime";
function Form({ className, ...props }) {
  return /* @__PURE__ */ jsx21(AriaForm, { ...props, className: `bk:grid bk:gap-3 ${className ?? ""}` });
}

// srcts/inputs/listbox.tsx
import { composeRenderProps as composeRenderProps19, ListBox as AriaListBox } from "react-aria-components";
import { jsx as jsx22 } from "react/jsx-runtime";
function ListBox2({ className, ...props }) {
  return /* @__PURE__ */ jsx22(AriaListBox, { ...props, className: composeRenderProps19(className, (given) => `${LISTBOX2} ${given ?? ""}`) });
}
function ListBoxItem2(props) {
  return /* @__PURE__ */ jsx22(ComboBoxItem, { ...props });
}
var LISTBOX2 = "bk:box-border bk:grid bk:min-w-0 bk:content-start bk:overflow-y-auto bk:rounded-md bk:border bk:border-input bk:p-1 bk:font-sans bk:text-sm bk:outline-none bk:data-focus-visible:outline-2 bk:data-focus-visible:outline-offset-1 bk:data-focus-visible:outline-ring bk:data-empty:px-2.5 bk:data-empty:py-2 bk:data-empty:italic bk:data-empty:text-muted-foreground";

// srcts/inputs/numberfield.tsx
import { useState as useState7 } from "react";
import { Button as Button5, composeRenderProps as composeRenderProps20, Group as Group3, Input as Input3, NumberField as AriaNumberField } from "react-aria-components";
import { ChevronDown as ChevronDown2, ChevronUp } from "./vendor-723651e1.js";
import { Fragment as Fragment12, jsx as jsx23, jsxs as jsxs16 } from "react/jsx-runtime";
function NumberField({ bound, outdated = false, maxValue, ...props }) {
  const largest = outdated ? void 0 : maxValue;
  return bound === void 0 ? /* @__PURE__ */ jsx23(Drawn5, { ...props, maxValue: largest }) : /* @__PURE__ */ jsx23(BoundNumber, { ...props, maxValue: largest, bound });
}
function BoundNumber({
  bound,
  onChange,
  onInput,
  onBlur,
  commitBehavior = "validate",
  validationBehavior = "aria",
  ...props
}) {
  const held2 = typeof bound.value === "number" ? bound.value : NaN;
  const [kept, setKept] = useState7(null);
  const [seen, setSeen] = useState7(held2);
  if (!Object.is(seen, held2)) {
    setSeen(held2);
    if (kept !== null && !kept.typing) setKept(null);
  }
  const fits = (value) => inRange(value, props.minValue, props.maxValue, props.step);
  return /* @__PURE__ */ jsxs16(Fragment12, { children: [
    /* @__PURE__ */ jsx23(
      Drawn5,
      {
        ...props,
        commitBehavior,
        validationBehavior,
        value: kept?.value ?? held2,
        onInput: (event) => {
          setKept((now) => now?.typing === true ? now : { value: now?.value ?? held2, typing: true });
          onInput?.(event);
        },
        onChange: (value) => {
          if (!fits(value)) {
            setKept(null);
          } else if (kept?.typing === true) {
            bound.type(value);
            bound.flush();
            setKept(Object.is(value, held2) ? null : { value, typing: false });
          } else {
            setKept({ value, typing: false });
            void bound.commit(value).then((took) => {
              if (!took) setKept(null);
            });
          }
          onChange?.(value);
        },
        onBlur: (event) => {
          setKept((now) => now?.typing === true ? null : now);
          onBlur?.(event);
        }
      }
    ),
    /* @__PURE__ */ jsx23(Refusal, { error: bound.error })
  ] });
}
function inRange(value, min, max, step) {
  if (Number.isNaN(value)) return false;
  if (min !== void 0 && value < min) return false;
  if (max !== void 0 && value > max) return false;
  if (step === void 0 || !(step > 0)) return true;
  const steps = (value - (min ?? 0)) / step;
  return Math.abs(steps - Math.round(steps)) < 1e-9;
}
function Drawn5({ label, description, errorMessage, placeholder, className, onInput, ...props }) {
  return /* @__PURE__ */ jsxs16(AriaNumberField, { ...props, className: composeRenderProps20(className, (given) => `bk:group ${styles.field} ${given ?? ""}`), children: [
    label === void 0 ? null : /* @__PURE__ */ jsx23(Label, { children: label }),
    /* @__PURE__ */ jsxs16(Group3, { className: FIELD3, children: [
      /* @__PURE__ */ jsx23(Input3, { placeholder, onInput, className: INPUT2 }),
      /* @__PURE__ */ jsxs16("div", { className: STEPPERS, children: [
        /* @__PURE__ */ jsx23(Button5, { slot: "increment", className: STEPPER, children: /* @__PURE__ */ jsx23(Icon, { icon: ChevronUp, size: 10 }) }),
        /* @__PURE__ */ jsx23(Button5, { slot: "decrement", className: STEPPER, children: /* @__PURE__ */ jsx23(Icon, { icon: ChevronDown2, size: 10 }) })
      ] })
    ] }),
    /* @__PURE__ */ jsx23(Description, { children: description }),
    /* @__PURE__ */ jsx23(FieldError, { children: errorMessage })
  ] });
}
var FIELD3 = "bk:flex bk:min-w-0 bk:items-stretch bk:justify-self-stretch bk:overflow-hidden bk:rounded-md bk:border bk:border-input bk:bg-transparent bk:font-sans bk:text-sm bk:data-focus-within:outline-2 bk:data-focus-within:outline-offset-1 bk:data-focus-within:outline-ring bk:data-invalid:border-destructive bk:data-disabled:opacity-50";
var INPUT2 = "bk:min-w-0 bk:flex-1 bk:border-0 bk:bg-transparent bk:px-2 bk:py-1 bk:font-sans bk:text-sm bk:text-inherit bk:outline-none bk:placeholder:text-muted-foreground";
var STEPPERS = "bk:flex bk:flex-col bk:border-l bk:border-input bk:group-data-invalid:border-destructive";
var STEPPER = "bk:flex bk:flex-1 bk:cursor-pointer bk:items-center bk:justify-center bk:border-0 bk:bg-transparent bk:px-1 bk:py-0 bk:text-muted-foreground bk:data-hovered:bg-accent bk:data-hovered:text-accent-foreground bk:data-pressed:bg-accent bk:data-disabled:cursor-default bk:data-disabled:opacity-50";

// srcts/inputs/radio.tsx
import { useRef as useRef6 } from "react";
import { composeRenderProps as composeRenderProps21, RadioButton, RadioField, RadioGroup as AriaRadioGroup } from "react-aria-components";
import { Fragment as Fragment13, jsx as jsx24, jsxs as jsxs17 } from "react/jsx-runtime";
function RadioGroup({ bound, outdated = false, ...props }) {
  return bound === void 0 ? /* @__PURE__ */ jsx24(DrawnGroup2, { ...props }) : /* @__PURE__ */ jsx24(BoundGroup2, { ...props, bound, outdated });
}
function BoundGroup2({
  bound,
  outdated,
  onChange,
  children,
  ...props
}) {
  const { known: known2, items } = useItems();
  const held2 = typeof bound.value === "string" ? bound.value : null;
  const gone = lacking(held2 === null ? [] : [held2], known2);
  const marking = !outdated && gone.length > 0;
  return /* @__PURE__ */ jsxs17(Fragment13, { children: [
    /* @__PURE__ */ jsx24(
      DrawnGroup2,
      {
        ...props,
        value: held2,
        isInvalid: props.isInvalid === true || marking,
        onChange: (value) => {
          void bound.commit(value);
          onChange?.(value);
        },
        children: /* @__PURE__ */ jsx24(ItemsContext.Provider, { value: items, children })
      }
    ),
    marking ? /* @__PURE__ */ jsx24(NotAvailable, { values: gone }) : null,
    /* @__PURE__ */ jsx24(Refusal, { error: bound.error })
  ] });
}
function DrawnGroup2({ label, description, errorMessage, children, className, ...props }) {
  return /* @__PURE__ */ jsxs17(AriaRadioGroup, { ...props, className: composeRenderProps21(className, (given) => `bk:group ${styles.field} ${given ?? ""}`), children: [
    label === void 0 ? null : /* @__PURE__ */ jsx24(Label, { children: label }),
    /* @__PURE__ */ jsx24("div", { className: "bk:flex bk:gap-1.5 bk:group-data-[orientation=horizontal]:flex-row bk:group-data-[orientation=horizontal]:flex-wrap bk:group-data-[orientation=horizontal]:gap-4 bk:group-data-[orientation=vertical]:flex-col", children }),
    /* @__PURE__ */ jsx24(Description, { children: description }),
    /* @__PURE__ */ jsx24(FieldError, { children: errorMessage })
  ] });
}
function Radio({ children, description, className, ...props }) {
  const ref = useRef6(null);
  useItem(props.value, ref);
  return /* @__PURE__ */ jsxs17(RadioField, { ...props, ref, className: "bk:grid bk:gap-1 bk:font-sans", children: [
    /* @__PURE__ */ jsxs17(RadioButton, { className: composeRenderProps21(className, (given) => `bk:group ${RADIO} ${given ?? ""}`), children: [
      /* @__PURE__ */ jsx24("span", { className: CIRCLE }),
      children
    ] }),
    /* @__PURE__ */ jsx24(Description, { className: "bk:ml-6", children: description })
  ] });
}
var RADIO = "bk:flex bk:w-fit bk:cursor-pointer bk:items-center bk:gap-2 bk:text-sm bk:data-disabled:cursor-default bk:data-disabled:opacity-50";
var CIRCLE = "bk:box-border bk:size-4 bk:shrink-0 bk:rounded-full bk:border bk:border-input bk:bg-transparent bk:outline-offset-2 bk:outline-ring bk:transition-[border-width] bk:group-data-focus-visible:outline-2 bk:group-data-selected:border-[5px] bk:group-data-selected:border-primary bk:group-data-invalid:border-destructive";

// srcts/inputs/searchfield.tsx
import { composeRenderProps as composeRenderProps22, Group as Group4, Input as Input4, SearchField as AriaSearchField } from "react-aria-components";
import { Search, X as X2 } from "./vendor-723651e1.js";
import { Fragment as Fragment14, jsx as jsx25, jsxs as jsxs18 } from "react/jsx-runtime";
function SearchField({ bound, ...props }) {
  return bound === void 0 ? /* @__PURE__ */ jsx25(Drawn6, { ...props }) : /* @__PURE__ */ jsx25(BoundSearch, { ...props, bound });
}
function BoundSearch({ bound, onChange, onKeyDown, onBlur, onClear, ...props }) {
  const [text, typed] = useTyped(bound);
  return /* @__PURE__ */ jsxs18(Fragment14, { children: [
    /* @__PURE__ */ jsx25(
      Drawn6,
      {
        ...props,
        value: text ?? (typeof bound.value === "string" ? bound.value : ""),
        onChange: (value) => {
          typed(value);
          bound.type(value);
          onChange?.(value);
        },
        onKeyDown: (event) => {
          if (event.key === "Enter" && !event.nativeEvent.isComposing) bound.flush();
          event.continuePropagation();
          onKeyDown?.(event);
        },
        onBlur: (event) => {
          bound.flush();
          onBlur?.(event);
        },
        onClear: () => {
          bound.flush();
          onClear?.();
        }
      }
    ),
    /* @__PURE__ */ jsx25(Refusal, { error: bound.error })
  ] });
}
function Drawn6({ label, description, errorMessage, placeholder, className, ...props }) {
  return /* @__PURE__ */ jsxs18(AriaSearchField, { ...props, className: composeRenderProps22(className, (given) => `bk:group ${styles.field} ${given ?? ""}`), children: [
    label === void 0 ? null : /* @__PURE__ */ jsx25(Label, { children: label }),
    /* @__PURE__ */ jsxs18(Group4, { className: FIELD4, children: [
      /* @__PURE__ */ jsx25("span", { className: "bk:flex bk:shrink-0 bk:text-muted-foreground", children: /* @__PURE__ */ jsx25(Icon, { icon: Search }) }),
      /* @__PURE__ */ jsx25(Input4, { placeholder, className: INPUT3 }),
      /* @__PURE__ */ jsx25(FieldButton, { className: "bk:group-data-empty:invisible", children: /* @__PURE__ */ jsx25(Icon, { icon: X2, size: 10 }) })
    ] }),
    /* @__PURE__ */ jsx25(Description, { children: description }),
    /* @__PURE__ */ jsx25(FieldError, { children: errorMessage })
  ] });
}
var FIELD4 = "bk:flex bk:min-h-7.5 bk:min-w-0 bk:items-center bk:gap-1.5 bk:justify-self-stretch bk:rounded-md bk:border bk:border-input bk:bg-transparent bk:py-0.5 bk:pr-1 bk:pl-2 bk:font-sans bk:text-sm bk:data-focus-within:outline-2 bk:data-focus-within:outline-offset-1 bk:data-focus-within:outline-ring bk:group-data-invalid:border-destructive bk:data-disabled:opacity-50";
var INPUT3 = "bk:min-w-0 bk:flex-1 bk:border-0 bk:bg-transparent bk:p-0 bk:font-sans bk:text-sm bk:text-inherit bk:outline-none bk:placeholder:text-muted-foreground bk:[&::-webkit-search-cancel-button]:hidden";

// srcts/inputs/select.tsx
import { useContext as useContext6, useLayoutEffect as useLayoutEffect3, useMemo as useMemo3, useRef as useRef7, useState as useState8 } from "react";
import {
  Button as Button6,
  Collection as Collection3,
  composeRenderProps as composeRenderProps23,
  ListBox as ListBox3,
  Popover as Popover3,
  Select as AriaSelect,
  SelectStateContext,
  SelectValue,
  useLocale as useLocale2
} from "react-aria-components";
import { ChevronDown as ChevronDown3 } from "./vendor-723651e1.js";
import { Fragment as Fragment15, jsx as jsx26, jsxs as jsxs19 } from "react/jsx-runtime";
function Select(props) {
  const { bound, outdated = false, ordered = false, ...rest } = props;
  if (bound === void 0) return /* @__PURE__ */ jsx26(Drawn7, { ...rest });
  return /* @__PURE__ */ jsx26(BoundSelect, { ...rest, bound, outdated, ordered });
}
function SelectItem(props) {
  return /* @__PURE__ */ jsx26(ComboBoxItem, { ...props });
}
function SelectSection(props) {
  return /* @__PURE__ */ jsx26(ComboBoxSection, { ...props });
}
function BoundSelect({
  bound,
  outdated,
  ordered,
  onChange,
  onOpenChange,
  ...props
}) {
  const several = props.selectionMode === "multiple";
  const values = several ? strings(bound.value) : typeof bound.value === "string" ? [bound.value] : [];
  const [known2, setKnown] = useState8(null);
  const unknown = props.items === void 0 && typeof props.children === "function";
  const ids2 = unknown ? null : known2;
  const [picks, setPicks] = useState8(null);
  const pending = useRef7(null);
  const shown = picks ?? values;
  const asIs = ids2 === null ? values : values.filter((value) => !ids2.includes(value));
  const marking = !outdated && ids2 !== null && asIs.length > 0;
  const submit = (next, opened) => {
    const value = merged(strings(bound.get()), opened, next);
    if (value === null) return;
    void bound.commit(ordered && ids2 !== null ? inOrder(value, ids2) : value);
  };
  const common = {
    ...props,
    isInvalid: props.isInvalid === true || marking,
    asIs,
    onKnown: (next) => {
      setKnown((last) => last !== null && sameIds(last, next) ? last : next);
    }
  };
  const line = marking ? /* @__PURE__ */ jsx26(
    NotAvailable,
    {
      values: asIs,
      onRemove: several ? () => {
        void bound.commit(held(strings(bound.get()), ids2));
      } : void 0
    }
  ) : null;
  if (!several) {
    return /* @__PURE__ */ jsxs19(Fragment15, { children: [
      /* @__PURE__ */ jsx26(
        Drawn7,
        {
          ...common,
          selectionMode: "single",
          value: values[0] ?? null,
          onChange: (key) => {
            if (key !== null && !Array.isArray(key)) void bound.commit(String(key));
            onChange?.(key);
          },
          onOpenChange
        }
      ),
      line,
      /* @__PURE__ */ jsx26(Refusal, { error: bound.error })
    ] });
  }
  return /* @__PURE__ */ jsxs19(Fragment15, { children: [
    /* @__PURE__ */ jsx26(
      Drawn7,
      {
        ...common,
        selectionMode: "multiple",
        value: [...shown],
        onOpenChange: (isOpen) => {
          if (isOpen) {
            pending.current ??= { picks: values, opened: values };
            setPicks(pending.current.picks);
          } else {
            const left = pending.current;
            pending.current = null;
            setPicks(null);
            if (left !== null) submit(left.picks, left.opened);
          }
          onOpenChange?.(isOpen);
        },
        onChange: (keys) => {
          if (Array.isArray(keys)) {
            const next = keys.map(String);
            if (pending.current === null) {
              submit(next, values);
            } else {
              pending.current = { ...pending.current, picks: next };
              setPicks(next);
            }
          }
          onChange?.(keys);
        }
      }
    ),
    line,
    /* @__PURE__ */ jsx26(Refusal, { error: bound.error })
  ] });
}
function sameIds(a, b) {
  return a.length === b.length && a.every((id, i) => id === b[i]);
}
function Drawn7({ label, description, errorMessage, items, children, asIs = [], onKnown, className, ...props }) {
  const value = useSameValue(props.value);
  const { locale } = useLocale2();
  const list = useMemo3(() => new Intl.ListFormat(locale), [locale]);
  return /* @__PURE__ */ jsxs19(
    AriaSelect,
    {
      ...props,
      value,
      className: composeRenderProps23(className, (given) => `bk:group ${styles.field} ${given ?? ""}`),
      children: [
        label === void 0 ? null : /* @__PURE__ */ jsx26(Label, { children: label }),
        /* @__PURE__ */ jsxs19(Button6, { className: TRIGGER, children: [
          /* @__PURE__ */ jsx26(SelectValue, { className: ({ isPlaceholder }) => isPlaceholder && asIs.length === 0 ? `${VALUE} ${PLACEHOLDER}` : VALUE, children: ({ state, selectedText, defaultChildren, isPlaceholder }) => asIs.length > 0 ? list.format([...state.selectedItems.map((item) => item.textValue), ...asIs]) : isPlaceholder ? defaultChildren : selectedText || defaultChildren }),
          /* @__PURE__ */ jsx26("span", { className: CHEVRON2, children: /* @__PURE__ */ jsx26(Icon, { icon: ChevronDown3, size: 12 }) })
        ] }),
        /* @__PURE__ */ jsx26(Description, { children: description }),
        /* @__PURE__ */ jsx26(FieldError, { children: errorMessage }),
        /* @__PURE__ */ jsx26(Popover3, { offset: 4, className: LIST2, children: /* @__PURE__ */ jsx26(ListBox3, { renderEmptyState: () => /* @__PURE__ */ jsx26("p", { className: NOTE3, children: "No options" }), className: LISTBOX3, children: items === void 0 ? typeof children === "function" ? null : children : /* @__PURE__ */ jsx26(Collection3, { items, children }) }) }),
        onKnown === void 0 ? null : /* @__PURE__ */ jsx26(Known, { onKnown })
      ]
    }
  );
}
function Known({ onKnown }) {
  const state = useContext6(SelectStateContext);
  useLayoutEffect3(() => {
    if (state === null) return;
    const { collection } = state;
    const ids2 = [...collection.getKeys()].flatMap(
      (key) => collection.getItem(key)?.type === "item" ? [String(key)] : []
    );
    onKnown(ids2);
  });
  return null;
}
var TRIGGER = "bk:flex bk:min-h-7.5 bk:w-full bk:min-w-0 bk:cursor-pointer bk:items-center bk:gap-2 bk:justify-self-stretch bk:rounded-md bk:border bk:border-input bk:bg-transparent bk:py-1 bk:pr-1.5 bk:pl-2 bk:text-left bk:font-sans bk:text-sm bk:text-inherit bk:outline-none bk:data-focus-visible:outline-2 bk:data-focus-visible:outline-offset-1 bk:data-focus-visible:outline-ring bk:group-data-invalid:border-destructive bk:data-disabled:cursor-default bk:data-disabled:opacity-50";
var VALUE = "bk:min-w-0 bk:flex-1 bk:truncate bk:group-data-invalid:text-destructive-text";
var PLACEHOLDER = "bk:text-muted-foreground";
var CHEVRON2 = "bk:flex bk:shrink-0 bk:text-muted-foreground bk:transition-transform bk:group-data-open:rotate-180";
var LIST2 = "bk:box-border bk:min-w-(--trigger-width) bk:overflow-hidden bk:rounded-lg bk:bg-popover bk:font-sans bk:text-sm bk:text-popover-foreground bk:shadow-md bk:outline-none";
var LISTBOX3 = "bk:box-border bk:max-h-[inherit] bk:overflow-y-auto bk:rounded-lg bk:border bk:border-border bk:p-1 bk:outline-none";
var NOTE3 = "bk:m-0 bk:px-2.5 bk:py-2 bk:text-center bk:text-sm bk:italic bk:text-muted-foreground";

// srcts/inputs/slider.tsx
import { useContext as useContext7, useLayoutEffect as useLayoutEffect4, useRef as useRef8, useState as useState9 } from "react";
import {
  composeRenderProps as composeRenderProps24,
  Slider as AriaSlider,
  SliderFill,
  SliderOutput as SliderOutput2,
  SliderStateContext,
  SliderThumb,
  SliderTrack as SliderTrack2,
  useLocale as useLocale3
} from "react-aria-components";
import { Fragment as Fragment16, jsx as jsx27, jsxs as jsxs20 } from "react/jsx-runtime";
function Slider2({ bound, outdated = false, ...props }) {
  return bound === void 0 ? /* @__PURE__ */ jsx27(Drawn8, { ...props }) : /* @__PURE__ */ jsx27(BoundSlider, { ...props, bound, outdated });
}
function BoundSlider({
  bound,
  outdated,
  onChange,
  onChangeEnd,
  ...props
}) {
  const several = Array.isArray(bound.value) || Array.isArray(props.defaultValue);
  const held2 = numbers(bound.value);
  const [moving, setMoving] = useState9(null);
  const [seen, setSeen] = useState9(bound.value);
  if (!jsonEqual(seen, bound.value)) {
    setSeen(bound.value);
    setMoving(null);
  }
  const shown = useRef8([]);
  const start = useRef8(null);
  const min = props.minValue ?? 0;
  const fits = (value2) => inRange(value2, min, props.maxValue ?? 100, props.step ?? 1);
  const lacked = moving === null ? held2.filter((value2) => !fits(value2)) : [];
  const marking = !outdated && lacked.length > 0;
  const drawn = moving ?? (held2.length > 0 ? held2 : [min]);
  const value = several ? [...drawn] : drawn[0] ?? min;
  const { locale } = useLocale3();
  return /* @__PURE__ */ jsxs20(Fragment16, { children: [
    /* @__PURE__ */ jsx27(
      Drawn8,
      {
        ...props,
        value,
        invalid: marking,
        said: moving === null && held2.length === 0 ? () => "" : lacked.length > 0 ? (formatted) => held2.map(formatted).join(held2.length === 2 ? "\u2013" : ", ") : void 0,
        onChange: (next) => {
          start.current ??= shown.current;
          setMoving(numbers(next));
          onChange?.(next);
        },
        onChangeEnd: (next) => {
          const ended = numbers(next);
          const began = start.current ?? shown.current;
          start.current = null;
          if (jsonEqual(ended, began)) {
            setMoving(null);
          } else {
            const given = ended.map((x, i) => Object.is(x, began[i]) ? held2[i] ?? x : x);
            setMoving(ended);
            void bound.commit(several ? given : given[0] ?? min).then((took) => {
              if (!took) setMoving(null);
            });
          }
          onChangeEnd?.(next);
        },
        watch: /* @__PURE__ */ jsx27(
          Watch,
          {
            onShown: (values) => {
              shown.current = values;
            }
          }
        )
      }
    ),
    marking ? /* @__PURE__ */ jsx27(NotAvailable, { values: lacked.map((x) => new Intl.NumberFormat(locale, props.formatOptions).format(x)) }) : null,
    /* @__PURE__ */ jsx27(Refusal, { error: bound.error })
  ] });
}
function Watch({ onShown }) {
  const state = useContext7(SliderStateContext);
  useLayoutEffect4(() => {
    if (state !== null && !state.values.some((_, i) => state.isThumbDragging(i))) onShown(state.values);
  });
  return null;
}
function numbers(value) {
  if (typeof value === "number") return [value];
  return Array.isArray(value) ? value.filter((x) => typeof x === "number") : [];
}
function Drawn8({ label, thumbLabels, fillOffset, said, invalid = false, watch, className, ...props }) {
  return /* @__PURE__ */ jsxs20(
    AriaSlider,
    {
      ...props,
      "data-invalid": invalid || void 0,
      className: composeRenderProps24(className, (given) => `bk:group ${SLIDER2} ${given ?? ""}`),
      children: [
        label === void 0 ? /* @__PURE__ */ jsx27("span", {}) : /* @__PURE__ */ jsx27(Label, { children: label }),
        /* @__PURE__ */ jsx27(SliderOutput2, { className: OUTPUT2, children: ({ state }) => said === void 0 ? state.getFormattedValue() : said((value) => state.getFormattedValue(value)) }),
        /* @__PURE__ */ jsx27(SliderTrack2, { className: TRACK2, children: ({ state }) => /* @__PURE__ */ jsxs20(Fragment16, { children: [
          /* @__PURE__ */ jsx27("div", { className: BAR, children: /* @__PURE__ */ jsx27(SliderFill, { offset: fillOffset, className: FILL }) }),
          state.values.map((_, i) => /* @__PURE__ */ jsx27(SliderThumb, { index: i, "aria-label": thumbLabels?.[i], className: THUMB2 }, i))
        ] }) }),
        watch
      ]
    }
  );
}
var SLIDER2 = "bk:grid bk:grid-cols-[1fr_auto] bk:items-center bk:gap-x-2 bk:gap-y-1 bk:font-sans bk:data-disabled:opacity-50";
var OUTPUT2 = "bk:text-sm bk:text-muted-foreground bk:tabular-nums bk:group-data-invalid:text-destructive-text";
var TRACK2 = "bk:relative bk:col-span-2 bk:flex bk:h-5 bk:items-center bk:mx-2";
var BAR = "bk:relative bk:h-1.5 bk:w-full bk:rounded-full bk:bg-border";
var FILL = "bk:rounded-full bk:bg-primary bk:group-data-invalid:bg-destructive";
var THUMB2 = "bk:top-1/2 bk:box-border bk:size-4 bk:cursor-grab bk:rounded-full bk:border-2 bk:border-primary bk:bg-popover bk:shadow-sm bk:outline-offset-2 bk:outline-ring bk:data-dragging:cursor-grabbing bk:data-focus-visible:outline-2 bk:data-disabled:cursor-default bk:group-data-invalid:border-destructive";

// srcts/inputs/switch.tsx
import { composeRenderProps as composeRenderProps25, SwitchButton, SwitchField } from "react-aria-components";
import { Fragment as Fragment17, jsx as jsx28, jsxs as jsxs21 } from "react/jsx-runtime";
function Switch({ bound, onChange, ...props }) {
  if (bound === void 0) return /* @__PURE__ */ jsx28(Drawn9, { ...props, onChange });
  return /* @__PURE__ */ jsxs21(Fragment17, { children: [
    /* @__PURE__ */ jsx28(
      Drawn9,
      {
        ...props,
        isSelected: bound.value === true,
        onChange: (selected) => {
          void bound.commit(selected);
          onChange?.(selected);
        }
      }
    ),
    /* @__PURE__ */ jsx28(Refusal, { error: bound.error })
  ] });
}
function Drawn9({ children, description, errorMessage, className, ...props }) {
  return /* @__PURE__ */ jsxs21(SwitchField, { ...props, className: "bk:grid bk:gap-1 bk:font-sans", children: [
    /* @__PURE__ */ jsxs21(SwitchButton, { className: composeRenderProps25(className, (given) => `bk:group ${SWITCH} ${given ?? ""}`), children: [
      /* @__PURE__ */ jsx28("span", { className: TRACK3, children: /* @__PURE__ */ jsx28("span", { className: HANDLE }) }),
      children
    ] }),
    /* @__PURE__ */ jsx28(Description, { children: description }),
    /* @__PURE__ */ jsx28(FieldError, { children: errorMessage })
  ] });
}
var SWITCH = "bk:flex bk:w-fit bk:cursor-pointer bk:items-center bk:gap-2 bk:text-sm bk:data-disabled:cursor-default bk:data-disabled:opacity-50";
var TRACK3 = "bk:box-border bk:flex bk:h-5 bk:w-9 bk:shrink-0 bk:items-center bk:rounded-full bk:border bk:border-input bk:bg-muted bk:px-px bk:outline-offset-2 bk:outline-ring bk:transition-colors bk:group-data-focus-visible:outline-2 bk:group-data-selected:border-primary bk:group-data-selected:bg-primary";
var HANDLE = "bk:size-4 bk:rounded-full bk:bg-muted-foreground bk:shadow-sm bk:transition-transform bk:group-data-selected:translate-x-full bk:group-data-selected:bg-primary-foreground";

// srcts/inputs/tags.tsx
import { createContext as createContext3, useContext as useContext8 } from "react";
import { Button as Button7, composeRenderProps as composeRenderProps26, Tag as AriaTag, TagGroup as AriaTagGroup, TagList as TagList2, Text as Text3 } from "react-aria-components";
import { X as X3 } from "./vendor-723651e1.js";
import { Fragment as Fragment18, jsx as jsx29, jsxs as jsxs22 } from "react/jsx-runtime";
var colors = {
  gray: "bk:border-border bk:bg-muted",
  accent: "bk:border-primary bk:bg-muted"
};
var ColorContext = createContext3("gray");
function TagGroup2({
  label,
  description,
  errorMessage,
  items,
  children,
  renderEmptyState,
  color = "gray",
  className,
  ...props
}) {
  return /* @__PURE__ */ jsxs22(AriaTagGroup, { ...props, className: `${styles.field} ${className ?? ""}`, children: [
    label === void 0 ? null : /* @__PURE__ */ jsx29(Label, { children: label }),
    /* @__PURE__ */ jsx29(ColorContext.Provider, { value: color, children: /* @__PURE__ */ jsx29(TagList2, { items, renderEmptyState, className: "bk:flex bk:min-w-0 bk:flex-wrap bk:gap-1", children }) }),
    description === void 0 ? null : /* @__PURE__ */ jsx29(Description, { children: description }),
    errorMessage === void 0 ? null : /* @__PURE__ */ jsx29(Text3, { slot: "errorMessage", className: styles.alert, children: errorMessage })
  ] });
}
function Tag2({ children, color, className, ...props }) {
  const textValue = typeof children === "string" ? children : void 0;
  const groupColor = useContext8(ColorContext);
  return /* @__PURE__ */ jsx29(
    AriaTag,
    {
      textValue,
      ...props,
      className: composeRenderProps26(className, (given) => `${TAG} ${colors[color ?? groupColor]} ${given ?? ""}`),
      children: composeRenderProps26(children, (children2, { allowsRemoving }) => /* @__PURE__ */ jsxs22(Fragment18, { children: [
        /* @__PURE__ */ jsx29("span", { className: "bk:truncate", children: children2 }),
        allowsRemoving ? /* @__PURE__ */ jsx29(Button7, { slot: "remove", className: REMOVE, children: /* @__PURE__ */ jsx29(Icon, { icon: X3, size: 10 }) }) : null
      ] }))
    }
  );
}
var TAG = "bk:inline-flex bk:max-w-full bk:min-w-0 bk:cursor-default bk:items-center bk:gap-0.5 bk:rounded-sm bk:border bk:px-1.5 bk:py-0 bk:font-sans bk:text-xs bk:leading-5 bk:outline-none bk:data-allows-removing:pr-0.5 bk:data-focus-visible:outline-2 bk:data-focus-visible:outline-ring bk:data-selected:border-primary bk:data-selected:bg-primary bk:data-selected:text-primary-foreground bk:data-disabled:opacity-50";
var REMOVE = "bk:flex bk:size-4 bk:shrink-0 bk:cursor-pointer bk:items-center bk:justify-center bk:rounded-sm bk:border-0 bk:bg-transparent bk:p-0 bk:text-inherit bk:data-hovered:bg-accent bk:data-hovered:text-accent-foreground";

// srcts/inputs/textfield.tsx
import { composeRenderProps as composeRenderProps27, Input as Input5, TextArea, TextField as AriaTextField } from "react-aria-components";
import { Fragment as Fragment19, jsx as jsx30, jsxs as jsxs23 } from "react/jsx-runtime";
function TextField({ bound, ...props }) {
  return bound === void 0 ? /* @__PURE__ */ jsx30(Drawn10, { ...props }) : /* @__PURE__ */ jsx30(BoundText, { ...props, bound });
}
function BoundText({ bound, onChange, onKeyDown, onBlur, ...props }) {
  const [text, typed] = useTyped(bound);
  const lines = props.lines ?? 1;
  return /* @__PURE__ */ jsxs23(Fragment19, { children: [
    /* @__PURE__ */ jsx30(
      Drawn10,
      {
        ...props,
        value: text ?? (typeof bound.value === "string" ? bound.value : ""),
        onChange: (value) => {
          typed(value);
          bound.type(value);
          onChange?.(value);
        },
        onKeyDown: (event) => {
          if (event.key === "Enter" && !event.nativeEvent.isComposing && (lines <= 1 || event.ctrlKey || event.metaKey)) {
            bound.flush();
          }
          event.continuePropagation();
          onKeyDown?.(event);
        },
        onBlur: (event) => {
          bound.flush();
          onBlur?.(event);
        }
      }
    ),
    /* @__PURE__ */ jsx30(Refusal, { error: bound.error })
  ] });
}
function Drawn10({ label, description, errorMessage, placeholder, lines = 1, className, ...props }) {
  return /* @__PURE__ */ jsxs23(AriaTextField, { ...props, className: composeRenderProps27(className, (given) => `${styles.field} ${given ?? ""}`), children: [
    label === void 0 ? null : /* @__PURE__ */ jsx30(Label, { children: label }),
    lines > 1 ? /* @__PURE__ */ jsx30(TextArea, { rows: lines, placeholder, className: styles.box }) : /* @__PURE__ */ jsx30(Input5, { placeholder, className: styles.box }),
    /* @__PURE__ */ jsx30(Description, { children: description }),
    /* @__PURE__ */ jsx30(FieldError, { children: errorMessage })
  ] });
}

// srcts/inputs/toggle.tsx
import { createContext as createContext4, useContext as useContext9, useId, useRef as useRef9 } from "react";
import { composeRenderProps as composeRenderProps28, ToggleButton as AriaToggleButton, ToggleButtonGroup as AriaToggleButtonGroup } from "react-aria-components";
import { Fragment as Fragment20, jsx as jsx31, jsxs as jsxs24 } from "react/jsx-runtime";
function ToggleButton({ bound, onChange, ...props }) {
  if (bound === void 0) return /* @__PURE__ */ jsx31(DrawnButton, { ...props, onChange });
  return /* @__PURE__ */ jsxs24(Fragment20, { children: [
    /* @__PURE__ */ jsx31(
      DrawnButton,
      {
        ...props,
        isSelected: bound.value === true,
        onChange: (selected) => {
          void bound.commit(selected);
          onChange?.(selected);
        }
      }
    ),
    /* @__PURE__ */ jsx31(Refusal, { error: bound.error })
  ] });
}
var InGroup = createContext4(false);
function DrawnButton({ className, ...props }) {
  const ref = useRef9(null);
  const inGroup = useContext9(InGroup);
  useItem(props.id === void 0 ? void 0 : String(props.id), ref);
  return /* @__PURE__ */ jsx31(
    AriaToggleButton,
    {
      ...props,
      ref,
      className: composeRenderProps28(className, (given) => `${TOGGLE} ${inGroup ? "" : ALONE} ${given ?? ""}`)
    }
  );
}
function ToggleButtonGroup({ bound, outdated = false, ordered = false, ...props }) {
  return bound === void 0 ? /* @__PURE__ */ jsx31(DrawnGroup3, { ...props }) : /* @__PURE__ */ jsx31(BoundGroup3, { ...props, bound, outdated, ordered });
}
function BoundGroup3({
  bound,
  outdated,
  ordered,
  onSelectionChange,
  children,
  ...props
}) {
  const { known: known2, items } = useItems();
  const several = props.selectionMode === "multiple";
  const shown = several ? strings(bound.value) : typeof bound.value === "string" ? [bound.value] : [];
  const gone = lacking(shown, known2);
  const marking = !outdated && gone.length > 0;
  return /* @__PURE__ */ jsxs24(Fragment20, { children: [
    /* @__PURE__ */ jsx31(
      DrawnGroup3,
      {
        ...props,
        selectedKeys: shown,
        invalid: marking,
        onSelectionChange: (keys) => {
          const picks = [...keys].map(String);
          if (several) {
            const value = merged(strings(bound.get()), shown, picks);
            if (value !== null) void bound.commit(ordered && known2 !== null ? inOrder(value, known2) : value);
          } else {
            const [pick] = picks;
            void (pick === void 0 ? bound.unset() : bound.commit(pick));
          }
          onSelectionChange?.(keys);
        },
        children: /* @__PURE__ */ jsx31(ItemsContext.Provider, { value: items, children })
      }
    ),
    marking ? /* @__PURE__ */ jsx31(
      NotAvailable,
      {
        values: gone,
        onRemove: several ? () => {
          void bound.commit(held(strings(bound.get()), known2));
        } : void 0
      }
    ) : null,
    /* @__PURE__ */ jsx31(Refusal, { error: bound.error })
  ] });
}
function DrawnGroup3({ label, description, invalid = false, children, className, ...props }) {
  const labelId = useId();
  const descriptionId = useId();
  const described = description !== void 0 && description !== "";
  const group = /* @__PURE__ */ jsx31(
    AriaToggleButtonGroup,
    {
      ...props,
      "aria-labelledby": props["aria-labelledby"] ?? (label === void 0 ? void 0 : labelId),
      "aria-describedby": described ? descriptionId : void 0,
      "data-invalid": invalid || void 0,
      className: composeRenderProps28(className, (given) => `${GROUP} ${given ?? ""}`),
      children: /* @__PURE__ */ jsx31(InGroup.Provider, { value: true, children })
    }
  );
  if (label === void 0 && !described) return group;
  return /* @__PURE__ */ jsxs24("div", { className: styles.field, children: [
    label === void 0 ? null : /* @__PURE__ */ jsx31("span", { id: labelId, className: styles.label, children: label }),
    group,
    described ? /* @__PURE__ */ jsx31("p", { id: descriptionId, className: styles.hint, children: description }) : null
  ] });
}
var GROUP = "bk:flex bk:w-fit bk:max-w-full bk:flex-wrap bk:gap-0.5 bk:rounded-md bk:border bk:border-border bk:p-0.5 bk:font-sans bk:data-[orientation=vertical]:flex-col bk:data-invalid:border-destructive";
var TOGGLE = "bk:inline-flex bk:min-h-6 bk:cursor-pointer bk:items-center bk:justify-center bk:gap-1 bk:rounded-sm bk:border-0 bk:bg-transparent bk:px-2 bk:py-0.5 bk:font-sans bk:text-sm bk:text-inherit bk:outline-none bk:data-hovered:bg-accent bk:data-hovered:text-accent-foreground bk:data-focus-visible:outline-2 bk:data-focus-visible:outline-offset-1 bk:data-focus-visible:outline-ring bk:data-selected:bg-primary bk:data-selected:text-primary-foreground bk:data-disabled:cursor-default bk:data-disabled:opacity-50";
var ALONE = "bk:rounded-md bk:border bk:border-solid bk:border-border bk:data-selected:border-primary";

// srcts/inputs/tokenfield.tsx
import { composeRenderProps as composeRenderProps29, Token as AriaToken, TokenField as AriaTokenField, TokenInput } from "react-aria-components";
import { jsx as jsx32, jsxs as jsxs25 } from "react/jsx-runtime";
function TokenField({
  label,
  description,
  className,
  inputRef,
  children,
  ...props
}) {
  return /* @__PURE__ */ jsxs25(AriaTokenField, { ...props, className: composeRenderProps29(className, (given) => `${styles.field} ${given ?? ""}`), children: [
    label === void 0 ? null : /* @__PURE__ */ jsx32(Label, { children: label }),
    /* @__PURE__ */ jsx32(TokenInput, { ref: inputRef, className: INPUT4, children }),
    description === void 0 ? null : /* @__PURE__ */ jsx32(Description, { children: description })
  ] });
}
function Token({ className, ...props }) {
  return /* @__PURE__ */ jsx32(AriaToken, { ...props, className: composeRenderProps29(className, (given) => `${TOKEN} ${given ?? ""}`) });
}
var INPUT4 = "bk:box-border bk:min-h-7.5 bk:w-full bk:rounded-md bk:border bk:border-input bk:bg-transparent bk:px-2 bk:py-1 bk:font-sans bk:text-sm bk:outline-none bk:data-focused:outline-2 bk:data-focused:outline-offset-1 bk:data-focused:outline-ring bk:data-disabled:opacity-50";
var TOKEN = "bk:mx-0.5 bk:inline-flex bk:h-5 bk:cursor-default bk:items-center bk:whitespace-nowrap bk:rounded-sm bk:border bk:border-primary bk:bg-muted bk:px-1.5 bk:text-xs bk:data-selected:bg-primary bk:data-selected:text-primary-foreground";

// srcts/inputs/index.ts
import { Autocomplete as Autocomplete2, FileTrigger, Text as Text4, useFilter as useFilter2 } from "react-aria-components";

// srcts/layout/breadcrumbs.tsx
import { Breadcrumb as AriaBreadcrumb, Breadcrumbs as AriaBreadcrumbs, composeRenderProps as composeRenderProps31 } from "react-aria-components";
import { ChevronRight as ChevronRight3 } from "./vendor-723651e1.js";

// srcts/layout/link.tsx
import { composeRenderProps as composeRenderProps30, Link as AriaLink } from "react-aria-components";
import { jsx as jsx33 } from "react/jsx-runtime";
function Link({ variant = "primary", className, ...props }) {
  return /* @__PURE__ */ jsx33(AriaLink, { ...props, className: composeRenderProps30(className, (given) => `${LINK} ${VARIANTS[variant]} ${given ?? ""}`) });
}
var LINK = "bk:cursor-pointer bk:rounded-sm bk:underline bk:outline-none bk:transition bk:[-webkit-tap-highlight-color:transparent] bk:data-focus-visible:outline-2 bk:data-focus-visible:outline-offset-2 bk:data-focus-visible:outline-ring bk:data-disabled:cursor-default bk:data-disabled:no-underline bk:forced-colors:data-disabled:text-[GrayText]";
var VARIANTS = {
  primary: "bk:text-primary-text bk:decoration-primary-text/60 bk:data-hovered:decoration-primary-text",
  secondary: "bk:text-inherit bk:decoration-current/50 bk:data-hovered:decoration-current"
};

// srcts/layout/breadcrumbs.tsx
import { Fragment as Fragment21, jsx as jsx34, jsxs as jsxs26 } from "react/jsx-runtime";
function Breadcrumbs({ className, ...props }) {
  return /* @__PURE__ */ jsx34(AriaBreadcrumbs, { ...props, className: `${BREADCRUMBS} ${className ?? ""}` });
}
function Breadcrumb({ className, id, ...props }) {
  return /* @__PURE__ */ jsx34(AriaBreadcrumb, { ...props, id, className: composeRenderProps31(className, (given) => `${BREADCRUMB} ${given ?? ""}`), children: ({ isCurrent }) => /* @__PURE__ */ jsxs26(Fragment21, { children: [
    /* @__PURE__ */ jsx34(Link, { variant: "secondary", ...props }),
    isCurrent ? null : /* @__PURE__ */ jsx34("span", { className: "bk:flex bk:shrink-0 bk:text-muted-foreground", children: /* @__PURE__ */ jsx34(Icon, { icon: ChevronRight3, size: 12 }) })
  ] }) });
}
var BREADCRUMBS = "bk:m-0 bk:flex bk:list-none bk:flex-wrap bk:items-center bk:gap-1 bk:p-0 bk:font-sans bk:text-sm";
var BREADCRUMB = "bk:flex bk:items-center bk:gap-1";

// srcts/layout/disclosure.tsx
import { useContext as useContext10 } from "react";
import {
  Button as Button8,
  composeRenderProps as composeRenderProps32,
  Disclosure as AriaDisclosure,
  DisclosurePanel as AriaDisclosurePanel,
  DisclosureStateContext,
  Heading as Heading3
} from "react-aria-components";
import { ChevronRight as ChevronRight4 } from "./vendor-723651e1.js";
import { jsx as jsx35, jsxs as jsxs27 } from "react/jsx-runtime";
function Disclosure({ className, ...props }) {
  return /* @__PURE__ */ jsx35(AriaDisclosure, { ...props, className: composeRenderProps32(className, (given) => `${DISCLOSURE} ${given ?? ""}`) });
}
function DisclosureHeader({ children, level = 3, className }) {
  const expanded = useContext10(DisclosureStateContext)?.isExpanded ?? false;
  return /* @__PURE__ */ jsx35(Heading3, { level, className: `${HEADING4} ${className ?? FACE}`, children: /* @__PURE__ */ jsxs27(Button8, { slot: "trigger", className: TRIGGER2, children: [
    /* @__PURE__ */ jsx35(Marker, { expanded }),
    children
  ] }) });
}
function DisclosurePanel({ className, ...props }) {
  return /* @__PURE__ */ jsx35(AriaDisclosurePanel, { ...props, className: composeRenderProps32(className, (given) => `${PANEL} ${given ?? ""}`) });
}
function Marker({ expanded }) {
  return /* @__PURE__ */ jsx35("span", { className: `bk:flex bk:shrink-0 bk:transition-transform bk:motion-reduce:transition-none ${expanded ? "bk:rotate-90" : ""}`, children: /* @__PURE__ */ jsx35(Icon, { icon: ChevronRight4, size: 12 }) });
}
var DISCLOSURE = "bk:font-sans";
var HEADING4 = "bk:m-0 bk:font-sans";
var FACE = "bk:text-base bk:font-semibold";
var TRIGGER2 = "bk:flex bk:w-full bk:cursor-pointer bk:items-center bk:gap-1.5 bk:rounded-sm bk:border-0 bk:bg-transparent bk:p-0 bk:text-left bk:text-inherit bk:[font:inherit] bk:outline-none bk:data-focus-visible:outline-2 bk:data-focus-visible:outline-offset-2 bk:data-focus-visible:outline-ring bk:data-disabled:cursor-default bk:data-disabled:opacity-50";
var PANEL = "bk:h-(--disclosure-panel-height) bk:overflow-clip bk:motion-safe:transition-[height]";

// srcts/layout/meter.tsx
import { composeRenderProps as composeRenderProps33, Meter as AriaMeter, ProgressBar as AriaProgressBar } from "react-aria-components";
import { TriangleExclamationPoint } from "./vendor-723651e1.js";
import { Fragment as Fragment22, jsx as jsx36, jsxs as jsxs28 } from "react/jsx-runtime";
function Meter({ label, className, ...props }) {
  return /* @__PURE__ */ jsx36(AriaMeter, { ...props, className: composeRenderProps33(className, (given) => `${METER} ${given ?? ""}`), children: ({ percentage, valueText }) => /* @__PURE__ */ jsxs28(Fragment22, { children: [
    /* @__PURE__ */ jsxs28("div", { className: "bk:flex bk:justify-between bk:gap-2", children: [
      /* @__PURE__ */ jsx36(Label, { children: label }),
      /* @__PURE__ */ jsxs28("span", { className: `bk:text-sm ${percentage >= 80 ? "bk:text-destructive-text" : "bk:text-muted-foreground"}`, children: [
        percentage >= 80 ? /* @__PURE__ */ jsx36(Alert, {}) : null,
        ` ${valueText ?? ""}`
      ] })
    ] }),
    /* @__PURE__ */ jsx36("div", { className: `${TRACK4} bk:w-64`, children: /* @__PURE__ */ jsx36("div", { className: `${FILL2} bk:left-0 ${fill(percentage)}`, style: { width: `${String(percentage)}%` } }) })
  ] }) });
}
function ProgressBar({ label, className, ...props }) {
  return /* @__PURE__ */ jsx36(AriaProgressBar, { ...props, className: composeRenderProps33(className, (given) => `${PROGRESS} ${given ?? ""}`), children: ({ percentage, valueText, isIndeterminate }) => /* @__PURE__ */ jsxs28(Fragment22, { children: [
    /* @__PURE__ */ jsxs28("div", { className: "bk:flex bk:justify-between bk:gap-2", children: [
      /* @__PURE__ */ jsx36(Label, { children: label }),
      /* @__PURE__ */ jsx36("span", { className: "bk:text-sm bk:text-muted-foreground", children: valueText })
    ] }),
    /* @__PURE__ */ jsx36("div", { className: `${TRACK4} bk:overflow-hidden`, children: /* @__PURE__ */ jsx36(
      "div",
      {
        className: `${FILL2} bk:left-0 bk:bg-primary ${isIndeterminate ? "bk:motion-safe:animate-[bk-progress-slide_1s_ease-out_infinite]" : ""}`,
        style: { width: `${String(isIndeterminate ? 40 : percentage ?? 0)}%` }
      }
    ) })
  ] }) });
}
function fill(percentage) {
  if (percentage < 70) return "bk:bg-success";
  if (percentage < 80) return "bk:bg-warning";
  return "bk:bg-destructive";
}
function Alert() {
  return /* @__PURE__ */ jsx36("span", { className: "bk:inline-flex bk:align-text-bottom", children: /* @__PURE__ */ jsx36(Icon, { icon: TriangleExclamationPoint, size: 16, label: "Alert" }) });
}
var METER = "bk:flex bk:max-w-full bk:flex-col bk:gap-2 bk:font-sans";
var PROGRESS = "bk:flex bk:w-64 bk:max-w-full bk:flex-col bk:gap-2 bk:font-sans";
var TRACK4 = "bk:relative bk:h-2 bk:max-w-full bk:rounded-full bk:bg-border bk:outline bk:outline-1 bk:-outline-offset-1 bk:outline-transparent";
var FILL2 = "bk:absolute bk:top-0 bk:h-full bk:rounded-full bk:forced-colors:bg-[Highlight]";

// srcts/layout/tabs.tsx
import {
  composeRenderProps as composeRenderProps34,
  SelectionIndicator,
  Tab as AriaTab,
  TabList as AriaTabList,
  TabPanel as AriaTabPanel,
  Tabs as AriaTabs
} from "react-aria-components";
import { Fragment as Fragment23, jsx as jsx37, jsxs as jsxs29 } from "react/jsx-runtime";
function Tabs({ className, ...props }) {
  return /* @__PURE__ */ jsx37(
    AriaTabs,
    {
      ...props,
      className: composeRenderProps34(className, (given, { orientation }) => `${TABS} ${ORIENTED[orientation]} ${given ?? ""}`)
    }
  );
}
function TabList({ className, ...props }) {
  return /* @__PURE__ */ jsx37(
    AriaTabList,
    {
      ...props,
      className: composeRenderProps34(className, (given, { orientation }) => `${LIST3} ${LISTED[orientation]} ${given ?? ""}`)
    }
  );
}
function Tab({ className, children, ...props }) {
  return /* @__PURE__ */ jsx37(AriaTab, { ...props, className: composeRenderProps34(className, (given) => `${TAB} ${given ?? ""}`), children: composeRenderProps34(children, (children2) => /* @__PURE__ */ jsxs29(Fragment23, { children: [
    children2,
    /* @__PURE__ */ jsx37(SelectionIndicator, { className: PILL })
  ] })) });
}
function TabPanel({ className, ...props }) {
  return /* @__PURE__ */ jsx37(AriaTabPanel, { ...props, className: composeRenderProps34(className, (given) => `${PANEL2} ${given ?? ""}`) });
}
var TABS = "bk:flex bk:max-w-full bk:gap-4 bk:font-sans";
var ORIENTED = { horizontal: "bk:flex-col", vertical: "bk:flex-row" };
var LIST3 = "bk:-m-1 bk:flex bk:max-w-full bk:overflow-x-auto bk:overflow-y-clip bk:p-1 bk:[scrollbar-width:none]";
var LISTED = { horizontal: "bk:flex-row", vertical: "bk:flex-col bk:items-start" };
var TAB = "bk:relative bk:isolate bk:flex bk:cursor-default bk:items-center bk:rounded-full bk:px-3 bk:py-1.5 bk:font-sans bk:text-sm bk:font-medium bk:text-muted-foreground bk:outline-none bk:transition-colors bk:forced-color-adjust-none bk:[-webkit-tap-highlight-color:transparent] bk:data-hovered:text-inherit bk:data-selected:text-primary-foreground bk:data-focus-visible:outline-2 bk:data-focus-visible:outline-offset-2 bk:data-focus-visible:outline-ring bk:data-disabled:cursor-default bk:data-disabled:opacity-50 bk:forced-colors:data-selected:text-[HighlightText]";
var PILL = "bk:absolute bk:inset-0 bk:-z-1 bk:rounded-full bk:bg-primary bk:motion-safe:transition-[translate,width,height] bk:forced-colors:bg-[Highlight]";
var PANEL2 = "bk:box-border bk:flex-1 bk:rounded-sm bk:p-4 bk:font-sans bk:text-sm bk:outline-none bk:data-focus-visible:outline-2 bk:data-focus-visible:outline-ring";

// srcts/layout/toolbar.tsx
import {
  composeRenderProps as composeRenderProps35,
  Separator as AriaSeparator,
  ToggleButtonGroupContext,
  Toolbar as AriaToolbar
} from "react-aria-components";
import { jsx as jsx38 } from "react/jsx-runtime";
function Toolbar({ className, ...props }) {
  return /* @__PURE__ */ jsx38(ToggleButtonGroupContext, { value: { orientation: props.orientation }, children: /* @__PURE__ */ jsx38(
    AriaToolbar,
    {
      ...props,
      className: composeRenderProps35(className, (given, { orientation }) => `${TOOLBAR} ${ORIENTED2[orientation]} ${given ?? ""}`)
    }
  ) });
}
function Separator({ className, ...props }) {
  return /* @__PURE__ */ jsx38(AriaSeparator, { ...props, className: `${SEPARATOR} ${ACROSS[props.orientation ?? "horizontal"]} ${className ?? ""}` });
}
var TOOLBAR = "bk:flex bk:flex-wrap bk:gap-2";
var ORIENTED2 = { horizontal: "bk:flex-row bk:items-center", vertical: "bk:flex-col bk:items-start" };
var SEPARATOR = "bk:m-0 bk:border-none bk:bg-border bk:forced-colors:bg-[ButtonBorder]";
var ACROSS = { horizontal: "bk:h-px bk:w-full", vertical: "bk:w-px bk:self-stretch" };

// srcts/layout/index.ts
import { DisclosureGroup } from "react-aria-components";

// srcts/dnd/dnd.tsx
import { composeRenderProps as composeRenderProps36, DropIndicator as AriaDropIndicator, useDragAndDrop as useAriaDragAndDrop } from "react-aria-components";
import { GripVertical } from "./vendor-723651e1.js";
import { jsx as jsx39 } from "react/jsx-runtime";
function useDragAndDrop(options) {
  return useAriaDragAndDrop({ renderDropIndicator: (target) => /* @__PURE__ */ jsx39(DropIndicator, { target }), ...options });
}
function DropIndicator(props) {
  return /* @__PURE__ */ jsx39(AriaDropIndicator, { ...props, className: composeRenderProps36(props.className, (given) => `${INDICATOR} ${given ?? ""}`) });
}
function DragButton() {
  return /* @__PURE__ */ jsx39(Button, { slot: "drag", variant: "quiet", children: /* @__PURE__ */ jsx39(Icon, { icon: GripVertical, size: 12 }) });
}
var INDICATOR = "bk:outline-none bk:data-drop-target:outline-1 bk:data-drop-target:outline-primary bk:forced-colors:data-drop-target:outline-[Highlight]";

// srcts/gridlist/gridlist.tsx
import {
  composeRenderProps as composeRenderProps37,
  GridList as AriaGridList,
  GridListHeader as AriaGridListHeader,
  GridListItem as AriaGridListItem,
  GridListSection
} from "react-aria-components";
import { Fragment as Fragment24, jsx as jsx40, jsxs as jsxs30 } from "react/jsx-runtime";
function GridList(props) {
  return /* @__PURE__ */ jsx40(AriaGridList, { ...props, className: composeRenderProps37(props.className, (given) => `${LIST4} ${given ?? ""}`) });
}
function GridListItem({ children, ...props }) {
  const textValue = props.textValue ?? (typeof children === "string" ? children : void 0);
  return /* @__PURE__ */ jsx40(AriaGridListItem, { ...props, textValue, className: composeRenderProps37(props.className, (given) => `${ITEM3} ${given ?? ""}`), children: composeRenderProps37(children, (children2, { selectionMode, selectionBehavior, allowsDragging }) => /* @__PURE__ */ jsxs30(Fragment24, { children: [
    allowsDragging && /* @__PURE__ */ jsx40(DragButton, {}),
    selectionMode !== "none" && selectionBehavior === "toggle" && /* @__PURE__ */ jsx40(Checkbox, { slot: "selection" }),
    children2
  ] })) });
}
function GridListHeader({ className, ...props }) {
  return /* @__PURE__ */ jsx40(AriaGridListHeader, { ...props, className: `${HEADER2} ${className ?? ""}` });
}
var LIST4 = "bk:grid bk:content-start bk:gap-0.5 bk:overflow-auto bk:font-sans bk:outline-none bk:data-[orientation=horizontal]:flex bk:data-[orientation=horizontal]:flex-row bk:data-[orientation=horizontal]:flex-nowrap bk:data-empty:px-2 bk:data-empty:py-0.5 bk:data-empty:text-sm bk:data-empty:text-muted-foreground bk:data-focus-visible:outline-2 bk:data-focus-visible:outline-ring";
var ITEM3 = "bk:flex bk:shrink-0 bk:cursor-pointer bk:select-none bk:items-center bk:gap-2 bk:rounded-sm bk:px-2 bk:py-0.5 bk:text-sm bk:outline-none bk:-outline-offset-2 bk:data-focus-visible:outline-2 bk:data-focus-visible:outline-ring bk:data-hovered:bg-accent bk:data-hovered:text-accent-foreground bk:data-pressed:bg-accent bk:data-selected:bg-primary/10 bk:data-disabled:cursor-default bk:data-disabled:opacity-50";
var HEADER2 = "bk:block bk:px-2 bk:pt-1 bk:text-xs bk:font-semibold bk:text-muted-foreground";

// srcts/table/table.tsx
import {
  Cell as AriaCell,
  Collection as Collection4,
  Column as AriaColumn,
  composeRenderProps as composeRenderProps38,
  Row as AriaRow,
  Table as AriaTable,
  TableBody as AriaTableBody,
  TableHeader as AriaTableHeader,
  useTableOptions
} from "react-aria-components";
import { ArrowUp } from "./vendor-723651e1.js";
import { jsx as jsx41, jsxs as jsxs31 } from "react/jsx-runtime";
function Table({ className, ...props }) {
  return /* @__PURE__ */ jsx41("div", { onScroll: props.onScroll, className: `${BOX3} ${className ?? ""}`, children: /* @__PURE__ */ jsx41(AriaTable, { ...props, className: TABLE }) });
}
function Column({ align = "start", ...props }) {
  return /* @__PURE__ */ jsx41(
    AriaColumn,
    {
      ...props,
      className: composeRenderProps38(props.className, (given) => `${COLUMN} ${align === "end" ? "bk:text-end" : "bk:text-start"} ${given ?? ""}`),
      children: composeRenderProps38(props.children, (children, { sortDirection }) => /* @__PURE__ */ jsxs31("span", { className: `${HEADING5} ${align === "end" ? "bk:flex-row-reverse" : ""}`, children: [
        children,
        sortDirection === void 0 ? null : /* @__PURE__ */ jsx41(Arrow2, { down: sortDirection === "descending" })
      ] }))
    }
  );
}
function TableHeader({ columns, children, ...props }) {
  const { selectionBehavior, selectionMode, allowsDragging } = useTableOptions();
  return /* @__PURE__ */ jsxs31(AriaTableHeader, { ...props, children: [
    allowsDragging && /* @__PURE__ */ jsx41(AriaColumn, { className: PART }),
    selectionBehavior === "toggle" && /* @__PURE__ */ jsx41(AriaColumn, { className: PART, children: selectionMode === "multiple" && /* @__PURE__ */ jsx41(Checkbox, { slot: "selection" }) }),
    /* @__PURE__ */ jsx41(Collection4, { items: columns, children })
  ] });
}
function TableBody(props) {
  return /* @__PURE__ */ jsx41(AriaTableBody, { ...props, className: composeRenderProps38(props.className, (given) => `${BODY} ${given ?? ""}`) });
}
function Row({ id, columns, children, ...props }) {
  const { selectionBehavior, allowsDragging } = useTableOptions();
  return /* @__PURE__ */ jsxs31(AriaRow, { id, ...props, className: composeRenderProps38(props.className, (given) => `${ROW2} ${given ?? ""}`), children: [
    allowsDragging && /* @__PURE__ */ jsx41(Cell, { className: "bk:w-px", children: /* @__PURE__ */ jsx41(DragButton, {}) }),
    selectionBehavior === "toggle" && /* @__PURE__ */ jsx41(Cell, { className: "bk:w-px", children: /* @__PURE__ */ jsx41(Checkbox, { slot: "selection" }) }),
    /* @__PURE__ */ jsx41(Collection4, { items: columns, children })
  ] });
}
function Cell(props) {
  return /* @__PURE__ */ jsx41(AriaCell, { ...props, className: composeRenderProps38(props.className, (given) => `${CELL2} ${given ?? ""}`) });
}
function Arrow2({ down }) {
  return /* @__PURE__ */ jsx41("span", { className: `bk:mb-0.5 bk:flex bk:shrink-0 bk:forced-colors:text-[ButtonText] ${down ? "bk:rotate-180" : ""}`, children: /* @__PURE__ */ jsx41(Icon, { icon: ArrowUp, size: 12 }) });
}
var BOX3 = "bk:min-w-0 bk:overflow-auto bk:font-sans";
var TABLE = "bk:w-full bk:border-collapse bk:text-left bk:text-sm";
var COLUMN = "bk:border-b bk:border-border bk:px-2 bk:py-1 bk:align-bottom bk:font-semibold bk:whitespace-nowrap bk:outline-none bk:-outline-offset-2 bk:data-focus-visible:outline-2 bk:data-focus-visible:outline-ring bk:data-allows-sorting:cursor-pointer";
var HEADING5 = "bk:flex bk:items-end bk:gap-1";
var PART = "bk:w-px bk:border-b bk:border-border bk:px-2 bk:py-1";
var BODY = "bk:data-empty:text-center bk:data-empty:text-sm bk:data-empty:italic bk:data-empty:text-muted-foreground";
var ROW2 = "bk:outline-none bk:-outline-offset-2 bk:data-focus-visible:outline-2 bk:data-focus-visible:outline-ring bk:data-hovered:bg-accent bk:data-hovered:text-accent-foreground bk:data-pressed:bg-accent bk:data-selected:bg-primary/10 bk:data-disabled:opacity-50 bk:data-selection-mode:select-none";
var CELL2 = "bk:border-b bk:border-border/60 bk:px-2 bk:py-1 bk:whitespace-nowrap bk:outline-none bk:-outline-offset-2 bk:[-webkit-tap-highlight-color:transparent] bk:data-focus-visible:outline-2 bk:data-focus-visible:outline-ring";

// srcts/tree/navigation-tree.tsx
import { createContext as createContext5, useContext as useContext11 } from "react";
import {
  Button as Button10,
  composeRenderProps as composeRenderProps40,
  Link as Link2,
  NavigationTree as AriaNavigationTree,
  NavigationTreeHeader as AriaNavigationTreeHeader,
  NavigationTreeItem as AriaNavigationTreeItem,
  NavigationTreeItemContent as AriaNavigationTreeItemContent,
  NavigationTreeSection as AriaNavigationTreeSection
} from "react-aria-components";

// srcts/tree/tree.tsx
import {
  Button as Button9,
  composeRenderProps as composeRenderProps39,
  Tree as AriaTree,
  TreeItem as AriaTreeItem,
  TreeItemContent
} from "react-aria-components";
import { ChevronRight as ChevronRight5 } from "./vendor-723651e1.js";
import { jsx as jsx42, jsxs as jsxs32 } from "react/jsx-runtime";
function Tree(props) {
  return /* @__PURE__ */ jsx42(AriaTree, { ...props, className: composeRenderProps39(props.className, (given) => `${TREE} ${given ?? ""}`) });
}
function TreeItem({ title, children, ...props }) {
  return /* @__PURE__ */ jsxs32(AriaTreeItem, { textValue: title, ...props, className: composeRenderProps39(props.className, (given) => `${ITEM4} ${given ?? ""}`), children: [
    /* @__PURE__ */ jsx42(TreeItemContent, { children: ({ selectionMode, selectionBehavior, hasChildItems, isExpanded, allowsDragging }) => /* @__PURE__ */ jsxs32("div", { className: "bk:flex bk:min-w-0 bk:flex-1 bk:items-center bk:gap-1", children: [
      allowsDragging && /* @__PURE__ */ jsx42(DragButton, {}),
      selectionMode !== "none" && selectionBehavior === "toggle" && /* @__PURE__ */ jsx42(Checkbox, { slot: "selection" }),
      /* @__PURE__ */ jsx42("div", { className: INDENT }),
      hasChildItems ? /* @__PURE__ */ jsx42(Button9, { slot: "chevron", className: EXPAND, children: /* @__PURE__ */ jsx42(Chevron, { expanded: isExpanded }) }) : /* @__PURE__ */ jsx42("div", { className: "bk:size-6 bk:shrink-0" }),
      /* @__PURE__ */ jsx42("span", { className: "bk:min-w-0 bk:truncate", children: title })
    ] }) }),
    children
  ] });
}
function Chevron({ expanded }) {
  return /* @__PURE__ */ jsx42(
    "span",
    {
      className: `bk:flex bk:text-muted-foreground bk:transition-transform bk:motion-reduce:transition-none ${expanded ? "bk:rotate-90" : ""}`,
      children: /* @__PURE__ */ jsx42(Icon, { icon: ChevronRight5 })
    }
  );
}
var TREE = "bk:grid bk:content-start bk:gap-0.5 bk:overflow-auto bk:font-sans bk:outline-none bk:data-focus-visible:outline-2 bk:data-focus-visible:outline-ring";
var ITEM4 = "bk:group bk:relative bk:flex bk:cursor-default bk:items-center bk:rounded-sm bk:px-1 bk:font-sans bk:text-sm bk:select-none bk:outline-none bk:-outline-offset-2 bk:data-focus-visible:outline-2 bk:data-focus-visible:outline-ring bk:data-hovered:bg-accent bk:data-hovered:text-accent-foreground bk:data-pressed:bg-accent bk:data-selected:bg-primary/10 bk:data-disabled:opacity-50";
var INDENT = "bk:w-[calc((var(--tree-item-level)_-_1)_*_var(--bk-spacing)_*_3)] bk:shrink-0";
var EXPAND = "bk:flex bk:size-6 bk:shrink-0 bk:cursor-default bk:items-center bk:justify-center bk:rounded-sm bk:border-0 bk:bg-transparent bk:p-0 bk:text-inherit bk:outline-none bk:[-webkit-tap-highlight-color:transparent] bk:data-focus-visible:outline-2 bk:data-focus-visible:outline-ring bk:data-disabled:opacity-50";

// srcts/tree/navigation-tree.tsx
import { Fragment as Fragment25, jsx as jsx43, jsxs as jsxs33 } from "react/jsx-runtime";
var IndicatorContext = createContext5(void 0);
var RenderLinkContext = createContext5(void 0);
function NavigationTree({ renderLink, ...props }) {
  return /* @__PURE__ */ jsx43(RenderLinkContext.Provider, { value: renderLink, children: /* @__PURE__ */ jsx43(AriaNavigationTree, { ...props, className: composeRenderProps40(props.className, (given) => `${TREE2} ${given ?? ""}`) }) });
}
function NavigationTreeItem(props) {
  return /* @__PURE__ */ jsx43(AriaNavigationTreeItem, { ...props, className: composeRenderProps40(props.className, (given) => `${ITEM5} ${given ?? ""}`) });
}
function NavigationTreeItemContent({ children }) {
  return /* @__PURE__ */ jsx43(AriaNavigationTreeItemContent, { children: ({ level, hasChildItems, isExpanded, isCurrent, isCurrentAncestor }) => /* @__PURE__ */ jsxs33(Fragment25, { children: [
    level > 1 && /* @__PURE__ */ jsx43("div", { className: "bk:shrink-0", style: { width: `calc((${String(level)} - 1) * var(--bk-spacing) * 4)` } }),
    /* @__PURE__ */ jsx43(IndicatorContext.Provider, { value: isCurrent ? "current" : isCurrentAncestor && !isExpanded ? "ancestor" : void 0, children }),
    /* @__PURE__ */ jsx43(Button10, { slot: "chevron", isDisabled: !hasChildItems, className: `${EXPAND2} ${hasChildItems ? "" : "bk:invisible"}`, children: /* @__PURE__ */ jsx43(Chevron, { expanded: isExpanded }) })
  ] }) });
}
function NavigationTreeSection(props) {
  return /* @__PURE__ */ jsx43(AriaNavigationTreeSection, { ...props, className: "bk:not-first:mt-4" });
}
function NavigationTreeHeader(props) {
  return /* @__PURE__ */ jsx43(AriaNavigationTreeHeader, { ...props, className: "bk:px-2 bk:py-1 bk:text-sm bk:font-semibold bk:text-muted-foreground" });
}
function NavigationTreeItemLink({ render, children, ...props }) {
  const indicator = useContext11(IndicatorContext);
  const given = useContext11(RenderLinkContext);
  return /* @__PURE__ */ jsx43(Link2, { render: render ?? given, ...props, className: LINK2, children: ({ isHovered }) => /* @__PURE__ */ jsxs33(Fragment25, { children: [
    /* @__PURE__ */ jsx43("span", { "aria-hidden": "true", "data-indicator": indicator ?? (isHovered ? "hover" : void 0), className: MARK }),
    children
  ] }) });
}
var TREE2 = "bk:flex bk:flex-col bk:gap-0.5 bk:overflow-auto bk:font-sans bk:outline-none";
var ITEM5 = "bk:group bk:relative bk:flex bk:cursor-default bk:items-center bk:rounded-md bk:font-sans bk:select-none bk:outline-none bk:-outline-offset-2 bk:[-webkit-tap-highlight-color:transparent] bk:data-focus-visible:outline-2 bk:data-focus-visible:outline-ring bk:data-disabled:opacity-50";
var LINK2 = "bk:relative bk:flex bk:min-w-0 bk:flex-1 bk:cursor-pointer bk:items-center bk:gap-2 bk:px-2 bk:py-1.5 bk:text-sm bk:text-current bk:no-underline bk:outline-none bk:group-[:not([data-href])]:cursor-default bk:data-disabled:cursor-default";
var MARK = "bk:absolute bk:start-0.5 bk:top-1/2 bk:h-[1lh] bk:w-0.5 bk:-translate-y-1/2 bk:rounded-full bk:forced-color-adjust-none bk:group-data-href:data-[indicator=hover]:bg-muted-foreground bk:data-[indicator=current]:bg-primary bk:data-[indicator=ancestor]:size-1 bk:data-[indicator=ancestor]:bg-muted-foreground bk:forced-colors:data-indicator:bg-[Highlight]";
var EXPAND2 = "bk:flex bk:size-6 bk:shrink-0 bk:cursor-default bk:items-center bk:justify-center bk:rounded-md bk:border-0 bk:bg-transparent bk:p-0 bk:text-inherit bk:outline-none bk:-outline-offset-2 bk:[-webkit-tap-highlight-color:transparent] bk:data-focus-visible:outline-2 bk:data-focus-visible:outline-ring";
export {
  AlertDialog,
  Autocomplete2 as Autocomplete,
  Breadcrumb,
  Breadcrumbs,
  Button,
  Calendar,
  Cell,
  Checkbox,
  CheckboxGroup,
  ColorArea,
  ColorField,
  ColorPicker,
  ColorSlider,
  ColorSwatch,
  ColorSwatchPicker,
  ColorSwatchPickerItem,
  ColorThumb,
  ColorWheel,
  Column,
  ComboBox,
  ComboBoxItem,
  ComboBoxSection,
  CommandPalette,
  DateField,
  DatePicker,
  DateRangePicker,
  Description,
  Dialog,
  DialogTrigger,
  Disclosure,
  DisclosureGroup,
  DisclosureHeader,
  DisclosurePanel,
  DropIndicator,
  DropZone,
  Field,
  FieldButton,
  FieldError,
  FileTrigger,
  Focusable,
  Form,
  FormLayout,
  GridList,
  GridListHeader,
  GridListItem,
  GridListSection,
  Heading2 as Heading,
  Icon,
  Keyboard,
  Label,
  Link,
  ListBox2 as ListBox,
  ListBoxItem2 as ListBoxItem,
  Menu,
  MenuItem,
  MenuSection,
  MenuTrigger,
  Meter,
  Modal,
  NavigationTree,
  NavigationTreeHeader,
  NavigationTreeItem,
  NavigationTreeItemContent,
  NavigationTreeItemLink,
  NavigationTreeSection,
  NumberField,
  Popover,
  ProgressBar,
  Radio,
  RadioGroup,
  RangeCalendar,
  Row,
  SearchField,
  Select,
  SelectItem,
  SelectSection,
  Separator,
  Sheet,
  Slider2 as Slider,
  SubmenuTrigger,
  Switch,
  Tab,
  TabList,
  TabPanel,
  Table,
  TableBody,
  TableHeader,
  Tabs,
  Tag2 as Tag,
  TagGroup2 as TagGroup,
  Text4 as Text,
  TextField,
  TimeField,
  Toast,
  UNSTABLE_ToastContent as ToastContent,
  UNSTABLE_ToastQueue as ToastQueue,
  ToastRegion,
  ToggleButton,
  ToggleButtonGroup,
  Token,
  TokenField,
  Toolbar,
  Tooltip,
  TooltipTrigger,
  Tree,
  TreeItem,
  iconMarkup,
  useDragAndDrop,
  useFilter2 as useFilter
};
