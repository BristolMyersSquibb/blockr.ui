// The slider of blockr.ui: React Aria's Slider
// (https://react-aria.adobe.com/Slider), drawn with the theme variables, after the Slider of
// React Aria's Tailwind starter kit
// (https://github.com/adobe/react-spectrum/blob/react-aria-components%401.22.1/starters/tailwind/src/Slider.tsx).
// It takes what that takes: the props of React Aria's Slider, such as
// minValue, maxValue, step and formatOptions, and the label, the labels of
// the thumbs and the offset of the fill that the starter kit adds.
// It has one thumb, or one for each number where its value is an array.
// The number, or the numbers, show at the right of the label.
//
// Given `bound`, it shows and edits the number that its `bound` gives, or an
// array of numbers where it has several thumbs, as blockr.ui's other inputs do
// (./inputs.tsx).
// It shows the number that it moves to while the user drags a thumb, and
// submits it as the user lets go, through onChangeEnd, so that a drag is one
// edit and one undo step, as it submits a step of a thumb by a key.
// It keeps the number that the user let go of until the binding shows a value
// again, or the host refuses it.
// It submits only a number that the user gave it, so never one that it pulled
// into its range or onto its step, as where the user lets go of a thumb without
// moving it.
// So where the user moves one thumb of several, it submits the numbers of the
// others as the value holds them.
// React Aria draws a thumb only within the range, and on the step, so a thumb
// of a number outside them sits at the end, or on the step, nearest to it.
// The slider then shows the number at the right of the label as it is, marks
// itself invalid, and says below itself that the number is not available, as
// the select says of a value that its items lack (./combobox.tsx), but not
// while it is `outdated`, as while the request that gives its maxValue asks
// again.
// The user drops such a number by moving its thumb.
// It says below itself why its last submit was refused, the `error` of its
// `bound`.

import { useContext, useLayoutEffect, useRef, useState } from "react";
import type { ReactElement } from "react";
import {
  composeRenderProps,
  Slider as AriaSlider,
  SliderFill,
  SliderOutput,
  SliderStateContext,
  SliderThumb,
  SliderTrack,
  useLocale,
} from "react-aria-components";
import type { SliderProps as AriaSliderProps } from "react-aria-components";
import type { Bound } from "./bound.ts";
import type { Json } from "./bound.ts";
import { jsonEqual } from "./json.ts";
import { Label, NotAvailable, Refusal } from "./inputs.tsx";
import { inRange } from "./numberfield.tsx";

type Value = number | number[];

export interface SliderProps<T extends Value = Value> extends AriaSliderProps<T> {
  // What the starter kit adds.
  readonly label?: string;
  readonly thumbLabels?: readonly string[];
  readonly fillOffset?: number;
  // The value that it shows and submits.
  readonly bound?: Bound;
  readonly outdated?: boolean;
}

export function Slider<T extends Value = Value>({ bound, outdated = false, ...props }: SliderProps<T>): ReactElement {
  return bound === undefined ? (
    <Drawn {...(props as DrawnProps)} />
  ) : (
    <BoundSlider {...(props as DrawnProps)} bound={bound} outdated={outdated} />
  );
}

type DrawnProps = Omit<SliderProps, "bound" | "outdated"> & {
  // What the output says in place of React Aria's numbers, as where the
  // slider holds a number that it lacks, and whether it marks the slider
  // invalid.
  readonly said?: (format: (value: number) => string) => string;
  readonly invalid?: boolean;
  // What reads the numbers that React Aria shows, inside the slider.
  readonly watch?: ReactElement;
};

function BoundSlider({
  bound,
  outdated,
  onChange,
  onChangeEnd,
  ...props
}: DrawnProps & { readonly bound: Bound; readonly outdated: boolean }): ReactElement {
  const several = Array.isArray(bound.value) || Array.isArray(props.defaultValue);
  const held = numbers(bound.value);
  // The numbers that the user moves the thumbs to, from the first move of a
  // drag until the binding shows a value again, or the host refused them.
  const [moving, setMoving] = useState<readonly number[] | null>(null);
  const [seen, setSeen] = useState<Json | undefined>(bound.value);
  if (!jsonEqual(seen, bound.value)) {
    setSeen(bound.value);
    setMoving(null);
  }
  // The numbers that React Aria showed while no thumb moved, and those it
  // showed as the drag began, which the numbers let go of must differ from.
  const shown = useRef<readonly number[]>([]);
  const start = useRef<readonly number[] | null>(null);

  const min = props.minValue ?? 0;
  const fits = (value: number): boolean => inRange(value, min, props.maxValue ?? 100, props.step ?? 1);
  const lacked = moving === null ? held.filter((value) => !fits(value)) : [];
  const marking = !outdated && lacked.length > 0;
  const drawn = moving ?? (held.length > 0 ? held : [min]);
  const value = several ? [...drawn] : (drawn[0] ?? min);
  const { locale } = useLocale();

  return (
    <>
      <Drawn
        {...props}
        value={value}
        invalid={marking}
        said={
          moving === null && held.length === 0
            ? () => ""
            : lacked.length > 0
              ? (formatted) => held.map(formatted).join(held.length === 2 ? "–" : ", ")
              : undefined
        }
        onChange={(next) => {
          start.current ??= shown.current;
          setMoving(numbers(next));
          onChange?.(next);
        }}
        onChangeEnd={(next) => {
          const ended = numbers(next);
          const began = start.current ?? shown.current;
          start.current = null;
          if (jsonEqual(ended, began)) {
            setMoving(null);
          } else {
            // A thumb that the user did not move keeps the number that the
            // value holds, as one that the slider pulled in.
            const given = ended.map((x, i) => (Object.is(x, began[i]) ? (held[i] ?? x) : x));
            setMoving(ended);
            void bound.commit(several ? given : (given[0] ?? min)).then((took) => {
              if (!took) setMoving(null);
            });
          }
          onChangeEnd?.(next);
        }}
        watch={
          <Watch
            onShown={(values) => {
              shown.current = values;
            }}
          />
        }
      />
      {marking ? <NotAvailable values={lacked.map((x) => new Intl.NumberFormat(locale, props.formatOptions).format(x))} /> : null}
      <Refusal error={bound.error} />
    </>
  );
}

// Hands `onShown` the numbers that React Aria shows, each time it has drawn
// them while no thumb moves.
function Watch({ onShown }: { onShown: (values: readonly number[]) => void }): null {
  const state = useContext(SliderStateContext);
  useLayoutEffect(() => {
    if (state !== null && !state.values.some((_, i) => state.isThumbDragging(i))) onShown(state.values);
  });
  return null;
}

// The numbers of a value, as a slider holds them.
function numbers(value: Json | undefined | Value): number[] {
  if (typeof value === "number") return [value];
  return Array.isArray(value) ? value.filter((x): x is number => typeof x === "number") : [];
}

// The label and the numbers, the track with its fill and its thumbs.
function Drawn({ label, thumbLabels, fillOffset, said, invalid = false, watch, className, ...props }: DrawnProps): ReactElement {
  return (
    <AriaSlider
      {...props}
      data-invalid={invalid || undefined}
      className={composeRenderProps(className, (given) => `bk:group ${SLIDER} ${given ?? ""}`)}
    >
      {label === undefined ? <span /> : <Label>{label}</Label>}
      <SliderOutput className={OUTPUT}>
        {({ state }) => (said === undefined ? state.getFormattedValue() : said((value) => state.getFormattedValue(value)))}
      </SliderOutput>
      <SliderTrack className={TRACK}>
        {({ state }) => (
          <>
            <div className={BAR}>
              <SliderFill offset={fillOffset} className={FILL} />
            </div>
            {state.values.map((_, i) => (
              <SliderThumb key={i} index={i} aria-label={thumbLabels?.[i]} className={THUMB} />
            ))}
          </>
        )}
      </SliderTrack>
      {watch}
    </AriaSlider>
  );
}

// The classes of the slider and its parts.
// The label and the numbers share a row above the track, which spans the
// slider.
// The numbers, the fill and the thumbs take the colour of danger where the
// slider is invalid, as where it shows a number that it lacks.
const SLIDER =
  "bk:grid bk:grid-cols-[1fr_auto] bk:items-center bk:gap-x-2 bk:gap-y-1 bk:font-sans bk:data-disabled:opacity-50";
const OUTPUT = "bk:text-sm bk:text-muted-foreground bk:tabular-nums bk:group-data-invalid:text-destructive-text";
const TRACK = "bk:relative bk:col-span-2 bk:flex bk:h-5 bk:items-center bk:mx-2";
const BAR = "bk:relative bk:h-1.5 bk:w-full bk:rounded-full bk:bg-border";
const FILL = "bk:rounded-full bk:bg-primary bk:group-data-invalid:bg-destructive";
const THUMB =
  "bk:top-1/2 bk:box-border bk:size-4 bk:cursor-grab bk:rounded-full bk:border-2 bk:border-primary bk:bg-popover bk:shadow-sm bk:outline-offset-2 bk:outline-ring bk:data-dragging:cursor-grabbing bk:data-focus-visible:outline-2 bk:data-disabled:cursor-default bk:group-data-invalid:border-destructive";
