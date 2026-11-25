// The parts of a colour picker of blockr.ui, React Aria's, drawn with the
// theme variables: the area, ColorArea (https://react-aria.adobe.com/ColorArea),
// which changes two channels of a colour, such as its saturation and its
// brightness, the slider, ColorSlider
// (https://react-aria.adobe.com/ColorSlider), which changes one, such as its
// hue, the wheel, ColorWheel (https://react-aria.adobe.com/ColorWheel), which
// changes its hue around a ring, and the thumb, ColorThumb, which the user
// drags in each of them.
// They are drawn after the ColorArea, the ColorSlider, the ColorWheel and the
// ColorThumb of React Aria's Tailwind starter kit
// (https://github.com/adobe/react-spectrum/tree/react-aria-components%401.22.1/starters/tailwind/src),
// and take the props of React Aria's parts, and the slider the label that the
// starter kit adds.
// The area is a square of at most 14rem, the slider a track of 14rem, with
// its label and the value of its channel above it, and the wheel a ring of
// 200 pixels across.
// The track of a slider draws the colours of its channel over a
// checkerboard, which shows through the colours of a channel of alpha.
//
// Given `bound`, each shows and changes the colour that its `bound` gives, as
// blockr.ui's other inputs do: a string of hex (./colors.ts).
// It shows the colour that the user drags it to, and submits that colour as the
// user lets go, through React Aria's onChangeEnd, which a step by an arrow key
// ends too.
// It says below itself why its last submit was refused, the `error` of its
// `bound`.
// Inside a bound ColorPicker, the user letting go of a part is a change that
// the picker submits.

import { useContext } from "react";
import type { ReactElement } from "react";
import {
  ColorArea as AriaColorArea,
  ColorSlider as AriaColorSlider,
  ColorThumb as AriaColorThumb,
  ColorWheel as AriaColorWheel,
  ColorWheelTrack,
  composeRenderProps,
  Label,
  SliderOutput,
  SliderTrack,
} from "react-aria-components";
import type {
  Color,
  ColorAreaProps as AriaColorAreaProps,
  ColorSliderProps as AriaColorSliderProps,
  ColorThumbProps,
  ColorWheelProps as AriaColorWheelProps,
} from "react-aria-components";
import { styles } from "../styles/index.ts";
import type { Bound } from "./bound.ts";
import { Ended, useDragged } from "./colors.ts";
import { Refusal } from "./inputs.tsx";

export function ColorThumb({ className, style, ...props }: ColorThumbProps): ReactElement {
  return (
    <AriaColorThumb
      {...props}
      className={composeRenderProps(className, (given) => `${THUMB} ${given ?? ""}`)}
      style={composeRenderProps(style, (given, { defaultStyle, isDisabled }) => ({
        ...defaultStyle,
        ...given,
        backgroundColor: isDisabled ? undefined : defaultStyle.backgroundColor,
        boxShadow: "0 0 0 1px black, inset 0 0 0 1px black",
      }))}
    />
  );
}

export interface ColorAreaProps extends AriaColorAreaProps {
  // The value that it shows and submits.
  readonly bound?: Bound;
}

export function ColorArea({ bound, ...props }: ColorAreaProps): ReactElement {
  return bound === undefined ? <Area {...props} /> : <BoundPart Part={Area} props={props} bound={bound} />;
}

export interface ColorSliderProps extends AriaColorSliderProps {
  // What the starter kit adds.
  readonly label?: string;
  // The value that it shows and submits.
  readonly bound?: Bound;
}

export function ColorSlider({ bound, ...props }: ColorSliderProps): ReactElement {
  return bound === undefined ? <Slider {...props} /> : <BoundPart Part={Slider} props={props} bound={bound} />;
}

export interface ColorWheelProps extends Omit<AriaColorWheelProps, "outerRadius" | "innerRadius"> {
  // The value that it shows and submits.
  readonly bound?: Bound;
}

export function ColorWheel({ bound, ...props }: ColorWheelProps): ReactElement {
  return bound === undefined ? <Wheel {...props} /> : <BoundPart Part={Wheel} props={props} bound={bound} />;
}

// What a part that the user drags takes, of what React Aria's take.
interface Dragged {
  readonly value?: string | Color;
  readonly onChange?: (color: Color) => void;
  readonly onChangeEnd?: (color: Color) => void;
}

// A part bound to the value, which shows the colour that the user drags it
// to, and submits it as the user lets go.
function BoundPart<P extends Dragged>({
  Part,
  props,
  bound,
}: {
  Part: (props: P) => ReactElement;
  props: P;
  bound: Bound;
}): ReactElement {
  const dragged = useDragged(bound);
  return (
    <>
      <Part
        {...props}
        value={dragged.shown}
        onChange={(color: Color) => {
          dragged.change(color);
          props.onChange?.(color);
        }}
        onChangeEnd={(color: Color) => {
          dragged.end(color);
          props.onChangeEnd?.(color);
        }}
      />
      <Refusal error={bound.error} />
    </>
  );
}

// Calls onChangeEnd, and tells a bound ColorPicker around the part that the
// user let go of it.
function useEnd(onChangeEnd: ((color: Color) => void) | undefined): (color: Color) => void {
  const ended = useContext(Ended);
  return (color) => {
    onChangeEnd?.(color);
    ended?.(color);
  };
}

function Area({ className, style, onChangeEnd, ...props }: AriaColorAreaProps): ReactElement {
  return (
    <AriaColorArea
      {...props}
      onChangeEnd={useEnd(onChangeEnd)}
      className={composeRenderProps(className, (given) => `${AREA} ${given ?? ""}`)}
      style={composeRenderProps(style, (given, { defaultStyle, isDisabled }) => ({
        ...defaultStyle,
        ...given,
        background: isDisabled ? undefined : defaultStyle.background,
      }))}
    >
      <ColorThumb />
    </AriaColorArea>
  );
}

function Slider({ label, className, onChangeEnd, ...props }: Omit<ColorSliderProps, "bound">): ReactElement {
  return (
    <AriaColorSlider
      {...props}
      onChangeEnd={useEnd(onChangeEnd)}
      className={composeRenderProps(className, (given) => `${SLIDER} ${given ?? ""}`)}
    >
      <Label className={styles.label}>{label}</Label>
      <SliderOutput className={OUTPUT} />
      <SliderTrack
        className={({ orientation, isDisabled }) =>
          `${TRACK} ${orientation === "vertical" ? "bk:h-50 bk:w-6" : "bk:h-6 bk:w-full"} ${isDisabled ? DISABLED : ""}`
        }
        style={({ defaultStyle, isDisabled }) => ({
          ...defaultStyle,
          background: isDisabled ? undefined : `${String(defaultStyle.background)}, ${CHECKERBOARD}`,
        })}
      >
        <ColorThumb />
      </SliderTrack>
    </AriaColorSlider>
  );
}

function Wheel({ onChangeEnd, ...props }: Omit<ColorWheelProps, "bound">): ReactElement {
  return (
    <AriaColorWheel {...props} onChangeEnd={useEnd(onChangeEnd)} outerRadius={100} innerRadius={74}>
      <ColorWheelTrack
        className={WHEEL_TRACK}
        style={({ defaultStyle, isDisabled }) => ({
          ...defaultStyle,
          background: isDisabled ? undefined : `${String(defaultStyle.background)}, ${CHECKERBOARD}`,
        })}
      />
      <ColorThumb />
    </AriaColorWheel>
  );
}

// The classes of the parts.
// The thumb grows while it shows the focus of the keyboard, and draws in the
// colour of the text while the user drags it, where it shows no colour.
// A disabled part draws in the colour of an outline, `border`, through the
// attributes that React Aria sets on it.
// The thumb's white ring and its black rings, in its style, stay black and
// white in both modes, so that they stand out on any colour.
const THUMB =
  "bk:box-border bk:size-4.5 bk:top-[50%] bk:left-[50%] bk:rounded-full bk:border-2 bk:border-white bk:data-focus-visible:size-8 bk:data-dragging:bg-foreground bk:forced-colors:data-dragging:bg-[ButtonBorder] bk:data-disabled:border-border bk:data-disabled:bg-border bk:forced-colors:data-disabled:border-[GrayText] bk:forced-colors:data-disabled:bg-[GrayText]";
const AREA =
  "bk:aspect-square bk:w-full bk:max-w-56 bk:rounded-lg bk:bg-border bk:forced-colors:bg-[GrayText]";
const SLIDER =
  "bk:grid-cols-[1fr_auto] bk:flex-col bk:items-center bk:gap-2 bk:font-sans bk:data-[orientation=horizontal]:grid bk:data-[orientation=horizontal]:w-56 bk:data-[orientation=vertical]:flex";
const OUTPUT = "bk:text-sm bk:font-medium bk:text-muted-foreground bk:data-[orientation=vertical]:hidden";
const TRACK = "bk:group bk:col-span-2 bk:rounded-md";
const DISABLED = "bk:bg-border bk:forced-colors:bg-[GrayText]";
const WHEEL_TRACK = "bk:data-disabled:bg-border bk:forced-colors:data-disabled:bg-[GrayText]";
const CHECKERBOARD = "repeating-conic-gradient(#ccc 0% 25%, white 0% 50%) 50% / 16px 16px";
