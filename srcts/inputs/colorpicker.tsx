// The colour picker of blockr.ui: React Aria's ColorPicker
// (https://react-aria.adobe.com/ColorPicker), drawn with the theme variables, a button that
// shows the colour in a swatch and opens a popover, in which the user picks
// the colour with the parts that the picker holds.
// It is drawn after the ColorPicker of React Aria's Tailwind starter kit
// (https://github.com/adobe/react-spectrum/blob/react-aria-components%401.22.1/starters/tailwind/src/ColorPicker.tsx),
// and takes what that takes: the props of React Aria's ColorPicker, the label
// that the starter kit shows beside the swatch, and the parts as its
// children.
// Without children, it holds the starter kit's: an area of the saturation and
// the brightness, a slider of the hue and a field of the hex.
// It takes `isDisabled` too, which disables its button, as for a value that
// a checkbox beside it turns off.
// React Aria draws the popover in a portal at the end of the page's body, as
// every overlay of blockr.ui (../overlays/index.ts).
//
// Given `bound`, it shows and picks the colour that its `bound` gives, as
// blockr.ui's other inputs do: a string of hex (./colors.ts).
// It shows the colour that the user changes it to, and submits that colour as
// the user ends a change in one of its parts: lets go of the area, a slider or
// the wheel, picks a swatch, or leaves the field or presses Enter there.
// Each of blockr.ui's parts tells it so, so that it submits the colour then,
// whichever parts it holds.
// It says below itself why its last submit was refused, the `error` of its
// `bound`.

import type { ReactElement, ReactNode } from "react";
import { ColorPicker as AriaColorPicker, Button } from "react-aria-components";
import type { ColorPickerProps as AriaColorPickerProps } from "react-aria-components";
import { Dialog, DialogTrigger, Popover } from "../overlays/index.ts";
import type { Bound } from "./bound.ts";
import { ColorField } from "./colorfield.tsx";
import { ColorArea, ColorSlider } from "./colorparts.tsx";
import { Ended, useDragged } from "./colors.ts";
import { ColorSwatch } from "./colorswatches.tsx";
import { Refusal } from "./inputs.tsx";

export interface ColorPickerProps extends Omit<AriaColorPickerProps, "children"> {
  // What the starter kit adds.
  readonly label?: string;
  readonly children?: ReactNode;
  // Whether its button is disabled, so that the user opens no popover.
  readonly isDisabled?: boolean;
  // The value that it shows and submits.
  readonly bound?: Bound;
}

export function ColorPicker({ bound, ...props }: ColorPickerProps): ReactElement {
  return bound === undefined ? <Drawn {...props} /> : <BoundPicker {...props} bound={bound} />;
}

function BoundPicker({ bound, onChange, ...props }: Omit<ColorPickerProps, "bound"> & { readonly bound: Bound }): ReactElement {
  const dragged = useDragged(bound);
  return (
    <>
      <Ended value={dragged.end}>
        <Drawn
          {...props}
          value={dragged.shown}
          onChange={(color) => {
            dragged.change(color);
            onChange?.(color);
          }}
        />
      </Ended>
      <Refusal error={bound.error} />
    </>
  );
}

// The button, with the swatch and the label, and the popover of the parts.
function Drawn({ label, children, isDisabled, ...props }: Omit<ColorPickerProps, "bound">): ReactElement {
  return (
    <AriaColorPicker {...props}>
      <DialogTrigger>
        <Button isDisabled={isDisabled} className={BUTTON}>
          <ColorSwatch />
          {label === undefined ? null : <span>{label}</span>}
        </Button>
        <Popover placement="bottom start">
          <Dialog className="bk:flex bk:flex-col bk:gap-2">
            {children ?? (
              <>
                <ColorArea colorSpace="hsb" xChannel="saturation" yChannel="brightness" />
                <ColorSlider colorSpace="hsb" channel="hue" />
                <ColorField label="Hex" />
              </>
            )}
          </Dialog>
        </Popover>
      </DialogTrigger>
    </AriaColorPicker>
  );
}

// The button draws no box of its own, and the outline of the focus of the
// keyboard around the swatch and the label.
const BUTTON =
  "bk:flex bk:cursor-pointer bk:items-center bk:gap-2 bk:rounded-sm bk:border-0 bk:bg-transparent bk:p-0 bk:font-sans bk:text-sm bk:text-inherit bk:outline-none bk:data-focus-visible:outline-2 bk:data-focus-visible:outline-offset-2 bk:data-focus-visible:outline-ring bk:data-disabled:cursor-default bk:data-disabled:opacity-50";
