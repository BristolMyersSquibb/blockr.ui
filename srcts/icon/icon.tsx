// Draws an icon of Lucide's, which a module takes from @blockr/ui/icons
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.7.3#the-icons).
// Each icon there is its shape as data: a list of SVG elements, each a tag with its attributes,
// on Lucide's grid of 24 pixels.
// The component Icon draws the shape with React.
// The function iconMarkup() writes the same drawing as SVG markup, which is what a board action's icon is
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.16#a-board-action).
//
// Both draw the shape in the colour of the text around it, with Lucide's round caps and joins.
// The shape's stroke is 1.875 pixels wide on Lucide's grid.
// At 14 pixels, that draws 1.09 pixels wide, as a stroke of 1.25 on a grid of 16 does,
// the stroke of blockr.ui's icons before Lucide's.
// Lucide's own stroke of 2 would draw 1.17 pixels wide.

import { createElement } from "react";
import type { ReactElement } from "react";
import type { IconNode } from "lucide";

// The width and height of an icon in pixels, unless a caller gives another size.
const SIZE = 14;

// The width of an icon's stroke on Lucide's grid.
const STROKE_WIDTH = 1.875;

// Lucide's grid.
const VIEW_BOX = "0 0 24 24";

// The attributes of an SVG element, by their names in SVG, as an icon's shape gives them.
type Attributes = IconNode[number][1];

interface IconProps {
  // The icon's shape, from @blockr/ui/icons.
  icon: IconNode;
  // Its width and height in pixels.
  size?: number;
  // What a screen reader says for the icon.
  // Without one, a screen reader skips the icon,
  // since what holds an icon names what it does, as a button's label does.
  label?: string;
}

export function Icon({ icon, size = SIZE, label }: IconProps): ReactElement {
  return (
    <svg
      width={size}
      height={size}
      viewBox={VIEW_BOX}
      fill="none"
      stroke="currentColor"
      strokeWidth={STROKE_WIDTH}
      strokeLinecap="round"
      strokeLinejoin="round"
      {...(label === undefined ? { "aria-hidden": true } : { role: "img", "aria-label": label })}
    >
      {icon.map(([tag, attributes], i) => createElement(tag, { key: i, ...attributes }))}
    </svg>
  );
}

// The drawing of Icon without a label, as SVG markup.
// It writes the attributes of the svg element in the order that Icon gives them to React,
// so that the markup reads as the element that Icon draws.
export function iconMarkup(icon: IconNode, size: number = SIZE): string {
  const svg = attributeList({
    width: size,
    height: size,
    viewBox: VIEW_BOX,
    fill: "none",
    stroke: "currentColor",
    "stroke-width": STROKE_WIDTH,
    "stroke-linecap": "round",
    "stroke-linejoin": "round",
    "aria-hidden": "true",
  });
  return `<svg${svg}>${icon.map(([tag, attributes]) => `<${tag}${attributeList(attributes)}/>`).join("")}</svg>`;
}

// The attributes of an element as markup, each after a space.
// It leaves out an attribute without a value, as React does.
// A value is escaped, so that the markup holds it as it is.
function attributeList(attributes: Attributes): string {
  return Object.entries(attributes)
    .flatMap(([name, value]) => (value === undefined ? [] : [` ${name}="${escaped(String(value))}"`]))
    .join("");
}

// A value as an attribute of markup holds it,
// with each character that could end the attribute or start markup as a character reference.
function escaped(value: string): string {
  return value.replace(/[&<>"']/g, (char) => `&#${String(char.charCodeAt(0))};`);
}
