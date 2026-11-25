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
export {
  Icon,
  iconMarkup
};
