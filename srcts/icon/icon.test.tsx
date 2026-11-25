// What Icon draws for an icon of Lucide's, and the SVG markup that iconMarkup() writes for one.

import { BrainCircuit, CircuitBoard, Ellipsis, Pin, TriangleAlert } from "lucide";
import type { IconNode } from "lucide";
import { act } from "react";
import type { ReactElement } from "react";
import { createRoot } from "react-dom/client";
import { afterEach, expect, test, vi } from "vitest";
import { Icon, iconMarkup } from "./index.ts";

// React warns where a test draws outside act() unless this is set.
(globalThis as { IS_REACT_ACT_ENVIRONMENT?: boolean }).IS_REACT_ACT_ENVIRONMENT = true;

const unmounts: (() => void)[] = [];

afterEach(() => {
  for (const unmount of unmounts.splice(0)) unmount();
});

// Draws a React element into an element of the page, in act(), and gives what it drew.
function draw(node: ReactElement): Element {
  const el = document.createElement("div");
  document.body.append(el);
  const root = createRoot(el);
  act(() => {
    root.render(node);
  });
  unmounts.push(() => {
    act(() => {
      root.unmount();
    });
    el.remove();
  });
  if (el.firstElementChild === null) throw new Error("The element drew nothing.");
  return el.firstElementChild;
}

// The element that markup makes, put into the page.
function parsed(markup: string): Element {
  const el = document.createElement("div");
  el.innerHTML = markup;
  if (el.firstElementChild === null) throw new Error("The markup makes no element.");
  return el.firstElementChild;
}

function attributes(el: Element): Record<string, string> {
  return Object.fromEntries([...el.attributes].map(({ name, value }) => [name, value]));
}

test("draws an icon's shape at 14 pixels on Lucide's grid, in the colour of the text around it, with round caps and joins and a stroke of 1.875", () => {
  const error = vi.spyOn(console, "error");
  const svg = draw(<Icon icon={Pin} />);

  expect(svg.namespaceURI).toBe("http://www.w3.org/2000/svg");
  expect(svg.localName).toBe("svg");
  expect(attributes(svg)).toEqual({
    width: "14",
    height: "14",
    viewBox: "0 0 24 24",
    fill: "none",
    stroke: "currentColor",
    "stroke-width": "1.875",
    "stroke-linecap": "round",
    "stroke-linejoin": "round",
    "aria-hidden": "true",
  });
  expect([...svg.children].map((child) => [child.localName, attributes(child)])).toEqual(Pin);

  // An icon with circles, rects and paths, and one whose shape fills a part, draw each element as Lucide gives it.
  for (const icon of [CircuitBoard, BrainCircuit]) {
    expect([...draw(<Icon icon={icon} />).children].map((child) => [child.localName, attributes(child)])).toEqual(icon);
  }
  // React warns of no attribute and no element without a key.
  expect(error).not.toHaveBeenCalled();
  error.mockRestore();
});

test("draws an icon at the size given, on the same grid", () => {
  const svg = draw(<Icon icon={Pin} size={20} />);

  expect(svg.getAttribute("width")).toBe("20");
  expect(svg.getAttribute("height")).toBe("20");
  expect(svg.getAttribute("viewBox")).toBe("0 0 24 24");
  expect(svg.getAttribute("stroke-width")).toBe("1.875");
});

test("hides an icon from screen readers, unless it is given a label, which they say for it", () => {
  const hidden = draw(<Icon icon={Pin} />);
  expect(hidden.getAttribute("aria-hidden")).toBe("true");
  expect(hidden.hasAttribute("role")).toBe(false);
  expect(hidden.hasAttribute("aria-label")).toBe(false);

  const labelled = draw(<Icon icon={Pin} label="Pinned" />);
  expect(labelled.getAttribute("role")).toBe("img");
  expect(labelled.getAttribute("aria-label")).toBe("Pinned");
  expect(labelled.hasAttribute("aria-hidden")).toBe(false);
});

test("iconMarkup() writes an icon as SVG markup, at 14 pixels unless given another size", () => {
  expect(iconMarkup(Ellipsis)).toBe(
    '<svg width="14" height="14" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="1.875" stroke-linecap="round" stroke-linejoin="round" aria-hidden="true">' +
      '<circle cx="12" cy="12" r="1"/><circle cx="19" cy="12" r="1"/><circle cx="5" cy="12" r="1"/></svg>',
  );
  expect(iconMarkup(Ellipsis, 20)).toMatch(/^<svg width="20" height="20" viewBox="0 0 24 24" /);
});

test("iconMarkup() writes the same drawing as Icon: put into the page, its markup is the element that Icon draws", () => {
  const icons: [IconNode, number | undefined][] = [
    [Pin, undefined],
    [TriangleAlert, 20],
    [CircuitBoard, 16],
    [BrainCircuit, 24],
  ];
  for (const [icon, size] of icons) {
    const markup = size === undefined ? iconMarkup(icon) : iconMarkup(icon, size);
    const svg = draw(size === undefined ? <Icon icon={icon} /> : <Icon icon={icon} size={size} />);
    expect(parsed(markup).outerHTML).toBe(svg.outerHTML);
    expect(parsed(markup).isEqualNode(svg)).toBe(true);
  }
});

test("iconMarkup() holds each value of an attribute as it is, and leaves out an attribute without a value, as Icon does", () => {
  const note = `a "quoted" <b> & 'c'`;
  const icon: IconNode = [["path", { d: "M1 1h22", "data-note": note, fill: undefined }]];
  const markup = iconMarkup(icon);

  expect(markup).toContain('data-note="a &#34;quoted&#34; &#60;b&#62; &#38; &#39;c&#39;"');
  const path = parsed(markup).firstElementChild;
  expect(path?.getAttribute("data-note")).toBe(note);
  expect(path?.hasAttribute("fill")).toBe(false);
  expect(path?.isEqualNode(draw(<Icon icon={icon} />).firstElementChild ?? null)).toBe(true);
});
