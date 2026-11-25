// The colours of blockr.ui, each on the stub of Bound, and the swatch and the
// picker drawn for their caller.

import { act } from "react";
import type { ReactElement } from "react";
import { createRoot } from "react-dom/client";
import { describe, expect, test } from "vitest";
import { parseColor } from "react-aria-components";
import { find, openDialog, press, step, stubBound, texts, type, useStubBound } from "../stubs/index.ts";
import {
  ColorArea,
  ColorField,
  ColorPicker,
  ColorSlider,
  ColorSwatch,
  ColorSwatchPicker,
  ColorSwatchPickerItem,
  ColorWheel,
} from "./index.ts";
import type { Bound } from "./index.ts";

// Draws the component on the page, until the test ends.
function draw(node: ReactElement): HTMLElement {
  const el = document.createElement("div");
  document.body.append(el);
  const root = createRoot(el);
  act(() => {
    root.render(node);
  });
  return el;
}

// A stub of Bound that holds the colour given, and the UI given drawn on it.
function bound(fill: string, ui: (bound: Bound) => ReactElement) {
  const stub = stubBound(fill);
  function Ui(): ReactElement {
    return ui(useStubBound(stub));
  }
  return { stub, el: draw(<Ui />) };
}

// The hex of a colour after a step of one of its channels in the colour space
// of HSB, as a part writes it.
// React Aria puts the channel on its step, which is 1 for each channel of HSB.
function stepped(hex: string, channel: "hue" | "brightness", by: number): string {
  const color = parseColor(hex).toFormat("hsb");
  return color.withChannelValue(channel, Math.round(color.getChannelValue(channel) + by)).toString("hex");
}

// The options of a swatch picker, by the names of their colours, which the
// swatch inside each gives, and the one picked.
function swatches(el: ParentNode): { names: string[]; picked: string[] } {
  const options = [...el.querySelectorAll('[role="option"]')];
  const name = (option: Element): string => option.querySelector('[role="img"]')?.getAttribute("aria-label") ?? "";
  return {
    names: options.map(name),
    picked: options.filter((option) => option.getAttribute("aria-selected") === "true").map(name),
  };
}

// The option of a swatch picker whose swatch shows the colour given.
function swatchOption(el: ParentNode, hex: string): HTMLElement {
  const found = [...el.querySelectorAll<HTMLElement>('[role="option"]')].find(
    (option) => option.querySelector('[role="img"]')?.getAttribute("aria-label") === named(hex),
  );
  if (found === undefined) throw new Error(`Found no swatch of ${hex}.`);
  return found;
}

// The name that React Aria gives a colour, by which a swatch is labelled.
const named = (hex: string): string => parseColor(hex).getColorName("en-US");

describe("a swatch", () => {
  test("is an image named by its colour, which it draws over a checkerboard", () => {
    const el = draw(<ColorSwatch color="#66c2a5" />);
    const swatch = find(el, '[role="img"]');

    expect(swatch.getAttribute("aria-label")).toBe("light pale green cyan");
    expect(swatch.getAttribute("aria-roledescription")).toBe("color swatch");
    expect(swatch.style.background).toContain("repeating-conic-gradient");
    expect(swatch.className).toContain("bk:size-8");
  });

  test("puts its caller's classes after its own, as blockr.ui's other components do", () => {
    const el = draw(<ColorSwatch color="#66c2a5" className="bk:size-3! bk:rounded-full!" />);
    const swatch = find(el, '[role="img"]');

    expect(swatch.className).toMatch(/bk:size-8 .*bk:size-3! bk:rounded-full!$/);
    expect(swatch.className).toContain("bk:border-border");
  });
});

describe("a bound colour field", () => {
  test("shows the colour of its bound as hex, and submits what the user types as the user presses Enter or leaves it", async () => {
    const { stub, el } = bound("#66c2a5", (b) => <ColorField bound={b} label="Fill" />);
    const input = find(el, "input", HTMLInputElement);
    expect(input.value).toBe("#66C2A5");

    await step(() => {
      type(input, "#8da0cb");
    });
    expect(stub.committed).toEqual([]);
    await step(() => {
      press(input, "Enter");
    });
    expect(stub.committed).toEqual(["#8DA0CB"]);
    expect(input.value).toBe("#8DA0CB");

    await step(() => {
      type(input, "e78ac3");
    });
    await step(() => {
      input.blur();
    });
    expect(stub.committed).toEqual(["#8DA0CB", "#E78AC3"]);
  });

  test("keeps the alpha of its bound's colour, which its hex does not show", async () => {
    const { stub, el } = bound("#66c2a580", (b) => <ColorField bound={b} label="Fill" />);
    const input = find(el, "input", HTMLInputElement);
    expect(input.value).toBe("#66C2A5");

    await step(() => {
      type(input, "#8da0cb");
      press(input, "Enter");
    });

    expect(stub.committed).toEqual(["#8DA0CB80"]);
  });

  test("keeps what the user types while the value of its bound changes, until the user leaves it", async () => {
    const { stub, el } = bound("#66c2a5", (b) => <ColorField bound={b} label="Fill" />);
    const input = find(el, "input", HTMLInputElement);

    await step(() => {
      type(input, "#8da0cb");
    });
    // Another user sets another colour while the user types.
    await step(() => {
      stub.set("#FC8D62");
    });
    expect(input.value).toBe("#8da0cb");

    await step(() => {
      input.blur();
    });
    expect(stub.committed).toEqual(["#8DA0CB"]);
  });

  test("submits nothing where the user clears it or types no colour, and shows the colour of its bound again", async () => {
    const { stub, el } = bound("#66c2a5", (b) => <ColorField bound={b} label="Fill" />);
    const input = find(el, "input", HTMLInputElement);

    await step(() => {
      type(input, "");
      press(input, "Enter");
    });
    expect(input.value).toBe("#66C2A5");
    await step(() => {
      type(input, "#zz");
    });
    await step(() => {
      input.blur();
    });

    expect(input.value).toBe("#66C2A5");
    expect(stub.committed).toEqual([]);
  });

  test("shows the colour of its bound again where its submit was refused, and says why below itself", async () => {
    const { stub, el } = bound("#66c2a5", (b) => <ColorField bound={b} label="Fill" />);
    const input = find(el, "input", HTMLInputElement);
    stub.refuse("Pick a colour of the palette.");

    await act(async () => {
      type(input, "#8da0cb");
      press(input, "Enter");
      await Promise.resolve();
    });

    expect(stub.committed).toEqual(["#8DA0CB"]);
    expect(input.value).toBe("#66C2A5");
    expect(texts(el, '[role="alert"]')).toEqual(["Pick a colour of the palette."]);
  });
});

describe("the bound parts of a picker", () => {
  test("submit the colour that a step of the user gives as the user ends it, an arrow key on the slider among them", async () => {
    const hue = stepped("#66c2a5", "hue", 1);
    const { stub, el } = bound("#66c2a5", (b) => <ColorSlider bound={b} colorSpace="hsb" channel="hue" label="Hue" />);
    const slider = find(el, 'input[type="range"]', HTMLInputElement);
    expect(el.querySelector("output")?.textContent).toBe("161.09°");

    await step(() => {
      slider.focus();
      press(slider, "ArrowRight");
    });

    expect(stub.committed).toEqual([hue]);
    expect(el.querySelector("output")?.textContent).toBe("162°");
  });

  test("submit the colour of a step on the area and on the wheel at once, each as hex", async () => {
    const brighter = stepped("#66c2a5", "brightness", 1);
    const turned = stepped("#66c2a5", "hue", 1);
    const { stub, el } = bound("#66c2a5", (b) => {
      const fill = b;
      return (
        <>
          <ColorArea bound={fill} colorSpace="hsb" xChannel="saturation" yChannel="brightness" />
          <ColorWheel bound={fill} />
        </>
      );
    });
    const [, brightness, wheel] = [...el.querySelectorAll<HTMLInputElement>('input[type="range"]')];

    await step(() => {
      brightness?.focus();
      if (brightness !== undefined) press(brightness, "ArrowUp");
    });
    expect(stub.committed).toEqual([brighter]);

    await step(() => {
      stub.set("#66c2a5");
    });
    await step(() => {
      wheel?.focus();
      if (wheel !== undefined) press(wheel, "ArrowRight");
    });
    expect(stub.committed).toEqual([brighter, turned]);
  });

  test("show the colour of their bound again where its submit was refused, and say why below themselves", async () => {
    const { stub, el } = bound("#66c2a5", (b) => <ColorSlider bound={b} colorSpace="hsb" channel="hue" label="Hue" />);
    const slider = find(el, 'input[type="range"]', HTMLInputElement);
    stub.refuse("Keep the hue of the brand.");

    await act(async () => {
      slider.focus();
      press(slider, "ArrowRight");
      await Promise.resolve();
    });

    expect(stub.committed).toEqual([stepped("#66c2a5", "hue", 1)]);
    expect(el.querySelector("output")?.textContent).toBe("161.09°");
    expect(texts(el, '[role="alert"]')).toEqual(["Keep the hue of the brand."]);
  });
});

describe("a bound swatch picker", () => {
  const palette = ["#66c2a5", "#fc8d62", "#8da0cb"] as const;

  test("marks the colour of its bound, and submits the colour that the user picks at once", async () => {
    const { stub, el } = bound("#66c2a5", (b) => (
      <ColorSwatchPicker bound={b} aria-label="Palette">
        {palette.map((hex) => (
          <ColorSwatchPickerItem key={hex} color={hex} />
        ))}
      </ColorSwatchPicker>
    ));
    expect(swatches(el)).toEqual({ names: palette.map(named), picked: [named("#66c2a5")] });

    await step(() => {
      swatchOption(el, "#fc8d62").click();
    });

    expect(stub.committed).toEqual(["#FC8D62"]);
    expect(swatches(el).picked).toEqual([named("#fc8d62")]);
  });
});

describe("a colour picker", () => {
  test("shows its colour in a swatch on its button, which opens the starter kit's parts in a popover", async () => {
    const el = draw(<ColorPicker label="Fill" defaultValue="#66c2a5" />);
    const button = find(el, "button");
    expect(find(button, '[role="img"]').getAttribute("aria-label")).toBe("light pale green cyan");
    expect(button.textContent).toBe("Fill");

    await step(() => {
      button.click();
    });

    const dialog = openDialog();
    // The area holds two sliders, of the saturation and of the brightness, and
    // the slider of the hue takes the channel's name as its label.
    expect(dialog.querySelectorAll('input[type="range"]')).toHaveLength(3);
    expect(texts(dialog, "label")).toEqual(["Hue", "Hex"]);
    expect(find(dialog, 'input:not([type="range"])', HTMLInputElement).value).toBe("#66C2A5");
  });

  test("opens no popover while it is disabled", async () => {
    const el = draw(<ColorPicker label="Fill" defaultValue="#66c2a5" isDisabled />);
    const button = find(el, "button", HTMLButtonElement);
    expect(button.disabled).toBe(true);

    await step(() => {
      button.click();
    });

    expect(document.querySelector('[role="dialog"]')).toBeNull();
  });

  test("bound, submits the colour as the user ends a change in one of its parts: a step of the hue, a swatch picked or the hex typed", async () => {
    const hue = stepped("#66c2a5", "hue", 1);
    const { stub, el } = bound("#66c2a5", (b) => (
      <ColorPicker bound={b} label="Fill">
        <ColorSlider colorSpace="hsb" channel="hue" label="Hue" />
        <ColorField label="Hex" />
        <ColorSwatchPicker aria-label="Palette">
          <ColorSwatchPickerItem color="#fc8d62" />
        </ColorSwatchPicker>
      </ColorPicker>
    ));
    await step(() => {
      find(el, "button").click();
    });
    const dialog = openDialog();

    const slider = find(dialog, 'input[type="range"]', HTMLInputElement);
    await step(() => {
      slider.focus();
      press(slider, "ArrowRight");
    });
    expect(stub.committed).toEqual([hue]);

    await step(() => {
      find(dialog, '[role="option"]').click();
    });
    expect(stub.committed).toEqual([hue, "#FC8D62"]);
    expect(find(el, 'button [role="img"]').getAttribute("aria-label")).toBe(named("#fc8d62"));

    const field = find(dialog, 'input:not([type="range"])', HTMLInputElement);
    await step(() => {
      type(field, "#8da0cb");
      press(field, "Enter");
    });
    expect(stub.committed).toEqual([hue, "#FC8D62", "#8DA0CB"]);
  });

  test("bound, shows the colour of its bound again where its submit was refused, and says why beside its button", async () => {
    const { stub, el } = bound("#66c2a5", (b) => (
      <ColorPicker bound={b} label="Fill">
        <ColorSwatchPicker aria-label="Palette">
          <ColorSwatchPickerItem color="#fc8d62" />
        </ColorSwatchPicker>
      </ColorPicker>
    ));
    await step(() => {
      find(el, "button").click();
    });
    stub.refuse("Pick a colour of the palette.");

    await act(async () => {
      find(openDialog(), '[role="option"]').click();
      await Promise.resolve();
    });

    expect(stub.committed).toEqual(["#FC8D62"]);
    expect(find(el, 'button [role="img"]').getAttribute("aria-label")).toBe(named("#66c2a5"));
    expect(texts(el, '[role="alert"]')).toEqual(["Pick a colour of the palette."]);
  });
});
