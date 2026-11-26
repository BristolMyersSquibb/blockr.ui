// The inputs of blockr.ui, each on the stub of Bound: the inputs of a form,
// a number field's text as the user types, the inputs that the UI of a board
// option may draw, and the button.
// How core's binding submits what an input gives it, such as the pause before
// it submits what the user typed, is core's to test, and so is core's
// useRequest().

import { act } from "react";
import type { ReactElement } from "react";
import { describe, expect, test } from "vitest";
import {
  chipLabels,
  draw,
  fieldError,
  labelled,
  listed,
  notAvailable,
  numberSaid,
  optionFromLabel,
  optionLabels,
  press,
  removeChip,
  step,
  stubBound,
  texts,
  toggle,
  type,
  useStubBound,
} from "../stubs/index.ts";
import type { StubBound } from "../stubs/index.ts";
import { Button, Checkbox, ComboBox, ComboBoxItem, FormLayout, NumberField, TextField } from "./index.ts";
import type { Json } from "./index.ts";

// A stub of Bound for each input of the form below.
interface Stubs {
  readonly n: StubBound;
  readonly direction: StubBound;
  readonly label: StubBound;
  readonly by: StubBound;
  readonly keep: StubBound;
}

const stubs = (values: Partial<Record<keyof Stubs, Json>> = {}): Stubs => ({
  n: stubBound(values.n ?? 6),
  direction: stubBound(values.direction ?? "head"),
  label: stubBound(values.label ?? "x"),
  by: stubBound(values.by),
  keep: stubBound(values.keep ?? false),
});

// A form of a block's UI, as a module draws one, with its inputs in a
// FormLayout.
function Form({ stubs }: { stubs: Stubs }): ReactElement {
  return (
    <FormLayout>
      <NumberField bound={useStubBound(stubs.n)} label="Number of rows" minValue={0} maxValue={150} step={1} />
      <ComboBox
        bound={useStubBound(stubs.direction)}
        label="Direction"
        placeholder="Choose an end"
        defaultItems={[
          { id: "head", name: "Head" },
          { id: "tail", name: "Tail" },
        ]}
      >
        {(item) => <ComboBoxItem id={item.id}>{item.name}</ComboBoxItem>}
      </ComboBox>
      <TextField bound={useStubBound(stubs.label)} label="Label" />
      <ComboBox bound={useStubBound(stubs.by)} selectionMode="multiple" label="By columns" defaultItems={ids(["a", "b", "c"])}>
        {(item) => <ComboBoxItem id={item.id}>{item.id}</ComboBoxItem>}
      </ComboBox>
      <Checkbox bound={useStubBound(stubs.keep)}>Keep</Checkbox>
    </FormLayout>
  );
}

// Items of the ids given, which read as their ids.
const ids = (values: readonly string[]) => values.map((id) => ({ id }));

function form(values: Partial<Record<keyof Stubs, Json>> = {}) {
  const bound = stubs(values);
  const drawn = draw(<Form stubs={bound} />);
  const inputs = {
    n: labelled(drawn.el, "Number of rows"),
    direction: labelled(drawn.el, "Direction"),
    label: labelled(drawn.el, "Label"),
    by: labelled(drawn.el, "By columns"),
    keep: labelled(drawn.el, "Keep"),
  };
  return { stubs: bound, ...drawn, inputs };
}

describe("the inputs of a form", () => {
  test("lay out one below the other, show the values of their bounds, and submit nothing until the user acts", () => {
    const { el, stubs, inputs } = form();

    expect(el.firstElementChild?.className).toContain("bk:grid");
    expect([inputs.n.value, inputs.direction.value, inputs.label.value, inputs.keep.checked]).toEqual(["6", "Head", "x", false]);
    const all = [stubs.n, stubs.direction, stubs.label, stubs.by, stubs.keep];
    expect(all.flatMap((stub) => [...stub.committed, ...stub.typed, ...stub.flushed])).toEqual([]);
  });

  test("take values they did not make in the elements they drew", () => {
    const { stubs, el, inputs } = form();

    act(() => {
      stubs.n.set(20);
      stubs.direction.set("tail");
    });

    expect(labelled(el, "Number of rows")).toBe(inputs.n);
    expect([inputs.n.value, inputs.direction.value]).toEqual(["20", "Tail"]);
  });

  test("hand each keystroke in a text field to its bound, and submit what the user typed at once on Enter and as the user leaves", () => {
    const { stubs, inputs } = form();

    act(() => {
      type(inputs.label, "yz");
    });
    expect([stubs.label.typed, stubs.label.flushed, inputs.label.value]).toEqual([["y", "yz"], [], "yz"]);
    act(() => {
      press(inputs.label, "Enter");
    });
    act(() => {
      type(inputs.label, "w");
    });
    act(() => {
      inputs.label.blur();
    });

    expect([stubs.label.flushed, stubs.label.committed, inputs.label.value]).toEqual([["yz", "w"], [], "w"]);
  });

  test("submit a number that the user types once the user presses Enter, as the end of an edit, and hand its bound nothing while the user types", () => {
    const { stubs, inputs } = form();

    act(() => {
      type(inputs.n, "20");
    });
    expect([stubs.n.typed, stubs.n.flushed, stubs.n.committed, inputs.n.value]).toEqual([[], [], [], "20"]);
    act(() => {
      press(inputs.n, "Enter");
    });

    expect([stubs.n.typed, stubs.n.flushed, stubs.n.committed]).toEqual([[20], [20], []]);
    expect(inputs.n.value).toBe("20");
  });

  test("submit typing at once on Enter and as the user leaves, and nothing above the most", () => {
    const { stubs, inputs } = form();

    act(() => {
      type(inputs.n, "200");
    });
    expect(inputs.n.getAttribute("aria-invalid")).toBe("true");
    act(() => {
      inputs.n.blur();
    });
    expect(stubs.n.flushed).toEqual([]);

    act(() => {
      type(inputs.n, "10");
      press(inputs.n, "Enter");
    });
    expect(inputs.n.getAttribute("aria-invalid")).toBeNull();
    act(() => {
      type(inputs.label, "yz");
    });
    act(() => {
      inputs.label.blur();
    });
    expect([stubs.n.flushed, stubs.label.flushed]).toEqual([[10], ["yz"]]);
  });

  test("submit a pick at once, and show the value of their bound again where its submit was refused", async () => {
    const { stubs, el, inputs } = form();

    await step(() => {
      inputs.direction.click();
    });
    await step(() => {
      optionFromLabel(inputs.direction, "Tail").click();
    });
    expect(stubs.direction.committed).toEqual(["tail"]);

    stubs.keep.refuse("The block before keeps the rows.");
    await act(async () => {
      toggle(inputs.keep);
      await Promise.resolve();
    });
    expect([stubs.keep.committed, inputs.keep.checked]).toEqual([[true], false]);
    expect(texts(el, '[role="alert"]')).toEqual(["The block before keeps the rows."]);
  });

  test("submit the strings of several once, as the list closes, built from the value of its bound then", async () => {
    const { stubs, inputs } = form({ by: ["a"] });

    await step(() => {
      inputs.by.click();
    });
    // Another user adds a string while the list is open.
    act(() => {
      stubs.by.set(["c", "a"]);
    });
    await step(() => {
      optionFromLabel(inputs.by, "a").click();
    });
    await step(() => {
      optionFromLabel(inputs.by, "b").click();
    });
    expect(stubs.by.committed).toEqual([]);
    await step(() => {
      press(inputs.by, "Escape");
    });

    expect(stubs.by.committed).toEqual([["c", "b"]]);
    expect(chipLabels(inputs.by)).toEqual(["c", "b"]);
  });

  test("drop a number that the user typed but did not end, as the component unmounts", () => {
    const { stubs, unmount, inputs } = form();

    act(() => {
      type(inputs.n, "10");
    });
    unmount();

    expect([stubs.n.committed, stubs.n.typed, stubs.n.flushed]).toEqual([[], [], []]);
  });

  test("submit a number at once as the user steps it, by a button or an arrow key, and show the value of its bound again where its submit was refused", async () => {
    const { stubs, el, inputs } = form();
    const steps = [...el.querySelectorAll<HTMLButtonElement>('button[slot="increment"], button[slot="decrement"]')];
    expect(steps.map((button) => button.getAttribute("aria-label"))).toEqual(["Increase", "Decrease"]);

    await step(() => {
      steps[0]?.dispatchEvent(new PointerEvent("pointerdown", { bubbles: true, pointerId: 1, pointerType: "mouse", button: 0 }));
      steps[0]?.dispatchEvent(new PointerEvent("pointerup", { bubbles: true, pointerId: 1, pointerType: "mouse", button: 0 }));
      steps[0]?.click();
    });
    expect(stubs.n.committed).toEqual([7]);
    expect(inputs.n.value).toBe("7");

    await step(() => {
      inputs.n.focus();
      press(inputs.n, "ArrowDown");
    });
    expect(stubs.n.committed).toEqual([7, 6]);
    expect(inputs.n.value).toBe("6");

    stubs.n.refuse("At least 6 rows.");
    await step(() => {
      press(inputs.n, "ArrowDown");
    });
    expect(stubs.n.committed).toEqual([7, 6, 5]);
    expect(inputs.n.value).toBe("6");
    expect(texts(el, '[role="alert"]')).toEqual(["At least 6 rows."]);
  });
});

describe("a number field's text as the user types", () => {
  test("stays as the user typed it, hands its bound nothing until the user leaves, and takes the field's format then, which submits it", () => {
    const stub = stubBound(25);
    const { el } = draw(<Rows stub={stub} />);
    const input = labelled(el, "Preview rows");

    act(() => {
      input.focus();
      type(input, "1000");
    });
    expect([stub.flushed, stub.typed, input.value]).toEqual([[], [], "1000"]);

    act(() => {
      input.blur();
    });
    expect(input.value).toBe("1,000");
    expect(stub.flushed).toEqual([1000]);
  });

  test("stays as the user typed it as its bound gives another number, and submits it as the user leaves, as the later edit", () => {
    const stub = stubBound(25);
    const { el } = draw(<Rows stub={stub} />);
    const input = labelled(el, "Preview rows");

    act(() => {
      input.focus();
      type(input, "40");
    });
    // Another user sets another number while the user is still typing.
    act(() => {
      stub.set(30);
    });
    expect(input.value).toBe("40");
    expect(stub.flushed).toEqual([]);

    act(() => {
      input.blur();
    });
    expect(stub.flushed).toEqual([40]);
    expect(input.value).toBe("40");
  });

  test("gives way to the number that its bound gave as the user typed, where the user leaves with the number that the field showed", () => {
    const stub = stubBound(25);
    const { el } = draw(<Rows stub={stub} />);
    const input = labelled(el, "Preview rows");

    act(() => {
      input.focus();
      type(input, "25");
    });
    act(() => {
      stub.set(30);
    });
    expect(input.value).toBe("25");

    act(() => {
      input.blur();
    });
    expect(stub.flushed).toEqual([]);
    expect(input.value).toBe("30");
  });
});

// A number field of at least 1, as the UI of core's board option n_rows draws
// it, whose most may be outdated, as while the request that gives it asks.
function Rows({ stub, max, outdated }: { stub: StubBound; max?: number; outdated?: boolean }): ReactElement {
  return <NumberField bound={useStubBound(stub)} label="Preview rows" minValue={1} maxValue={max} step={1} outdated={outdated} />;
}

// What the browser's own number input says of the number in the text, with
// the range and the step of the field of Rows, as the field says it.
const said = (text: string, max?: number): string | null => numberSaid(text, { min: 1, max, step: 1 });

// A select of several strings, as the UI of a package's board option may draw
// one, which submits the picks in the order of its items.
function Severities({ stub, outdated }: { stub: StubBound; outdated?: boolean }): ReactElement {
  return (
    <ComboBox
      bound={useStubBound(stub)}
      selectionMode="multiple"
      label="Severities"
      defaultItems={ids(["message", "warning", "error"])}
      ordered
      outdated={outdated}
    >
      {(item) => <ComboBoxItem id={item.id}>{item.id}</ComboBoxItem>}
    </ComboBox>
  );
}


interface Mode {
  readonly id: string;
  readonly name: string;
}

const modes: readonly Mode[] = [
  { id: "light", name: "Light" },
  { id: "dark", name: "Dark" },
];

// A select of one, as the UI of core's board option dark_mode may draw it,
// whose items are none yet where `items` is undefined.
function Mode({ stub, items, outdated }: { stub: StubBound; items?: readonly Mode[]; outdated?: boolean }): ReactElement {
  return (
    <ComboBox bound={useStubBound(stub)} label="Light/dark mode" placeholder="Choose a mode" defaultItems={items} outdated={outdated}>
      {(item) => <ComboBoxItem id={item.id}>{item.name}</ComboBoxItem>}
    </ComboBox>
  );
}

describe("the inputs of a board option's UI", () => {
  test("show the value of their bound, a number of at least 1, and submit only one that fits", () => {
    const stub = stubBound(25);
    const { el } = draw(<Rows stub={stub} />);
    const input = labelled(el, "Preview rows");
    expect(input.value).toBe("25");

    act(() => {
      type(input, "0");
    });
    act(() => {
      press(input, "Enter");
    });
    expect(input.value).toBe("25");
    act(() => {
      type(input, "50");
    });
    act(() => {
      press(input, "Enter");
    });
    act(() => {
      stub.set(10);
    });

    expect(stub.flushed).toEqual([50]);
    expect(input.value).toBe("10");
  });

  test("submit a number above an outdated most, and mark none until it is current, but hold to the least", () => {
    const stub = stubBound(25);
    const { el, redraw } = draw(<Rows stub={stub} max={30} outdated />);
    const input = labelled(el, "Preview rows");
    const shown = () => [input.value, input.getAttribute("aria-invalid")];

    act(() => {
      type(input, "0");
    });
    expect(shown()).toEqual(["0", "true"]);
    act(() => {
      press(input, "Enter");
    });
    expect(shown()).toEqual(["25", null]);
    act(() => {
      type(input, "50");
    });
    expect(shown()).toEqual(["50", null]);
    act(() => {
      press(input, "Enter");
    });
    expect(shown()).toEqual(["50", null]);
    expect(stub.flushed).toEqual([50]);

    redraw(<Rows stub={stub} max={30} />);
    expect(shown()).toEqual(["50", "true"]);
    expect(stub.flushed).toEqual([50]);
  });

  test("say why a number does not fit while the user types it, in the words of the browser's own number input, and mark it", () => {
    const stub = stubBound(25);
    const { el } = draw(<Rows stub={stub} max={30} />);
    const input = labelled(el, "Preview rows");

    const shown: (string | null)[][] = [];
    for (const text of ["50", "0", "2.5", "", "20"]) {
      act(() => {
        type(input, text);
      });
      shown.push([input.getAttribute("aria-invalid"), fieldError(el)]);
    }
    expect(shown).toEqual([
      ["true", said("50", 30)],
      ["true", said("0", 30)],
      ["true", said("2.5", 30)],
      [null, null],
      [null, null],
    ]);
    expect(said("50", 30)).not.toBeNull();
    act(() => {
      press(input, "Enter");
    });
    expect(stub.flushed).toEqual([20]);
  });

  test("say why the value of their bound does not fit, as once the most shrank", () => {
    const stub = stubBound(25);
    const { el, redraw } = draw(<Rows stub={stub} max={30} />);
    expect(fieldError(el)).toBeNull();

    redraw(<Rows stub={stub} max={20} />);
    expect(fieldError(el)).toBe(said("25", 20));
    expect(labelled(el, "Preview rows").value).toBe("25");
    expect(stub.flushed).toEqual([]);
  });

  test("go back to the value of their bound in place of a number outside the range on Enter and as the user leaves, which clears what it said", () => {
    const stub = stubBound(25);
    const { el } = draw(<Rows stub={stub} max={30} />);
    const input = labelled(el, "Preview rows");
    const shown = () => [input.value, fieldError(el)];

    act(() => {
      type(input, "50");
    });
    expect(shown()).toEqual(["50", said("50", 30)]);
    act(() => {
      press(input, "Enter");
    });
    expect(shown()).toEqual(["25", null]);

    act(() => {
      type(input, "60");
    });
    act(() => {
      input.blur();
    });
    expect(shown()).toEqual(["25", null]);
    expect(stub.flushed).toEqual([]);
  });

  test("submit a number that fits once the most grows, as the user presses Enter or leaves", () => {
    const stub = stubBound(25);
    const { el, redraw } = draw(<Rows stub={stub} max={30} />);
    const input = labelled(el, "Preview rows");

    act(() => {
      type(input, "50");
    });
    redraw(<Rows stub={stub} max={100} />);
    expect(stub.flushed).toEqual([]);
    act(() => {
      press(input, "Enter");
    });
    expect(stub.flushed).toEqual([50]);

    act(() => {
      type(input, "150");
    });
    redraw(<Rows stub={stub} max={200} />);
    act(() => {
      input.blur();
    });
    expect(stub.flushed).toEqual([50, 150]);
    expect(input.value).toBe("150");
  });

  test("submit the picked severities in the order of the choices", async () => {
    const stub = stubBound(["error"]);
    const { el } = draw(<Severities stub={stub} />);
    const field = labelled(el, "Severities");

    await step(() => {
      field.click();
    });
    expect(optionLabels(field)).toEqual(["message", "warning", "error"]);
    await step(() => {
      optionFromLabel(field, "message").click();
    });
    await step(() => {
      press(field, "Escape");
    });

    expect(stub.committed).toEqual([["message", "error"]]);
  });

  test("show a string that the items lack as it is, say below the field that it is not available, and drop it as the user removes its chip", async () => {
    const stub = stubBound(["error", "debug"]);
    const { el } = draw(<Severities stub={stub} />);
    const field = labelled(el, "Severities");

    expect([chipLabels(field), field.getAttribute("aria-invalid"), notAvailable(el)]).toEqual([["error", "debug"], "true", ["Not available: debug"]]);
    await removeChip(field, step, "debug");

    expect(stub.committed).toEqual([["error"]]);
    expect([chipLabels(field), field.getAttribute("aria-invalid"), notAvailable(el)]).toEqual([["error"], null, []]);
    expect(await listed(field, step)).toEqual(["message", "warning", "error"]);
  });

  test("keep outdated items on screen, and mark no string that they lack until they are current", async () => {
    const stub = stubBound(["error", "debug"]);
    const { el, redraw } = draw(<Severities stub={stub} outdated />);
    const field = labelled(el, "Severities");

    expect([chipLabels(field), field.getAttribute("aria-invalid"), notAvailable(el)]).toEqual([["error", "debug"], null, []]);
    expect(await listed(field, step)).toEqual(["message", "warning", "error"]);

    redraw(<Severities stub={stub} />);
    expect([chipLabels(field), field.getAttribute("aria-invalid"), notAvailable(el)]).toEqual([["error", "debug"], "true", ["Not available: debug"]]);
    expect(stub.committed).toEqual([]);
  });

  // The field shows a pick that the items lack by its id, and the lines below
  // the inputs what is not available.
  const shownMode = async (el: HTMLElement, field: HTMLInputElement) => [
    field.value,
    await listed(field, step),
    field.getAttribute("aria-invalid"),
    notAvailable(el),
  ];

  test("show a mode that the items lack as it is, and say below the field that it is not available once they are known", async () => {
    const stub = stubBound("auto");
    const { el, redraw } = draw(<Mode stub={stub} />);
    const field = labelled(el, "Light/dark mode");
    expect(await shownMode(el, field)).toEqual(["auto", [], null, []]);

    redraw(<Mode stub={stub} items={modes} />);
    expect(await shownMode(el, field)).toEqual(["auto", ["Light", "Dark"], "true", ["Not available: auto"]]);

    redraw(<Mode stub={stub} items={[...modes, { id: "auto", name: "Auto" }]} />);
    expect(await shownMode(el, field)).toEqual(["Auto", ["Light", "Dark", "Auto"], null, []]);
    expect(stub.committed).toEqual([]);
  });

  test("keep outdated items on screen, and mark no mode that they lack until they are current", async () => {
    const stub = stubBound("auto");
    const { el, redraw } = draw(<Mode stub={stub} items={modes} outdated />);
    const field = labelled(el, "Light/dark mode");
    expect(await shownMode(el, field)).toEqual(["auto", ["Light", "Dark"], null, []]);

    redraw(<Mode stub={stub} items={modes} />);
    expect(await shownMode(el, field)).toEqual(["auto", ["Light", "Dark"], "true", ["Not available: auto"]]);
    expect(stub.committed).toEqual([]);
  });

  test("submit the mode the user picks", async () => {
    const stub = stubBound("light");
    const { el } = draw(<Mode stub={stub} items={modes} />);
    const field = labelled(el, "Light/dark mode");

    await step(() => {
      field.click();
    });
    await step(() => {
      optionFromLabel(field, "Dark").click();
    });

    expect(stub.committed).toEqual(["dark"]);
  });
});

describe("the button", () => {
  test("takes one of its looks, and names its shortcut to assistive technology, which React Aria's Button passes on to no element", () => {
    const { el } = draw(
      <>
        <Button variant="secondary" aria-keyshortcuts="Control+S Meta+S">
          Save
        </Button>
        <Button variant="destructive">Remove</Button>
      </>,
    );
    const [save, remove] = [...el.querySelectorAll("button")];

    expect(save?.getAttribute("aria-keyshortcuts")).toBe("Control+S Meta+S");
    expect(save?.className).toContain("bk:border-border");
    expect(remove?.hasAttribute("aria-keyshortcuts")).toBe(false);
    expect(remove?.className).toContain("bk:bg-destructive");
  });
});

