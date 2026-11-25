// The inputs of blockr.ui that pick among choices the user sees whole, React
// Aria's drawn with the theme variables: the checkbox, the switch and the
// toggle button, each of a boolean, and the groups of several: the checkbox
// group, the radio group and the toggle button group, each on the stub of
// Bound.
// The rest, such as the keys and the focus, is React Aria's.

import { act } from "react";
import type { ReactElement } from "react";
import { createRoot } from "react-dom/client";
import { describe, expect, test } from "vitest";
import { labelled, notAvailable, step, stubBound, texts, toggleButton, toggles, useStubBound } from "../stubs/index.ts";
import type { StubBound } from "../stubs/index.ts";
import { Checkbox, CheckboxGroup, Radio, RadioGroup, Switch, ToggleButton, ToggleButtonGroup } from "./index.ts";
import type { Json } from "./index.ts";

// A stub of Bound for each input of the Picker below.
interface Picks {
  readonly keep: StubBound;
  readonly on: StubBound;
  readonly bold: StubBound;
  readonly end: StubBound;
  readonly levels: StubBound;
  readonly side: StubBound;
  readonly marks: StubBound;
}

// The stubs of the inputs, each holding the value given, the booleans false
// and the groups none where no value is given.
const picks = (values: Partial<Record<keyof Picks, Json>> = {}): Picks => ({
  keep: stubBound(values.keep ?? false),
  on: stubBound(values.on ?? false),
  bold: stubBound(values.bold ?? false),
  end: stubBound(values.end),
  levels: stubBound(values.levels),
  side: stubBound(values.side),
  marks: stubBound(values.marks),
});

// Draws the component on the page, and unmounts it.
async function draw(node: ReactElement): Promise<{ el: HTMLElement; unmount: () => Promise<void> }> {
  const el = document.createElement("div");
  document.body.append(el);
  const root = createRoot(el);
  await step(() => {
    root.render(node);
  });
  return {
    el,
    unmount: () =>
      step(() => {
        root.unmount();
      }),
  };
}

// Clicks the element, and waits for the submit it makes to settle.
async function click(el: HTMLElement): Promise<void> {
  await act(async () => {
    el.click();
    await Promise.resolve();
  });
}

function Picker({ picks, outdated = false, ordered = false }: { picks: Picks; outdated?: boolean; ordered?: boolean }): ReactElement {
  return (
    <>
      <Checkbox bound={useStubBound(picks.keep)} description="Whether it keeps the rows">
        Keep
      </Checkbox>
      <Switch bound={useStubBound(picks.on)}>On</Switch>
      <ToggleButton bound={useStubBound(picks.bold)}>Bold</ToggleButton>
      <RadioGroup bound={useStubBound(picks.end)} label="End" outdated={outdated}>
        <Radio value="head">Head</Radio>
        <Radio value="tail">Tail</Radio>
      </RadioGroup>
      <CheckboxGroup bound={useStubBound(picks.levels)} label="Levels" outdated={outdated} ordered={ordered}>
        {["message", "warning", "error"].map((level) => (
          <Checkbox key={level} value={level}>
            {level}
          </Checkbox>
        ))}
      </CheckboxGroup>
      <ToggleButtonGroup bound={useStubBound(picks.side)} label="Side" outdated={outdated}>
        <ToggleButton id="left">Left</ToggleButton>
        <ToggleButton id="right">Right</ToggleButton>
      </ToggleButtonGroup>
      <ToggleButtonGroup bound={useStubBound(picks.marks)} selectionMode="multiple" label="Marks" outdated={outdated} ordered={ordered}>
        <ToggleButton id="dot">Dot</ToggleButton>
        <ToggleButton id="ring">Ring</ToggleButton>
        <ToggleButton id="star">Star</ToggleButton>
      </ToggleButtonGroup>
    </>
  );
}

// The labels of the checkboxes or radios of the group whose label is the one
// given, with whether each is picked.
function boxes(el: HTMLElement, group: string): { readonly label: string; readonly picked: boolean }[] {
  const found = [...el.querySelectorAll('[role="group"], [role="radiogroup"]')].find(
    (node) => el.ownerDocument.getElementById(node.getAttribute("aria-labelledby") ?? "")?.textContent === group,
  );
  return [...(found?.querySelectorAll("label") ?? [])].map((label) => ({
    label: label.textContent.trim(),
    picked: label.querySelector("input")?.checked === true,
  }));
}

describe("a bound checkbox, switch or toggle button", () => {
  test("shows the boolean that its bound holds, and submits the user's pick at once", async () => {
    const stubs = picks();
    const { el } = await draw(<Picker picks={stubs} />);
    const bold = (): string | null => [...el.querySelectorAll("button")].find((b) => b.textContent === "Bold")?.getAttribute("aria-pressed") ?? null;
    expect([labelled(el, "Keep").checked, labelled(el, "On").checked, bold()]).toEqual([false, false, "false"]);

    await click(labelled(el, "Keep"));
    await click(labelled(el, "On"));
    await click(find(el, "Bold"));

    expect([stubs.keep.committed, stubs.on.committed, stubs.bold.committed]).toEqual([[true], [true], [true]]);
    expect([labelled(el, "Keep").checked, labelled(el, "On").checked, bold()]).toEqual([true, true, "true"]);
    expect(labelled(el, "On").getAttribute("role")).toBe("switch");
  });

  test("takes a value that it did not make, and shows its bound's value again where its submit was refused, saying why below itself", async () => {
    const stubs = picks();
    const { el } = await draw(<Picker picks={stubs} />);
    const keep = labelled(el, "Keep");

    await step(() => {
      stubs.on.set(true);
    });
    expect([labelled(el, "Keep"), keep.checked, labelled(el, "On").checked]).toEqual([keep, false, true]);

    stubs.keep.refuse("The rows are kept by the block before.");
    await click(keep);
    expect([stubs.keep.committed, keep.checked]).toEqual([[true], false]);
    expect(texts(el, '[role="alert"]')).toEqual(["The rows are kept by the block before."]);
  });

  test("names its description as the checkbox's", async () => {
    const { el } = await draw(<Picker picks={picks()} />);
    const keep = labelled(el, "Keep");

    expect(el.ownerDocument.getElementById(keep.getAttribute("aria-describedby") ?? "")?.textContent).toBe("Whether it keeps the rows");
  });
});

describe("a bound group", () => {
  test("of radios submits the pick at once", async () => {
    const stubs = picks({ end: "head" });
    const { el } = await draw(<Picker picks={stubs} />);
    expect(boxes(el, "End")).toEqual([
      { label: "Head", picked: true },
      { label: "Tail", picked: false },
    ]);

    await click(labelled(el, "Tail"));

    expect(stubs.end.committed).toEqual(["tail"]);
    expect(boxes(el, "End")).toEqual([
      { label: "Head", picked: false },
      { label: "Tail", picked: true },
    ]);
  });

  test("of checkboxes submits each pick at once, built from the value of its bound as it is then, and in the order of its checkboxes where ordered", async () => {
    const stubs = picks({ levels: ["error"] });
    const { el } = await draw(<Picker picks={stubs} />);

    await click(labelled(el, "message"));
    expect(stubs.levels.committed).toEqual([["error", "message"]]);

    // Another user adds a level, which the next pick keeps.
    await step(() => {
      stubs.levels.set(["warning", "error"]);
    });
    await click(labelled(el, "message"));
    expect(stubs.levels.committed.at(-1)).toEqual(["warning", "error", "message"]);

    const sorted = picks({ levels: ["error"] });
    const { el: other } = await draw(<Picker picks={sorted} ordered />);
    await click(labelled(other, "message"));
    expect(sorted.levels.committed).toEqual([["message", "error"]]);
  });

  test("of toggle buttons of one submits the pick at once, and unsets the value as the user unpicks it", async () => {
    const stubs = picks({ side: "left" });
    const { el } = await draw(<Picker picks={stubs} />);
    expect(toggles(el, "Side")).toEqual([
      { label: "Left", picked: true },
      { label: "Right", picked: false },
    ]);

    await click(toggleButton(el, "Side", "Right"));
    await click(toggleButton(el, "Side", "Right"));

    expect([stubs.side.committed, stubs.side.value]).toEqual([["right", undefined], undefined]);
    expect(toggles(el, "Side").map((side) => side.picked)).toEqual([false, false]);
  });

  test("of toggle buttons of several submits each pick at once, built from the value of its bound as it is then, and in the order of its buttons where ordered", async () => {
    const stubs = picks({ marks: ["star"] });
    const { el } = await draw(<Picker picks={stubs} />);

    await click(toggleButton(el, "Marks", "Dot"));
    await click(toggleButton(el, "Marks", "Star"));

    expect(stubs.marks.committed).toEqual([["star", "dot"], ["dot"]]);

    const sorted = picks({ marks: ["star"] });
    const { el: other } = await draw(<Picker picks={sorted} ordered />);
    await click(toggleButton(other, "Marks", "Dot"));
    expect(sorted.marks.committed).toEqual([["dot", "star"]]);
  });

  test("draws no item for a value that none of its items holds, marks itself invalid and says below itself that it is not available, but not while outdated", async () => {
    const values = { end: "middle", levels: ["error", "debug"], side: "up", marks: ["dot", "cross"] };
    const shown = async (outdated: boolean) => {
      const { el, unmount } = await draw(<Picker picks={picks(values)} outdated={outdated} />);
      const seen = {
        end: boxes(el, "End"),
        levels: boxes(el, "Levels"),
        side: toggles(el, "Side"),
        marks: toggles(el, "Marks"),
        // The groups that mark themselves invalid, by their roles.
        invalid: [...el.querySelectorAll("[role][data-invalid]")].map((node) => node.getAttribute("role")),
        lines: notAvailable(el),
        // The buttons that drop the values that a group of several lacks.
        removes: [...el.querySelectorAll("button")].flatMap((button) => (button.textContent === "Remove" ? [button.getAttribute("aria-label")] : [])),
      };
      await unmount();
      return seen;
    };

    expect(await shown(false)).toEqual({
      end: [
        { label: "Head", picked: false },
        { label: "Tail", picked: false },
      ],
      levels: [
        { label: "message", picked: false },
        { label: "warning", picked: false },
        { label: "error", picked: true },
      ],
      side: [
        { label: "Left", picked: false },
        { label: "Right", picked: false },
      ],
      marks: [
        { label: "Dot", picked: true },
        { label: "Ring", picked: false },
        { label: "Star", picked: false },
      ],
      invalid: ["radiogroup", "group", "radiogroup", "toolbar"],
      lines: ["Not available: middle", "Not available: debug", "Not available: up", "Not available: cross"],
      removes: ["Remove debug", "Remove cross"],
    });
    const outdated = await shown(true);
    expect([outdated.invalid, outdated.lines, outdated.removes]).toEqual([[], [], []]);
    expect([outdated.levels.length, outdated.marks.length]).toEqual([3, 3]);
  });

  test("keeps a value that its items lack as the user picks another item of a group of several, and drops it by Remove", async () => {
    const stubs = picks({ levels: ["error", "debug"], marks: ["dot", "cross"] });
    const { el } = await draw(<Picker picks={stubs} />);

    await click(labelled(el, "message"));
    expect(notAvailable(el)).toEqual(["Not available: debug", "Not available: cross"]);
    await click(find(el, "Remove", "Remove debug"));
    await click(find(el, "Remove", "Remove cross"));

    expect([stubs.levels.committed, stubs.marks.committed]).toEqual([
      [
        ["error", "debug", "message"],
        ["error", "message"],
      ],
      [["dot"]],
    ]);
    expect([notAvailable(el), el.querySelectorAll("[role][data-invalid]").length]).toEqual([[], 0]);
  });

  test("drops a value that its items lack from a group of one as the user picks an item", async () => {
    const stubs = picks({ end: "middle", side: "up" });
    const { el } = await draw(<Picker picks={stubs} />);

    await click(labelled(el, "Head"));
    await click(toggleButton(el, "Side", "Left"));

    expect([stubs.end.committed, stubs.side.committed]).toEqual([["head"], ["left"]]);
    expect(notAvailable(el)).toEqual([]);
  });
});

// The button of the text given, and of the accessible name given where it has
// one, as a toggle button outside a group or the button that drops a value.
function find(el: HTMLElement, text: string, name?: string): HTMLButtonElement {
  const found = [...el.querySelectorAll("button")].find(
    (button) => button.textContent === text && (name === undefined || button.getAttribute("aria-label") === name),
  );
  if (found === undefined) throw new Error(`Found no button ${text}.`);
  return found;
}
