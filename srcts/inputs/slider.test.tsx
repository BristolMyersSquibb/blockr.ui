// The slider of blockr.ui, React Aria's Slider drawn with the theme
// variables: what it shows, and what it does with its `bound`, on the stub of
// Bound.
// The user moves a thumb by the keys here, which React Aria takes as a drag
// that starts and ends at once, since happy-dom measures no track to drag
// along.
// The rest, such as dragging and the focus, is React Aria's.

import { act } from "react";
import type { ReactElement } from "react";
import { createRoot } from "react-dom/client";
import { describe, expect, test } from "vitest";
import { notAvailable, press, step, stubBound, texts, useStubBound } from "../stubs/index.ts";
import type { StubBound } from "../stubs/index.ts";
import { Slider } from "./index.ts";

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

// A slider of one thumb and one of two, each on a stub of its own.
function Bound({ size, span, outdated = false }: { size: StubBound; span: StubBound; outdated?: boolean }): ReactElement {
  return (
    <>
      <Slider bound={useStubBound(size)} label="Size" minValue={0} maxValue={100} step={5} outdated={outdated} />
      <Slider bound={useStubBound(span)} label="Span" minValue={0} maxValue={10} thumbLabels={["From", "To"]} outdated={outdated} />
    </>
  );
}

// The slider whose label is the one given: what it shows at the right of the
// label, the numbers of its thumbs, and whether it marks itself invalid.
function slider(el: HTMLElement, label: string) {
  const found = [...el.querySelectorAll<HTMLElement>('[role="group"]')].find(
    (group) => el.ownerDocument.getElementById(group.getAttribute("aria-labelledby") ?? "")?.textContent === label,
  );
  if (found === undefined) throw new Error(`Found no slider ${label}.`);
  const thumbs = [...found.querySelectorAll<HTMLInputElement>('input[type="range"]')];
  return {
    shows: found.querySelector("output")?.textContent ?? null,
    thumbs: thumbs.map((thumb) => Number(thumb.value)),
    invalid: found.hasAttribute("data-invalid"),
    thumb: (i = 0): HTMLInputElement => {
      const thumb = thumbs[i];
      if (thumb === undefined) throw new Error(`Found no thumb ${String(i)} of ${label}.`);
      return thumb;
    },
  };
}

// Presses the key on the thumb, as the user steps it, and waits for the
// submit it makes to settle.
async function stepped(thumb: HTMLInputElement, key: string): Promise<void> {
  await act(async () => {
    thumb.focus();
    press(thumb, key);
    await Promise.resolve();
  });
}

describe("a slider", () => {
  test("shows its number at the right of its label, or the numbers of its thumbs", async () => {
    const { el } = await draw(
      <>
        <Slider label="Size" defaultValue={30} />
        <Slider label="Span" defaultValue={[2, 6]} maxValue={10} thumbLabels={["From", "To"]} />
      </>,
    );

    expect([slider(el, "Size").shows, slider(el, "Size").thumbs]).toEqual(["30", [30]]);
    expect([slider(el, "Span").shows, slider(el, "Span").thumbs]).toEqual(["2–6", [2, 6]]);
    expect(slider(el, "Span").thumb(1).getAttribute("aria-label")).toBe("To");
  });
});

describe("a bound slider", () => {
  test("shows the number that its bound holds, and submits a step of a thumb by a key at once, as one edit", async () => {
    const [size, span] = [stubBound(20), stubBound([2, 6])];
    const { el } = await draw(<Bound size={size} span={span} />);
    expect([slider(el, "Size").shows, slider(el, "Span").shows]).toEqual(["20", "2–6"]);

    await stepped(slider(el, "Size").thumb(), "ArrowRight");
    await stepped(slider(el, "Span").thumb(1), "ArrowLeft");

    expect([size.committed, span.committed]).toEqual([[25], [[2, 5]]]);
    expect([slider(el, "Size").shows, slider(el, "Span").shows]).toEqual(["25", "2–5"]);
  });

  test("takes a value that it did not make, and shows its bound's value again where its submit was refused, saying why below itself", async () => {
    const size = stubBound(20);
    const { el } = await draw(<Bound size={size} span={stubBound([2, 6])} />);

    await step(() => {
      size.set(50);
    });
    expect(slider(el, "Size").thumbs).toEqual([50]);

    size.refuse("At most 50.");
    await stepped(slider(el, "Size").thumb(), "ArrowRight");
    expect(size.committed).toEqual([55]);
    expect([slider(el, "Size").shows, slider(el, "Size").thumbs]).toEqual(["50", [50]]);
    expect(texts(el, '[role="alert"]')).toEqual(["At most 50."]);
  });

  test("shows a number outside its range, or off its step, as it is, with the thumb at the nearest end, marks itself invalid and says below itself that the number is not available, but not while outdated", async () => {
    const shown = async (outdated: boolean) => {
      const { el, unmount } = await draw(<Bound size={stubBound(150)} span={stubBound([2, 12])} outdated={outdated} />);
      const seen = [[slider(el, "Size"), slider(el, "Span")].map(({ shows, thumbs, invalid }) => ({ shows, thumbs, invalid })), notAvailable(el)];
      await unmount();
      return seen;
    };

    expect(await shown(false)).toEqual([
      [
        { shows: "150", thumbs: [100], invalid: true },
        { shows: "2–12", thumbs: [2, 10], invalid: true },
      ],
      ["Not available: 150", "Not available: 12"],
    ]);
    expect(await shown(true)).toEqual([
      [
        { shows: "150", thumbs: [100], invalid: false },
        { shows: "2–12", thumbs: [2, 10], invalid: false },
      ],
      [],
    ]);

    const { el } = await draw(<Bound size={stubBound(22)} span={stubBound()} />);
    expect([slider(el, "Size").shows, slider(el, "Size").thumbs, slider(el, "Size").invalid, notAvailable(el)]).toEqual([
      "22",
      [20],
      true,
      ["Not available: 22"],
    ]);
  });

  test("submits no number that it pulled into its range, as where the user steps a thumb against its end, but one that the user moves it to", async () => {
    const size = stubBound(150);
    const { el } = await draw(<Bound size={size} span={stubBound()} />);

    await stepped(slider(el, "Size").thumb(), "ArrowRight");
    await stepped(slider(el, "Size").thumb(), "End");
    expect(size.committed).toEqual([]);
    expect(slider(el, "Size").shows).toBe("150");

    await stepped(slider(el, "Size").thumb(), "ArrowLeft");
    expect(size.committed).toEqual([95]);
    expect([slider(el, "Size").shows, slider(el, "Size").invalid, notAvailable(el)]).toEqual(["95", false, []]);
  });

  test("submits the number of a thumb that the user did not move as the value holds it, rather than as the slider pulled it in", async () => {
    const span = stubBound([2, 12]);
    const { el } = await draw(<Bound size={stubBound()} span={span} />);

    await stepped(slider(el, "Span").thumb(0), "ArrowRight");

    expect(span.committed).toEqual([[3, 12]]);
    expect([slider(el, "Span").shows, notAvailable(el)]).toEqual(["3–12", ["Not available: 12"]]);
  });

  test("shows nothing at the right of its label while its bound holds no value, and submits the number that the user moves to", async () => {
    const size = stubBound();
    const { el } = await draw(<Bound size={size} span={stubBound()} />);
    expect([slider(el, "Size").shows, slider(el, "Size").thumbs]).toEqual(["", [0]]);

    await stepped(slider(el, "Size").thumb(), "ArrowRight");

    expect(size.committed).toEqual([5]);
  });
});
