// The select of blockr.ui without a field to type into, React Aria's Select
// drawn with the theme variables: what blockr.ui adds to it, the rows of the
// values that its items lack, and what it does with its `bound`, on the stub
// of Bound.
// The rest, such as the keys and the focus, is React Aria's.

import { act } from "react";
import type { ReactElement } from "react";
import { createRoot } from "react-dom/client";
import { describe, expect, test } from "vitest";
import {
  listed,
  notAvailable,
  optionFromLabel,
  picked,
  pickedLabels,
  press,
  selectButton,
  selectShows,
  step,
  stubBound,
  texts,
  useStubBound,
} from "../stubs/index.ts";
import type { StubBound } from "../stubs/index.ts";
import { Select, SelectItem, SelectSection, Text } from "./index.ts";

interface Fruit {
  readonly id: string;
  readonly name: string;
}

const fruits: readonly Fruit[] = [
  { id: "apple", name: "Apple" },
  { id: "banana", name: "Banana" },
  { id: "cherry", name: "Cherry" },
];

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

const row = (fruit: Fruit): ReactElement => <SelectItem id={fruit.id}>{fruit.name}</SelectItem>;

// The selects of one and of several, each on a stub of its own, whose items
// are none yet where `unknown`, as while the request that gives them asks.
function Bound({
  fruit,
  basket,
  outdated = false,
  ordered = false,
  unknown = false,
}: {
  fruit: StubBound;
  basket: StubBound;
  outdated?: boolean;
  ordered?: boolean;
  unknown?: boolean;
}): ReactElement {
  const items = unknown ? undefined : fruits;
  return (
    <>
      <Select bound={useStubBound(fruit)} label="Fruit" placeholder="Choose a fruit" items={items} outdated={outdated}>
        {row}
      </Select>
      <Select bound={useStubBound(basket)} selectionMode="multiple" label="Fruits" items={items} outdated={outdated} ordered={ordered}>
        {row}
      </Select>
    </>
  );
}

describe("a select", () => {
  test("opens its list from its button, and shows the pick, the picks of several in the order they were picked, or the placeholder", async () => {
    const seen: unknown[] = [];
    const { el } = await draw(
      <>
        <Select label="Fruit" placeholder="Choose a fruit" items={fruits} onChange={(key) => seen.push(key)}>
          {row}
        </Select>
        <Select label="Fruits" selectionMode="multiple" items={fruits} defaultValue={["banana", "apple"]}>
          {row}
        </Select>
      </>,
    );
    const fruit = selectButton(el, "Fruit");
    expect([selectShows(fruit), selectShows(selectButton(el, "Fruits"))]).toEqual(["Choose a fruit", "Banana and Apple"]);
    expect(await listed(fruit, step)).toEqual(["Apple", "Banana", "Cherry"]);

    await picked(fruit, step, "Cherry");

    expect([seen, selectShows(fruit)]).toEqual([["cherry"], "Cherry"]);
  });

  test("lists its items under the headings of their sections, each named by its label, and shows the label of the pick", async () => {
    const { el } = await draw(
      <Select label="Fruit" defaultValue="lemon">
        <SelectSection id="sweet" title="Sweet" items={fruits.slice(0, 2)}>
          {row}
        </SelectSection>
        <SelectSection id="sour" title="Sour">
          <SelectItem id="lemon" textValue="Lemon">
            <Text slot="label">Lemon</Text>
            <Text slot="description">Yellow</Text>
          </SelectItem>
        </SelectSection>
      </Select>,
    );
    const fruit = selectButton(el, "Fruit");

    expect(selectShows(fruit)).toBe("Lemon");
    expect(await listed(fruit, step)).toEqual(["Sweet: Apple", "Sweet: Banana", "Sour: Lemon"]);
  });

  test("names its description and its error message as the button's, and marks an error as invalid", async () => {
    const { el } = await draw(
      <Select label="Fruit" items={fruits} description="The fruit to eat" errorMessage="Pick a fruit." isInvalid>
        {row}
      </Select>,
    );
    const fruit = selectButton(el, "Fruit");
    const described = (fruit.getAttribute("aria-describedby") ?? "").split(" ").map((id) => el.ownerDocument.getElementById(id)?.textContent);

    expect(described).toEqual(["The fruit to eat", "Pick a fruit."]);
    expect(fruit.closest("[data-invalid]")).not.toBeNull();
  });
});

describe("a bound select", () => {
  test("submits the pick of one as the user picks it, and says below itself why its last submit was refused", async () => {
    const stub = stubBound("apple");
    const { el } = await draw(<Bound fruit={stub} basket={stubBound()} />);
    const fruit = selectButton(el, "Fruit");

    await picked(fruit, step, "Banana");
    expect([stub.committed, selectShows(fruit)]).toEqual([["banana"], "Banana"]);

    stub.refuse("Cherries are out of season.");
    await step(() => {
      fruit.click();
    });
    await act(async () => {
      optionFromLabel(fruit, "Cherry").click();
      await Promise.resolve();
    });
    expect([stub.committed, selectShows(fruit)]).toEqual([["banana", "cherry"], "Banana"]);
    expect(texts(el, '[role="alert"]')).toEqual(["Cherries are out of season."]);
  });

  test("submits the picks of several once, as the list closes, after the strings that its bound holds then", async () => {
    const stub = stubBound();
    const { el } = await draw(<Bound fruit={stubBound()} basket={stub} />);
    const several = selectButton(el, "Fruits");

    await step(() => {
      several.click();
    });
    // Another user adds strings while the list is open.
    await step(() => {
      stub.set(["cherry", "apple"]);
    });
    await step(() => {
      optionFromLabel(several, "Banana").click();
    });
    expect(stub.committed).toEqual([]);
    expect(pickedLabels(several)).toEqual(["Banana"]);
    await step(() => {
      press(several.ownerDocument.activeElement as HTMLElement, "Escape");
    });

    expect(stub.committed).toEqual([["cherry", "apple", "banana"]]);
    expect(selectShows(several)).toBe("Cherry, Apple, and Banana");
  });

  test("submits the picks in the order of its items where ordered, and the picks of a list that goes while open never", async () => {
    const stub = stubBound(["cherry"]);
    const { el, unmount } = await draw(<Bound fruit={stubBound()} basket={stub} ordered />);
    const several = selectButton(el, "Fruits");

    await picked(several, step, "Apple");
    expect(stub.committed).toEqual([["apple", "cherry"]]);

    await step(() => {
      several.click();
    });
    await step(() => {
      optionFromLabel(several, "Banana").click();
    });
    await unmount();
    expect(stub.committed).toEqual([["apple", "cherry"]]);
  });

  test("shows a value that its items lack by its id on its button, after the picks that the items hold, marks itself invalid and says below itself that it is not available, but not while outdated", async () => {
    const shown = async (outdated: boolean) => {
      const { el, unmount } = await draw(<Bound fruit={stubBound("durian")} basket={stubBound(["apple", "durian"])} outdated={outdated} />);
      const [one, several] = [selectButton(el, "Fruit"), selectButton(el, "Fruits")];
      const seen = {
        shows: [selectShows(one), selectShows(several)],
        invalid: [one, several].map((button) => button.closest("[data-invalid]") !== null),
        one: await listed(one, step),
        several: await listed(several, step),
        lines: notAvailable(el),
        // The button that drops the values that a select of several lacks.
        removes: [...el.querySelectorAll("button")].flatMap((button) => (button.textContent === "Remove" ? [button.getAttribute("aria-label")] : [])),
      };
      await unmount();
      return seen;
    };

    expect(await shown(false)).toEqual({
      shows: ["durian", "Apple and durian"],
      invalid: [true, true],
      one: ["Apple", "Banana", "Cherry"],
      several: ["Apple", "Banana", "Cherry"],
      lines: ["Not available: durian", "Not available: durian"],
      removes: ["Remove durian"],
    });
    expect(await shown(true)).toEqual({
      shows: ["durian", "Apple and durian"],
      invalid: [false, false],
      one: ["Apple", "Banana", "Cherry"],
      several: ["Apple", "Banana", "Cherry"],
      lines: [],
      removes: [],
    });
  });

  test("shows a value as it is while it has no items yet", async () => {
    const { el } = await draw(<Bound fruit={stubBound("durian")} basket={stubBound()} unknown />);
    const one = selectButton(el, "Fruit");

    expect([selectShows(one), one.closest("[data-invalid]")]).toEqual(["durian", null]);
  });

  test("drops a value that its items lack as the user picks an item of a select of one, or presses Remove below a select of several", async () => {
    const [fruit, basket] = [stubBound("durian"), stubBound(["apple", "durian"])];
    const { el } = await draw(<Bound fruit={fruit} basket={basket} />);
    const [one, several] = [selectButton(el, "Fruit"), selectButton(el, "Fruits")];

    await picked(one, step, "Banana");
    await act(async () => {
      [...el.querySelectorAll("button")].find((button) => button.getAttribute("aria-label") === "Remove durian")?.click();
      await Promise.resolve();
    });

    expect([fruit.committed, basket.committed]).toEqual([["banana"], [["apple"]]]);
    expect([selectShows(one), selectShows(several), one.closest("[data-invalid]"), several.closest("[data-invalid]"), notAvailable(el)]).toEqual([
      "Banana",
      "Apple",
      null,
      null,
      [],
    ]);
  });
});
