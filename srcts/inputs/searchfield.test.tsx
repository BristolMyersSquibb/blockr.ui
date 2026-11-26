// The search field of blockr.ui, React Aria's SearchField drawn with the theme
// variables: its button that clears the search, Escape, and what it does with
// its `bound`, on the stub of Bound, which it hands what the user types as
// blockr.ui's TextField does.
// The rest, such as the focus, is React Aria's.

import type { ReactElement } from "react";
import { createRoot } from "react-dom/client";
import { describe, expect, test } from "vitest";
import { labelled, press, step, stubBound, texts, type, useStubBound } from "../stubs/index.ts";
import type { StubBound } from "../stubs/index.ts";
import { SearchField } from "./index.ts";

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

function Bound({ query }: { query: StubBound }): ReactElement {
  return <SearchField bound={useStubBound(query)} label="Search" />;
}

// The button of the field that clears the search.
function clearer(field: HTMLInputElement): HTMLButtonElement {
  const found = field.closest('[role="group"]')?.querySelector('button[aria-label="Clear search"]');
  if (!(found instanceof HTMLButtonElement)) throw new Error("Found no button that clears the search.");
  return found;
}

describe("a search field", () => {
  test("clears the search by its button, which Tab skips, and by Escape, which goes on to the elements around it where the search is empty", async () => {
    const heard: string[] = [];
    const { el } = await draw(
      <div
        onKeyDown={(event) => {
          heard.push(event.key);
        }}
      >
        <SearchField label="Search" defaultValue="iris" />
      </div>,
    );
    const field = labelled(el, "Search");
    expect([field.value, field.type, clearer(field).tabIndex]).toEqual(["iris", "search", -1]);

    await step(() => {
      clearer(field).click();
    });
    expect(field.value).toBe("");

    await step(() => {
      type(field, "head");
    });
    await step(() => {
      press(field, "Escape");
    });
    expect([field.value, heard]).toEqual(["", []]);
    await step(() => {
      press(field, "Escape");
    });
    expect(heard).toEqual(["Escape"]);
  });
});

describe("a bound search field", () => {
  test("hands each keystroke to its bound, whose binding submits once the user pauses, and submits nothing itself", async () => {
    const query = stubBound("");
    const { el } = await draw(<Bound query={query} />);
    const field = labelled(el, "Search");

    await step(() => {
      type(field, "iris");
    });

    expect([query.typed, query.committed, query.flushed, field.value]).toEqual([["i", "ir", "iri", "iris"], [], [], "iris"]);
  });

  test("submits at once on Enter, as the user leaves the field, and as the user clears the search", async () => {
    const query = stubBound("");
    const { el } = await draw(<Bound query={query} />);
    const field = labelled(el, "Search");

    await step(() => {
      type(field, "a");
      press(field, "Enter");
    });
    await step(() => {
      type(field, "ab");
      field.blur();
    });
    await step(() => {
      clearer(field).click();
    });

    expect(query.flushed).toEqual(["a", "ab", ""]);
  });

  test("shows a value that it did not make, and what the user types until its bound gives another", async () => {
    const query = stubBound("");
    const { el } = await draw(<Bound query={query} />);
    const field = labelled(el, "Search");

    await step(() => {
      query.set("iris");
    });
    expect(field.value).toBe("iris");

    await step(() => {
      type(field, "head");
    });
    expect([field.value, query.value]).toEqual(["head", "iris"]);
    await step(() => {
      query.set("tail");
    });
    expect(field.value).toBe("tail");
  });

  test("says below itself why its last submit was refused, the error of its bound", async () => {
    const query = stubBound("");
    const { el } = await draw(<Bound query={query} />);
    const field = labelled(el, "Search");

    query.refuse("No such column.");
    await step(() => {
      type(field, "wt");
      press(field, "Enter");
    });

    expect([query.flushed, texts(el, '[role="alert"]'), field.value]).toEqual([["wt"], ["No such column."], "wt"]);
  });
});
