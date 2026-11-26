// The parts of blockr.ui that take no binding, React Aria's drawn with the theme
// variables: the tags, the list box in place, which an Autocomplete narrows with a
// search field, the form, the label, the description and the error of a field,
// the drop zone and React Aria's FileTrigger, which a caller composes with a button.
// The rest, such as the keys and the focus, is React Aria's.

import { useState } from "react";
import type { ReactElement } from "react";
import { createRoot } from "react-dom/client";
import { Group, Input, TextField as AriaTextField } from "react-aria-components";
import { describe, expect, test } from "vitest";
import { drop, find, labelled, picking, step, texts, type } from "../stubs/index.ts";
import {
  Autocomplete,
  Button,
  Checkbox,
  Description,
  DropZone,
  FieldError,
  FileTrigger,
  Form,
  Label,
  ListBox,
  ListBoxItem,
  SearchField,
  Tag,
  TagGroup,
  Text,
  TextField,
  useFilter,
} from "./index.ts";

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

const fruits = [
  { id: "apple", name: "Apple" },
  { id: "banana", name: "Banana" },
  { id: "cherry", name: "Cherry" },
];

// The classes of the element that the selector finds in `el`.
function classes(el: ParentNode, selector: string): string[] {
  return [...find(el, selector).classList];
}

// The labels of the rows of a list box, by the role that React Aria gives a
// row of a list box, and of those it shows as picked.
function rows(list: ParentNode): { readonly all: string[]; readonly picked: string[] } {
  const options = [...list.querySelectorAll('[role="option"]')];
  return {
    all: options.map((option) => option.textContent.trim()),
    picked: options.filter((option) => option.getAttribute("aria-selected") === "true").map((option) => option.textContent.trim()),
  };
}

describe("the tags", () => {
  test("show their texts under the group's label, and each a button that removes it where the group takes onRemove", async () => {
    function Tags(): ReactElement {
      const [left, setLeft] = useState(fruits);
      return (
        <TagGroup
          label="Fruits"
          items={left}
          onRemove={(keys) => {
            setLeft((now) => now.filter((fruit) => !keys.has(fruit.id)));
          }}
        >
          {(fruit) => <Tag id={fruit.id}>{fruit.name}</Tag>}
        </TagGroup>
      );
    }
    const { el } = await draw(<Tags />);
    const list = find(el, '[role="grid"]');
    expect(el.ownerDocument.getElementById(list.getAttribute("aria-labelledby") ?? "")?.textContent).toBe("Fruits");
    expect(texts(list, '[role="row"]')).toEqual(["Apple", "Banana", "Cherry"]);

    await step(() => {
      find(list, '[role="row"]:nth-child(2) button').click();
    });

    expect(texts(list, '[role="row"]')).toEqual(["Apple", "Cherry"]);
  });
});

describe("the list box", () => {
  test("shows its rows in place, and picks one, or several, where its selectionMode does", async () => {
    const { el } = await draw(
      <>
        <ListBox aria-label="One" items={fruits} selectionMode="single" defaultSelectedKeys={["banana"]}>
          {(fruit) => <ListBoxItem id={fruit.id}>{fruit.name}</ListBoxItem>}
        </ListBox>
        <ListBox aria-label="Several" items={fruits} selectionMode="multiple">
          {(fruit) => <ListBoxItem id={fruit.id}>{fruit.name}</ListBoxItem>}
        </ListBox>
      </>,
    );
    const [one, several] = [find(el, '[aria-label="One"]'), find(el, '[aria-label="Several"]')];
    expect(rows(one)).toEqual({ all: ["Apple", "Banana", "Cherry"], picked: ["Banana"] });

    await step(() => {
      find(one, '[role="option"]:nth-child(1)').click();
    });
    await step(() => {
      find(several, '[role="option"]:nth-child(1)').click();
    });
    await step(() => {
      find(several, '[role="option"]:nth-child(3)').click();
    });

    expect([rows(one).picked, rows(several).picked]).toEqual([["Apple"], ["Apple", "Cherry"]]);
  });

  test("narrows its rows to those that hold what the user types into a search field, in an Autocomplete", async () => {
    function Narrowed(): ReactElement {
      const { contains } = useFilter({ sensitivity: "base" });
      return (
        <Autocomplete filter={contains}>
          <SearchField label="Find a fruit" />
          <ListBox aria-label="Fruits" items={fruits} renderEmptyState={() => "No fruit"}>
            {(fruit) => <ListBoxItem id={fruit.id}>{fruit.name}</ListBoxItem>}
          </ListBox>
        </Autocomplete>
      );
    }
    const { el } = await draw(<Narrowed />);
    const list = find(el, '[role="listbox"]');

    await step(() => {
      type(labelled(el, "Find a fruit"), "an");
    });
    expect(rows(list).all).toEqual(["Banana"]);

    await step(() => {
      type(labelled(el, "Find a fruit"), "kiwi");
    });
    expect([list.hasAttribute("data-empty"), list.textContent]).toEqual([true, "No fruit"]);
  });
});

describe("the form", () => {
  test("shows the errors of its validationErrors below the fields they name, and leaves the checks to its caller where its validationBehavior is aria", async () => {
    const submitted: string[] = [];
    const { el } = await draw(
      <Form
        aria-label="Name"
        validationBehavior="aria"
        validationErrors={{ name: "The board holds a block of that name." }}
        onSubmit={(event) => {
          event.preventDefault();
          submitted.push("submitted");
        }}
      >
        <TextField label="Name" name="name" isRequired />
        <button type="submit">Save</button>
      </Form>,
    );
    const form = find(el, "form", HTMLFormElement);

    expect([form.noValidate, form.getAttribute("aria-label")]).toEqual([true, "Name"]);
    expect(texts(el, '[slot="errorMessage"]')).toEqual(["The board holds a block of that name."]);
    expect(labelled(el, "Name").getAttribute("aria-invalid")).toBe("true");

    await step(() => {
      find(el, "button").click();
    });
    expect(submitted).toEqual(["submitted"]);
  });
});

describe("the parts of a field", () => {
  test("draw the label, the description and the error of a field that a caller composes, in the classes of those of blockr.ui's inputs", async () => {
    const { el } = await draw(
      <>
        <AriaTextField isInvalid className="composed">
          <Label>Name</Label>
          <Input />
          <Description>Names the block on the board.</Description>
          <FieldError>The board holds a block of that name.</FieldError>
        </AriaTextField>
        <TextField label="Title" description="Heads the block." errorMessage="The title is too long." isInvalid className="drawn" />
      </>,
    );
    const [composed, drawn] = [find(el, ".composed"), find(el, ".drawn")];
    const input = labelled(el, "Name");
    const describedBy = (input.getAttribute("aria-describedby") ?? "").split(" ");

    expect(describedBy.map((id) => el.ownerDocument.getElementById(id)?.textContent)).toEqual([
      "Names the block on the board.",
      "The board holds a block of that name.",
    ]);
    for (const part of ["label", '[slot="description"]', '[slot="errorMessage"]']) {
      expect(classes(composed, part)).toEqual(classes(drawn, part));
    }
  });

  test("name a group of controls by a label outside a field", async () => {
    const { el } = await draw(
      <>
        <Group aria-labelledby="colour">
          <Label id="colour">Colour</Label>
          <Checkbox>Set</Checkbox>
        </Group>
        <TextField label="Title" className="drawn" />
      </>,
    );
    const group = find(el, '[role="group"]');

    expect(el.ownerDocument.getElementById(group.getAttribute("aria-labelledby") ?? "")?.textContent).toBe("Colour");
    expect(classes(group, "label")).toEqual(classes(find(el, ".drawn"), "label"));
  });
});

describe("the drop zone", () => {
  test("hears a file that the user drops on it, and names itself by its label", async () => {
    const dropped: string[] = [];
    const { el } = await draw(
      <DropZone
        onDrop={(event) => {
          for (const item of event.items) if (item.kind === "file") dropped.push(item.name);
        }}
      >
        <Text slot="label">Drop a file</Text>
      </DropZone>,
    );
    const zone = find(el, ".bk\\:border-dashed");
    const button = find(zone, "button");
    expect(el.ownerDocument.getElementById(button.getAttribute("aria-labelledby")?.split(" ").at(-1) ?? "")?.textContent).toBe("Drop a file");

    await step(() => {
      drop(zone, new File(["a,b\n"], "sales.csv", { type: "text/csv" }));
    });
    // React Aria reads the dropped file in a promise.
    await step(() => undefined);

    expect(dropped).toEqual(["sales.csv"]);
  });

  test("draws its label as a field's label, beside the classes given to it", async () => {
    const { el } = await draw(
      <>
        <DropZone className="plain">
          <Text slot="label">Drop a file</Text>
        </DropZone>
        <DropZone className="given">
          {() => (
            <Text slot="label" className="bk:sr-only">
              Drop a file
            </Text>
          )}
        </DropZone>
        <Label>Name</Label>
      </>,
    );
    const label = classes(el, "label");

    expect([classes(el, '.plain [slot="label"]'), classes(el, '.given [slot="label"]')]).toEqual([label, [...label, "bk:sr-only"]]);
  });
});

describe("the file trigger", () => {
  test("opens the browser's file picker from the button it holds, and hands on the files the user picks", async () => {
    const picked: string[] = [];
    const { el } = await draw(
      <FileTrigger
        acceptedFileTypes={["text/csv"]}
        onSelect={(files) => {
          for (const file of files ?? []) picked.push(file.name);
        }}
      >
        <Button>Choose a file</Button>
      </FileTrigger>,
    );
    const clicked = picking(new File(["a,b\n"], "sales.csv", { type: "text/csv" }));

    await step(() => {
      find(el, "button").click();
    });

    expect([clicked, picked]).toEqual([[{ accept: "text/csv", connected: true }], ["sales.csv"]]);
  });
});
