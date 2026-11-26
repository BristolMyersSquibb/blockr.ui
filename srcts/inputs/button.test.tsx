// The button of blockr.ui, React Aria's Button drawn with the theme variables: the ref
// that it takes.
// The rest, such as how the button takes a press, is React Aria's.

import { createRef } from "react";
import { createRoot } from "react-dom/client";
import { expect, test } from "vitest";
import { step } from "../stubs/index.ts";
import { Button } from "./index.ts";

test("gives its element to the ref given, until it unmounts", async () => {
  const ref = createRef<HTMLButtonElement>();
  const el = document.createElement("div");
  document.body.append(el);
  const root = createRoot(el);

  await step(() => {
    root.render(<Button ref={ref}>Save</Button>);
  });
  expect(ref.current).toBeInstanceOf(HTMLButtonElement);
  expect(ref.current).toBe(el.querySelector("button"));

  await step(() => {
    root.unmount();
  });
  expect(ref.current).toBeNull();
});
