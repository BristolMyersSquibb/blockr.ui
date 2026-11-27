// What the stubs make happy-dom do as a browser does (./dom.ts), where the
// tests of the components show it only in passing.

import { expect, test } from "vitest";
import "./dom.ts";

test("fires selectionchange only as the selection moves, so that a listener that collapses the selection again ends, as in a browser", () => {
  const line = document.createElement("p");
  line.textContent = "hello";
  document.body.append(line);
  const text = line.firstChild;
  const selection = document.getSelection();
  let heard = 0;
  // Collapses the selection to the start of the text, as React Aria's date
  // segments collapse it into themselves.
  // The cap ends the test where every collapse fires the event.
  const collapseAgain = () => {
    heard += 1;
    if (heard < 10) selection?.collapse(text);
  };
  document.addEventListener("selectionchange", collapseAgain);
  selection?.collapse(text, 3);
  document.removeEventListener("selectionchange", collapseAgain);

  // The selection moves twice: to the offset 3, and then to the start of the
  // text, where the listener collapses it.
  expect([heard, selection?.anchorNode, selection?.anchorOffset]).toEqual([2, text, 0]);
});
