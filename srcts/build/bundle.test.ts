// What blockr.ui ships in inst/js, run as the page runs it.
// The page's import map resolves each of React's entry points to its ES module in inst/js.
// Here vi.doMock() does so for what the tests import after it, React Aria's files among them.

import { join, resolve, dirname } from "node:path";
import { fileURLToPath, pathToFileURL } from "node:url";
import { Pin } from "lucide";
import { expect, test, vi } from "vitest";
import { COMPONENTS, importFile, REACT_ENTRIES } from "../import-map/index.ts";

const repo = resolve(dirname(fileURLToPath(import.meta.url)), "../..");

// Imports a file of inst/js as blockr.ui ships it.
async function shipped<T>(file: string): Promise<T> {
  return (await import(pathToFileURL(join(repo, "inst", "js", file)).href)) as T;
}

for (const name of REACT_ENTRIES) vi.doMock(name, () => shipped(importFile(name)));

const react = await shipped<typeof import("react")>(importFile("react"));
const client = await shipped<typeof import("react-dom/client")>(importFile("react-dom/client"));

// Draws an element with the React of React's file, in a transition,
// which React draws in a later task.
// On a busy machine, drawing can take longer than a fixed wait.
// So a test retries its checks for up to a second.
function draw(node: import("react").ReactElement): { el: HTMLElement; unmount: () => void } {
  const el = document.createElement("div");
  document.body.append(el);
  const view = client.createRoot(el);
  react.startTransition(() => {
    view.render(node);
  });
  return {
    el,
    unmount: () => {
      view.unmount();
      el.remove();
    },
  };
}

test("React's entry points read one React", async () => {
  // A hook of react.js works in a component that react-dom-client.js draws,
  // which it does only where both read the same React.
  const Counter = () => react.createElement("span", null, String(react.useState(41)[0] + 1));
  const drawn = draw(react.createElement(Counter));

  await vi.waitFor(() => {
    expect(drawn.el.textContent).toBe("42");
  });
  drawn.unmount();
});

test("React Aria's files draw on the React of React's file, and keep their state there", async () => {
  const aria = await shipped<typeof import("react-aria-components")>(importFile("react-aria-components"));
  const stately = await shipped<typeof import("react-stately")>(importFile("react-stately"));

  // A component of React Aria's, holding state from react-stately, which React keeps.
  // The hooks would fail where React Aria's files read a React other than react-dom-client.js does.
  const Toggle = () => {
    const state = stately.useToggleState();
    return react.createElement(aria.Button, { onPress: () => { state.toggle(); } }, state.isSelected ? "on" : "off");
  };
  const drawn = draw(react.createElement(Toggle));

  await vi.waitFor(() => {
    expect(drawn.el.querySelector("button")?.textContent).toBe("off");
  });
  const button = drawn.el.querySelector("button");
  expect(button?.className).toBe("react-aria-Button");
  button?.click();
  await vi.waitFor(() => {
    expect(button?.textContent).toBe("on");
  });
  drawn.unmount();
});

test("The file of the components draws an icon on the React of React's file, as the markup of iconMarkup() has it", async () => {
  const ui = await shipped<typeof import("../index.ts")>(importFile(COMPONENTS));
  const drawn = draw(react.createElement(ui.Icon, { icon: Pin }));
  const markup = document.createElement("div");
  markup.innerHTML = ui.iconMarkup(Pin);

  await vi.waitFor(() => {
    expect(drawn.el.innerHTML).toBe(markup.innerHTML);
  });
  expect(drawn.el.querySelectorAll("svg > path")).toHaveLength(Pin.length);
  drawn.unmount();
});
