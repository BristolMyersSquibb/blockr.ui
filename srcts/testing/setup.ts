// What runs around every test, as vitest.config.ts names this file.
// After each test, it unmounts every React root that the test made, and then
// empties the page.
// React runs the cleanups of the effects as a root unmounts.
// These clear the timers that React Aria set, such as the timer of a dialog.
// Left pending, such a timer can run after Vitest has torn down the page, and
// its error fails the run.
// Without the unmount, a tooltip that a test left open would close as one of
// a later test opens.
// A mock of react-dom/client records each root that createRoot() makes, so
// that the roots that the stubs' draw() makes unmount too.
// A test file's afterEach() runs before this file's, so a test file must not
// empty the page.
// Once the page is empty, a root whose overlay is still open fails to unmount.
// Loading the stubs makes happy-dom do as a browser does where blockr.ui's
// components need it (../stubs/dom.ts).

import "../stubs/index.ts";
import { act } from "react";
import type * as Client from "react-dom/client";
import { afterEach, vi } from "vitest";

// The roots that createRoot() made during the test.
// Vitest moves vi.mock() above the imports.
// Vitest moves vi.hoisted() above vi.mock() as well, so that the mock can reach
// the set.
const roots = vi.hoisted(() => new Set<Client.Root>());

vi.mock("react-dom/client", async (importOriginal) => {
  const client = await importOriginal<typeof Client>();
  return {
    ...client,
    createRoot: (...args: Parameters<typeof client.createRoot>): Client.Root => {
      const root = client.createRoot(...args);
      roots.add(root);
      return root;
    },
  };
});

afterEach(() => {
  // Unmounting a root that the test unmounted already does nothing.
  // The page empties even where a root fails to unmount, so that the next test
  // starts afresh.
  try {
    act(() => {
      for (const root of roots) root.unmount();
    });
  } finally {
    roots.clear();
    if (typeof document !== "undefined") document.body.replaceChildren();
  }
});
