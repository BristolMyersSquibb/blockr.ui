// A test file that prints a warning as it loads, outside its tests, which
// ../../stubs/setup.test.ts runs to see that the setup of the tests fails the
// file, but not its test (./vitest.config.ts).

import { expect, test } from "vitest";

console.warn("A warning as the file loads");

test("prints nothing", () => {
  expect(document.body.children).toHaveLength(0);
});
