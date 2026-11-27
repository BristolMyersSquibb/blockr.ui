// Tests that print to the console, which ../../stubs/setup.test.ts runs to see
// which of them the setup of the tests fails (./vitest.config.ts).

import { afterEach, describe, expect, test } from "vitest";
import { expectedErrors, expectedWarnings } from "../../stubs/index.ts";

test("prints nothing", () => {
  expect(document.body.children).toHaveLength(0);
});

test("prints an error", () => {
  console.error("An error that %s did not set off on purpose", "the test");
});

test("prints a warning", () => {
  console.warn("A warning\nwith a second line");
});

test("takes the errors that it sets off on purpose off the console", () => {
  const taken = expectedErrors(/on purpose/);
  console.error("An error set off on %s", "purpose");
  console.error("Another error set off on purpose");
  expect(taken).toEqual(["An error set off on purpose", "Another error set off on purpose"]);
});

test("prints an error other than those that it sets off on purpose", () => {
  expectedErrors(/on purpose/);
  console.error("An error that the test does not expect");
});

test("prints a warning other than those that it sets off on purpose", () => {
  expectedWarnings(/on purpose/);
  console.warn("A warning that the test does not expect");
});

test("prints an error that the setup of the package allows", () => {
  console.error("An error that another package fixes later, as %s", "it says");
});

describe("a hook that runs after a test", () => {
  afterEach(() => {
    console.error("An error after the test");
  });

  test("prints an error after the test, in the hook", () => {
    expect(document.body.children).toHaveLength(0);
  });
});
