// The stub of Bound: what it gives an input through useStubBound(), what it
// records of what the input does, and how it takes or refuses a submit.

import { act } from "react";
import type { ReactElement } from "react";
import { expect, test } from "vitest";
import { TextField } from "../inputs/index.ts";
import type { Bound } from "../inputs/index.ts";
import { draw, labelled } from "./index.ts";
import { stubBound, useStubBound } from "./bound.ts";
import type { StubBound } from "./bound.ts";

// A component that takes the stub's Bound through the hook, and hands it to
// the test, which calls the methods of an input on it.
function bind(stub: StubBound): { readonly bound: () => Bound } {
  let last: Bound | undefined;
  function Probe(): ReactElement | null {
    last = useStubBound(stub);
    return null;
  }
  draw(<Probe />);
  return {
    bound: () => {
      if (last === undefined) throw new Error("The probe drew nothing.");
      return last;
    },
  };
}

test("gives an input its value, and a Bound of its own for each value, so that the input draws again as the test sets another", () => {
  const stub = stubBound("Sales");
  function Field(): ReactElement {
    return <TextField bound={useStubBound(stub)} label="Label" />;
  }
  const { el } = draw(<Field />);
  expect(labelled(el, "Label").value).toBe("Sales");

  act(() => {
    stub.set("Costs");
  });
  expect(labelled(el, "Label").value).toBe("Costs");
});

test("records each value that the input commits, unsets, types and flushes, and takes each submit as its value", async () => {
  const stub = stubBound("a");
  const { bound } = bind(stub);

  expect(await act(() => bound().commit("b"))).toBe(true);
  expect([stub.value, bound().value]).toEqual(["b", "b"]);

  act(() => {
    bound().type("bc");
  });
  expect([stub.value, bound().get()]).toEqual(["b", "bc"]);
  act(() => {
    bound().flush();
    bound().flush();
  });
  expect([stub.value, bound().get()]).toEqual(["bc", "bc"]);

  expect(await act(() => bound().unset())).toBe(true);
  expect([stub.value, stub.committed, stub.typed, stub.flushed]).toEqual([undefined, ["b", undefined], ["bc"], ["bc"]]);
});

test("refuses every submit with the error given, leaving its value, until the test takes them again", async () => {
  const stub = stubBound("a");
  const { bound } = bind(stub);

  stub.refuse("Not this one.");
  expect(await act(() => bound().commit("b"))).toBe(false);
  expect([stub.value, stub.error, bound().error]).toEqual(["a", "Not this one.", "Not this one."]);

  stub.refuse(null);
  expect(await act(() => bound().commit("c"))).toBe(true);
  expect([stub.value, stub.error, bound().error]).toEqual(["c", null, null]);
  expect(stub.committed).toEqual(["b", "c"]);
});

test("is no Bound itself, so that an input takes it only through useStubBound()", () => {
  const stub = stubBound("a");

  // @ts-expect-error -- the stub lacks the methods that an input calls, such as commit().
  const bound: Bound = stub;
  expect("commit" in bound).toBe(false);
});
