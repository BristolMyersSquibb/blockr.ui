// The stub of Bound, the interface through which an input of blockr.ui takes
// the value that it shows and submits
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.7.3#testing).
// It holds a value, records each value that an input commits, types and
// flushes, and refuses a submit with an error where a test asks it to.
// So blockr.ui tests its inputs without core, and a package tests a component
// apart from any host.
//
// The stub plays a host that takes every submit at once, with no pause:
//
// 1. Its commit() submits the value given, and its unset() submits no value.
// 2. Its type() holds what the user typed, which its get() then gives, and
//    submits nothing.
// 3. Its flush() submits what the user typed, where the user typed anything
//    since the last submit.
// 4. A submit that the stub takes becomes its value, and clears its error.
//    A submit that it refuses leaves its value, and gives the refusal as its
//    error.
// 5. Its set() gives the input another value, as a host announces one.
//
// The rules of a binding, such as the pause before what the user typed is
// submitted, are core's, which core's tests check with core's own stubs.
//
// An input reads the stub's value and error as it draws.
// A test whose input draws in a component of its own takes the stub through
// useStubBound(), which draws the component again as the stub's value or
// error changes, as core's useValue() draws it as a host announces a change.

import { useSyncExternalStore } from "react";
import type { Bound, Json } from "../inputs/index.ts";

export interface StubBound extends Bound {
  // Each value that the input committed, in order, and undefined for each
  // unset(), whether the stub took it or refused it.
  readonly committed: readonly (Json | undefined)[];
  // Each value that the input typed, in order.
  readonly typed: readonly (Json | undefined)[];
  // Each value that a flush() submitted, in order: what the user had typed.
  readonly flushed: readonly (Json | undefined)[];
  // Gives the input another value, as a host announces one.
  set(value: Json | undefined): void;
  // Refuses every submit from now on with the error given, or takes every
  // submit again where given null.
  refuse(error: string | null): void;
  // Calls the listener whenever the value or the error changes, and returns
  // what stops it.
  // It is a function of its own, which reads no `this`, so that a caller may
  // pass it on, as useStubBound() passes it to React.
  readonly subscribe: (listener: () => void) => () => void;
}

// A stub that holds the value given, or none.
export function stubBound(value?: Json): StubBound {
  let held: Json | undefined = value;
  let error: string | null = null;
  let refusal: string | null = null;
  // What the user typed since the last submit, if anything.
  let pending: { readonly value: Json | undefined } | null = null;
  const committed: (Json | undefined)[] = [];
  const typed: (Json | undefined)[] = [];
  const flushed: (Json | undefined)[] = [];
  const listeners = new Set<() => void>();
  const changed = (): void => {
    for (const listener of [...listeners]) listener();
  };

  const submit = (next: Json | undefined): Promise<boolean> => {
    pending = null;
    if (refusal !== null) {
      error = refusal;
      changed();
      return Promise.resolve(false);
    }
    held = next;
    error = null;
    changed();
    return Promise.resolve(true);
  };

  return {
    get value() {
      return held;
    },
    get error() {
      return error;
    },
    committed,
    typed,
    flushed,
    get: () => (pending === null ? held : pending.value),
    commit: (next) => {
      committed.push(next);
      return submit(next);
    },
    unset: () => {
      committed.push(undefined);
      return submit(undefined);
    },
    type: (next) => {
      typed.push(next);
      pending = { value: next };
    },
    flush: () => {
      if (pending === null) return;
      flushed.push(pending.value);
      void submit(pending.value);
    },
    set: (next) => {
      held = next;
      changed();
    },
    refuse: (next) => {
      refusal = next;
    },
    subscribe: (listener) => {
      listeners.add(listener);
      return () => listeners.delete(listener);
    },
  };
}

// The Bound that an input takes from the stub, as the stub holds it while the
// component draws.
// It is an object of its own for each value and error that the stub holds, as
// core's useValue() gives, and the component draws again as either changes.
export function useStubBound(stub: StubBound): Bound {
  return useSyncExternalStore(stub.subscribe, () => snapshotOf(stub));
}

// The last snapshot of each stub, which a component reads again until the
// stub's value or error changes.
const snapshots = new WeakMap<StubBound, Bound>();

function snapshotOf(stub: StubBound): Bound {
  const last = snapshots.get(stub);
  if (last !== undefined && Object.is(last.value, stub.value) && last.error === stub.error) return last;
  const next: Bound = {
    value: stub.value,
    error: stub.error,
    get: () => stub.get(),
    commit: (value) => stub.commit(value),
    unset: () => stub.unset(),
    type: (value) => {
      stub.type(value);
    },
    flush: () => {
      stub.flush();
    },
  };
  snapshots.set(stub, next);
  return next;
}
