// The stub of Bound, the interface through which an input of blockr.ui takes
// the value that it shows and submits
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.7.3#testing).
// It holds a value, records each value that an input commits, types and
// flushes, and refuses a submit with an error where a test asks it to.
// So blockr.ui tests its inputs without core, and a package tests a component
// apart from any host.
//
// A test holds the stub, and a component that the test draws binds an input
// to it through useStubBound(), as in
// `<ComboBox bound={useStubBound(fruit)} label="Fruit" />`, as a module's UI
// binds one to a host through core's useValue().
// The hook gives the input a Bound of its own for each value and error that
// the stub holds, so that the input draws again as either changes.
// The stub is no Bound itself, so that TypeScript refuses an input that takes
// the stub in place of what the hook gives, which would never draw again.
//
// The stub plays a host that takes every submit at once, with no pause:
//
// 1. The input's commit() submits the value given, and its unset() submits no
//    value.
// 2. The input's type() hands the stub what the user typed, which the input's
//    get() then gives, and submits nothing.
// 3. The input's flush() submits what the user typed, where the user typed
//    anything since the last submit.
// 4. A submit that the stub takes becomes its value, and clears its error.
//    A submit that it refuses leaves its value, and gives the refusal as its
//    error.
// 5. The test's set() gives the input another value, as a host announces one.
//
// The rules of a binding, such as the pause before what the user typed is
// submitted, are core's, which core's tests check with core's own stubs.

import { useSyncExternalStore } from "react";
import type { Bound, Json } from "../inputs/index.ts";

export interface StubBound {
  // The value that the stub holds, which it gives the input to show.
  readonly value: Json | undefined;
  // Why the stub refused the last submit, which the input shows below itself,
  // or null.
  readonly error: string | null;
  // Each value that the input committed, in order, and undefined for each
  // unset(), whether the stub took it or refused it.
  readonly committed: readonly (Json | undefined)[];
  // Each value that the input typed, in order.
  readonly typed: readonly (Json | undefined)[];
  // Each value that the input's flush() submitted, in order: what the user had
  // typed.
  readonly flushed: readonly (Json | undefined)[];
  // Gives the input another value, as a host announces one.
  set(value: Json | undefined): void;
  // Refuses every submit from now on with the error given, or takes every
  // submit again where given null.
  refuse(error: string | null): void;
}

// What useStubBound() reads of a stub, which the stub keeps from tests:
// the methods that an input calls, and what calls the listener given whenever
// the value or the error changes, and returns what stops it.
interface Internals {
  readonly methods: Omit<Bound, "value" | "error">;
  readonly subscribe: (listener: () => void) => () => void;
}

// The internals of each stub, by the stub.
const internals = new WeakMap<StubBound, Internals>();

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

  const stub: StubBound = {
    get value() {
      return held;
    },
    get error() {
      return error;
    },
    committed,
    typed,
    flushed,
    set: (next) => {
      held = next;
      changed();
    },
    refuse: (next) => {
      refusal = next;
    },
  };
  internals.set(stub, {
    methods: {
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
    },
    subscribe: (listener) => {
      listeners.add(listener);
      return () => listeners.delete(listener);
    },
  });
  return stub;
}

// The Bound through which an input takes the stub's value, which a component
// that a test draws passes the input, as a module's UI passes it what core's
// useValue() returns.
// It is an object of its own for each value and error that the stub holds, so
// that the component draws again as either changes.
export function useStubBound(stub: StubBound): Bound {
  const own = internals.get(stub);
  if (own === undefined) throw new Error("useStubBound() takes a stub that stubBound() made.");
  return useSyncExternalStore(own.subscribe, () => snapshotOf(stub, own));
}

// The last snapshot of each stub, which a component reads again until the
// stub's value or error changes.
const snapshots = new WeakMap<StubBound, Bound>();

function snapshotOf(stub: StubBound, own: Internals): Bound {
  const last = snapshots.get(stub);
  if (last !== undefined && Object.is(last.value, stub.value) && last.error === stub.error) return last;
  const next: Bound = { value: stub.value, error: stub.error, ...own.methods };
  snapshots.set(stub, next);
  return next;
}
