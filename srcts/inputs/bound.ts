// The interface through which an input of blockr.ui takes the value that it
// shows and submits, its prop `bound`
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.7.3#the-interface-bound).
// Core's useValue() returns one, which binds a block's state arg or a board
// option's value, and a test makes one with stubBound() of @blockr/ui/stubs.
// The interface names no host, no block and no board option, so the inputs
// hold nothing of core's.
// What implements it for a host keeps the contract's rules, as core's binding
// does
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.7#the-contract):
//
// 1. The binding holds a value of the host's, and gives it to the input as
//    `value`.
// 2. The input calls commit() as the user picks, type() as the user types,
//    and flush() as the user presses Enter or leaves the input.
// 3. The binding waits for the user's pause before it submits what the user
//    typed.
// 4. While the user types, the binding gives the input no value that the host
//    announces.
// 5. Where the host refuses a submit, the binding gives the input why, as
//    `error`, and the input shows it below itself.
//
// So every input keeps the rules alike, whoever implements Bound for a host.
// The interface leaves out the release() of core's binding, which core's hook
// calls as the input unmounts, so that an input never ends a binding that its
// caller made.
// An input without a `bound` takes React Aria's value props instead.

// The plain JSON values that an input shows and submits.
// They are read-only, so that no input changes a value that its `bound` holds
// in place.
export type Json = null | boolean | number | string | readonly Json[] | { readonly [key: string]: Json };

export interface Bound {
  // The value that the binding last gave the input to show.
  readonly value: Json | undefined;
  // Why the last submit was refused, which the input shows below itself.
  readonly error: string | null;
  // The value as it is held now.
  get(): Json | undefined;
  // Submits a pick at once, and resolves with whether it was taken.
  commit(value: Json): Promise<boolean>;
  // Clears the value, as for an arg without a default.
  unset(): Promise<boolean>;
  // Takes what the user typed, which is submitted once the user pauses.
  type(value: Json | undefined): void;
  // Submits what the user typed at once, as on Enter or as the user leaves the
  // input.
  flush(): void;
}
