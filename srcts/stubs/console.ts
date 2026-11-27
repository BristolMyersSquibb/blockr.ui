// What a test does with an error that it sets off on purpose, which the setup
// of the tests would fail it on otherwise (./setup.ts).

import { format } from "node:util";
import { onTestFinished } from "vitest";

// Takes the errors that a test sets off on purpose off the console, those whose
// messages match one of the patterns, for the rest of the test, and gives the
// messages that it took, in order, which the test counts.
// A pattern reads the message as the console prints it, with the values of
// placeholders such as %s filled in, as in React's messages.
// Any other error reaches the console, and the setup fails the test on it.
export function expectedErrors(...patterns: readonly RegExp[]): string[] {
  const taken: string[] = [];
  const print = console.error;
  console.error = (...args: unknown[]) => {
    const message = format(...args);
    if (patterns.some((pattern) => message.search(pattern) >= 0)) taken.push(message);
    else print(...args);
  };
  onTestFinished(() => {
    console.error = print;
  });
  return taken;
}
