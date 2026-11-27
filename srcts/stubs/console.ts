// What a test does with an error or a warning that it sets off on purpose,
// which the setup of the tests would fail it on otherwise (./setup.ts).

import { format } from "node:util";
import { onTestFinished } from "vitest";

// Takes the errors that a test sets off on purpose off the console, those whose
// messages match one of the patterns, for the rest of the test.
// It gives the messages that it took, in order, which the test counts.
// A pattern reads the message as the console prints it, with the values of
// placeholders such as %s filled in, as in React's messages.
// Any other error reaches the console, and the setup fails the test on it.
export function expectedErrors(...patterns: readonly RegExp[]): string[] {
  return expected("error", patterns);
}

// Takes the warnings that a test sets off on purpose off the console, as
// expectedErrors() takes the errors.
export function expectedWarnings(...patterns: readonly RegExp[]): string[] {
  return expected("warn", patterns);
}

// Takes the messages of the level that match one of the patterns off the
// console, for the rest of the test, and gives those that it took.
function expected(level: "error" | "warn", patterns: readonly RegExp[]): string[] {
  const taken: string[] = [];
  const print = level === "error" ? console.error : console.warn;
  console[level] = (...args: unknown[]) => {
    const message = format(...args);
    if (patterns.some((pattern) => message.search(pattern) >= 0)) taken.push(message);
    else print(...args);
  };
  onTestFinished(() => {
    console[level] = print;
  });
  return taken;
}
