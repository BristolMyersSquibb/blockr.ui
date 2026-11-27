// The setup that fails a test on an error or a warning that the test prints to
// the console, through console.error or console.warn.
// A package's vitest.config.ts lists it in `setupFiles`, as
// `@blockr/ui/stubs/setup`, before its own setup files, and blockr.ui's lists
// it as ./srcts/stubs/setup.ts.
// Vitest shows what a passing test prints only on request, so that such a
// message would go unseen.
// The messages include React's warnings of an update outside act(): the stubs
// turn on React's act environment, in which React expects each update that a
// test sets off to run inside act() (./react.ts).
//
// After each test, the setup fails the test where anything printed an error or
// a warning while it ran, its hooks included.
// Vitest runs the hooks after a test in the reverse order of their
// registration, and the setup registers its hook first.
// So the setup's check runs after the hooks of the test file and of the setup
// files listed after it.
// After the last test of a file, the setup fails the file where anything
// printed one outside a test, as the file loaded or between two tests.
// Every message still reaches the console, so that Vitest shows it with the
// test that fails.
// A test names the errors and the warnings that it sets off on purpose with
// expectedErrors() and expectedWarnings() (./console.ts), which take them off
// the console before the setup sees them.
// A package that sets off a message that it cannot fix yet, such as an error
// that another package fixes later, holds it as an exception with
// allowMessage(), in a setup file of its own.
//
// The setup loads nothing of React, unlike the stubs (./index.ts).
// React DOM reads the hook of React's developer tools as it loads, and some
// tests set that hook first, as core's tests do to count React's commits.
// Those tests can run with the setup too.
//
// The setup also gives happy-dom the element search, which browsers draw as a
// plain HTMLElement, but which happy-dom does not know
// (https://github.com/capricorn86/happy-dom/issues/2640).
// React warns of an element that the DOM does not know, as where a UI draws a
// search, and the setup would fail the test on that warning.
// The entry is that of section, which browsers draw alike.
// The entry goes once a release of happy-dom knows the element.

import { format } from "node:util";
import HTMLElementConfig from "happy-dom/lib/config/HTMLElementConfig.js";
import { afterAll, afterEach, beforeEach } from "vitest";

const elements: Record<string, unknown> = HTMLElementConfig;
elements.search ??= HTMLElementConfig.section;

// The checks of the messages that every test may print, which allowMessage()
// adds.
const allowed: ((message: string) => boolean)[] = [];

// Whether a test runs, from the setup's hook that runs before it to the one
// that runs after it.
let running = false;

// The messages that the running test printed, and those printed outside a
// test, each by its level and its first line.
const printed: string[] = [];
const outside: string[] = [];

// Lets every message that the check accepts pass, in every test from here on,
// as an exception that a package holds for the moment.
// The check reads the message as the console prints it, with the values of
// placeholders such as %s filled in.
// It runs as the message is printed, so that it can read where the message came
// from in the stack that new Error() gives.
export function allowMessage(check: (message: string) => boolean): void {
  allowed.push(check);
}

for (const level of ["error", "warn"] as const) {
  const print = console[level].bind(console);
  console[level] = (...args: unknown[]) => {
    const message = format(...args);
    if (!allowed.some((check) => check(message))) {
      (running ? printed : outside).push(`console.${level}: ${message.split("\n")[0] ?? message}`);
    }
    print(...args);
  };
}

// Fails with the messages, under the heading, where there are any, and forgets
// them.
function fail(heading: string, messages: string[]): void {
  if (messages.length > 0) throw new Error(`${heading}:\n${messages.splice(0).join("\n")}`);
}

beforeEach(() => {
  running = true;
});

afterEach(() => {
  running = false;
  fail("The test printed what it did not set off on purpose", printed);
});

afterAll(() => {
  fail("The file printed outside its tests", outside);
});
