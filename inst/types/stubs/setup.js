// srcts/stubs/setup.ts
import { format } from "node:util";
import HTMLElementConfig from "happy-dom/lib/config/HTMLElementConfig.js";
import { afterAll, afterEach, beforeEach } from "vitest";
var elements = HTMLElementConfig;
elements.search ??= HTMLElementConfig.section;
var allowed = [];
var running = false;
var printed = [];
var outside = [];
function allowMessage(check) {
  allowed.push(check);
}
for (const level of ["error", "warn"]) {
  const print = console[level].bind(console);
  console[level] = (...args) => {
    const message = format(...args);
    if (!allowed.some((check) => check(message))) {
      (running ? printed : outside).push(`console.${level}: ${message.split("\n")[0] ?? message}`);
    }
    print(...args);
  };
}
function fail(heading, messages) {
  if (messages.length > 0) throw new Error(`${heading}:
${messages.splice(0).join("\n")}`);
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
export {
  allowMessage
};
