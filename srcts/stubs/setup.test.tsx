// The setup that fails a test on an error or a warning that it prints
// (./setup.ts), with which blockr.ui's own tests run too (../../vitest.config.ts),
// and expectedErrors(), with which a test takes the errors that it sets off on
// purpose off the console (./console.ts).
// The first test runs the tests of ../testing/console in a Vitest of their own,
// with the setup, and reads which of them the setup fails, and why.

import { execFile } from "node:child_process";
import { mkdir, mkdtemp, readFile, rm } from "node:fs/promises";
import { basename, dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { promisify } from "node:util";
import { expect, test } from "vitest";
import { draw, expectedErrors } from "./index.ts";

const repo = resolve(dirname(fileURLToPath(import.meta.url)), "../..");

// The parts of the report of Vitest's reporter json that the test reads.
interface Report {
  readonly testResults: readonly {
    readonly name: string;
    readonly status: string;
    readonly message: string;
    readonly assertionResults: readonly { readonly title: string; readonly status: string; readonly failureMessages: readonly string[] }[];
  }[];
}

// Runs the tests of ../testing/console in a Vitest of their own, and gives its
// report.
// The run fails, since some of the tests fail, as they should.
// It runs without the variables that this Vitest sets for its workers, such as
// VITEST_WORKER_ID, so that it runs as a Vitest of its own.
async function runFixtures(): Promise<Report> {
  const scratch = join(repo, "node_modules", ".tmp");
  await mkdir(scratch, { recursive: true });
  const dir = await mkdtemp(join(scratch, "blockr-ui-setup-"));
  const env = Object.fromEntries(Object.entries(process.env).filter(([name]) => !name.startsWith("VITEST")));
  try {
    const run = promisify(execFile)(
      process.execPath,
      [
        join(repo, "node_modules", "vitest", "vitest.mjs"),
        "run",
        "--config",
        join(repo, "srcts", "testing", "console", "vitest.config.ts"),
        "--reporter=json",
        `--outputFile=${join(dir, "report.json")}`,
      ],
      { cwd: repo, env },
    );
    await run.catch(() => undefined);
    return JSON.parse(await readFile(join(dir, "report.json"), "utf8")) as Report;
  } finally {
    await rm(dir, { recursive: true, force: true });
  }
}

// The message of an error that a test failed with, without its stack.
function withoutStack(failure: string): string {
  return failure.split("\n    at ")[0] ?? failure;
}

test("fails a test that prints an error or a warning that it did not set off on purpose, and a file that prints one outside its tests", async () => {
  const report = await runFixtures();
  const files = Object.fromEntries(report.testResults.map((file) => [basename(file.name), file]));
  const results = (name: string) =>
    files[name]?.assertionResults.map(({ title, status, failureMessages }) => [title, status, failureMessages.map(withoutStack)]);
  const heading = "Error: The test printed what it did not set off on purpose:";

  expect(results("tests.fixture.ts")).toEqual([
    ["prints nothing", "passed", []],
    ["prints an error", "failed", [`${heading}\nconsole.error: An error that the test did not set off on purpose`]],
    // The failure gives the first line of each message.
    ["prints a warning", "failed", [`${heading}\nconsole.warn: A warning`]],
    ["takes the errors that it sets off on purpose off the console", "passed", []],
    ["prints an error other than those that it sets off on purpose", "failed", [`${heading}\nconsole.error: An error that the test does not expect`]],
    // The setup of the package holds the error as an exception (../testing/console/allow.ts).
    ["prints an error that the setup of the package allows", "passed", []],
    ["prints an error after the test, in the hook", "failed", [`${heading}\nconsole.error: An error after the test`]],
  ]);
  // The warning that the file prints as it loads fails the file, after its
  // test, which passes.
  expect(results("loading.fixture.ts")).toEqual([["prints nothing", "passed", []]]);
  expect(files["loading.fixture.ts"]?.message).toBe("The file printed outside its tests:\nconsole.warn: A warning as the file loads");
}, 60_000);

test("takes only the errors that match its patterns off the console, in order, and passes every other error on", () => {
  const others = expectedErrors(/^Another/);
  const taken = expectedErrors(/^Set off on purpose/, /twice/g);
  console.error("Set off on purpose, %s", "once");
  console.error("Another error");
  console.error("Set off %s", "twice");
  console.error("Set off %s again", "twice");

  // A pattern with the flag g matches each message from its start.
  expect([taken, others]).toEqual([["Set off on purpose, once", "Set off twice", "Set off twice again"], ["Another error"]]);
});

test("draws the element search as a plain HTMLElement, as browsers do, so that React warns of no unknown element", () => {
  const { el } = draw(
    <search>
      <input type="search" aria-label="Find" />
    </search>,
  );
  const search = el.firstElementChild;

  expect([search?.localName, search instanceof HTMLElement, search instanceof HTMLUnknownElement]).toEqual(["search", true, false]);
});
