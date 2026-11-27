import { fileURLToPath } from "node:url";
import { defineConfig } from "vitest/config";

// The tests of this directory, which ../../stubs/setup.test.ts runs in a Vitest
// of their own, to see which of them the setup of the tests fails
// (../../stubs/setup.ts).
// They run with that setup, and after it with a setup of their own, which holds
// an exception, as a package's setup would (./allow.ts).
export default defineConfig({
  root: fileURLToPath(new URL("../../..", import.meta.url)),
  test: {
    environment: "happy-dom",
    include: ["srcts/testing/console/*.fixture.ts"],
    setupFiles: ["./srcts/stubs/setup.ts", "./srcts/testing/console/allow.ts"],
  },
});
