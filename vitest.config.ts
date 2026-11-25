import { fileURLToPath } from "node:url";
import { defineConfig } from "vitest/config";

// A test that loads a module that the build preset bundled resolves the module's imports,
// where the page resolves them through its import map.
// It resolves the imports of blockr.ui's components to their source,
// and those of React and of React Aria to the copies in node_modules.
const source = (path: string): string => fileURLToPath(new URL(path, import.meta.url));

export default defineConfig({
  resolve: {
    alias: [{ find: /^@blockr\/ui$/, replacement: source("./srcts/index.ts") }],
  },
  test: {
    environment: "happy-dom",
    include: ["srcts/**/*.test.{ts,tsx}"],
    // A test that bundles the whole of React Aria takes several seconds,
    // and longer while the other test files run beside it.
    testTimeout: 60_000,
    hookTimeout: 60_000,
    // Every run measures the coverage of the tests, `npm test` included.
    // The typescript job of blockr.ci uploads coverage/lcov.info to Codecov.
    // The lcov reporter writes that file, and an HTML report beside it in coverage/lcov-report.
    // Istanbul counts the code of the sources in srcts alone.
    // V8 counts all the code that runs, TypeScript's compiler included.
    // With `include` set, a source in srcts that no test imports counts as uncovered.
    // Vitest leaves out the test files by itself.
    // The `exclude` option leaves out the helpers of the tests, in srcts/testing.
    coverage: {
      enabled: true,
      provider: "istanbul",
      reporter: ["text", "lcov"],
      include: ["srcts/**/*.{ts,tsx}"],
      exclude: ["srcts/testing/**"],
    },
  },
});
