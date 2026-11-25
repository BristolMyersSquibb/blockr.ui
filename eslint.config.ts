import eslint from "@eslint/js";
import { defineConfig, globalIgnores } from "eslint/config";
import tseslint from "typescript-eslint";

export default defineConfig(
  globalIgnores(["inst/", "docs/", "coverage/"]),
  eslint.configs.recommended,
  tseslint.configs.strictTypeChecked,
  tseslint.configs.stylisticTypeChecked,
  {
    languageOptions: {
      parserOptions: {
        projectService: true,
        tsconfigRootDir: import.meta.dirname,
      },
    },
  },
  {
    // Each directory of srcts is a module, and its index.ts names what it
    // offers. Every file outside the module, a file directly in srcts
    // included, imports from that file alone. The directory testing is
    // exempt, since it holds only test helpers.
    files: ["srcts/**/*.{ts,tsx}"],
    rules: {
      "no-restricted-imports": [
        "error",
        {
          patterns: [
            {
              regex: String.raw`^(\.\.?/)+(?!\.|testing/)[^/]+/(?!index\.ts$)`,
              message: "Import another module through its index.ts.",
            },
          ],
        },
      ],
    },
  },
);
