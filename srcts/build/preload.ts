// What each file that a build writes imports up front,
// which blockr.ui ships to packages beside the build preset, as `@blockr/ui/build`.
// A build writes it into preload.json beside its files.
// R reads that file, to name in the head of the page that it serves what the page imports as it starts,
// beside the modules that it names itself.
// A browser fetches a file that such a link names at once, as the page loads.
// It fetches what that file imports only as it runs the file, so R names those files too.
// The list holds, for each file that imports others up front, each of them:
// an import by name, which the page's import map resolves, as `react`,
// and a file beside it, by its path from the file, as `./vendor-3f2a9c1d.js`.
// A file that the same build wrote counts as well,
// such as a file of the code that esbuild splits off where entries share it.
// A stylesheet's `@import` of a file beside it counts too.
// A dynamic import is left out, since the page loads it only as it runs the import.
// So a package writes the file from the metafiles of its builds,
// as in `writeFile("inst/js/preload.json", JSON.stringify(staticImports(results.map((r) => r.metafile))))`.
// The file imports no npm package, as ./preset.ts does.

import { dirname, relative } from "node:path/posix";
import type { Metafile } from "esbuild";

// The static imports of each file that the builds of the metafiles given wrote,
// by the file's name, of the files that import any.
export function staticImports(metafiles: readonly (Metafile | undefined)[]): Record<string, string[]> {
  const files: [string, string[]][] = [];
  for (const metafile of metafiles) {
    for (const [path, output] of Object.entries(metafile?.outputs ?? {})) {
      const imports = output.imports.flatMap((entry) => {
        if (entry.kind !== "import-statement" && entry.kind !== "import-rule") return [];
        // The metafile gives a file that the build wrote by its path from the build's working directory.
        return [entry.external === true ? entry.path : `./${relative(dirname(path), entry.path)}`];
      });
      if (imports.length > 0) files.push([path.slice(path.lastIndexOf("/") + 1), [...new Set(imports)]]);
    }
  }
  return Object.fromEntries(files.sort(([a], [b]) => (a < b ? -1 : a > b ? 1 : 0)));
}
