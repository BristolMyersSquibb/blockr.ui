// @vitest-environment node

// What a package's build lists of what each of its files imports up front,
// with staticImports() as blockr.ui ships it beside the build preset, as `@blockr/ui/build`,
// from the metafiles of its builds.
// R reads the list to name those files in the head of the page that it serves.

import { mkdir, mkdtemp, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath, pathToFileURL } from "node:url";
import * as esbuild from "esbuild";
import { afterEach, beforeEach, expect, test } from "vitest";
import type * as Build from "./index.ts";

const repo = resolve(dirname(fileURLToPath(import.meta.url)), "../..");

// The build preset and the list as blockr.ui ships them.
const { preset, staticImports } = (await import(pathToFileURL(join(repo, "inst/types/build/index.js")).href)) as typeof Build;

let root: string;

beforeEach(async () => {
  root = await mkdtemp(join(tmpdir(), "blockr-ui-preload-"));
});

afterEach(async () => {
  await rm(root, { recursive: true, force: true });
});

async function write(files: Record<string, string>): Promise<void> {
  for (const [path, content] of Object.entries(files)) {
    await mkdir(dirname(join(root, path)), { recursive: true });
    await writeFile(join(root, path), content);
  }
}

test("lists what each file of a package's build imports up front, by its name or as a file beside it", async () => {
  await write({
    "srcts/dock.ts": [
      'import { useState } from "react";',
      'import { Button } from "@blockr/ui";',
      'import { dockview } from "./vendor-1a2b3c4d.js";',
      'export const later = () => import("./vendor-9c8d7e6f.js");',
      "export const all = [useState, Button, dockview];",
      "",
    ].join("\n"),
    "srcts/dock.css": '@import "./vendor-5e6f7a8b.css";\n.dock {\n  color: red;\n}\n',
    "srcts/plain.ts": "export const plain = 1;\n",
  });

  // The package leaves its imports of its vendor files imports,
  // as a package imports the libraries that it bundles from its vendor file.
  const options = {
    ...preset,
    absWorkingDir: root,
    outdir: join(root, "inst/js"),
    external: ["./vendor-*"],
    metafile: true,
    logLevel: "silent",
  } satisfies esbuild.BuildOptions;
  const js = await esbuild.build({ ...options, entryPoints: ["srcts/dock.ts", "srcts/plain.ts"] });
  const css = await esbuild.build({ ...options, entryPoints: ["srcts/dock.css"] });

  const listed = staticImports([js.metafile, css.metafile, undefined]);

  // A file that imports nothing up front has no entry,
  // and a dynamic import is left out, which the page loads only as it runs the import.
  expect(Object.keys(listed)).toEqual(["dock.css", "dock.js"]);
  expect(listed["dock.css"]).toEqual(["./vendor-5e6f7a8b.css"]);
  expect([...(listed["dock.js"] ?? [])].sort()).toEqual(["./vendor-1a2b3c4d.js", "@blockr/ui", "react"]);
});

test("lists the files of shared code that a build splits off, by their paths from the file that imports them", async () => {
  await write({
    "srcts/shared.ts": 'import { useState } from "react";\nexport const shared = () => useState;\n',
    "srcts/later.ts": "export const later = 2;\n",
    "srcts/one.ts": 'import { shared } from "./shared.ts";\nexport const one = shared;\nexport const load = () => import("./later.ts");\n',
    "srcts/two.ts": 'import { shared } from "./shared.ts";\nexport const two = shared;\nexport const load = () => import("./later.ts");\n',
  });

  const { metafile } = await esbuild.build({
    ...preset,
    absWorkingDir: root,
    entryPoints: ["srcts/one.ts", "srcts/two.ts"],
    outdir: join(root, "inst/js"),
    splitting: true,
    chunkNames: "shared-[hash]",
    metafile: true,
    logLevel: "silent",
  });

  const listed = staticImports([metafile]);

  // The two entries import the file of the code that they share up front,
  // and that file imports React.
  // The file that they load as they run their dynamic imports is left out.
  const chunks = Object.keys(listed).filter((file) => file.startsWith("shared-"));
  expect(chunks).toHaveLength(1);
  const [chunk = ""] = chunks;
  expect(listed[chunk]).toEqual(["react"]);
  expect(listed["one.js"]).toEqual([`./${chunk}`]);
  expect(listed["two.js"]).toEqual([`./${chunk}`]);
});
