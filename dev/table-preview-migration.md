# Migration: HTML table preview → blockr.ui (de-triplicate)

> **SUPERSEDED (2026-06-12):** a thorough review turned this lift-and-shift
> into a parity-preserving rewrite-to-spec. The authoritative spec is
> `_blockr.design/open/html-table-preview/` (phases 1–4). Key deltas vs this
> doc: dplyr/pillar are KEPT on purpose (remote tables + label preservation);
> input-id collision fixed; real lazy pagination (NA-parity-normalized window
> query, prototyped); JS lifecycle via `shiny:value` replaces the
> MutationObserver + 1.5s timer; tests required (incl. dbplyr). This file is
> kept for the original call-site inventory only.

**Status:** spec / handover. Not started.
**Owner:** (unassigned — pick up from here)
**Author of spec:** prior session, 2026-06-12

## Goal

Relocate the shared server-side paginated **HTML table preview** primitive to
`blockr.ui` as its single canonical home, delete the duplicated/mirrored copies
in `blockr.extra`, `blockr.dm`, and `blockr.bi`, and repoint every consumer at
`blockr.ui`. This kills a triplicated source file plus a verbatim CSS mirror —
the same drift hazard that motivated the move (pre-CRAN these copies are kept in
sync by a comment; post-CRAN they silently diverge).

Why blockr.ui: the table preview is a **mandatory rendering primitive** (a
widget that blocks are built on), not optional polish, so it belongs in the
load-bearing UI-primitives package. It already consumes blockr.ui's `--blockr-*`
design tokens. (Contrast with the scale-map/theme work, which is *optional* and
must NOT land in blockr.ui — see the separate blockr.theme thread.)

## Current state (verified 2026-06-12)

### The primitive — TRIPLICATED
`R/table-preview.R` exists as near-identical copies in **two** packages:
- `blockr.extra/R/table-preview.R` (canonical-ish: `table_preview_css()` is
  `@export`ed here with roxygen)
- `blockr.dm/R/table-preview.R` (same code, `table_preview_css()` NOT exported,
  no roxygen)

`diff` between them is **whitespace / lint reflow only** (multi-line arg
wrapping, one JS `var cl = header.classList` alias). **Behaviorally identical.**
The extra copy's file header even says: *"DUPLICATED: identical file lives in
blockr.extra/R/table-preview.R and blockr.dm/R/table-preview.R. Keep both in
sync until this moves to a shared package (blockr.core or blockr.ui)."*

Contents of `table-preview.R` (the unit that moves):
| Function | Role | Deps |
|---|---|---|
| `apply_table_sort(data, col, dir)` | sort a frame for a page | dplyr |
| `col_type_label(x)` | `<chr>`/`<dbl>`/… type tag | base |
| `format_column_inner(x, max_chars)` | pillar-formatted cell text | pillar |
| `build_html_table(...)` | the renderer (returns tagList incl. css+js) | shiny |
| `table_preview_css()` | **exported** base CSS `tags$style` | shiny |
| `table_sort_js()` / `table_pagination_js()` | inline JS | shiny |

### The 4th partial copy — bi's verbatim CSS mirror
`blockr.bi/R/html-table-block.R` has a DIFFERENT widget (`html_table()`, the
section-aware summary/pivot renderer) that **reuses the `.blockr-table` class
names + base CSS**. It pulls the CSS via:
```r
shared_css <- if (requireNamespace("blockr.extra", quietly = TRUE)) {
  blockr.extra::table_preview_css()
} else {
  htmltools::tags$style(htmltools::HTML(html_table_shared_css_fallback()))  # verbatim subset
}
```
`html_table_shared_css_fallback()` (R/html-table-block.R ~L549) is a
hand-mirrored subset of `table_preview_css()` — delete it after the move.

### Glue (stays mostly in blockr.extra)
`blockr.extra/R/table-preview-glue.R`:
- `html_table_result(result, block, session)` + `html_table_render(result,
  session, page_size)` — generic renderUI wrappers (read board option
  `page_size`, call `build_html_table`). **Reusable → move to blockr.ui.**
- `render_block_output_with_option` / `render_block_ui_with_option` + the S3
  overrides `block_output.data_block`, `block_ui.data_block`,
  `block_output.transform_block`, `block_ui.transform_block`. These implement
  the **opt-in `options(blockr.html_table_preview = TRUE)` behavior** (swap DT
  for the html preview globally). This is blockr.extra policy/wiring + relies on
  the `.onLoad` capture of original methods in `blockr.extra/R/zzz.R`
  (`get_original_block_output_data_block()` etc.). **Stays in blockr.extra.**

### Consumers (monorepo-wide), and how they call in
| Pkg | File | Uses | Currently resolves to |
|---|---|---|---|
| blockr.extra | table-preview-glue.R | `build_html_table`, sort | own copy |
| blockr.extra | dynamic-output.R:125 | `build_html_table` | own copy |
| blockr.dm | dm-block.R:446,460 | `apply_table_sort`, `build_html_table` | own copy |
| blockr.bi | html-table-block.R:112 | `table_preview_css` | `blockr.extra::` (+ fallback) |
| blockr.tidymodels | predict-block.R:55 | `html_table_result` | **`blockr.core::` — BROKEN** |

### ⚠ Latent bug to fix as part of this
`blockr.tidymodels/R/predict-block.R:55` calls
`blockr.core::html_table_result(result, x, session)`. That function exists in
**neither** blockr.core's source **nor** any NAMESPACE (grep-confirmed).
blockr.tidymodels only Imports blockr.core. So this call errors at runtime today
(dead/broken path). The move is the natural place to repoint it at
`blockr.ui::html_table_result()` and add the blockr.ui dependency.

## Target design

### blockr.ui becomes the canonical home
- Add `R/table-preview.R` = the extra copy verbatim (the **exported** variant).
- Add `html_table_render()` + `html_table_result()` (moved from extra glue).
- **Export** from blockr.ui (these are called cross-package): `table_preview_css`,
  `build_html_table`, `apply_table_sort`, `html_table_render`,
  `html_table_result`. Keep `col_type_label`, `format_column_inner`,
  `table_sort_js`, `table_pagination_js` internal.
- DESCRIPTION: add `Imports: dplyr, pillar`. Bump Version (0.0.0.9007 → .9008).

> **Decision point — dependency weight.** blockr.ui currently imports only
> core/htmltools/rlang/shiny. This adds **dplyr + pillar**. dplyr is heavy-ish
> but already ubiquitous across the stack; pillar is small. Accept it (simplest,
> keeps the primitive whole), OR the fallback alternative = put the primitive in
> **blockr.core** instead (every consumer already imports core → zero new
> dependency edges, and it matches the existing `blockr.core::html_table_result`
> call). Recommendation: **blockr.ui** on altitude grounds (core should not own a
> concrete table widget), accepting the two deps. Confirm with maintainer before
> starting if unsure — this is the one reversible-but-annoying choice.

### Per-package edits
1. **blockr.ui** — add files + exports + deps as above; `roxygen2::roxygenise()`;
   bump Version.
2. **blockr.extra**
   - DELETE `R/table-preview.R`.
   - `table-preview-glue.R`: remove `html_table_result`/`html_table_render`
     (moved); keep the S3 overrides + `render_block_*_with_option`, but have them
     call `blockr.ui::html_table_result(...)`.
   - `dynamic-output.R:125`: `build_html_table` → `blockr.ui::build_html_table`.
   - DESCRIPTION: add `Imports: blockr.ui`. (Leave dplyr/pillar — still used
     elsewhere; verify.) Bump Version.
   - Re-roxygenise (the `@export`s of the removed funcs leave NAMESPACE).
   - **Verify the `.onLoad` S3-capture in zzz.R still works** after the split —
     the original-method capture for data_block/transform_block must run before
     any render. (No code change expected; just test the opt-in path.)
3. **blockr.dm**
   - DELETE `R/table-preview.R`.
   - `dm-block.R`: `apply_table_sort`/`build_html_table` →
     `blockr.ui::` prefixed.
   - DESCRIPTION: add `Imports: blockr.ui`. Bump Version.
4. **blockr.bi**
   - `html-table-block.R`: replace the `requireNamespace("blockr.extra")` block
     (L112-116) with a direct `blockr.ui::table_preview_css()`.
   - DELETE `html_table_shared_css_fallback()` (~L543-612) and its doc.
   - DESCRIPTION: add `Imports: blockr.ui`. Remove `Imports: blockr.extra`
     **only after** confirming nothing else in bi needs it — note
     `inst/examples/tile-dock-dag/app.R` references blockr.extra, so extra may
     need to drop to `Suggests` rather than be removed. Bump Version.
5. **blockr.tidymodels**
   - `predict-block.R:55`: `blockr.core::html_table_result` →
     `blockr.ui::html_table_result` (fixes the latent bug).
   - DESCRIPTION: add `Imports: blockr.ui`. Bump Version.

## Resulting dependency graph
blockr.ui (owns primitive) ← extra, dm, bi, tidymodels. No package reaches into
another for this anymore; bi no longer depends on extra for CSS. Single source
of the CSS + JS + hash-free renderer. Mirror/fallback deleted.

## Verification (must pass before calling done)
- `R CMD check` (or `devtools::check()`) clean on: blockr.ui, blockr.extra,
  blockr.dm, blockr.bi, blockr.tidymodels.
- Install all from local source (order: ui → extra/dm → bi/tidymodels).
- Live smoke test, all rendering identically to pre-move (screenshot compare):
  - data/transform block preview with `options(blockr.html_table_preview=TRUE)`
    (blockr.extra path) — sorting + pagination + horizontal-scroll-restore work.
  - dm block preview (blockr.dm path).
  - `html_table()` summary/pivot widget (blockr.bi path) — shared CSS still
    applies, delta CSS unaffected.
  - tidymodels predict-block preview (was broken; now renders).
- Grep sweep: no remaining `blockr.extra::table_preview_css`,
  no `html_table_shared_css_fallback`, no second `table-preview.R`.

## Notes / gotchas
- CSS ships as inline `shiny::tags$style` (NOT an htmlDependency/inst asset), so
  there is **no packageVersion cache-bust** concern — but still bump DESCRIPTION
  Versions for normal release hygiene.
- The `--blockr-*` design tokens the CSS references are defined in
  blockr.dock / blockr.ui's `inst/assets/css`; the CSS uses `var(--x, #fallback)`
  so it renders standalone without them. No token work needed.
- dm's copy did NOT export `table_preview_css`; the canonical (exported) version
  is blockr.extra's. The dm/extra diff is cosmetic only — safe to collapse to one.
- Do all git ops with `/usr/lib/git-core/git` (the bare `git` is a stub here).
- Do NOT attribute commits to Claude / no Co-Authored-By (repo rule).
