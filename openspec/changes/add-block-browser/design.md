## Context

`add-sidebar-primitive` shipped the panel; `block_sidebar_body()` was kept as the literal modal-body content reused unchanged. That body uses `block_registry_selectize()` as the chooser - a custom-rendered selectize that already shows icon, name, description, category, and package per row. The data is fine; the shape is what stopped fitting once the chooser left the modal.

This change replaces the chooser for add / append / prepend with a card-list browser laid out for the sidebar. The per-card content is the same metadata `block_registry_selectize()` already renders; only the layout and the interaction change.

### Design evolution

The first implementation grew a multi-select model with a parallel/sequential connection toggle, selection-order badges, an arity-aware cap for parallel prepend, `target_input` slot arbitration, and a reorderable chip tray. In review this was judged to be doing too much: the connection modes and chip tray added real surface area, and - ironically - multi-select still couldn't express "add the same block N times" without a further stepper. We pulled back to a **single-shot** model:

- Clicking a card adds that one block immediately with defaults.
- Opening a card (chevron) reveals a small form (id / title / link / port) and an explicit add button for tweaking before adding.
- Adding the same block several times = clicking it several times; unique ids are guaranteed by seeding `rand_names()` with the board's existing ids.

The multi-select / connection-mode exploration and the AI-search idea are captured under "Deferred work" below so they can be picked up later.

## Goals

- One new public function - `blockr.ui::block_browser_ui()` - drop-in body for the three block-add action handlers.
- Two ways to add, both single-shot: click the card (defaults) or open it and use the in-card add button (edited).
- Search and per-card expand are client-side only; no server round-trip until a block is added.
- `block_browser_server(id)` returns a reactive carrying one block: `list(type, id, title, link_id, block_input, target_input)`, fields not relevant to the mode set to `NULL`.
- Suggested ids are unique against the board, so repeated adds yield distinct blocks.
- Cards render from `blks_metadata(available_blocks())`; blocks with missing fields render with safe fallbacks.

## Non-Goals

- No multi-select, connection modes, order badges, arity cap, slot arbitration, chip tray, or quantity stepper (see Deferred work).
- No fuzzy / ranked / AI search in this change (see Deferred work).
- No new metadata schema in `blockr.core`.
- No move of `blks_metadata()` to `blockr.ui`.

## API

The block browser is a Shiny module:

```r
block_browser_ui(
  id,                                    # module id (parent passes session$ns("..."))
  board,                                 # board (current state)
  mode = c("add", "append", "prepend"),
  trigger_id = NULL                      # source / target block id for append / prepend
)

block_browser_server(id)                  # returns reactive(committed block spec)
block_browser_dep()                       # htmlDependency, attached automatically
```

`id` is the module id - `ns` was the tell that the old fragment wanted to be a module, so it now follows the standard pattern (`block_browser_ui(session$ns("browser"), ...)` at the call site, `block_browser_server("browser")` for the server). When `mode = "prepend"` and `trigger_id` is non-NULL, `block_browser_ui()` reads the target block's input arity and stamps it on the root via `data-target-arity` (integer for finite, `"inf"` for variadic/`NA`). The CSS uses it to show the `target_input` picker only when the target has more than one input slot.

### Server-side flow

```r
# In add_block_action()'s moduleServer body:
added <- blockr.ui::block_browser_server("browser")   # reactive, fires per add

observeEvent(trigger(), {
  blockr.ui::show_sidebar(
    sidebar_id,
    title = "Add new block",
    ui = blockr.ui::block_browser_ui(session$ns("browser"), board$board,
                                     mode = "add")
  )
})

observeEvent(added(), {
  spec <- added()
  # spec$type        registry name (e.g. "new_dataset_block")
  # spec$id          unique block id (already avoids existing board ids)
  # spec$title       user title or NULL (NULL -> block keeps default name)
  # spec$link_id     append / prepend only
  # spec$block_input append only (new block's input port)
  # spec$target_input prepend into arity>1 target only
  blk <- do.call(spec$type, list())
  if (!is.null(spec$title)) blockr.core::block_name(blk) <- spec$title
  update(list(blocks = stats::setNames(list(blk), spec$id), links = ...))
  # Re-render the browser (pinned) with the UPDATED board so the next
  # suggested id is fresh, or hide it (unpinned):
  blockr.ui::keep_or_hide_sidebar(
    sidebar_id,
    ui = blockr.ui::block_browser_ui(session$ns("browser"), board$board,
                                     mode = "add"),
    title = "Add new block"
  )
})
```

### Client-side flow

The card list is plain HTML. JS does three things:

1. **Search filter.** `input` events on `.blockr-block-browser-search` hide cards whose `data-name + data-description + data-package + data-category` don't contain the (case-insensitive) query. Empty category sections collapse via `:has()` CSS.
2. **Per-card expand.** Clicking the chevron toggles `.card-expanded`, revealing the advanced form (id / title / link / port) and the in-card add button.
3. **Add.** A plain card-body click commits the block with its (default) field values. The in-card add button commits with the (possibly edited) field values. Clicks elsewhere inside the advanced form (the inputs) do nothing. Commit gathers the card's applicable fields, records them on the root element, and dispatches a `blockr-block-browser:commit` event.

Field applicability is read from computed style (a hidden field returns `null`), so the mode → visible-fields matrix lives only in the CSS.

The browser is a proper **`Shiny.InputBinding`** (`blockr.ui.blockBrowser`, bound to `.blockr-block-browser`), not an ad-hoc `Shiny.setInputValue` caller: `getValue` returns the recorded spec (with a `nonce` so repeat adds re-fire), `subscribe` listens for the commit event, and `receiveMessage` is reserved for the R→JS card filter the AI-search change will use. Because `show_sidebar()`'s content swap runs `Shiny.bindAll()`, the binding is mounted through Shiny's normal lifecycle - no MutationObserver needed. Bundled JS lives in `inst/assets/js/blockr-block-browser.js`.

### DOM shape

```text
.blockr-block-browser[id=NS(id)("commit")][data-mode][data-target-arity?]
├── .blockr-block-browser-context           ; "Append from <name>" / "Prepend to <name>" (append/prepend)
├── input.blockr-block-browser-search
├── .blockr-block-browser-categories
│   └── .blockr-block-browser-category[data-category="input"]
│       ├── h3 "Input"                       ; coloured by category
│       └── .blockr-block-browser-cards
│           └── .blockr-block-browser-card[data-block-type][data-name ...]
│               ├── .blockr-block-browser-card-header
│               │   ├── .blockr-block-browser-card-icon       ; tinted by category
│               │   ├── .blockr-block-browser-card-name
│               │   ├── .blockr-block-browser-card-package
│               │   └── button.blockr-block-browser-card-chevron
│               ├── .blockr-block-browser-card-description     ; clamped collapsed, full when expanded
│               └── .blockr-block-browser-card-advanced        ; shown when .card-expanded
│                   ├── field id / title / link-id / block-input / target-input
│                   └── button.blockr-block-browser-card-add    ; "Add" / "Append" / "Prepend"
└── .blockr-block-browser-empty              ; shown when search matches nothing
```

### Per-card fields by mode

| Mode      | Fields shown                                               |
| --------- | ---------------------------------------------------------- |
| `add`     | `id`, `title`                                              |
| `append`  | `id`, `title`, `link_id`, `block_input`                    |
| `prepend` | `id`, `title`, `link_id`, `target_input` (only arity > 1)  |

Defaults: `id` and `link_id` are `rand_names()` seeded with the board's existing block / link ids (computed once per render, unique among cards and against the board); `title` is empty (placeholder = block name; empty → block keeps its default name); `block_input` defaults to the new block's first input port; `target_input` to the target's first input slot.

### Unique-id strategy

`block_browser_ui()` calls `rand_names(old_names = board_block_ids(board), n = <n cards>)` once, so the per-card id defaults are unique among themselves and avoid the board's existing ids. Because the action handler re-renders the browser with the updated board after each add (when the sidebar is pinned), the next render's suggestions avoid the just-added block too. This is what makes "click dataset three times → three dataset blocks with distinct ids" work without any multi-select machinery.

## Risks / Trade-offs

- **Click-to-add is a single click.** Easy to misfire, but the block is trivially removable on the board, and it's exactly the fast path users asked for. Configuring first is one chevron click away.
- **Structured commit changes the `blockr.dock` action-handler shape.** One observer per action instead of five; contained to three files. Documented in NEWS.
- **Substring search may miss synonyms.** Mitigated long-term by the AI-search deferred item; near-term by per-block metadata.
- **Per-card form values live in the DOM, not Shiny inputs.** The JS reads them on add. A board's serialization can't snapshot a half-filled browser - never useful, not a regression.

## Deferred work

These were explored or requested but are intentionally out of scope here. Captured so they can be reopened with context.

### Multi-select + addition modes (parked)

An earlier iteration supported selecting several blocks before a single confirm, with:

- A **parallel / sequential** connection toggle: parallel = all new blocks share the trigger's source/target; sequential = the new blocks form a chain in selection order.
- **Selection-order badges** on cards and a reorderable **chip tray** in the footer (drag to reorder the chain, × to remove).
- An **arity-aware cap** for parallel prepend (can't select more blocks than the target's input arity) and **`target_input` slot arbitration** (each parallel-prepended block claims a distinct target slot).
- A multi-block commit spec: `list(connection, blocks = list(list(type, id, ...), ...))`.

It was pulled because the component was doing too much for the value, and multi-select still couldn't express "the same block N times" without an extra **quantity stepper** (a Framework7-style `− N +` per card). If revisited, the cleanest framing discussed was: add a `count` field per block entry that the action handler expands via `rand_names(count)`; counts are simple for add/append but need slot-expansion design for parallel prepend. Revisit this as its own change rather than re-growing the single-shot browser.

### AI-assisted block search (next change: `add-block-browser-ai-search`)

The intended next piece of work is to replace the substring search bar with a conversational assistant that maps a natural-language question to candidate blocks, so users from any background land on the right block. This is its own openspec change; the binding-based browser shipped here is its substrate. Agreed shape:

- **Conversational chat** via `shinychat::chat_mod_ui()` / `chat_mod_server()` mounted on an `ellmer` client, following the `blockr.assistant` pattern (`R/extension.R`, `R/tools-read.R` in <https://github.com/BristolMyersSquibb/blockr.assistant>). Renders in place of the search bar when AI is configured; the plain search bar is the fallback otherwise.
- A **`search_blocks` ellmer tool** over `blockr.core::registry_metadata(fields = "all")` (id / name / description / category / arguments / package) - the same registry surface `blockr.assistant`'s `tool_list_available_blocks()` uses (`R/block-registry.R` in blockr.core).
- **Findability across expertise** as a first-class requirement: the system prompt instructs the model to map SQL / dplyr / pandas / Excel / plain-English phrasings to the registered block ("WHERE / subset rows / keep matching → filter"; "SELECT / pick columns → select"). A `phrasing → expected block` **eval fixture** guards it (skipped without an API key). An optional `keywords`/`aliases` registry metadata field is a cross-package robustness follow-up (also helps the substring fallback).
- **`block_browser_server()`** is added here (the browser gains a server). The R→JS card filter rides the binding's **`receiveMessage`** (the channel reserved in this change) - the `search_blocks` result is sent to `input[[id]]` and the binding highlights / narrows the cards. The add/commit path (the binding's value channel) is unchanged, so this needs **no second `blockr.dock` adoption**.
- **Client injection**: the caller passes an `ellmer::Chat` (or constructor); `{shinychat}` + `{ellmer}` go in **Suggests**, guarded by `rlang::is_installed()`. No LLM configured → plain search.
- **Lifecycle**: start with a fresh conversation per sidebar open (simplest); revisit turn persistence later.

Because the commit channel is already a clean input binding, the AI layer is purely additive on the `blockr.ui` side and does not change the contract `blockr.dock` consumes.

## Migration / Compatibility

- `block_sidebar_body(mode = "edit")` stays for stack edit flows.
- `block_sidebar_body(mode = "add" | "append" | "prepend")` is removed in `blockr.dock` PR 2 (nothing in-tree calls it). Out-of-tree consumers migrate to `blockr.ui::block_browser_ui()`.
- The per-field inputs (`<mode>_block_selection`, `<mode>_block_id`, ...) are removed in favour of the block browser module (`block_browser_server()` returns the committed block). Called out in NEWS for both packages.
- No change to the action-handler trigger / signature, so the wiring around the action handlers is untouched.
