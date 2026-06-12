## Why

The block browser (`add-block-browser`) ships a case-insensitive substring search over `name + description + package + category`. That fails the users it most needs to serve: people coming from different backgrounds use different words for the same operation. A SQL user types "WHERE clause", a pandas user "boolean mask", an Excel user "remove rows that don't match", a statistician "case selection" - none contain the substring "filter", so today's search returns nothing, yet all of them want the `filter` block. Same story for "SELECT columns" / "pick columns" / "keep only these fields" -> the `select` block.

Substring search cannot bridge vocabulary; a language model can. This change replaces the search bar (when an LLM backend is configured) with a conversational assistant that maps a natural-language question to the registered blocks that perform it, then narrows the card list to the matches. The cards stay the canonical add affordance - the chat is a smarter finder, not a new way to add.

This is designed-for in `add-block-browser` (the `block_browser_server()` module and the input binding's reserved `receiveMessage` channel exist precisely so this layer can sit on top without changing the commit contract). It is additive: it does not alter how a block is added, so it needs no second `blockr.dock` action-handler rewrite.

## What Changes

- **NEW** `block_browser_ui(id, board, target = NULL, search = c("text", "chat"))` gains a `search` argument. `"text"` (default) renders the plain search input as today; `"chat"` renders a `shinychat::chat_mod_ui()` slot in its place.
- **NEW** `block_browser_server(id, chat = NULL)` gains an optional `chat` argument. When supplied (an `ellmer::Chat` or a zero-arg constructor returning one), the server mounts the chat module, registers the registry tools, and wires the card filter. When `NULL` (default) the server behaves exactly as today - the committed-block reactive is unchanged.
- **NEW** Two `ellmer` tools registered on the chat client: `search_blocks()` (reads `blockr.core::registry_metadata()` - name / description / category / arguments / package - so the model can map a request to a block) and `highlight_blocks(ids)` (narrows the card list to the given block ids by sending a filter message to the browser's input binding).
- **NEW** The `blockr.ui.blockBrowser` input binding gains a `receiveMessage` handler for `{action: "filter", ids}` / `{action: "clear_filter"}` that hides non-matching cards (and clears on demand) - the R->JS channel reserved in `add-block-browser`.
- **NEW** A system prompt that instructs the model to map SQL / dplyr / pandas / Excel / plain-English phrasings to the registered block, with seed examples.
- **NEW** `{shinychat}` and `{ellmer}` added to `Suggests`, guarded with `rlang::is_installed()`. No LLM configured (or packages absent) -> the plain search bar.

## What's *not* in this change

- The add / commit path. Clicking a card (or the in-card add button) and the committed-block reactive are untouched; the chat only filters.
- A bundled LLM provider or API key handling. The caller supplies the `ellmer::Chat`; `blockr.ui` stays provider-agnostic.
- A `blockr.core` registry metadata schema change. A `keywords` / `aliases` field would further improve matching (and the plain-search fallback) but is a separate cross-package follow-up; this change relies on the model's world knowledge plus existing descriptions / arguments.
- Conversation persistence across sidebar opens. v1 starts a fresh conversation per open; persistence is a follow-up.
- The `blockr.dock` wiring of a board `llm_model` option into `block_browser_server(chat = ...)`. That is a small, optional `blockr.dock` change (Phase 2 here); the `blockr.ui` capability is usable by any caller that supplies a client.

## Capabilities

### Modified Capabilities

- `block-browser`: gains an optional AI-assisted search layer (chat UI in place of the search bar, registry tools, card-filter via the input binding's `receiveMessage`). The block-picking and commit behaviour is unchanged.

## Phases as separate PRs

1. **PR 1 (`blockr.ui`):** `search = "chat"` UI mode, `block_browser_server(chat = )`, the `search_blocks` / `highlight_blocks` tools, the binding `receiveMessage` filter, the system prompt, `Suggests` deps with guards, unit tests (tools + filter), a stubbed-client shinytest2 for the filter bridge, an API-key-gated findability eval, vignette section, and an example.
2. **PR 2 (`blockr.dock`, optional):** thread a board `llm_model` option into `block_browser_server(chat = ...)` in the add / append / prepend action handlers, mirroring `blockr.assistant`.

## Impact

- **Affected packages.** `blockr.ui` gains the AI-search layer behind `Suggests`. `blockr.dock` optionally wires a client (Phase 2); without it, the dock keeps the plain search bar.
- **Affected dependencies.** `blockr.ui` adds `shinychat` and `ellmer` to `Suggests` only - no new hard dependency.
- **Affected APIs.** `block_browser_ui()` gains `search`; `block_browser_server()` gains `chat`. Both default to the current behaviour, so existing callers and PR2 of `add-block-browser` are unaffected.
- **Affected downstream.** Extensions inherit the smarter search for free once a client is configured upstream.
