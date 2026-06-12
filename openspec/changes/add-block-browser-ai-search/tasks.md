## 1. Phase 1 - AI search in `blockr.ui` (PR 1)

Additive on top of `add-block-browser`. Both new arguments default to the current behaviour, so nothing downstream changes until a caller opts in.

- [ ] 1.1 `DESCRIPTION`: add `shinychat` and `ellmer` to `Suggests`.
- [ ] 1.2 `R/block-browser.R` - `block_browser_ui(id, board, target = NULL, search = c("text", "chat"))`:
  - `search = "text"` (default) renders the existing search `<input>`.
  - `search = "chat"` renders `shinychat::chat_mod_ui(NS(id)("search_chat"))` in the search slot; `rlang::check_installed("shinychat")` first (informative error if absent).
  - Everything below the search slot (categories, cards, per-card forms) is unchanged.
- [ ] 1.3 `R/block-browser.R` - `block_browser_server(id, chat = NULL)`:
  - Keep the existing `eventReactive(input$commit, ...)` committed-block reactive as the return value.
  - When `chat` is non-NULL: `rlang::check_installed(c("shinychat", "ellmer"))`, resolve the client (`if (is.function(chat)) chat() else chat`), `register_tool(search_blocks_tool())`, `register_tool(highlight_blocks_tool(session))`, `set_system_prompt(block_finder_prompt())`, and `shinychat::chat_mod_server("search_chat", client)`.
- [ ] 1.4 Tools (`R/block-browser-tools.R` or similar):
  - `search_blocks_tool()` - `ellmer::tool()` returning `blockr.core::registry_metadata(fields = c("name", "description", "category", "arguments", "package"))`; no arguments; a description telling the model to use it to map a request to a block.
  - `highlight_blocks_tool(session)` - `ellmer::tool(function(ids) ...)` that sends `session$sendInputMessage("commit", list(action = "filter", ids = ids))` (or `list(action = "clear_filter")` for empty `ids`); `ids` typed as a string array.
- [ ] 1.5 `block_finder_prompt()` - system prompt instructing the model to map SQL / dplyr / pandas / Excel / plain-English phrasings to the registered block, call `search_blocks` then `highlight_blocks`, with seed examples (WHERE/subset -> filter; SELECT/pick columns -> select; GROUP BY+agg -> aggregate; JOIN -> merge).
- [ ] 1.6 `inst/assets/js/blockr-block-browser.js` - add `receiveMessage` to the binding: `action = "filter"` hides cards whose `data-block-type` is not in `ids` (reuse the `.hidden` + empty-state logic), `action = "clear_filter"` shows all; never touch `el._blockrBrowserValue`.
- [ ] 1.7 Unit tests (no LLM):
  - `search_blocks_tool()` returns one row per registry block with the expected columns.
  - `highlight_blocks_tool()` sends the expected `sendInputMessage` payload (mock session) for a non-empty `ids` and for the clear case.
  - `block_browser_server(chat = NULL)` is unchanged (regression).
  - `block_browser_ui(search = "chat")` errors when `{shinychat}` is not installed (`local_mocked_bindings` / skip if installed).
- [ ] 1.8 shinytest2 (stubbed, no LLM): drive `session$sendInputMessage("commit", list(action = "filter", ids = ...))` and assert only matching cards are visible; `clear_filter` restores them.
- [ ] 1.9 Findability eval `tests/testthat/test-block-browser-findability.R`: `phrasing -> expected block` fixture (filter / select / head / merge / aggregate ...) asserted against a live model; `skip_if` no API key, `skip_on_cran`, `skip_on_ci` unless a key is provisioned.
- [ ] 1.10 Vignette section + example: how to pass an `ellmer::Chat` to enable the chat finder; the plain-search fallback when none is supplied.
- [ ] 1.11 `devtools::check()` clean (with and without `{shinychat}`/`{ellmer}` installed); `lintr::lint_package()` clean; `openspec validate add-block-browser-ai-search --strict` clean.

## 2. Phase 2 - `blockr.dock` wiring (PR 2, optional)

- [ ] 2.1 In the add / append / prepend action handlers, when a board `llm_model` option is set, pass `search = "chat"` to `block_browser_ui()` and the resolved client to `block_browser_server(chat = ...)` (mirror `blockr.assistant`'s `llm_model` option). With no option set, keep `search = "text"` / `chat = NULL`.
- [ ] 2.2 Tests: the handler passes a client through when the option is set, and falls back to text search when not.
- [ ] 2.3 `devtools::check()` + `lintr::lint_package()` clean on `blockr.dock`.

## 3. Validation

- [ ] 3.1 `openspec validate add-block-browser-ai-search --strict` - no spec / template / scenario errors.
- [ ] 3.2 `devtools::check()` on `blockr.ui` with the AI packages absent - 0 errors / 0 warnings (proves the Suggests guards).
- [ ] 3.3 `devtools::check()` on `blockr.ui` with the AI packages present - 0 errors / 0 warnings.
- [ ] 3.4 `lintr::lint_package()` clean.
