## Context

`add-block-browser` deliberately left two seams for this work: `block_browser_server()` exists as a module server (today it only relays the committed block), and the `blockr.ui.blockBrowser` input binding reserves `receiveMessage` for an R->JS channel. The AI search plugs into both - it adds a chat client and tools on the server side, and uses `receiveMessage` to filter the cards - without touching the value channel (how a block is added).

The reference implementation is `blockr.assistant` (`R/extension.R`, `R/tools-read.R`): a `shinychat::chat_mod_ui()` / `chat_mod_server()` pair mounted on an `ellmer` client built from a board `llm_model` option, with tools registered via `client$register_tool(ellmer::tool(...))`. Its `tool_list_available_blocks()` already reads `blockr.core::list_blocks()` + `registry_metadata(uids, "all")` - exactly the registry surface a block finder needs.

## Goals

- Replace the search bar with a chat finder when (and only when) a client is configured; fall back to the plain search bar otherwise.
- Bridge vocabulary across expertise levels: a SQL / pandas / Excel / plain-English phrasing reaches the right block.
- Keep the add / commit path byte-for-byte unchanged - the chat only narrows the cards.
- Keep `blockr.ui` provider-agnostic (caller supplies the `ellmer::Chat`) and the LLM stack in `Suggests`.

## Non-Goals

- No bundled provider, model defaults, or API-key handling.
- No change to the committed-block reactive or the binding's value channel.
- No `blockr.core` metadata change (a `keywords`/`aliases` field is a follow-up).
- No conversation persistence across opens in v1.

## API

```r
block_browser_ui(id, board, target = NULL, search = c("text", "chat"))
block_browser_server(id, chat = NULL)
```

- `search = "chat"` renders `shinychat::chat_mod_ui(NS(id)("search_chat"))` in place of the `<input>`. Guarded: when `search = "chat"` and `{shinychat}` is not installed, error with a clear message (don't silently degrade a misconfiguration).
- `chat` is an `ellmer::Chat` or a zero-arg constructor returning one (so each mount gets its own client, as `blockr.assistant` does). `NULL` keeps today's behaviour and ignores any chat UI.
- The caller is responsible for pairing the two: pass `search = "chat"` to the UI and a non-`NULL` `chat` to the server. (They are set together at one call site, e.g. the dock action handler.)

## Architecture

### Tools

Registered on the client by `block_browser_server()` when `chat` is supplied:

- `search_blocks()` - returns `blockr.core::registry_metadata(fields = c("name", "description", "category", "arguments", "package"))` (a data frame, one row per registered block). The model reads it to map a request to a block. Registries are ~10-50 blocks, so returning the whole catalogue fits the context; no server-side prefilter needed.
- `highlight_blocks(ids)` - the *action* tool. Given a character vector of registry ids (block types), it narrows the card list by sending `session$sendInputMessage(NS(id)("commit"), list(action = "filter", ids = ids))` to the browser's input binding. `highlight_blocks(character())` (or a dedicated clear) sends `list(action = "clear_filter")`.

The model both answers in prose ("the `filter` block keeps rows matching a condition - highlighted below") and calls `highlight_blocks()` to drive the UI. The two-tool split keeps "read the registry" separate from "act on the UI".

### Filter bridge (binding `receiveMessage`)

The `blockr.ui.blockBrowser` binding gains:

```js
receiveMessage: function (el, data) {
  if (!data) return;
  if (data.action === "filter") {        // hide non-matching cards
    filterCards(el, data.ids);
  } else if (data.action === "clear_filter") {
    clearFilter(el);
  }
}
```

`filterCards` toggles the existing `.hidden` class (reusing the search mechanism) so only cards whose `data-block-type` is in `ids` show, and toggles the empty-state. It does NOT touch `el._blockrBrowserValue` - the commit value is independent. A subsequent typed search (text mode) or a `clear_filter` resets it.

### Server flow

```r
block_browser_server <- function(id, chat = NULL) {
  moduleServer(id, function(input, output, session) {
    committed <- eventReactive(input$commit, { ... strip nonce ... })

    if (!is.null(chat)) {
      rlang::check_installed(c("shinychat", "ellmer"))
      client <- if (is.function(chat)) chat() else chat
      client$register_tool(search_blocks_tool())
      client$register_tool(highlight_blocks_tool(session))
      client$set_system_prompt(block_finder_prompt())
      shinychat::chat_mod_server("search_chat", client)
    }

    committed
  })
}
```

`highlight_blocks_tool(session)` closes over `session` so the tool can call `session$sendInputMessage("commit", ...)`. Conversation lifecycle in v1: the chat module is mounted once with the server; a fresh client per mount keeps it simple. (Re-mount-per-open and turn persistence, as `blockr.assistant` does with a `mount_idx`, are a follow-up if needed.)

### UI fallback

`search = "text"` (default) is unchanged. `search = "chat"` swaps only the search slot; the category list, cards, and per-card forms are identical. So the AI layer is purely the search affordance plus the filter side effect.

## Findability across expertise levels

This is the point of the change, so it is a first-class requirement, supported three ways:

1. **System prompt.** `block_finder_prompt()` instructs the model to translate the user's phrasing - whatever the dialect - to the registered block that performs the operation, with seed examples (`WHERE / subset rows / keep matching -> filter`; `SELECT / pick columns -> select`; `GROUP BY + agg -> the summarise/aggregate block`; `JOIN -> merge`). It tells the model to call `search_blocks` first, then `highlight_blocks` with the matches.
2. **Rich tool payload.** `search_blocks` returns `arguments` as well as `description`, so the model can disambiguate on argument names (a block with a `condition`/`predicate` argument is filter-like) even when descriptions are terse.
3. **Eval.** A `phrasing -> expected block` fixture (e.g. "drop rows where age is missing" -> filter, "first 10 rows" -> head, "join two tables" -> merge, "pick these columns" -> select) run against the live model, `skip_if` no API key. This is how we know a SQL user and an Excel user are served equally, and it guards prompt regressions.

An optional `keywords`/`aliases` registry field (cross-package, `blockr.core`) would help the model on niche/custom blocks and would also sharpen the plain-search fallback; deferred until the eval shows gaps.

## Testing

- **Unit (no LLM):** `search_blocks_tool()` returns the registry catalogue with the expected columns; `highlight_blocks_tool()` sends the right `sendInputMessage` payload (capture via a mock session). `block_browser_server(chat = NULL)` is unchanged (existing tests).
- **Filter bridge (shinytest2, stubbed):** drive `session$sendInputMessage("commit", list(action = "filter", ids = ...))` and assert only matching cards are visible; `clear_filter` restores them. No real LLM.
- **Findability eval:** `skip_if(no API key)`; asserts the model maps each fixture phrasing to the expected block (via the `highlight_blocks` call or a structured check). `skip_on_cran`, `skip_on_ci` unless a key is provisioned.

## Risks / Trade-offs

- **LLM latency / cost** on every search. Mitigated by keeping the plain search bar as the default and the AI an opt-in; the finder is for discovery, not every keystroke.
- **Model picks the wrong block.** The cards remain the source of truth and the user can ignore the highlight or clear it; the eval guards the common cases.
- **Lifecycle of a chat inside a transient sidebar.** v1 takes the simple path (fresh per mount); flagged for revisit.
- **Two new Suggests + a provider requirement.** Guarded and optional; `R CMD check` never needs an LLM (eval is key-gated).

## Migration / Compatibility

- Both new arguments default to the current behaviour, so `add-block-browser` PR2 (the `blockr.dock` adoption) needs no change to keep working with the plain search bar.
- Enabling AI in the dock is the Phase 2 wiring of a board `llm_model` option into `block_browser_server(chat = ...)`; until then the dock browser uses text search.
