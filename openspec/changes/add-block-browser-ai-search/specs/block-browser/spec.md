## ADDED Requirements

### Requirement: Optional AI-assisted search

`block_browser_ui()` SHALL accept a `search = c("text", "chat")` argument. `"text"` (default) renders the plain search input (unchanged). `"chat"` renders a `shinychat::chat_mod_ui()` slot in place of the search input. `block_browser_server()` SHALL accept an optional `chat` argument (an `ellmer::Chat`, or a zero-arg constructor returning one). When `chat` is non-NULL the server mounts the chat module, registers the registry tools, and sets the block-finder system prompt; when `NULL` (default) the server behaves exactly as without this change.

The AI layer SHALL be additive: the committed-block reactive returned by `block_browser_server()`, the input binding's value channel, and the add / commit path SHALL be unchanged whether or not `chat` is supplied. The chat only narrows the visible cards.

When `search = "chat"` and `{shinychat}` is not installed, `block_browser_ui()` SHALL raise an informative error (a misconfiguration must not silently fall back).

#### Scenario: No client means the plain search bar

- **WHEN** `block_browser_ui(id, board)` (default `search = "text"`) is rendered and `block_browser_server(id)` is mounted with no `chat`
- **THEN** the panel shows the search `<input>` and behaves exactly as the non-AI block browser
- **AND** no chat UI and no LLM client are created

#### Scenario: Chat replaces the search bar when configured

- **WHEN** `block_browser_ui(id, board, search = "chat")` is rendered and `block_browser_server(id, chat = client)` is mounted with an `ellmer::Chat`
- **THEN** the panel renders the chat UI in place of the search input
- **AND** the category list, cards, and per-card forms are identical to the non-AI browser

#### Scenario: Adding a block is unaffected by the chat

- **WHEN** the chat is configured and the user clicks a card (or its in-card add button)
- **THEN** the committed-block reactive fires with the same spec it would without the chat
- **AND** the chat conversation does not alter the spec

#### Scenario: chat mode without shinychat errors

- **WHEN** `block_browser_ui(id, board, search = "chat")` is called and `{shinychat}` is not installed
- **THEN** an informative error is raised

### Requirement: `search_blocks` registry tool

When `chat` is supplied, `block_browser_server()` SHALL register an `ellmer` tool named `search_blocks` that returns the registry catalogue via `blockr.core::registry_metadata()` with fields `name`, `description`, `category`, `arguments`, and `package` (one row per registered block). The tool takes no arguments and returns the whole catalogue (registry sizes are small enough to fit the model context); the model uses it to map a request to a block.

#### Scenario: search_blocks returns the catalogue

- **WHEN** the model calls `search_blocks()`
- **THEN** it receives one row per `blockr.core::list_blocks()` entry
- **AND** each row carries `name`, `description`, `category`, `arguments`, `package`

### Requirement: `highlight_blocks` tool filters the cards via the input binding

When `chat` is supplied, `block_browser_server()` SHALL register an `ellmer` tool named `highlight_blocks(ids)` that narrows the card list to the given registry ids by sending `session$sendInputMessage(NS(id)("commit"), list(action = "filter", ids = ids))` to the browser's input binding. An empty `ids` (or a dedicated clear) SHALL send `list(action = "clear_filter")`.

The `blockr.ui.blockBrowser` input binding SHALL handle these in `receiveMessage`: `action = "filter"` hides every card whose `data-block-type` is not in `ids` (reusing the `.hidden` mechanism and toggling the empty-state); `action = "clear_filter"` shows all cards. `receiveMessage` SHALL NOT modify the binding's value (`getValue` is unaffected).

#### Scenario: filter shows only the matched cards

- **WHEN** the binding receives `{action: "filter", ids: ["new_filter_block"]}`
- **THEN** the `new_filter_block` card is visible and all other cards have `.hidden`
- **AND** the committed-block value (`getValue`) is unchanged

#### Scenario: clear_filter restores all cards

- **WHEN** the binding receives `{action: "clear_filter"}`
- **THEN** every card becomes visible again

#### Scenario: highlight_blocks sends the filter message

- **WHEN** the model calls `highlight_blocks(c("new_filter_block", "new_select_block"))`
- **THEN** the server sends a `sendInputMessage` to `NS(id)("commit")` with `action = "filter"` and `ids = c("new_filter_block", "new_select_block")`

### Requirement: Findability across expertise levels

The block-finder system prompt SHALL instruct the model to map the user's phrasing - regardless of dialect (SQL, dplyr, pandas, Excel, plain English) - to the registered block that performs the operation, calling `search_blocks` to read the catalogue and `highlight_blocks` to narrow the cards. The prompt SHALL include seed examples bridging common cross-dialect vocabulary to blocks.

A findability eval fixture SHALL pair natural-language phrasings with their expected block and assert the model maps each correctly. The eval runs against a live model and SHALL be skipped when no API key is available (and on CRAN / CI without a provisioned key).

#### Scenario: SQL / Excel phrasings reach the filter block

- **WHEN** the user asks (in any of) "WHERE clause", "keep rows where x > 10", "remove rows that don't match", "subset the data"
- **THEN** the model highlights the `filter` block

#### Scenario: Column-selection phrasings reach the select block

- **WHEN** the user asks "SELECT columns", "pick only these fields", or "drop the other columns"
- **THEN** the model highlights the `select` block

#### Scenario: Eval is skipped without credentials

- **WHEN** the findability eval runs and no LLM API key is configured
- **THEN** the eval is skipped (it never fails `R CMD check` for lack of credentials)

### Requirement: AI dependencies are optional

`{shinychat}` and `{ellmer}` SHALL be declared in `Suggests`, not `Imports`. The AI code paths SHALL be guarded with `rlang::is_installed()` / `rlang::check_installed()`, so the package installs, checks, and runs the plain block browser without them.

#### Scenario: Package works without the AI packages

- **WHEN** `{shinychat}` and `{ellmer}` are not installed
- **THEN** `block_browser_ui()` / `block_browser_server()` with default arguments work and render the text search bar
- **AND** `R CMD check` passes without the AI packages installed
