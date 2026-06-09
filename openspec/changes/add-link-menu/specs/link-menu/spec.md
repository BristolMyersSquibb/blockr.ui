## ADDED Requirements

### Requirement: `link_menu_ui()` builder

`blockr.ui` SHALL export `link_menu_ui(id, board, anchor)` returning an `htmltools::tag` with `link_menu_dep()` attached, where `id` is the module id and `anchor` is a non-empty character scalar naming the right-clicked block (the anchor of the new link). `anchor` MUST be a member of `blockr.core::board_block_ids(board)`; otherwise a clean error of class `blockr_ui_link_menu_unknown_anchor` SHALL be raised.

The panel SHALL render up to two category-grouped sections. When both render, INCOMING SHALL appear above OUTGOING so the order matches the natural left-to-right data flow (sources feed the anchor; the anchor feeds targets):

- **INCOMING** (header text "INPUT FROM", card -> anchor): cards represent eligible source blocks for an incoming link. The section is rendered only when `anchor` itself has at least one free named input port or is variadic. When the section renders, eligibility = every board block id other than `anchor`. Candidates that `anchor` already reaches SHALL be excluded - adding `candidate -> anchor` would close a cycle.
- **OUTGOING** (header text "OUTPUT TO", anchor -> card): cards represent eligible target blocks for an outgoing link. Eligibility = every board block id other than `anchor` that has at least one free named input port (computed via `blockr.core::block_inputs()` minus already-wired ports) or has variadic arity (`is.na(blockr.core::block_arity(blk))`). Candidates that already reach `anchor` through the existing link graph SHALL be excluded - adding `anchor -> candidate` would close a cycle (direct or transitive).

Each section header SHALL be rendered only when its pool is non-empty. When both pools are empty the panel SHALL render an empty-state inside the cards container instead.

Cards in both sections SHALL share the `blockr-block-browser-card*` CSS classes. Each card SHALL display the candidate block's icon (category-tinted via `data-category`), its user-visible name (`blockr.core::block_name(blk)`), and an `id: <block_id>` subtitle. Cards SHALL NOT render a description or a package badge.

Each card SHALL carry a `data-direction` attribute set to either `"outgoing"` or `"incoming"` so the JS binding can construct the link in the correct orientation on commit.

The root element's `id` SHALL be `NS(id)("commit")` (the input-binding target read by `link_menu_server(id)` as `input$commit`). The root SHALL carry `data-anchor` set to `anchor` so the JS can echo it back on commit.

A small per-card advanced area SHALL be revealed by clicking the card's chevron. The advanced area SHALL render:

- A **Link ID** `textInput` named `<card-ns>_link_id`, pre-filled with `blockr.core::rand_names(board_link_ids)` so repeat opens propose fresh ids.
- A **Block input** `<select>` named `<card-ns>_block_input`, populated with the **target end's** free input ports, ONLY when the target end has finite arity > 1 free slots. "Target end" is the card for `data-direction="outgoing"` and the anchor for `data-direction="incoming"`. Variadic and arity-1 target ends render only the Link ID field.

An in-card "Add link" button SHALL commit using the form values; clicking the card body outside the advanced area SHALL commit with the default values.

#### Scenario: Anchor with free outputs and free-input candidates renders OUTGOING

- **WHEN** the board contains blocks `a` (a dataset_block, no inputs) and `b` (a head_block with one free input) AND `link_menu_ui("mod", board, anchor = "a")` is rendered
- **THEN** the OUTGOING section is present with one card for `b` carrying `data-direction="outgoing"`
- **AND** the INCOMING section is NOT rendered (the dataset_block has no inputs)
- **AND** the root element has `id="mod-commit"` and `data-anchor="a"`

#### Scenario: Anchor with free inputs and other blocks renders INCOMING

- **WHEN** the board contains blocks `a` (a dataset_block) and `b` (a head_block with one free input) AND `link_menu_ui("mod", board, anchor = "b")` is rendered
- **THEN** the INCOMING section is present with one card for `a` carrying `data-direction="incoming"`
- **AND** the OUTGOING section is NOT rendered (the dataset_block has no inputs, so it can't be a target)
- **AND** the root element has `data-anchor="b"`

#### Scenario: Variadic anchor renders both directions

- **WHEN** the anchor is a variadic block (e.g. `rbind_block` with `block_arity = NA`) AND the board has at least one other block with free inputs
- **THEN** both OUTGOING and INCOMING sections render (variadic accepts a fresh slot, so the anchor can be both source and target)

#### Scenario: OUTGOING excludes cycle-creating candidates (direct)

- **WHEN** an existing link `h -> r` is on the board AND `link_menu_ui("mod", board, anchor = "r")` is rendered
- **THEN** `h` does NOT appear as an OUTGOING card (adding `r -> h` would close the cycle `h -> r -> h`)

#### Scenario: OUTGOING excludes cycle-creating candidates (transitive)

- **WHEN** existing links `a -> h` and `h -> r` are on the board AND `link_menu_ui("mod", board, anchor = "r")` is rendered
- **THEN** both `h` (direct ancestor of `r`) and `a` (transitive ancestor) are excluded from the OUTGOING cards

#### Scenario: INCOMING excludes blocks the anchor already reaches

- **WHEN** an existing link `h -> r` is on the board AND `link_menu_ui("mod", board, anchor = "h")` is rendered
- **THEN** `r` does NOT appear as an INCOMING card (adding `r -> h` would close the cycle `r -> h -> r`)

#### Scenario: Targets with no free input ports are filtered out (OUTGOING)

- **WHEN** block `b` has a single named input and that input is already wired by an existing link AND `b`'s arity is 1 (not variadic) AND `b` is not the anchor
- **THEN** `b` does NOT appear as an OUTGOING card

#### Scenario: Arity-1 target renders only the Link ID field

- **WHEN** an OUTGOING card targets a block with finite arity 1 whose single port is free
- **THEN** the card's advanced area renders only the Link ID field (no Block input picker)

#### Scenario: Arity > 1 target renders the Block input picker

- **WHEN** an OUTGOING card targets a block with finite arity 2 and both ports are free
- **THEN** the card's advanced area renders the Block input picker with both port names as options
- **AND** when one of the target's ports is already wired, the picker offers only the free port

#### Scenario: INCOMING port picker reflects the anchor's free slots

- **WHEN** the anchor has finite arity 2 with both ports free
- **THEN** each INCOMING card's advanced area renders the Block input picker with both port names as options

#### Scenario: Unknown anchor id raises a classed error

- **WHEN** `link_menu_ui("mod", board, anchor = "no_such_block")` is called
- **THEN** an error of class `blockr_ui_link_menu_unknown_anchor` is raised

#### Scenario: No eligible candidates in either direction renders the empty-state

- **WHEN** the board contains only `anchor`, OR every other block has no free inputs AND the anchor itself has no free inputs
- **THEN** the cards container renders an empty-state message
- **AND** neither the OUTGOING nor the INCOMING section header is rendered
- **AND** the rendered tree still includes the search input (the panel does not return NULL)

### Requirement: `link_menu_server()` returns the committed link spec

`blockr.ui` SHALL export `link_menu_server(id)`, a Shiny module server returning a `reactive` that fires once per commit with the committed link spec:

```json
{ "source": "a", "target": "merge1", "link_id": "auspicious_lemur", "block_input": "y" }
```

The JS binding SHALL publish on `input$commit` the value `list(source, target, link_id, block_input, nonce)`. `source` and `target` SHALL be derived from the clicked card's `data-direction` and the root's `data-anchor`:

- `data-direction = "outgoing"` -> `source = anchor`, `target = card.data-block-type`
- `data-direction = "incoming"` -> `source = card.data-block-type`, `target = anchor`

`link_id` SHALL be the per-card link-id input value at commit time. `block_input` SHALL be the per-card block-input select value (the chosen / forced port on the **target** end), or `NULL` when the target end did not render the picker.

The server SHALL strip the internal `nonce` before the spec is returned. The reactive SHALL re-fire on repeat commits (the binding increments the nonce so identical specs still register as changes).

#### Scenario: Outgoing card commit composes source = anchor, target = card

- **WHEN** anchor is `"a"`, the user clicks an OUTGOING card with `data-block-type="merge1"` and the binding commits with `link_id = "lk1"`, `block_input = "x"`
- **THEN** `link_menu_server("mod")`'s returned reactive yields `list(source = "a", target = "merge1", link_id = "lk1", block_input = "x")` (no `nonce`)

#### Scenario: Incoming card commit composes source = card, target = anchor

- **WHEN** anchor is `"b"`, the user clicks an INCOMING card with `data-block-type="a"` and the binding commits with `link_id = "lk2"`, `block_input = NULL`
- **THEN** the returned spec is `list(source = "a", target = "b", link_id = "lk2", block_input = NULL)`

#### Scenario: Block input is NULL when the form did not render it

- **WHEN** the target end has arity 1 (picker hidden) and the binding commits with `block_input = NULL`
- **THEN** the returned spec has `block_input = NULL` (the consumer falls back to the target's first free slot)

#### Scenario: Repeat commits re-fire the reactive

- **WHEN** the same spec is committed twice (two `input$commit` updates with different `nonce`)
- **THEN** the returned reactive fires twice

### Requirement: Single-shot click-to-commit via input binding (no `Shiny.setInputValue`)

`blockr.ui` SHALL ship a `Shiny.InputBinding` registered as `blockr.ui.linkMenu` and bound to `.blockr-link-menu`. Clicking a card body SHALL read the card's `data-direction` and `data-block-type` + the root's `data-anchor` + the per-card form values, compose the spec accordingly, increment the binding's internal `nonce`, record the value on the root, and dispatch a `blockr-link-menu:commit` event. Clicking the chevron SHALL toggle the per-card advanced area open / closed without committing. Clicking the in-card "Add link" button SHALL commit using the current per-card form values. The binding SHALL NOT issue ad-hoc `Shiny.setInputValue` calls; `getValue` returns the recorded spec; `subscribe` listens for `blockr-link-menu:commit`.

#### Scenario: Card body click commits with defaults

- **WHEN** the user clicks a card body (not the chevron, not the advanced area)
- **THEN** the binding sets `input$commit` to an object with `source` / `target` (derived from direction + anchor), `link_id` (the seeded default), `block_input` (the first free slot, or NULL), and a `nonce`

#### Scenario: In-card "Add link" button commits with edited values

- **WHEN** the user opens a card, edits the Link ID and the Block input picker, and clicks "Add link"
- **THEN** the binding sets `input$commit` to an object carrying the edited `link_id` and `block_input`, plus an incremented `nonce`

#### Scenario: Chevron click toggles without committing

- **WHEN** the user clicks the card chevron
- **THEN** the per-card advanced area toggles open / closed
- **AND** `input$commit` is NOT updated

### Requirement: Live pool updates via `receiveMessage` (multi-link sessions)

The `blockr.ui.linkMenu` binding's `receiveMessage` SHALL accept a payload of the shape `list(type = "pool-update", eligible = list(outgoing = <chr>, incoming = <chr>), free_inputs = <named list of <chr>>, link_id_seed = <chr>)`. When such a message arrives, the binding SHALL:

- Toggle `.hidden` on every card based on whether the card's `data-block-type` is in the eligible pool for its `data-direction` - `outgoing` cards are filtered against `eligible$outgoing`, `incoming` cards against `eligible$incoming`.
- Rebuild each card's block-input `<select>` options from `free_inputs[target_id]`, where `target_id` is the card's `data-block-type` for `outgoing` and the root's `data-anchor` for `incoming`. The previously selected port SHALL be preserved when still valid; otherwise the select SHALL snap to the first available option so a subsequent card-body click never wires to a port that was just consumed. The picker field itself SHALL be hidden (`display: none`) when the post-update pool offers <= 1 options - a single-option dropdown adds chrome with no real choice, and the consumer's fallback resolves the sole free port from `spec$block_input = NULL` anyway. Cards whose advanced area never rendered a picker in the first place (single-input or variadic targets) SHALL be unaffected.
- Recompute the panel's `is-empty` state so the empty-state slot shows when every card is hidden.
- Optionally re-seed per-card Link ID `<input>` defaults from `link_id_seed`, leaving any field the user has manually edited untouched. The binding MAY append a monotonic per-card suffix so two cards reading from the same seed propose distinct ids.

The binding SHALL be tolerant of Shiny's `sendInputMessage` auto-unboxing of length-1 R character vectors - both `eligible$*` and `free_inputs[[...]]` values may arrive as JSON scalars instead of arrays, and the binding SHALL coerce as needed.

The binding MUST NOT touch card-expansion state, scroll position, or the search input value. The handler is the multi-link session lifeline: consumers push it after each commit so the user can keep clicking cards without losing their place.

#### Scenario: pool-update hides cards no longer eligible

- **WHEN** the menu is rendered with OUTGOING cards `{merge1, head1}` and INCOMING cards `{}`, and the binding receives `list(type = "pool-update", eligible = list(outgoing = c("head1"), incoming = character()), link_id_seed = "fresh_id")`
- **THEN** the `merge1` card gains `.hidden`
- **AND** the `head1` card remains visible
- **AND** the panel's `is-empty` class is NOT set (one card still visible)

#### Scenario: pool-update drains to empty-state when no cards remain

- **WHEN** the binding receives a `pool-update` with both `eligible$outgoing` and `eligible$incoming` empty
- **THEN** every card carries `.hidden`
- **AND** the panel carries the `is-empty` class so the empty-state slot is visible

#### Scenario: pool-update preserves the user's place

- **WHEN** the user has expanded a card, scrolled half-way down the panel, and typed `"merge"` into the search box, and the binding receives a `pool-update`
- **THEN** the expanded card stays expanded
- **AND** the scroll position is unchanged
- **AND** the search input value still reads `"merge"` (the message handler does not clear the filter)

#### Scenario: pool-update refreshes the per-card block-input select options

- **WHEN** an OUTGOING card targets `m` (arity 2, both ports `x` / `y` initially free) and the card's block-input select shows `c("x", "y")`
- **AND** a link to `m` on port `x` has been committed and the binding receives a `pool-update` whose `free_inputs[["m"]]` is `c("y")`
- **THEN** the card's block-input select is rebuilt to offer only `"y"`
- **AND** an OUTGOING card whose target had a single named input (arity 1) or is variadic - where the advanced area never rendered a picker in the first place - is left untouched

### Requirement: `link_eligible_pools()` helper

`blockr.ui` SHALL export `link_eligible_pools(board, anchor)` returning a list of the shape `list(outgoing = <chr>, incoming = <chr>)` using the same eligibility rules `link_menu_ui()` applies for its initial render. `anchor` MUST be a member of `blockr.core::board_block_ids(board)`; otherwise an error of class `blockr_ui_link_menu_unknown_anchor` SHALL be raised. The helper exists so consumers can recompute pools against the post-commit board without re-implementing the rules.

#### Scenario: Returns both pools for a variadic anchor

- **WHEN** `anchor` is a variadic block on a board with two other blocks `c1` and `c2` (each with at least one free input or variadic)
- **THEN** `link_eligible_pools(board, anchor)` returns `list(outgoing = c("c1", "c2"), incoming = c("c1", "c2"))`

#### Scenario: Empty incoming when the anchor has no free input

- **WHEN** `anchor` has no free input port and is not variadic
- **THEN** `link_eligible_pools(board, anchor)$incoming` is `character()`

#### Scenario: link_eligible_pools rejects an unknown anchor

- **WHEN** `link_eligible_pools(board, "no_such")` is called
- **THEN** an error of class `blockr_ui_link_menu_unknown_anchor` is raised

### Requirement: Client-side search filters cards in place

The bundled JS SHALL reuse `window.BlockrUI.cardSearch` (set up by `blockr-block-browser.js`) for the search filter, applying it across cards in both sections at once. Hide non-matching cards via `.hidden`; collapse empty category sections (and empty top-level direction sections) via CSS; show the empty-state when no card matches. There SHALL NOT be a server round-trip for search.

#### Scenario: Substring search hides non-matching cards across both directions

- **WHEN** the user types `"merge"` into the search input
- **THEN** cards in both OUTGOING and INCOMING sections whose `data-name + data-description + data-package + data-category` haystack contains `"merge"` (case-insensitive) remain visible
- **AND** non-matching cards get `.hidden`
- **AND** direction sections whose cards are all hidden are themselves hidden

### Requirement: `link_menu_dep()` dependency and CSS reuse

`blockr.ui` SHALL export `link_menu_dep()` returning an `htmltools::htmlDependency` referencing the bundled `inst/assets/css/blockr-link-menu.css` and `inst/assets/js/blockr-link-menu.js`. `link_menu_ui()` MUST attach this dependency automatically. The block-browser CSS (`block_browser_dep()`) SHALL also be attached so the shared card visual classes resolve.

#### Scenario: Both the shared block-browser CSS and the link-menu CSS/JS resolve

- **WHEN** the result of `link_menu_ui("mod", board, anchor = "a")` is passed through `htmltools::findDependencies()`
- **THEN** the resolved dependencies include both `blockr-block-browser` (for the card visuals) and `blockr-link-menu` (for the link-specific JS and CSS)

### Requirement: Drop-in replacement for `link_sidebar_body()`

`blockr.dock`'s `add_link_action` SHALL mount the module - `link_menu_server("menu")` once per handler returning the committed-link reactive - and on the trigger `show_sidebar(ui = blockr.ui::link_menu_ui(session$ns("menu"), board$board, anchor = trigger()))`. The sidebar title SHALL read `Connect <anchor>` so the wording works for both directions. The action handler SHALL `observeEvent()` the returned reactive to build the link using `spec$source` / `spec$target`, replacing the per-field observers (`create_link`, `add_link_input`, `add_link_id`, `add_link_confirm`). `link_sidebar_body()` SHALL be removed from `blockr.dock`.

#### Scenario: Action handler observes the returned reactive

- **WHEN** a user triggers `add_link_action` from any block (source or target side), clicks a candidate card, and the binding commits
- **THEN** `link_menu_server()`'s reactive fires with the spec containing both `source` and `target`
- **AND** the handler's single `observeEvent()` on it runs and calls `update(list(links = list(add = ...)))` with `new_link(from = spec$source, to = spec$target, ...)`

#### Scenario: link_sidebar_body is gone

- **WHEN** `blockr.dock` is checked after this change
- **THEN** there is no `link_sidebar_body()` definition in `R/`
- **AND** no `R/` file references it
