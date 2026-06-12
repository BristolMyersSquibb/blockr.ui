## ADDED Requirements

### Requirement: Board-reactive menu servers

`blockr.ui` SHALL extend `link_menu_server()` and `stack_menu_server()` with
a `board` argument that is a Shiny **reactive** returning the current board,
defaulting to `NULL`. When `board` is `NULL` the server SHALL behave exactly
as today (a static snapshot rendered once by `*_menu_ui()`), preserving
backward compatibility. When `board` is a reactive, the module SHALL
`observe()` it and, on every change after the initial value, emit a
self-scoped synchronisation message (see the sync-protocol requirement) to
keep the open menu consistent with the board WITHOUT re-rendering the panel.

The synchronisation message SHALL be addressed to the module's own binding
root (`NS(id)("commit")`). When the module's panel is not currently in the
DOM (another consumer owns the shared sidebar slot), the message SHALL have
no effect; the module SHALL NOT track or depend on which consumer owns the
slot.

#### Scenario: NULL board preserves snapshot behaviour

- **WHEN** `link_menu_server("menu")` is mounted with no `board` argument
- **THEN** no board observer is created and the menu behaves as the current
  static snapshot

#### Scenario: Board change emits a sync message

- **WHEN** `stack_menu_server("menu", board = reactive(rv$board))` is mounted
  AND a block is removed from `rv$board`
- **THEN** the module sends a `menu:sync` message to `menu-commit`
- **AND** the panel is not re-rendered (no `renderUI` / `show_sidebar` call
  originates from the module)

#### Scenario: Sync no-ops when the panel is absent

- **WHEN** the module's panel is not mounted in the DOM AND the board changes
- **THEN** the emitted message targets `menu-commit` and is harmlessly
  dropped by the client (no error, no effect on any other panel)

### Requirement: `menu:sync` reconciliation protocol

The instance menus' input binding SHALL accept a `menu:sync` `receiveMessage`
payload carrying the menu's full desired state: the set of cards that SHALL
exist now (each with an `id`, a `direction` for the link menu, and card
`html` for cards not already in the DOM), per-direction (link) or flat
(stack) eligibility id sets, per-target free input ports (link), the
currently-selected member ids (stack), and a fresh id seed. The client
reconciliation SHALL:

- Remove card nodes whose id is absent from the payload's card set.
- Insert card nodes present in the payload but absent from the DOM, using
  the carried `html`, into the correct category / direction section,
  creating the section container when none exists.
- Retune surviving cards: toggle their eligibility / `.hidden` state, rebuild
  the input-port `<select>` from the free-inputs map (preserving a
  still-valid current selection), and reconcile the stack's selected state.
- Recompute the panel's empty-state.

The reconciliation SHALL NOT alter scroll position, per-card expansion
state, the search input value, or any panel-level input the user has edited
(stack name / colour / id; a user-edited per-card link id). Card markup SHALL
be produced by the same R card builder used for the initial render (no
parallel client-side card template). The protocol SHALL generalise the
existing link-menu `pool-update` (which could only toggle cards already in
the DOM) so that newly-eligible or newly-added blocks appear without a
server re-render.

#### Scenario: Removed block's card disappears live

- **WHEN** a pinned link menu is open AND a block on the board is removed
- **THEN** that block's card is removed from the panel
- **AND** scroll position and the expansion state of other cards are
  unchanged

#### Scenario: Removing a link re-opens a drained target

- **WHEN** a pinned link menu shows its empty-state because the only target
  was fully wired AND the existing link is removed elsewhere
- **THEN** the now-eligible target's card appears (inserted from the payload
  `html`, since it was not previously in the DOM)
- **AND** the panel's empty-state clears

#### Scenario: Newly added block appears live

- **WHEN** a pinned menu is open AND a new block is added to the board
- **THEN** a card for it is inserted without re-rendering the panel
- **AND** the search query the user had typed is still in the search box

#### Scenario: In-progress stack edits survive a sync

- **WHEN** a pinned stack menu has a half-typed stack name AND an unrelated
  board mutation triggers a sync
- **THEN** the typed name is preserved and only the card set / selection is
  reconciled

### Requirement: Menu-owned validation

Incorporating PR #172 review feedback, the instance-menu servers SHALL take a
`session` argument and SHALL own validation of the committed payload: the
committed reactive SHALL fire only with an already-valid value, having
checked the relevant constraints against `board()` and surfaced any failure
via `notify()` from inside the module. For the stack menu the constraints
SHALL be id-uniqueness against `board_stack_ids(board())` (create flow),
name presence, and hex-colour format; for the link menu, id-uniqueness
against `board_link_ids(board())`. A failed validation SHALL NOT fire the
committed reactive. Validation SHALL use only `blockr.core` facilities;
`blockr.ui` SHALL NOT depend on `blockr.dock`.

Consequently the consumer (`blockr.dock`) SHALL NOT re-validate, and its
former validators (`valid_stack_id`, `valid_stack_name`, `valid_stack_color`,
and the id-collision link/block validators) SHALL be removed.

#### Scenario: Duplicate stack id is rejected in the module

- **WHEN** a stack create commit carries an id already in
  `board_stack_ids(board())`
- **THEN** the committed reactive does not fire
- **AND** a warning notification is shown from within the menu module

#### Scenario: Valid commit fires once

- **WHEN** a stack create commit carries a fresh id, a non-empty name, and a
  valid hex colour
- **THEN** the committed reactive fires exactly once with the validated value

### Requirement: Validated committed result

The instance-menu committed reactive SHALL return a validated result the
consumer can apply without reshaping or re-checking. The link menu MAY
return the constructed `blockr.core` `link` / `links` object directly (a core
class). The stack menu SHALL return a validated `blockr.core` `stack` (or a
validated spec) - NOT a `blockr.dock` `dock_stack`, which would invert the
dependency direction; the consumer performs the thin `dock_stack` upgrade.
The committed-spec field names that consumers already rely on SHALL be
preserved unless replaced by the richer object.

#### Scenario: Stack menu returns a core stack, not a dock_stack

- **WHEN** a stack commit is validated and fires
- **THEN** the returned object is a `blockr.core` `stack` (or validated spec)
  carrying the name, colour, and member ids
- **AND** it is not a `blockr.dock` `dock_stack` (the consumer upgrades it)

#### Scenario: Consumer applies the result without re-validating

- **WHEN** the consumer receives the committed result
- **THEN** it constructs the board update directly (link: `update(links =
  add ...)`; stack: upgrade to `dock_stack` then `update(stacks = add ...)`)
  with no further validation
