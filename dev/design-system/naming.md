# Design tokens: the naming grammar, and the names to decide

**Decided 2026-09-23: all proposals below accepted by the product owner.**

Working document, 2026-09-23. Step one of redoing the blockr design system from
what the packages actually do. It proposes one naming grammar, then lists every
concept that currently has more than one name, so each can be settled once.

Every number below is counted from the code: 91 distinct `--blockr-*` names
across the blockr packages (CSS, JS and inline styles written from R), plus the
token file on blockr.ui main, the `feat/theme-experiment` branch, and the old
spec in `blockr.docs/design-system/`. "Reads" is how many places use the name;
"p" is how many packages.

## 1. The grammar

Three kinds of token, and the second word of the name says which kind it is.

**Palette.** `--blockr-<hue>-<step>`. The raw colours: `grey` (10 steps), `blue` (5), and (new)
`red`, `amber`, `green` (about 4 steps each), plus `accent`, a ramp that
points at `blue` by default and is what a theme overrides. Named after what they look like.
Only other tokens read them. Components never do.

**Meaning.** Named after the job, never after the colour.

| Group | Pattern | Example |
|---|---|---|
| colour | `--blockr-color-<property>-<role>[-<state>]` | `--blockr-color-text-muted`, `--blockr-color-bg-hover`, `--blockr-color-border-danger` |
| type | `--blockr-font-<aspect>-<step>` | `--blockr-font-size-sm`, `--blockr-font-weight-medium`, `--blockr-font-mono` |
| shape | `--blockr-radius-<step>` | `--blockr-radius-md` |
| size | `--blockr-control-h[-<step>]` | `--blockr-control-h-sm` |
| elevation | `--blockr-shadow[-<step>]` | `--blockr-shadow-dropdown` |
| focus, motion | `--blockr-focus-ring`, `--blockr-transition` | |
| data marks | `--blockr-mark-<aspect>` | `--blockr-mark-radius` |

For colour, `<property>` is one of `text`, `bg`, `border`. It comes first
because a colour that works as text often fails as a background, and the name
should say which it was chosen for. This is what Primer (`fgColor-muted`,
`bgColor-danger-emphasis`) and Polaris (`--p-color-text-critical`,
`--p-color-bg-surface`) do, and what our text, background and border tokens
already do. Only the status and accent colours break it today:
`--blockr-color-danger` does not say whether it is for text or a fill.

**Local.** A setting that belongs to one component or one package:
`--blockr-<owner>-<thing>`, where `<owner>` is the package or component
(`dock`, `viz`, `select`). Example: `--blockr-dock-spinner-delay`.

**The rule that makes this checkable:** a global token's second word is one of
the reserved group words: `color`, `font`, `radius`, `control`, `shadow`,
`focus`, `transition`, `mark`, or a palette hue. Anything else is local, and a
local token is only ever read in its owner's own stylesheet. blockr.ui's token
tests (PR #38) can enforce both halves.

**Theming.** The grammar is what lets a theme stay small. Three rules make a
theme a handful of values instead of a stylesheet:

1. Components read meaning tokens only. A theme that changes
   `--blockr-color-bg-accent` reaches everything that reads it, and nothing
   that reads `--blockr-blue-600` directly.
2. The accent is a ramp of its own, `--blockr-accent-50` to `-900`, pointing at
   `blue` by default. A theme overrides those steps and every accent-derived
   token follows. In the theme playground, Forest had to redefine
   `--blockr-blue-*` with teal values to get the same effect, after which
   `blue-600` meant teal. The grey ramp works the same way for warm or cool
   neutrals, and dark mode is a second mapping of the same ramps.
3. Tints are computed, not written as literals:
   `color-mix(in srgb, var(--blockr-accent-600) 12%, transparent)` instead of
   `rgba(37, 99, 235, 0.12)`. Every playground theme had to restate the focus
   ring by hand because it bakes the blue in; viz's `primary-subtle` does the
   same.

With these, a theme is typically five accent values, optionally ten greys, and
perhaps a radius and a font face. Colours drawn in JS or painted in R (charts,
plots) do not read CSS tokens; chart colours come from blockr.theme's palettes,
by design.

**Renaming without breaking anything.** Old names stay in `blockr-tokens.css` as
aliases (`--blockr-color-text-primary: var(--blockr-color-text-default)`) for
a release, and the token tests warn on every read of an alias. Packages move
over when convenient; the alias goes when nothing reads it.

## 2. The names to decide

Proposed picks are marked **bold**. They are proposals; the choice is yours.

### Text

| Concept | Names in use | Reads | Value | Pick |
|---|---|---|---|---|
| main text | `color-text-primary` (125, 18p), `color-text` (3: dm, process, viz), `color-text-secondary` (74, 12p), `color-text-strong` (2, viz) | 204 | grey-900 (secondary: 700) | **`color-text-default`**, grey-900; secondary folds in |
| labels, descriptions | `color-text-muted` (108, 15p), `color-text-meta` (5, same grey-500) | 113 | grey-500 | **`color-text-muted`** |
| disabled only | `color-text-subtle` (101, 13p), `color-text-tertiary` (1, process) | 102 | grey-400 | **`color-text-disabled`** |
| text on an accent fill | `color-on-primary` (branch only) | 2 | white | **`color-text-on-accent`** |

Three levels, not five. Grey-700 and grey-900 are only 1.7:1 apart, so
"secondary" reads as nearly the same as main text. Grey-400 is 2.5:1 on white,
below WCAG's 4.5 for text and 3.0 for icons, so it can only serve disabled
controls, which WCAG exempts; placeholders and hints that must be read move to
`muted`. Grey-500 passes on white (4.8) and grey-50 (4.6) but not on grey-100
(4.4), so `muted` text must not sit on a hover or selected background.

Why rename `text-primary`: "primary" means two different things in the same
system. `color-text-primary` is near-black body text, and `color-primary` is the
blue accent. One of them has to give, and see "Accent" for the other half.

**Labels (decided 2026-09-23):** labels use `color-text-muted` (grey-500),
regular weight, 12px, 4px above their field. Not near-black: a label describes a
value and should not carry the same weight, which is what the three-level
proposal's near-black labels got wrong. This keeps labels inside the three text
colours; the earlier "secondary folds into default" becomes "secondary splits":
labels go to `muted`, the rest of what used grey-700 goes to `default`.

**Where the two old names go (decided 2026-09-23).** Every use of
`text-secondary` (74) and `text-subtle` (101) was sorted by the job it does.
Each job goes to one of the three levels; see `open-items.html`.

| Old name | Job | New token |
|---|---|---|
| `text-secondary` | labels: describe an input or a value | `text-muted` |
| | headings: name a region (section headers, `h4`, table headers, chart titles) | `text-default` |
| | content: menu items, tooltip and popover body, code, suggestions | `text-default` |
| | status lines and footers | `text-muted` |
| | hover state of a muted icon | `text-default` |
| `text-subtle` | placeholders | `text-muted` |
| | meta, hints, counts, empty states, column types, row numbers, `NA` | `text-muted` |
| | icons at rest (handles, remove buttons, fold arrows) | `text-muted` |
| | disabled controls | `text-disabled` |
| | a border drawn with a text colour | `border-strong` |

Headings are `default` because a heading names a region, and a table's column
name would otherwise match the type label under it. Placeholders are `muted`
because a placeholder is often the only hint of what goes in a field, and
grey-400 is 2.5:1.

### Surfaces

Seven-plus names for backgrounds, because there has never been a model of what
sits on what. Proposed: layers, the way IBM's Carbon does it. Each layer sits on
the one before, and anything placed on a layer takes that layer's field and
border tokens.

| Concept | Names in use | Reads | Value | Pick |
|---|---|---|---|---|
| the board canvas | `color-bg-page` (branch, 0 reads) | 0 | grey-50 | **`color-bg-page`** |
| a panel, a card | `color-bg-surface` (branch), `color-bg` (17: viz, outline), `color-surface` (1, sandbox) | 22 | white | **`color-bg-surface`** |
| floating: dropdown, popover | `color-bg-raised` (branch) | 7 | white | **`color-bg-raised`** |
| a quiet fill inside a surface | `color-bg-subtle` (33, 10p), `color-bg-muted` (1, viz), `color-surface-2` (1, viz), `color-surface-muted` (1, dm) | 36 | grey-50 or grey-100 | **`color-bg-subtle`** |
| an input field | `color-bg-input` (59, 17p) | 59 | grey-50 | **`color-bg-field`** (or keep `input`) |
| hover | `color-bg-hover` (34, 11p) | 34 | grey-100 | **`color-bg-hover`** |
| chosen: front tab, selected row | `color-bg-selected` (branch) | 0 | grey-100 | **`color-bg-selected`** |

The fallbacks disagree on what "subtle" is: grey-50 in most places, grey-100 in
`surface-2` and `bg-muted`. Pick one value when picking the name.

### Borders

| Concept | Names in use | Reads | Value | Pick |
|---|---|---|---|---|
| every border | `color-border` (254, 19p) | 254 | grey-200 | **`color-border-default`** (alias the old name; it is the most-read token of all) |
| hovered, or stronger | `color-border-hover` (5), `color-border-strong` (6, undefined, viz + dplyr + seasonal) | 11 | grey-300 both | **`color-border-strong`**, with `-hover` as its state use |

`border-hover` and `border-strong` have the same value and are used for the same
thing. One name.

### Accent (the blue)

| Concept | Names in use | Reads | Value | Pick |
|---|---|---|---|---|
| accent | `color-primary` (226, 18p) | 226 | blue-600 | **`color-*-accent`**, split by property: `text-accent`, `bg-accent`, `border-accent` |
| accent, hovered | `color-primary-hover` (26, 7p) | 26 | blue-700 | **`color-bg-accent-hover`** |
| light accent tint | `color-primary-bg` (14, 5p), `color-primary-subtle` (6, viz: rgba 37,99,235 at 0.09) | 20 | blue-50 | **`color-bg-accent-subtle`** |
| focus | `focus-ring` (29, 8p), `color-focus` (7: viz, ggplot, rgba at 0.45) | 36 | 3px ring at 0.12 | **`focus-ring`**; viz and ggplot's 0.45 ring is a second focus style and should go |

`accent` rather than `primary`, because of the clash with body text above. This
is the largest rename (226 reads), and the alias makes it free to do slowly.
Keeping `primary` and renaming the text instead is the alternative.

### Status

Today only warning has a full set (`warning`, `-text`, `-bg`). Proposed: every
status role gets the same three, following the grammar. `error` and `danger`
merge: the old spec already says "red is one colour", and three different reds
are in use as fallbacks (`#dc2626` ×9, Bootstrap's `#dc3545` ×6 in admiral, dm
and ggplot, `#b91c1c` ×2).

| Role | Names in use | Reads | Pick |
|---|---|---|---|
| danger | `color-danger` (20, 11p), `color-error` (12, 3p), `color-danger-bg` (1, sandbox) | 33 | **`color-text-danger`, `color-bg-danger`, `color-border-danger`** over one palette red |
| warning | `color-warning` (10), `color-warning-bg` (8), `color-warning-text` (2) | 20 | **`color-text-warning`, `color-bg-warning`, `color-border-warning`** |
| success | `color-success` (5, 3p) | 5 | **`color-text-success`, `color-bg-success`, `color-border-success`** |

Input borders (decided 2026-09-23): stay soft, `color-border-default`
(grey-200). The focus state (accent border and ring) marks the active field.

Required-empty cue (decided 2026-09-23): keeps today's soft amber border
(`#f59e0b`, 2.1:1) with the pale amber fill and the label asterisk. The
asterisk states "required" in text, so the border is a second cue, and the
spec wants it soft: attention, not error.

Value conflict to settle: the old spec and eight fallbacks say warning is
`#f59e0b`; the theme branch set `#d97706`. The spec value wins unless you
decide otherwise.

`color-negative` (branch only) colours negative numbers in the table preview.
That encodes data, and data colours belong in blockr.theme. **Remove** from the
UI tokens.

### Type, shape, size, elevation, motion

| Concept | Names in use | Reads | Pick |
|---|---|---|---|
| font sizes | `font-size-xs` (104), `-sm` (108), `-base` (61), `-md` (4: outline, viz; 0.9375 to 0.95rem), `-section` (5), `-title` (2) | 284 | **xs, sm, base, lg, xl**; `md` folds into `base` or becomes `lg`; `section` and `title` become `lg` and `xl` |
| weights | `font-weight-normal` (19), `-medium` (52), `-semibold` (28) | 99 | **keep**; `normal` is missing from the old spec |
| monospace | `font-mono` (15, 5p, undefined) | 15 | **`font-mono`**, defined at last |
| body face | `font` (2, input), `font-family` (branch) | 3 | **`font-sans`**, next to `font-mono` |
| radius | `radius-sm/md/lg/xl` (54), `radius-input` (5, input, 8px), `radius-pill` (branch; `999px` and `9999px` both in use) | 59 | **keep sm to xl and pill**; `radius-input` is `radius-lg` |
| control heights | `control-h` (6), `-sm` (21), `-xs` (1): in the old spec, read by viz, seasonal and io, defined nowhere | 28 | **define them**: 42, 30, 26px |
| shadows | `shadow` (1), `shadow-lg` (5), `shadow-dropdown` (4, undefined) | 10 | **`shadow-sm`, `shadow-md`, `shadow-lg`**; "dropdown" is a use, not a size |
| motion | `transition` (22, io only, undefined) | 22 | **define it**, or drop it and let io use a literal |

### Local settings using the global prefix

These read like global tokens and are not. Under the grammar each gets its
owner's name. Pure renames, each inside one package.

| Package | Names | Proposed |
|---|---|---|
| dock | `spinner-delay`, `sidebar-width-left`, `sidebar-width-right`, `sidebar-panel-width`, `stack-height` | `--blockr-dock-*` |
| dock (theme branch) | `block-header-gap`, `block-header-row-gap`, `block-header-actions-gap`, `block-title-*`, `block-icon-size`, `dock-panel-outline` | `--blockr-dock-header-*`, `--blockr-dock-panel-outline` |
| viz | `rank-fill`, `rank-track`, `rank-tick`, `rank-sub`, `rank-bar`, `rank-lane-min` | `--blockr-viz-rank-*` |

### How many greys

102 different greys are written as hex across the packages. The ten ramp steps
carry 1,799 of those uses (86%); only 600 and 800 are nearly idle (19 each).
The other 92 greys, 300 uses in 22 packages, are off the ramp: Bootstrap's
(`#6c757d`, `#dee2e6`, `#f8f9fa`), web-safe ones (`#333`, `#666`, `#999`,
`#ccc`, `#ddd`), warm ones. None of them follow dark mode or a theme.

- Keep the ten-step ramp; dark mode needs the range, and ten is ordinary
  (Tailwind 11, Radix 12).
- Rule: no grey that is not a ramp step. The token tests can flag any hex that
  is not in the palette.
- The meaning tokens use six steps, 50, 100, 200, 300, 500 and 900, plus 400
  for disabled. Backgrounds keep several names (page, field, hover, selected)
  because they are different jobs that diverge in dark mode, over three light
  values: white, 50 and 100.

### The other colours

Same pattern, proportionally worse. Share of uses that are on the palette:
grey 86%, blue 69%, red 42%, amber 34%, green 11%; yellow, teal, purple and
pink have no UI role and 0%. The off-palette colours are three different things:

1. **Block category colours and data colours.** Both legitimate, and both
   outside the UI rule, but they are different things:
   - *Block categories* use the Okabe-Ito palette, one colour per category,
     through `blk_color()` in blockr.dock: input `#0072B2`, transform
     `#009E73`, structured `#56B4E9`, plot `#E69F00`, table `#CC79A7`, model
     `#F0E442`, output `#D55E00`, utility `#CCCCCC`, uncategorised `#999999`.
     Part of the design system as a fixed set: kept as they are, identical
     under every theme and in dark mode.
   - *Data colours* (chart palettes, map markers) belong to blockr.theme.
   Together: 285 uses in palette files, 62 elsewhere.
2. **Near misses.** 127 uses of neighbouring Tailwind shades (`#fee2e2`,
   `#fca5a5`, `#b91c1c`, `#ef4444`, and their green and amber equivalents).
   Someone needed a pale red fill or a darker red for text and the palette had
   none. Evidence that each status hue needs a small ramp, as blue already has.
3. **Strays.** Bootstrap's `#dc3545` (18), `#ff0000`, and similar. Replaced.

Proposed palette: grey (10 steps), accent (5, blue by default), red, amber and
green with about four steps each; roughly 27 values. One rule for every hue:
no UI colour outside the palette.

### Small icons and rows (decided 2026-09-23)

Drag handles, remove buttons and fold arrows are drawn with thin strokes and
small dots in `text-muted`. Handles and remove buttons are hidden at rest and
appear on hover or keyboard focus of their row; remove turns `text-danger` on
hover. Fold arrows stay visible. (`icon-options.html`, option E.)

A row's content starts on the same edge as the label above it. The handle does
not take space at rest: it appears in the container's left padding, which must
be at least 14px. (`open-items.html`, option B.)

### Body face (decided 2026-09-23)

Open Sans, as bslib's Shiny preset ships it. Inter was tried across a whole
board, with the optical-size axis held at the text cut and macOS grayscale
smoothing, and did not win. The face is set in one place,
`--bs-body-font-family`, so a later switch changes that variable and the four
places in blockr.viz that name Open Sans (chart canvas, ECharts theme, table
CSS).

### Field labels everywhere (confirmed 2026-09-23)

The label decision above holds for every package: 12px, weight 400,
`text-muted`, 4px above the control. blockr.dplyr (12px/500), blockr.viz's
band labels and the code and function blocks (13px/500 at 5px) move to it.
(`block-layout.html` L2, option A.)

### Output title (decided 2026-09-23)

A chart's or table's own title is 16px (`font-size-lg`), weight 600,
`text-default`; subtitle 13px `text-muted`, caption 12px `text-muted`. It
replaces viz's 15px `font-size-md` in grey-800, so `md` stays out of the scale.
(`block-layout.html` L3, option B.)

### Decided on questions.html (2026-09-23)

Christoph's answers to the open-questions page. Ids refer to
`questions.html`; the options are drawn there. Still open, each for its own
topic page: G3 and G5 (gear band placement and closing), C1 (checkbox or
switch), M5 and M6 (popovers, tooltips), B1 and B2 (badge vocabulary and shape).

- **T6 One grid for fields:** B. One grid, two field sizes. One set of columns, about 130px each, 8 / 6 / 4 / 2 / 1 by panel width; a number or checkbox takes one, a select or text two, tags or an expression the full row (topics/06-grid.html).
- **G1 The gear button and the tools beside it:** A. The gear is the one control in the row that opens configuration, and the frame says so;
the reader's tools stay as quiet as the dock header's icons, and in simplified mode the gear leaves a row of quiet tools
behind. Icon muted at rest. Hover: default text, strong border, hover fill. Open: the main-button tint (hover over this
page's buttons to compare). Bootstrap gear-fill at 14px everywhere; the patient profile's 16px outline gear goes. One
class, .blockr-gear-btn, on every gear and on nothing else, so dock's one rule reaches the patient profile and
the report and stops hiding the composer's search. Tooltip "Settings".
- **G2 The header row in a narrow panel:** A. Title shares the row (viz today) (changed from the proposal). The title takes what the tools leave and wraps down.
- **G4 Inside the band: a heading, and how sections are separated:** A. The beak already says which button opened the band, and "Chart settings" under a chart's
gear adds a line without adding a fact. Sections are separated the way the layout part separates them on the face: a
section title and 24px of space, in muted, with no rules. A section that a checkbox switches on keeps the checkbox before
its title, and its fields appear below when checked. The band's accessible name moves to aria-label. Fields
use the params grid.
- **G6 What stays on the face, and what simplified mode leaves:** A, written down as the rule. A transform block's face holds the fields that define it and
the gear holds the rest; an output block's face holds the output and its sentence, and the sentence's slots are the only
way to put one of its arguments on the face; its gear keeps a row for every argument, slotted or not. Simplified mode removes the gear button and
nothing else, so what remains on the face stays live for the reader. The patient profile's three view toggles are reader
controls, so under A they leave the gear for the face. *Note: Idea for later (inbox): transform blocks could tell their story as a sentence with live words, as B draws it.*
- **T1 Telling the gear band apart:** B. Grey tray. The open band takes the subtle background, no border, and its fields turn white; in dark it takes the page colour (topics/01-gear-band.html).
- **C2 One of two or three fixed values:** B, a segmented control for a fixed set of two or three short values. Both values stay in
view, the pick is readable without a click, and it fills a grid cell like any field. Three segments need about 240px, so a segmented cell takes a "wide" track, not a "knobs" one. Selected = the tinted accent of the main
button. Four or more values, or labels too long for the cell, go to Select. Options that come from the data (columns, measures,
levels) stay Select at any count, which is the case the 2026-07-22 decision was made for. This replaces the cycle pills of this
kind, the viz Order select, the head block's "Tail" switch ("From: First / Last"), stats' shinyWidgets buttons and the
dm/io/pharma segmented copies, and it is the component the ggplot facet wrap/grid strip already is.
- **C3 Four or more values inside a row: filter operator, join type:** B, the pill opens a menu (Blockr.Select.menu() anchored on it), 26px, with a
caret so it reads as a choice. It keeps the row's shape and makes every value one click away. Same for the join key operator
and viz's .dd-func-btn (n, N, %, Σ). Join type sits in the join block's header, outside a row, so it becomes a
Select field in the grid ("Join type: left join"), and core merge's two checkboxes all.x/all.y
become that same field. The click-to-cycle pill is retired.
- **C4 Several from a list:** B. Tags for both (changed from the proposal). One multi control; fixed options are hidden until opened. *Note: Tags feel more flexible, and they are already in use.*
- **C5 Builder or code: how to switch the way a value is edited:** A, the pressed icon button, moved from 22px to the 26px tool size and to token colours. A
view toggle is a tool, so it looks like the gear and the other header tools, and it stays out of the field grid. Its tooltip names
the other state ("Edit as formula text", "Back to the builder"), and it sets aria-pressed, as today.
- **C6 Number fields:** B, a plain field with the Enter chip. Numbers follow the text-commit decision
(2026-07-02), so a board does not recompute on every arrow click, and the field looks like every other 42px field. The input
keeps type=number for the keyboard and hides the spinner arrows. A Bootstrap override brings Shiny's
numericInput to the same look; the commit timing there needs R-side work per block.
- **M1 The raised surface and its density:** A for every menu, dropdown and popover: bg-raised, 1px border-default,
radius-lg (8, the same as the field it opens from), shadow-md, 4px padding, rows 32px at 14px with
radius-sm and 10px side padding, meta text 13px. It is what Blockr.Select, the busiest menu, already does, plus the
4px inset the menus use. The beak stays with the gear band. The dock "…" menu, the download menus and the add menu
move to it.
- **M2 Row states: hover, current pick, disabled, and dark mode:** A. bg-hover becomes color-mix(in srgb, var(--blockr-color-text-default) 6%, transparent):
in light it lands on grey-100 as today, and in dark it is lighter than whatever it sits on, surface or raised. Hover and the
keyboard row look the same. On that row, meta text and icons turn default, which is the muted rule applied. The current pick
is 600, as Select already does (select-controls record, 2026-07-22); the views menu drops its blue fill for it. Disabled rows
stay listed in text-disabled, no hover, with the reason in a tooltip. In dark, bg-selected needs its
own value above bg-raised.
- **M3 Group titles and dividers:** A, with 8px above and 4px below, inset to the row text (10px). In a list that scrolls the title
is sticky. A divider (1px border-default, 4px above and below, edge to edge) separates groups that have no title,
such as ordinary actions from a destructive one.
- **M4 Placement: portal or inside the block:** A, one placement routine for all of them (Select's computePosition, moved to blockr.ui).
Sizes: a field dropdown takes the control's width, at least 190px. A menu from a button or a word sizes to its content, 180 to
320px. It lines up with the trigger's left edge, or its right edge when the trigger is in the header row. Height up to 320px,
then the list scrolls under its sticky titles and filter box. A filter box appears above 8 options. 8px from the viewport
edge.
- **B3 Counts on a control:** C. Tint of the host (changed from the proposal). The host's colour at 14%. Accent-600 on it is 3.9:1, under the
    4.5 small text needs. *Note: C as drawn fails small-text contrast (accent-600 on the 14% tint is 3.9:1); the count text takes accent-700.*
- **B4 When a badge may have colour:** A. Neutral unless the badge states a status or that something is on. Accent tint means on,
added or cutting rows, the same tint as the main button. Status tints only for danger, warning and success; io's blue "info"
("New directory (created on save)") becomes neutral, because blue means "on". Types, packages and significance levels are
neutral; the type icon beside the name already carries the type. Data colours (the arm chip in the cohort list, the tile's
colour-by pill) come from the board's scale and are outside this rule.
- **B5 How a block says it is filtering, and where the way back is:** B. The control that undoes the filter wears the accent tint while a filter is on, carries the
number of active filters (B3), and names the clause in its title. Size is the 26px xs button, so the patient
profile's segment grows from 23px to 26px. Where the count is itself a control (the cohort tag opens the sidebar), count and
reset join as one segment, as the patient profile does. The heatmap's dot goes; dots are for block status only. *Note: The principle holds; the CDEx table itself no longer carries drill state.*
- **B6 Tags: the × at rest:** B. The × appears on hover or focus of the field it belongs to, and turns red on its own
hover, as decided for rows. Tag text moves from grey-700 to text-default (a picked value is content). *Note: Only the hovered tag shows its ×, not every tag in the field.*
- **S1 Text on the chart canvas: one mark size, or the type scale?:** A, one mark size of 11px, as a token in the mark group that chart.js reads once per
render instead of repeating the literal in about 15 places. The gutter measurement, the label-row budget and the printed
charts keep their numbers. Outside the plot the chart follows the scale: title per block-layout Q3, subtitle 13px, caption
and status line 12px, tooltip per S3. The facet strip takes the section-title style (block-layout Q1, 0.05em) instead of its
own 0.03em copy. *Note: Whether ECharts takes the size cleanly is an implementation detail.*
- **S2 Canvas ink in dark mode: where the chart's own greys and face come from:** A, the canvas reads the tokens at render. One mechanism covers colours and the face, it
follows a theme's greys as well as the scheme, and chart.js already reads computed styles for the PNG export
(colorOf(), l.1738). Mapping: axis and data labels text-muted, axis line border-strong, split lines
border-default, separators and halo bg-surface, reference lines border-danger, face from the body. Exports (PNG, pptx) take
the light values, since they land on white slides and pages. The "ECharts Theme" board option then overlaps with blockr.theme
and the dark scheme; retiring it is a follow-up. *Note: Accepted on the condition that it has no hidden cost in chart.js.*
- **S3 Chart tooltips:** A, the raised card. It reuses the dropdown's layer and shadow, so it needs no new colours and
works in dark mode by itself; the swatch in the headline carries the series colour that the border carries today. Labels
muted, values default at 500 with tabular figures. The slider bubble stays inverse, because it labels a thumb during a drag. *Note: The same surface as the dropdowns.*
- **S4 Crossfilter: where the header row goes:** B, the standard header row. The crossfilter then has the anatomy of every other block, and
Reset still sits above every filter it clears. "Group by" and "Filter by" stop being two labels in one style: one labels a
field, the other titles a section. *Note: B is the general rule. The population filter (a BMS block that joins two blocks in one) keeps A.*
- **S5 Crossfilter: the controls inside a filter card:** B. Everything to the canon (changed from the proposal). Search 30px, 26px icons always shown, rows 30px like list rows. The 200px scroll area shows about 6 rows instead of 10. *Note: The scroll area then shows about 6 rows instead of 10.*
- **S6 Code editor: surface and syntax colours:** A. The editor is a large input, so it takes the field background, and its colours come from
four local tokens (--blockr-code-keyword, -string, -number, -comment)
that point at text-accent, text-success, text-warning and text-muted. Those are already tuned for contrast in both schemes
and follow a theme's accent. Line numbers muted, active line bg-hover, the input-line band mixed from the accent. Code is
13px (sm) on every code surface, so dplyr's expression input moves from 14px.

### Consistency pass (2026-09-23)

A review of design-system.md against this record, the token files and
index.html. These close contradictions; none reopens a decision.

- **Tooltips are radius 8.** The tooltip rule says it matches the chart's
  data tooltip, which is radius 8; the light card said 6. `radius-md` now
  serves 30px buttons, segments and messages.
- **The accent tint is a set of tokens.** The main button's look (7% fill,
  35% edge, 13% on hover) was written out by hand in nine places, mixed from
  the palette's `accent-600`. It is now `bg-accent-subtle`,
  `bg-accent-subtle-hover` and `border-accent-subtle`, mixed from
  `border-accent`. `bg-accent-subtle` was accent-50, nearly the same colour;
  the legacy `primary-bg` keeps accent-50. An "on" badge takes the same tint,
  so its edge goes from 30% to 35%.
- **Status edges are tokens:** `border-danger-subtle` and
  `border-success-subtle` at 35%, `border-warning-subtle` at 45%. Badges said
  30% and messages 35%; the drawings used 40 and 45% for amber, which is
  paler than the other hues. The destructive button's edge takes
  `border-danger-subtle`; its fill mixes from `border-danger` (6%, 12% on
  hover). Its hover was drawn but missing from the spec.
- **`text-accent-strong`** (accent-700, `text-accent` in dark) replaces the
  palette read for a count on an accent host and the avatar's initial.
- **The status dot reads local tokens,** `--blockr-dock-status-stale`,
  `-waiting`, `-failed`, `-unset`, each pointing at a meaning token.
  `-unset` points at `border-warning`, the same amber as the empty field's
  cue (amber-500 in light, as decided under "Five details").
- **The word "chip" is gone from the spec.** The Enter chip is the Enter
  button, an offer is a small dashed button, receipt chips are tags without
  ×, the arm chip is a data-coloured label. The chart prepare script's Apply
  is stated as the one exception to "no Apply button", with Run.
- **The 26px floor** names every exception: the 24px pill, tag and Enter
  button inside a 42px row or field, and the 16px checkbox box.
- **The body face** is set through `--bs-body-font-family`, by blockr and by
  a theme. There is no `--blockr-font-sans`; the token file said a theme
  sets it.
- **Picking a block** and **blockr.outline's move off `--md-*`** are now in
  the spec.
- **Token tests:** the spec claimed tests enforce the grammar and warn on
  legacy names. They do not exist yet; the spec lists them as to do.

### Renaming and the navbar (decided 2026-09-23)

(`topics/20-editable-text-and-navbar.html`.)

- **Q1, the edit look:** A. The text becomes a field (accent edge, focus
  ring) at its own size; a hover wash, no pencil. The block title's visible
  "Block name" label goes.
- **Q2, starting a rename:** A. Double-click everywhere, plus "Rename" in the
  item's menu. Enter commits, Escape restores, blur commits; empty or
  duplicate names are refused in place.
- **Q3, the workflow's name:** A. The ID, chosen at the first save, stays;
  it IS the name, like a file name: unique, and it may contain spaces. Changed
  from the proposal (a separate editable title): a title is not used. The ID
  is shown in the body face, not monospace. blockr.session's ID rules need to
  allow spaces.
- **Q4, the navbar:** B. On the system: 30px controls, tokens only, the save
  state as text, one "…" workflow menu instead of the two split buttons, the
  view menu as a quiet button, an accent-tint avatar at the far right. With
  Q3, the name shown is the ID.

### The block header (decided 2026-09-23)

(`topics/19-block-header.html`.)

- **Default header:** B. Compact: a 28px mark, the 16px title, the actions;
  no subtitle. Changed from the proposal (C, with the type inline). Switch to
  it soon; today's header is still acceptable until then. The block type and
  package become the mark's tooltip. A slightly smaller mark (24px) was
  floated and left open.
- **Bare mode:** C. Later, all in the tab strip (mark and name in the tab,
  actions right of the tabs, rename by double-click); it waits on dockViewR.
  No CSS-only interim (option B is dropped).
- **The mark at each size:** A. Header 28, list rows 24 with the package as a
  badge, tab 16. This settles the list-row question from topic 18.

### Picking a block (decided 2026-09-23)

(`topics/18-block-pickers.html`.)

- **Adding a block:** A. A menu at the "+", everywhere: the outline, a
  block's append, the board's Add block. The dock's block browser sidebar and
  its "configure before adding" go; the ID is generated and the title is
  renamed on the block.
- **Descriptions while browsing:** A. Never; rows show the name and the
  package only.
- **Picking a block already on the board** (Add panel, link to, add to stack):
  B. The same menu, listing the board's blocks (mark, title, type as meta).
  The Bootstrap modal with selectize goes.
- **The block's mark in a list row:** open; the direction is B (tinted square,
  package as a badge) but smaller than 32px. Settled with the header (topic 19).
- blockr.outline moves from its own `--md-*` variables to the blockr tokens.

### Five details (decided 2026-09-23)

(`topics/17-last-details.html`.)

- **Tooltip timing:** B. After the pointer rests 800ms, above the element;
  below only where there is no room above. Changed from the 400ms proposal:
  passing over a row of tools shows nothing.
- **Count on a secondary or destructive button:** A. The count takes its
  button's colour, as B3 does for the main button.
- **Status dot, inputs not set:** A. `amber-500`, matching the amber cue on the
  empty field; replaces `#eab308` in blockr.dock `R/plugin-block.R`.
- **Block-level error or warning:** A. The message style, above the output.
- **Busy cue:** B. Every block gets the composer's cue: past 300ms the output
  dims to 0.45 and a clock runs under it. blockr.core applies it.

### No popovers (decided 2026-09-23)

blockr has no popovers (`topics/05-popovers-tooltips.html`, Q1 option C; M5).
The block subtitle becomes plain text: the block's package and description
head its "…" menu, and "Copy block ID" is a menu item. Floating things are
then menus (choices and actions, opened by a click), the gear tray (settings,
in flow) and tooltips (names, on hover).

### Tooltips (decided 2026-09-23)

One tooltip style for every name shown on hover: a small light card, the
raised surface with a soft shadow, 12px text (`topics/05-popovers-tooltips.html`,
Q2 option C; M6). It matches the chart's data tooltip (S3), so blockr has one
floating style. Every icon-only button and every cut-off label gets one;
nothing else does. No native `title` tooltips remain. The dark tooltip was
the common convention; it lost because no popovers compete with a light card
here and the chart tooltip is already light.

### Badges, tags, pills, counts: shape tells (decided 2026-09-23)

(`topics/04-badges.html`, option B; answers B1 and B2.)

Four kinds, one class each in blockr.ui: `.blockr-badge` (you only read it:
"Categorical", "blockr.dplyr", "Added"), `.blockr-tag` (a value you picked,
with × when it can be removed), `.blockr-pill` (a click changes a setting,
e.g. the operator), `.blockr-count` (a number on a button). The word "chip"
goes.

Shape tells them apart: a badge has round ends (a capsule, 18px, 11px weight
500, 7px side padding, `bg-subtle` fill, `border-default` edge,
`text-muted`); tags, pills and buttons, which you act on, keep 4px corners.
Colour on a badge only for status and "on" (B4).

### Checkbox, not switch (decided 2026-09-23)

On/off is a bare checkbox everywhere: in the gear tray, on a block's face and
in the board options (`topics/03-checkbox-or-switch.html`, option A; C1).
Standing alone, the checkbox reads better than a switch. The one place a
switch looked better was a boolean inside a grey field shell, and that shell
is gone (topic 6b). If a boolean ever has to sit inside a field shell again,
this is the question to reopen. Core's `bslib::input_switch` calls and the
pharma header switch become checkboxes.

### The gear band: placement and closing (decided 2026-09-23)

(`topics/02-gear-band-behaviour.html`, answers G3 and G5.)

- **Placement:** B. In flow under the header row, in the grey tray (T1),
  sliding open over about 0.22s so the output is seen being pushed down and
  back up. Nothing is covered. The crossfilter's floating settings and the
  patient profile's popover move into the same tray.
- **Closing:** A. The gear and the Escape key only. A click on the output, or
  on a menu inside the tray, leaves it open. The open state lasts for the
  session (a re-render does not close it) and is not saved with the board.
  Changes apply as they are made; there is no Apply button. Several trays may
  be open at once.

### The sentence and its slots (decided 2026-09-23)

The premise: the sentence on a block's face looks as it will in print. The
design system does not lay it out beyond that (`topics/16-sentence-slots.html`).

- **Q1, line breaks:** A. One sentence. The author decides where lines break,
  by writing `\n` in the template; blockr adds none of its own.
- **Q2, a slot with many values:** B. The first three values, then "+N more",
  inside the same live word; the click opens the full list. Exports print the
  same text. The cut moves from 8 (`TITLE_MAX_VALUES`) to 3.
- **Q3, the "+" offers:** B. On their own line under the sentence, three at
  most; past three the last one reads "More settings" and opens the gear.
  Offers are not part of the printed sentence.
- **Q4, who writes the sentence:** A. Always the author, in the
  `#| subtitle:` template. Nothing is generated; a setting the template does
  not name stays in the gear.

### Rows are control height (decided 2026-09-23)

A row in an add-and-reorder list (filter conditions, columns) is 42px, the
same as an input, border included (`box-sizing: border-box`), as
`.blockr-row` in blockr.dplyr already is. Its operator pill is 24px inside it.

### A checkbox in the field grid (decided 2026-09-23)

A checkbox in the grid is the bare box and its words, with no field shell and
no empty label row. It sits on the bottom line of its grid row, which is the
controls' line, and takes one column. On a row of its own it takes only its own
height. (`topics/06b-checkbox-in-grid.html`, option C.)

### Section titles inside a block (decided 2026-09-23)

One style for "a group of fields starts here", on the block's face and in the
gear band: 12px (`font-size-xs`), weight 600, uppercase, 0.05em tracking,
`text-muted`. It replaces the five copies in use (settings band title,
`.dd-section-title`, `.dd-at-head`, `.dd-type-group-head` in viz, crossfilter
table and search headers in dm, select menu group titles in dplyr), three of
which draw in grey-400. There is no 16px heading inside a block.
It works as a divider in a long list of inputs more than as a title, so it
must not pull the eye away from the fields.
(`block-layout.html` L1, option A.)

### Buttons (decided 2026-09-23)

Four kinds, three sizes (`buttons.html`, section 3):

- **Main:** tinted accent (accent text, accent at 7% fill, 35% border; 13%
  fill and full border on hover). At most one per view. There is no solid
  accent button; Bootstrap's `btn-primary`, `btn-danger` and `btn-success` go.
- **Secondary:** surface background, `border-default`, `text-default`.
- **Quiet:** text only, `text-muted`, hover background on hover. Replaces the
  grey-400 text links.
- **Destructive:** the main style in red, only to confirm a removal.

Sizes follow the control heights: 42px beside inputs, 30px in toolbars and
dialogs, 26px in a header strip next to the gear. The crossfilter reset, the
patient-profile drill reset and dplyr's commit chip are the existing
instances of the main style and converge on these numbers.

## 3. Things found along the way

- **Components read the palette directly.** 587 reads of `--blockr-grey-*` and
  `--blockr-blue-*` across 19 packages (grey-400 alone: 157). The old spec's
  first rule forbids this. It is the main obstacle to dark mode being a
  one-file change, and the bulk of the cleanup.
- **Nothing the old spec names is dead.** Every token it lists is still read
  somewhere. The spec was outgrown, not wrong.

## 4. What to decide, in short

1. The blue: `accent` (proposed) or keep `primary`.
2. Text: three levels, `default / muted / disabled`; grey-400 fails contrast as text.
3. Surfaces: the layer model, `page / surface / raised`, plus `subtle`,
   `field`, `hover`, `selected`.
4. Status: `text-`, `bg-`, `border-` for each of danger, warning, success;
   `error` merges into `danger`; warning is `#f59e0b`.
5. Local settings: `--blockr-<owner>-*`.
6. Renames go through aliases, with the token tests warning on old names.

Once these are settled, the next step is the token tables with light and dark
values and contrast ratios, then one page per component, then the showcase.
