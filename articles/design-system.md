# The blockr design system

This is the reference for how blockr looks and behaves: tokens, the
block, its layout, controls, actions, the floating layer, labels and the
special blocks. It states each rule once, with exact values and token
names. The [design system
page](https://bristolmyerssquibb.github.io/blockr.ui/articles/design-system/index.md)
draws the whole system with the same table of contents, so every section
here has an anchor of the same name there. Token values come from
`inst/assets/css/blockr-tokens.css` and `blockr-tokens-dark.css` in
blockr.ui.

## 1. Principles

- Components read meaning tokens only. Palette tokens
  (`--blockr-grey-500`, `--blockr-blue-600`) are read by other tokens
  and never by a component.
- No UI colour outside the palette, and no grey that is not a ramp step.
  Data colours and block category colours are the two exceptions, each
  with its own source (see [Foundations](#foundations)).
- Tints are computed from meaning tokens with `color-mix()`, never from
  a palette token and never written as rgba literals, so they follow a
  theme and the dark scheme. The ones used in many places are tokens
  themselves (see [Borders](#foundations)).
- Three text levels: default, muted, disabled.
- Every configurable option is in the gear. A block with no options has
  no gear. Simplified mode removes the gear button and nothing else.
- Changes apply as they are made. There is no Apply button in a block’s
  settings. Text and numbers commit on Enter or blur.
- One control per kind of decision (see the table in
  [Controls](#controls)). Shape tells a reader what can be clicked:
  badges have round ends, things you act on have 4px corners.
- Breakpoints follow the panel, never the viewport. Every size step is a
  container query on the block.
- Floating things are menus (choices and actions, opened by a click) and
  tooltips (names, on hover). There are no popovers.
- The sentence on a block’s face looks as it will in print.

Drawn in the [design system
page](https://bristolmyerssquibb.github.io/blockr.ui/articles/design-system/index.html#principles).

------------------------------------------------------------------------

## 2. Foundations

### Token grammar

The second word of a token’s name says what kind it is.

| Kind | Pattern | Example | Read by |
|----|----|----|----|
| Palette | `--blockr-<hue>-<step>` | `--blockr-grey-200` | other tokens only |
| Meaning | `--blockr-color-<property>-<role>[-<state>]` | `--blockr-color-text-muted` | components |
|  | `--blockr-font-<aspect>-<step>` | `--blockr-font-size-sm` |  |
|  | `--blockr-radius-<step>`, `--blockr-control-h[-<step>]`, `--blockr-shadow-<step>` | `--blockr-radius-lg` |  |
|  | `--blockr-focus-<aspect>`, `--blockr-transition`, `--blockr-mark-<aspect>` | `--blockr-focus-outline` |  |
| Local | `--blockr-<owner>-<thing>` | `--blockr-dock-spinner-delay` | its owner’s stylesheet only |

A global token’s second word is one of `color`, `font`, `radius`,
`control`, `shadow`, `focus`, `transition`, `mark`, or a palette hue.
Anything else is local. For colour, `<property>` is `text`, `bg` or
`border` and comes first, because a colour chosen for text is not
automatically right as a fill.

### Palette

| Hue | Steps | Light values |
|----|----|----|
| grey | 50 100 200 300 400 500 600 700 800 900 | `#f9fafb` `#f3f4f6` `#e5e7eb` `#d1d5db` `#9ca3af` `#6b7280` `#4b5563` `#374151` `#1f2937` `#111827` |
| blue | 50 100 500 600 700 | `#eff6ff` `#dbeafe` `#3b82f6` `#2563eb` `#1d4ed8` |
| red | 50 100 600 700 | `#fef2f2` `#fee2e2` `#dc2626` `#b91c1c` |
| amber | 50 100 500 700 | `#fffbeb` `#fef3c7` `#f59e0b` `#b45309` |
| green | 50 100 600 700 | `#f0fdf4` `#dcfce7` `#16a34a` `#15803d` |
| accent | 50 100 500 600 700 | points at blue |

The meaning tokens use grey 50, 100, 200, 300, 500 and 900, plus 400 for
disabled.

### Text

| Token | Light | Use |
|----|----|----|
| `--blockr-color-text-default` | grey-900 | body text, values, headings (section headers, table headers, chart titles), menu items, tooltip body, code, the hover state of a muted icon |
| `--blockr-color-text-muted` | grey-500 | labels, placeholders, meta, hints, counts, empty states, column types, row numbers, `NA`, status lines and footers, icons at rest |
| `--blockr-color-text-disabled` | grey-400 | disabled controls only |
| `--blockr-color-text-on-accent` | `#ffffff` | text on a solid accent fill |
| `--blockr-color-text-accent` | accent-600 | accent text |
| `--blockr-color-text-accent-strong` | accent-700 | accent text on an accent tint (a count on the main button, the avatar initial), where accent-600 is 3.9:1 |
| `--blockr-color-text-danger` / `-warning` / `-success` | red-700 / amber-700 / green-700 | status text |

Muted is 4.8:1 on white, 4.6:1 on grey-50 and 4.4:1 on grey-100. Muted
text therefore never sits on a hover or selected background: on a
hovered row, meta text and icons turn default; on a selected row, they
turn `text-accent` with the rest of the row. Grey-400 is 2.5:1 and
serves disabled controls only.

### Surfaces

Surfaces are layers. Each sits on the one before, and anything placed on
a layer takes that layer’s field and border tokens.

| Token | Light | Job |
|----|----|----|
| `--blockr-color-bg-page` | grey-50 | the board canvas |
| `--blockr-color-bg-surface` | `#ffffff` | a panel, a card, a block |
| `--blockr-color-bg-raised` | `#ffffff` | menus, dropdowns, tooltips |
| `--blockr-color-bg-subtle` | grey-50 | a quiet fill inside a surface; the gear tray |
| `--blockr-color-bg-field` | grey-50 | an input field |
| `--blockr-color-bg-hover` | `color-mix(in srgb, var(--blockr-color-text-default) 6%, transparent)` | hover and the keyboard row |
| `--blockr-color-bg-selected` | `bg-accent-subtle` | the chosen thing: a selected row, the front tab of the active dock group; its text is `text-accent`, and it has no edge |
| `--blockr-color-bg-selected-inactive` | `color-mix(in srgb, var(--blockr-color-text-default) 10%, transparent)` | the front tab of every other dock group: selected, but not where you work; one step above hover |
| `--blockr-color-bg-accent` | accent-600 | solid accent fill (checked checkbox) |
| `--blockr-color-bg-accent-hover` | accent-700 | its hover |
| `--blockr-color-bg-accent-subtle` | `color-mix(in srgb, var(--blockr-color-border-accent) 7%, transparent)` | the accent tint (below) |
| `--blockr-color-bg-accent-subtle-hover` | the same at 13% | its hover |
| `--blockr-color-bg-danger` / `-warning` / `-success` | red-50 / amber-50 / green-50 | status fills |

`bg-hover` is a wash of the text colour, so a hovered row is one step
away from whatever it sits on, surface or raised, in both schemes. In
light it lands on grey-100.

### Borders

| Token | Light | Use |
|----|----|----|
| `--blockr-color-border-default` | grey-200 | every border, input borders included |
| `--blockr-color-border-strong` | grey-300 | hovered borders, stronger lines, a border drawn with a text colour |
| `--blockr-color-border-accent` | accent-600 | focus, full-strength accent edge |
| `--blockr-color-border-danger` / `-warning` / `-success` | red-600 / amber-500 / green-600 | status edges |
| `--blockr-color-border-accent-subtle` | `color-mix(in srgb, var(--blockr-color-border-accent) 35%, transparent)` | the accent tint’s edge |
| `--blockr-color-border-danger-subtle` / `-success-subtle` | the status border at 35% | the edge of a status badge, a message, the destructive button |
| `--blockr-color-border-warning-subtle` | the warning border at 45% | the same; amber is paler, so its edge is stronger |

Input borders stay soft (`border-default`). The focus state marks the
active field.

**The accent tint** is `text-accent` on `bg-accent-subtle` with a
`border-accent-subtle` edge; its hover is `bg-accent-subtle-hover` with
`border-accent`. It is the main button’s look, and every selected or
“on” state borrows it: a selected segment, an icon tile, a pressed icon
button, the open gear, a tag that is cutting rows, an “on” badge, the
“+N” overflow, the Enter button, a selected row and the active group’s
front tab (these two without the edge). Hovering a selected thing keeps
its look.

### Status

Every status role has a text, a background and a border token: danger,
warning, success. `error` is `danger`; there is one red. Warning’s
border is amber-500, `#f59e0b`. Blue is never a status colour, because
blue means “on”. Negative numbers in a table are data, and their colour
belongs to blockr.theme.

### Focus

Two styles:

| Element | Focus | Shown on |
|----|----|----|
| a field: text, number, select, code editor, a name being renamed | `border-accent` and `--blockr-focus-ring` (`0 0 0 3px rgba(37, 99, 235, 0.12)`) | `:focus`, mouse included, because it also means “you are typing here” |
| everything else: buttons, tools, the gear, segments, checkboxes, tabs, pills, legend items | `outline: var(--blockr-focus-outline)` (2px solid `border-accent`), `outline-offset: var(--blockr-focus-offset)` (2px) | `:focus-visible` only, so a mouse click shows nothing |

The ring alone is 1.2:1 on white and does not show on a frameless tool;
the outline is 5.2:1. The 2px offset keeps it off the gear’s frame and a
selected segment’s tint. The keyboard row in a menu is not focus (focus
stays in the filter box) and keeps the hover look. The ring is written
as a literal, so a theme that changes the accent restates it. The 0.45
ring in blockr.viz and blockr.ggplot (`--blockr-color-focus`) goes.

### Dark scheme

The dark scheme is `blockr-tokens-dark.css`, keyed off
`data-bs-theme="dark"` (the attribute bslib writes and blockr.core’s
dark-mode option toggles). It restates the grey and blue ramps
positionally (grey-50 is still the quietest surface, grey-900 the
loudest text) and restates the accent and status meaning tokens
directly.

| Token | Dark |
|----|----|
| `bg-page` / `bg-surface` / `bg-raised` | `#0e1219` / `#161b23` / `#1e242e` |
| `bg-hover` | the same 6% text wash, lighter than what it sits on |
| `bg-selected`, `bg-selected-inactive` | follow the accent tint and the text colour; not restated |
| `text-on-accent` | `#0e1219` (the accent fill is lighter in dark) |
| `text-accent` | accent-500 |
| `text-accent-strong` | `text-accent` |
| `text-` / `border-danger`, `-warning`, `-success` | `#f87171`, `#fbbf24`, `#4ade80` |
| `bg-danger`, `-warning`, `-success` | the same hues at 12% |
| `focus-ring` | `0 0 0 3px rgb(96 165 250 / 0.28)` |
| shadows | the same geometry at 0.5 to 0.7 black |

The border steps (grey 100 to 300) are compressed harder than a mirror,
because a light line on a dark field reads stronger than a grey line on
white. In dark the gear tray takes `bg-page`. Exports (PNG, pptx) always
take the light values.

### Theming

A theme overrides the palette, most usefully the accent ramp
(`--blockr-accent-50`, `-100`, `-500`, `-600`, `-700`); every
accent-derived token follows, and `blue-600` keeps meaning blue. A theme
is typically those five values, optionally the ten greys (for warm or
cool neutrals), and perhaps a radius and a font face. A theme that
changes the accent also restates `--blockr-focus-ring`. Colours painted
in JS or R (charts, plots) come from blockr.theme’s palettes, not from
these tokens.

### Type

The body face is Open Sans, as bslib’s Shiny preset ships it, set in one
place: `--bs-body-font-family`. A theme that ships a face sets
`--bs-body-font-family`. There is no `--blockr-font-sans`. The chart
canvas takes its face from the body.

| Token | Size | Use |
|----|----|----|
| `--blockr-font-size-xs` | 12px (0.75rem) | field labels, section titles, captions, status lines, tooltips, offers |
| `--blockr-font-size-sm` | 13px (0.8125rem) | output subtitle and sentence, code, menu meta text, tags |
| `--blockr-font-size-base` | 14px (0.875rem) | body, field values, menu rows, buttons at 42px, the block title |
| `--blockr-font-size-lg` | 16px (1rem) | output title, the one heading on an output block |
| `--blockr-font-size-xl` | 20px (1.25rem) | page titles outside the board (the block title is 14px, `base`) |

There is no `md` step. Two sizes sit off the scale on purpose: badges
and counts at 11px, and text on a chart canvas at
`--blockr-mark-font-size` (11px).

| Weight token | Value | Use |
|----|----|----|
| `--blockr-font-weight-normal` | 400 | body, labels, values |
| `--blockr-font-weight-medium` | 500 | buttons, badges, pills, slot words, a selected segment, values in a chart tooltip |
| `--blockr-font-weight-semibold` | 600 | block title, output title, section titles, counts, the current pick in a menu |

`--blockr-font-mono` is
`'SF Mono', 'Fira Code', 'Consolas', 'Monaco', monospace`. Code is 13px
on every code surface.

### Shape and size

| Token | Value | Use |
|----|----|----|
| `--blockr-radius-sm` | 4px | 26px buttons and tools, the gear, tags, pills, checkbox, menu rows, offers |
| `--blockr-radius-md` | 6px | 30px buttons, segments inside a segmented control, messages |
| `--blockr-radius-lg` | 8px | fields, 42px buttons, rows, menus, the gear tray, tooltips (the light card and the chart’s) |
| `--blockr-radius-xl` | 12px | block panels |
| `--blockr-radius-pill` | 999px | badges, counts |

| Token | Value | Use |
|----|----|----|
| `--blockr-control-h` | 42px | inputs, selects, segmented controls in the grid, rows, buttons beside inputs |
| `--blockr-control-h-sm` | 30px | buttons in toolbars and dialogs, the crossfilter card search |
| `--blockr-control-h-xs` | 26px | header tools, the gear, buttons in a header strip, the builder/code switch, segmented controls inside a row |

No control is smaller than 26px, except what sits inside a 42px row or
field (the 24px pill, tag and Enter button) and the 16px checkbox box,
whose label is part of its target.

### Elevation

| Token | Value |
|----|----|
| `--blockr-shadow-sm` | `0 4px 6px -1px rgb(0 0 0 / 0.1), 0 2px 4px -2px rgb(0 0 0 / 0.1)` |
| `--blockr-shadow-md` | `0 4px 12px rgba(0, 0, 0, 0.1)` |
| `--blockr-shadow-lg` | `0 25px 50px -12px rgb(0 0 0 / 0.25)` |

Menus, dropdowns, tooltips and the chart tooltip use `shadow-md` on
`bg-raised`. The gear tray is in flow and has no shadow. In dark,
shadows barely read; the lighter `bg-raised` separates a menu from the
surface.

### Motion

`--blockr-transition: 0.15s ease` for hover and state changes. The gear
tray slides open and closed over 0.22s, so the output is seen moving
down and up.

### Data marks

| Token | Value | Rule |
|----|----|----|
| `--blockr-mark-radius` | 2px | on the silhouette of a bar or box, on the value end only; ends on an axis or against a sibling stay square; never near half the mark’s thickness |
| `--blockr-mark-font-size` | 11px | all text drawn on a chart canvas: axis labels and names, data labels, legend keys |
| `--blockr-mark-weight-supporting` | 0.6 | opacity of a mark that supports a choice (crossfilter picker bars) |

Data colours (chart palettes, map markers) come from blockr.theme, never
from UI tokens.

### Block category colours

Okabe-Ito, one per category, through `blk_color()` in blockr.dock. A
fixed set: identical under every theme and in dark mode.

| Category      | Colour    |
|---------------|-----------|
| input         | `#0072B2` |
| transform     | `#009E73` |
| structured    | `#56B4E9` |
| plot          | `#E69F00` |
| table         | `#CC79A7` |
| model         | `#F0E442` |
| output        | `#D55E00` |
| utility       | `#CCCCCC` |
| uncategorised | `#999999` |

Drawn in the [design system
page](https://bristolmyerssquibb.github.io/blockr.ui/articles/design-system/index.html#foundations).

------------------------------------------------------------------------

## 3. The block

### Anatomy, top to bottom

1.  **Dock header (compact).** One row: the block’s mark (a 28px tinted
    square in its category colour, carrying the status dot), the block
    title at 14px (`font-size-base`) weight 600 `text-default`, the
    actions (controls, preview, “…”) on the right. No subtitle: the
    block type and package are the mark’s tooltip (“filter block ·
    blockr.dplyr”). About 52px.
2.  **Header row.** Output blocks put the output title and the sentence
    on the left and the tools on the right, gear last. Transform blocks
    have the tools only.
3.  **Gear tray.** Opens in flow directly under the header row.
4.  **Controls on the face.** Transform blocks only: fields in the grid,
    or rows for lists you add to and reorder.
5.  **The output.** Body, caption, status line.

There is no 16px section heading inside a block.

### The block’s mark and bare mode

- The mark is a tinted square (the category colour at about 18% on the
  surface) with the glyph in the category colour. Sizes: 28px in the
  dock header, 24px in list rows (menus, pickers) with the package as a
  badge, 16px in a tab.
- The header’s mark carries the block type and package as its tooltip;
  it is the one tooltip on something that is not a button or a cut-off
  label.
- Bare mode (a board option, for dashboards): the card has no header;
  the tab carries the mark and the name, the actions sit right of the
  tabs, and a double-click on the tab renames. It needs dockViewR’s tab
  components (cynkra/dockViewR#102); there is no CSS-only interim.

### Header row

- Tools are 26px, right-aligned, gear last.
- The output title shares the row with the tools. It takes the width the
  tools leave and wraps down.
- The output title is 16px (`font-size-lg`), weight 600, `text-default`.
  The subtitle below it is 13px `text-muted`. Title and subtitle belong
  to the output: they export with the chart or table.

### What stays on the face

- A transform block’s face holds the fields that define it; the gear
  holds the rest.
- An output block’s face holds the output and its sentence. A slot in
  the sentence is the only way to put one of its arguments on the face.
  Its gear keeps a row for every argument, slotted or not.
- Reader controls (for example the patient profile’s view toggles) sit
  on the face, not in the gear.
- Simplified and locked mode hide the gear button and nothing else. Dock
  does this with one CSS rule on `.blockr-gear-btn`; blocks never ask
  which mode they are in.

### The gear tray

- In flow under the header row, above the face’s controls. It pushes the
  content down and covers nothing. It slides open over 0.22s.
- Look: `bg-subtle` fill, no border, radius 8, padding 12px 14px 16px,
  16px below it. A 10px beak, 8px from the right edge, points at the
  gear. Fields inside turn `bg-surface` (white), so the tray reads as a
  separate layer. In dark the tray and its beak take `bg-page`.
- No heading. The tray’s accessible name is an `aria-label`. Sections
  are separated by a section title and 24px of space, with no rules. A
  tray with one section has no title at all.
- A section switched on by a checkbox keeps the checkbox before its
  title; its fields appear below when checked.
- Fields use the grid ([Layout](#layout)).
- It closes by the gear and by Escape only. A click on the output, on
  another block or on a menu inside the tray leaves it open.
- The open state lasts for the session (a re-render does not close it)
  and is not saved with the board. Several trays may be open at once.
- Changes apply as they are made. Text fields commit on Enter or blur.
  No tray has an Apply button for its fields; code is the one exception
  and commits on its own button (Run in the code and function blocks,
  Apply on the chart’s prepare script).
- The crossfilter’s settings and the patient profile’s gear panel use
  the same tray.

### The output

- **Body:** the chart, table or text.
- **Caption:** 12px `text-muted`. Chart, table and composer captions
  stay italic.
- **Status line:** 12px `text-muted`: row counts, filter state, timing,
  the drill receipt. See [Labels and status](#labels).

### The sentence and its slots

The sentence is the output’s subtitle, written by the block’s author in
the `#| subtitle:` template. Nothing is generated; a setting the
template does not name stays in the gear.

- **Line breaks:** one sentence. The author breaks lines by writing `\n`
  in the template; blockr adds no breaks of its own.
- **A slot** is a live word: accent text (`text-accent`), weight 500, a
  1px dashed underline 3px below the text, solid on hover. A click opens
  the list under the word (`Blockr.Select.menu()`). A flag word toggles
  in place.
- **Many values:** a slot shows the first three values, then “+N more”,
  inside the same live word; the click opens the full list. Exports
  print the same text. The cut is 3 (`TITLE_MAX_VALUES`).
- **Offers** (“+ name”, for a setting that is unset) sit on their own
  line under the sentence, three at most. Past three, the last one reads
  “More settings” and opens the gear. An offer is a small dashed button:
  12px `text-muted`, 1px dashed `border-strong`, radius 4, 6px side
  padding; on hover the border and text turn accent. It disappears once
  its setting is set. Offers are not part of the printed sentence.
- A clause in `[ ... ]` leaves with its value. The sentence never prints
  “(none)”.
- One template, two renderers: the face draws live words, exports print
  the same text.

Drawn in the [design system
page](https://bristolmyerssquibb.github.io/blockr.ui/articles/design-system/index.html#block).

------------------------------------------------------------------------

## 4. Layout

### The grid

One grid for fields, on the block’s face and in the gear tray alike. The
columns are about 130px wide; the panel’s width decides how many there
are.

| Panel width   | Columns |
|---------------|---------|
| under 290px   | 1       |
| 290px and up  | 2       |
| 560px and up  | 4       |
| 840px and up  | 6       |
| 1110px and up | 8       |

Gaps: 12px between columns, 16px between rows of fields. The grid steps
on a container query on the block. It moves from blockr.extra to
blockr.ui.

### Field sizes

A field says how many columns it needs.

| Size  | Columns       | Fields                                |
|-------|---------------|---------------------------------------|
| small | 1             | number, checkbox                      |
| large | 2             | select, text field, segmented control |
| full  | the whole row | tags (multi-select), expression       |

Columns line up across sections at every width. A section with few
fields leaves room on the right; fields do not stretch to fill it. At
two columns a large field fills the row.

### Spacing

| Between                                           | Space       |
|---------------------------------------------------|-------------|
| label and its control                             | 4px         |
| columns / rows of fields                          | 12px / 16px |
| a section title and its first field               | 10px        |
| the end of one section and the next section title | 24px        |
| rows in a list                                    | 6px         |
| the gear tray and what follows                    | 16px        |

### Section titles

“A group of fields starts here”, on the face and in the gear tray: 12px
(`font-size-xs`), weight 600, uppercase, 0.05em tracking, `text-muted`.
It works as a divider and must not pull the eye from the fields. One
class replaces the settings band title, `.dd-section-title`,
`.dd-at-head`, `.dd-type-group-head`, the crossfilter table and search
headers and the select menu group titles.

### Field labels

12px, weight 400, `text-muted`, 4px above the control, in every package.
The required-empty cue adds a trailing `*` to the label.

### Rows

A row in an add-and-reorder list (filter conditions, columns) is 42px,
the same as an input, border included (`box-sizing: border-box`), as
`.blockr-row` is. Look: `bg-field`, 1px `border-default`, radius 8,
padding 5px 10px, 10px gap between its parts, 6px between rows. Its
operator pill is 24px.

- A row’s content starts on the same edge as the label above it.
- The drag handle takes no space at rest. It appears in the container’s
  left padding, which is at least 14px.
- Handles and remove buttons appear on hover or keyboard focus of their
  row (see [Actions](#actions)).
- “Add condition” under the list is a quiet button.

Drawn in the [design system
page](https://bristolmyerssquibb.github.io/blockr.ui/articles/design-system/index.html#layout).

------------------------------------------------------------------------

## 5. Controls

### Which control for which decision

| Decision | Control |
|----|----|
| Turn an option on or off | Checkbox |
| One of two or three fixed, short values | Segmented control |
| One of four or more, or of anything from the data (columns, measures, levels) at any count | Select |
| The same, inside a row | Pill that opens a menu |
| Several from a list, from the data or from a fixed set | Select, multi, with tags |
| Kind of output (chart type) | Icon tiles, with group headings where types are grouped |
| How a value is edited (builder or code) | Pressed icon button |
| A number or a name | Field that commits on Enter or blur |

Not used: switches, radio buttons, click-to-cycle pills, toggle-pill
groups, rows of checkboxes for a fixed set, shinyWidgets and unstyled
Bootstrap inputs. The selected state of every choice control is the
accent tint: a selected segment, an icon tile, a pressed icon button,
the open gear, a selected row, the active group’s front tab.

### Text and number fields

- 42px, `bg-field`, 1px `border-default`, radius 8 (`radius-lg`), 12px
  side padding, 14px `text-default`. Placeholder `text-muted`.
- Focus: `border-accent` and `--blockr-focus-ring`, on any focus.
- Disabled: `text-disabled`.
- Required and empty: `border-warning` (amber-500, `#f59e0b`),
  `bg-warning` fill, and a `*` after the label. The cue clears when the
  field has a value.
- Commit on Enter or blur. The “Enter ↵” button (24px, 11px weight 500,
  the accent tint) is armed while the field is dirty. Escape reverts.
- Numbers use the same field. The input keeps `type=number` for the
  keyboard, hides the spinner arrows, and commits like text; arrow keys
  still step. A Bootstrap override brings Shiny’s `numericInput` to the
  same look.
- dplyr’s 30px inputs move to the 42px field.

### Select

`Blockr.Select` is the only dropdown, single and multi.

- In the grid it is a bordered 42px field with a caret. Inside a row it
  is bare.
- Options show the value first and the label after it as 13px muted meta
  text (for columns, see [Column names and their labels](#labels)); the
  label is cut first when space runs out.
- Its dropdown is a menu ([Floating layer](#floating)).

### Select, multi, with tags

- The picks sit as tags in the control. The dropdown lists only what is
  not picked. Tags can be dragged to reorder.
- A tag’s × appears on the tag under the pointer or with keyboard focus,
  never on every tag at once, and turns `text-danger` on its own hover.
  Its space stays reserved, so nothing moves.
- The “+N” overflow stands in for hidden tags: tag geometry in the
  accent tint; a click expands the field.
- The same control serves data-driven lists and fixed sets (such as the
  summary table’s statistics). Fixed options stay hidden until opened.

### Checkbox

On/off is a bare checkbox everywhere: in the gear tray, on a block’s
face and in the board options. Core’s
[`bslib::input_switch`](https://rstudio.github.io/bslib/reference/input_switch.html)
calls, Shiny’s `checkboxInput`s, the pharma header switch and the on/off
pills all become `Blockr.checkbox`; a Bootstrap `.form-check` override
covers the R side.

- Box: 16px, radius 4, 1px `border-strong`, `bg-surface`. Hover: border
  `text-muted`.
- Checked: `bg-accent` fill and border, the check in `text-on-accent`.
- Label after the box, 9px gap, 14px `text-default`. The label names the
  “on” state.
- A native input underneath; Space toggles; keyboard focus puts the
  focus outline around the box.
- Disabled: box at 50% opacity, label `text-disabled`.
- In the grid: the box and its words, with no field shell and no empty
  label row. It sits on the bottom line of its grid row (the controls’
  line) and takes one column. On a row of its own it takes only its own
  height.
- Checkboxes that together encode one choice (core merge’s
  `all.x`/`all.y`) become that choice as a Select (“Join type”).

If a boolean ever has to sit inside a field shell again, the switch
question reopens.

### Segmented control

For a fixed set of two or three short values. Both values stay in view
and the pick is readable without a click.

- In the grid: 42px, `bg-field`, 1px `border-default`, radius 8, 3px
  inner padding and gap. Segments share the width, radius 6, 14px
  `text-muted`; hover `text-default` on `bg-hover`.
- Selected segment: the accent tint, weight 500.
- It takes two columns, like a select. Three segments need about 240px.
- Inside a row: 26px, 2px padding and gap, radius 6, segments 12px at
  radius 4.
- Four or more values, labels too long for the cell, or values from the
  data go to Select.
- It replaces the cycle pills of this kind, viz’s Order select, the head
  block’s “Tail” switch (“From: First / Last”), stats’ shinyWidgets
  buttons and the dm, io and pharma segmented copies.

### A pill that opens a menu

For four or more values inside a row: the filter operator, the join key
operator, viz’s `.dd-func-btn` (n, N, %, Σ).

- 24px inside a 42px row, radius 4, 1px `border-default`, `bg-surface`,
  12px weight 500 `text-muted`, with a caret so it reads as a choice.
  Hover: `border-strong`, `text-default`.
- A click opens `Blockr.Select.menu()` anchored on the pill, listing
  every value.
- The click-to-cycle pill is retired. Join type, outside a row, is a
  Select field in the grid.

### Builder or code switch

A pressed icon button, 26px, the tool look (like the gear). Pressed: the
accent tint. It sets `aria-pressed`. Its tooltip names the other state
(“Edit as formula text”, “Back to the builder”). It stays out of the
field grid.

### Renaming in place

One pattern for every name that can be edited where it is shown: the
block title, an outline row, a view name, a tab (in bare mode).

- Start: a double-click on the name, or “Rename” in the item’s “…” or
  row menu. A single click keeps its other job (select, open).
- At rest the name is plain text; on hover it takes the `bg-hover` wash
  (radius 6, 4px 8px padding, pulled back by the padding so the text
  does not move). There is no pencil.
- While editing, the text becomes a field at its own size and weight:
  1px `border-accent`, the focus ring, `bg-surface`, the text selected.
  No label.
- Enter commits, Escape restores, a click elsewhere commits. An empty or
  duplicate name is refused in place: the field’s `border-danger` edge
  and a one-line 12px `text-danger` message under it.

Drawn in the [design system
page](https://bristolmyerssquibb.github.io/blockr.ui/articles/design-system/index.html#controls).

------------------------------------------------------------------------

## 6. Actions

### Buttons

Four kinds. All are weight 500 and show the focus outline on
`:focus-visible`.

| Kind | Rest | Hover | Use |
|----|----|----|----|
| Main | the accent tint: `text-accent`, `bg-accent-subtle`, `border-accent-subtle` | `bg-accent-subtle-hover`, `border-accent` | at most one per view |
| Secondary | `bg-surface`, `border-default`, `text-default` | `bg-hover`, `border-strong` | everything else with a frame |
| Quiet | text only, `text-muted`, 8px side padding | `bg-hover`, `text-default` | low-weight actions; replaces the grey-400 text links |
| Destructive | `text-danger`, `border-danger` at 6% fill, `border-danger-subtle` | `border-danger` at 12% fill, `border-danger` | only to confirm a removal |
| Disabled | `text-disabled`, transparent, `border-default` | none |  |

There is no solid accent button. Bootstrap’s `btn-primary`, `btn-danger`
and `btn-success` go.

| Size | Height | Text | Side padding | Radius | Where                            |
|------|--------|------|--------------|--------|----------------------------------|
| m    | 42px   | 14px | 16px         | 8px    | beside inputs                    |
| s    | 30px   | 13px | 11px         | 6px    | toolbars, dialogs                |
| xs   | 26px   | 12px | 9px          | 4px    | a header strip, next to the gear |

The crossfilter reset, the patient-profile drill reset and dplyr’s Enter
button are instances of the main style and take these numbers.

### Icon buttons (tools)

Header tools (search, download, the builder/code switch) are 26px,
radius 4, bare, icon `text-muted`. Hover: `bg-hover`, icon
`text-default`. Every icon-only button has a tooltip. Tools do not
shrink with the panel.

The dock header’s tools (the controls and preview toggles, the “…” menu)
are quieter than other tools: at rest their icon is `border-strong`
grey, on hover `text-disabled` on the `bg-hover` wash, so the header
reads as the block’s name. A toggle that is on shows its icon in
`text-accent`, with no fill and no frame. The accent tint’s fill and
frame are for the builder/code switch, which changes how a value is
edited rather than what is shown.

### The gear button

- 26px, radius 4, 1px `border-default`, the one framed square in the
  header row. Icon: Bootstrap `gear-fill` at 14px, `text-muted`.
- Hover: `text-default`, `border-strong`, `bg-hover`.
- While the tray is open: the accent tint.
- One class, `.blockr-gear-btn`, on every gear and on nothing else.
  Tooltip “Settings”.
- No gear when a block has no options, including per mode.

### Small icons

Drag handles, remove buttons and fold arrows are drawn with thin strokes
and small dots in `text-muted`. Handles and remove buttons are hidden at
rest and appear on hover or keyboard focus of their row; remove turns
`text-danger` on hover. Fold arrows stay visible. The crossfilter’s
filter cards are the exception (see [Special blocks](#special)).

### The navbar

The board’s toolbar: 48px high, `bg-surface`, a `border-default` rule
under it, every control 30px, tokens only.

- Left: the logo, then the workflow’s name, then its save state as 12px
  `text-muted` text (“Not saved”, “Saved just now”), then one quiet “…”
  workflow menu (save as, copy link, history, new). No split buttons.
- The workflow’s name is its ID: chosen in a dialog at the first save,
  unique, like a file name (spaces allowed). It is shown in the body
  face, weight 600, never monospace. There is no separate title.
- Right: the view menu as a quiet button (current view marked by weight
  600, no fill), the board options gear, and the account avatar last: a
  28px circle with the count’s accent fill (`border-accent` at 14%) and
  a `text-accent-strong` initial.
- Hover on every navbar control is `bg-hover`. The spinner’s label is
  the light tooltip.

Drawn in the [design system
page](https://bristolmyerssquibb.github.io/blockr.ui/articles/design-system/index.html#actions).

------------------------------------------------------------------------

## 7. Floating layer

blockr has no popovers. Floating things are menus (choices and actions,
opened by a click) and tooltips (names, on hover). The gear tray is in
flow and not part of this layer.

### Menus

Every menu, dropdown and list that floats uses one surface:

- `bg-raised`, 1px `border-default`, radius 8 (`radius-lg`, the same as
  the field it opens from), `shadow-md`, 4px inner padding.
- Rows 32px at 14px, radius 4 (`radius-sm`), 10px side padding. Meta
  text 13px.
- The dock “…” menu, the download menus, the add menu and the @ menu
  move to it. The beak belongs to the gear tray only.

**Row states.**

| State | Look |
|----|----|
| hover and keyboard row (same look) | `bg-hover`; meta text and icons turn `text-default` |
| current pick | weight 600 |
| disabled | listed in `text-disabled`, no hover, the reason in a tooltip |

The views menu drops its blue fill for the current view.

**Group titles and dividers.** A group title uses the section-title
style (12px, 600, uppercase, 0.05em, `text-muted`), 8px above and 4px
below, inset to the row text (10px). In a list that scrolls it is
sticky. A divider (1px `border-default`, 4px above and below, edge to
edge) separates groups without a title, such as ordinary actions from a
destructive one. A destructive item is `text-danger`.

**Placement.** One routine for all of them (Select’s `computePosition`,
moved to blockr.ui): fixed position on `body`, 4px from the trigger,
flips above when there is no room below, follows scroll, stays 8px from
the viewport edge.

| Menu                         | Width                               |
|------------------------------|-------------------------------------|
| field dropdown               | the control’s width, at least 190px |
| menu from a button or a word | sized to content, 180 to 320px      |

A menu lines up with the trigger’s left edge, or its right edge when the
trigger is in the header row. Height up to 320px, then the list scrolls
under its sticky titles and filter box. A filter box (30px) appears
above 8 options. A `Select.menu` names the role it sets in its title,
because the word that opened it names only the value.

**Content.** Long lists render up to 200 options and end with a count
row that asks you to type; server-searched lists say how many values
there are. Empty and loading rows are centred, italic, `text-muted` (“No
matches”, “All selected”, “Loading…”).

**Behaviour.** Arrows move, Enter picks, typing filters. Escape closes
and returns focus to the trigger; Tab and an outside click close. A pick
closes the menu; a multi pick keeps it open.

### The block’s “…” menu

Its head holds the block’s name, package and description (12px
`text-muted`, the name in `text-default` weight 600), above a divider.
Then the actions: Rename, Duplicate, Copy block ID (the ID as mono meta
text), and Remove after a divider in `text-danger`. The dock header has
no subtitle; the block type is the tooltip on its mark.

### Tooltips

One style for every name shown on hover: a small light card.
`bg-raised`, 1px `border-default`, `shadow-md`, radius 8 (`radius-lg`,
as the chart’s data tooltip), 12px `text-default`, padding 5px 9px. It
shows on hover and on keyboard focus, and leaves with the pointer or
focus; Escape hides it.

- Every icon-only button and every cut-off label gets one, and so does
  the block’s mark in the dock header (its type and package). Nothing
  else does: a button that says its name has no tooltip.
- No native `title` tooltips remain.
- It matches the chart’s data tooltip (see [Special blocks](#special)),
  so blockr has one floating style.
- It shows after the pointer rests 800ms, above the element; it flips
  below only where there is no room above.

### Picking a block

- **Adding a block** is one menu at the “+”, everywhere: the outline, a
  block’s append, the board’s Add block. Search on top, category titles,
  one row per block type with its 24px mark, its name and the package as
  a badge. Rows carry no description. The block’s ID is generated, and
  its title is renamed on the block after it is added. The dock’s block
  browser sidebar and its “configure before adding” step go.
- **Picking a block already on the board** (add a panel, link to, add to
  a stack) uses the same menu, listing the board’s blocks: mark, title,
  and the block type as meta text. The Bootstrap modal with selectize
  goes.

Drawn in the [design system
page](https://bristolmyerssquibb.github.io/blockr.ui/articles/design-system/index.html#floating).

------------------------------------------------------------------------

## 8. Labels and status

### Four kinds, told apart by shape

| Kind | Class | Job | Shape |
|----|----|----|----|
| Badge | `.blockr-badge` | you only read it: “Categorical”, “blockr.dplyr”, “Added” | capsule |
| Tag | `.blockr-tag` | a value you picked, with × when it can be removed | 4px corners |
| Pill | `.blockr-pill` | a click changes a setting (the operator) | 4px corners |
| Count | `.blockr-count` | a number on a button | capsule |

The word “chip” goes. Round ends mean read-only; tags, pills and
buttons, which you act on, keep 4px corners.

### Badges

- 18px capsule (`radius-pill`), 11px weight 500, 7px side padding,
  `bg-subtle` fill, 1px `border-default`, `text-muted`.
- Neutral unless it states a status or that something is on:
  - **on, added, cutting rows:** the accent tint (`bg-accent-subtle`,
    `border-accent-subtle`, `text-accent`);
  - **danger, warning, success:** `bg-<status>`,
    `border-<status>-subtle`, `text-<status>`.
- Types, packages and significance levels are neutral; the type icon
  beside a name already carries the type. io’s blue “info” badge becomes
  neutral.
- Data colours (the arm label in the cohort list, the tile’s colour-by
  pill) come from the board’s scale and are outside this rule.
- A count with a unit (“4 blocks”) is a badge.

### Tags

24px, radius 4, 1px `border-default`, `bg-surface`, 13px `text-default`,
8px side padding. The × follows [Controls](#controls). The accent tint
on a tag means the value is cutting rows. The crossfilter’s “Filter by”
pills are tags by this rule, and the read-only items in a drill receipt
are tags without ×.

### Pills

Specified under “A pill that opens a menu” in [Controls](#controls).

### Counts

- 16px capsule, at least 16px wide, 4px side padding, 11px weight 600,
  tabular figures.
- Fill: the host control’s colour at 14%. On an accent host that is
  `color-mix(in srgb, var(--blockr-color-border-accent) 14%, transparent)`
  with `text-accent-strong` text.
- Shown only when the number is above zero.
- On a secondary or destructive button the count takes that button’s
  colour: `text-default` on a `bg-hover` fill, or `text-danger` on
  `border-danger` at 14%. A count never brings a second colour into a
  button.

### Showing that a block filters

- The control that undoes the filter (Reset) is a 26px xs main button
  while a filter is on, disabled when none is. It carries the number of
  active filters as a count, and its tooltip names the clause.
- Where the count is itself a control (the patient profile’s cohort tag
  opens the sidebar), count and reset join as one 26px segment.
- Dots are for block status only; the heatmap’s filter dot goes.

### Block status

An 8px dot on the block’s mark with a 2px `bg-surface` ring, one spec
for dock and the DAG (`block_status_style()`). Its fills are local
tokens owned by blockr.dock, each pointing at a meaning token:

| Token | Points at | State |
|----|----|----|
| `--blockr-dock-status-stale` | `text-muted` | stale |
| `--blockr-dock-status-waiting` | `border-warning`, drawn as a hollow ring (1.5px, `bg-surface` inside) | an input is not ready: nothing linked in, or the block upstream is unset, waiting or failed |
| `--blockr-dock-status-failed` | `border-danger` | failed |
| `--blockr-dock-status-unset` | `border-warning` | a required input is empty; the same amber as the empty field’s cue |

Unset and waiting share the amber and differ by shape: the block that
needs you has the solid dot, and the blocks below it, which wait for it,
have rings. `-unset` replaces the off-palette `#eab308` in blockr.dock
`block_status_style()`.

### Status line and plain text

The status line under the output is 12px `text-muted`: row counts,
filter state, timing, the drill receipt. These stay plain text, with no
badge: column types in the table preview (int, dbl), the status line and
its receipt, the filter trail printed in captions (in column labels, see
below), the Big N at the end of a column header.

### Column names and their labels

A column has a name (`AGE`) and often a label (“Age”). The board speaks
in names, an exhibit in labels.

| Where | Shows |
|----|----|
| a control that holds a column: Select options and the chosen value, tags, filter rows, crossfilter cards, gear fields, the @ menu | the name, then the label as meta text: 13px `text-muted`, 8px after the name, no separator, cut first when space runs out (12px inside a tag) |
| an exhibit: chart axes, legends and tooltips, output and summary tables, exports | the label only; an output table header shows the name in its tooltip |
| the table preview (a building surface) | the name, with the label on its own line under it, 12px `text-muted` |
| filter text printed with an exhibit: the filter trail in captions and exports | labels: “Sex = F; Age 18 to 64” |
| filter text that stays on the board: crossfilter shelf pills, drill receipts | names: “SEX = F” |

- No label, or a label equal to the name: the name alone. Never “unset”,
  never the name twice.
- A tooltip that shows both reads `AGE · Age`.
- An aggregate reads “Mean of Age”, on a chart, in a table and in a rank
  table header.
- Code and decode pairs (PARAMCD and PARAM) follow the same rule: where
  one is picked, the code with the decode as meta (“ALB Albumin (g/L)”);
  in an exhibit, the decode, with the code in the tooltip.
- One reader for every package: `attr(x, "label", exact = TRUE)`,
  returning one string or nothing. Without `exact`, a column with haven
  value labels returns those, and the readers that expect one string
  break.
- A sentence slot prints the name (`{@x}`) or the label (`{label(@x)}`),
  as its author writes it.

### Messages

- Required and empty: the amber cue on the field
  ([Controls](#controls)), never a banner.
- Syntax and validation text: `text-danger`.
- Empty and loading states in lists: centred, italic, `text-muted`.
- A block-level error or warning (a failed expression, dropped rows)
  uses the message style, above the output: `text-<status>` on
  `bg-<status>` with a `border-<status>-subtle` edge, 13px, radius 6.
  Danger for errors, the amber set for warnings.

Drawn in the [design system
page](https://bristolmyerssquibb.github.io/blockr.ui/articles/design-system/index.html#labels).

------------------------------------------------------------------------

## 9. Special blocks

### Charts

- **Around the plot:** the header row, output title, sentence, caption
  and status line follow [The block](#block). Axis titles, legend titles
  and tooltips use column labels ([Labels](#labels)); blockr.ggplot’s
  axis titles move from names to labels. Header tools are 26px (the
  chart’s and composer’s 30px tools move to 26px).

- **On the canvas:** all text at `--blockr-mark-font-size` (11px), read
  by chart.js once per render. The gutter measurement (11px), the 14px
  label-row budget and the printed charts keep their numbers.

- **Canvas ink** is read from the tokens at render, so it follows the
  scheme and a theme’s greys:

  | Canvas element       | Token            |
  |----------------------|------------------|
  | axis and data labels | `text-muted`     |
  | axis line            | `border-strong`  |
  | split lines          | `border-default` |
  | separators, halo     | `bg-surface`     |
  | reference lines      | `border-danger`  |
  | face                 | the body face    |

  Exports (PNG, pptx) take the light values. Retiring the “ECharts
  Theme” board option is a follow-up.

- **Facet strip:** the section-title style (12px, 600, uppercase,
  0.05em, `text-muted`).

- **Legend band:** HTML. Its items take the focus outline. The 25 x 14px
  swatch stays.

- **Data tooltip:** the raised card (`bg-raised`, `border-default`,
  radius 8, `shadow-md`). Headline 13px weight 600 with the series
  swatch, which carries the series colour; rows 12px, labels
  `text-muted`, values `text-default` at 500 with tabular figures.

- **Slider bubble:** stays inverse (dark label), because it labels a
  thumb during a drag.

- **Data colours:** blockr.theme palettes.

### Crossfilter

- **Header row:** the standard one. Row count, “Reset all” and the gear
  sit on the header row at the top. “Group by” is a field with a label;
  “Filter by” is a section title over the tags and cards. (The
  population filter, a block that joins two blocks in one, keeps its own
  layout.)
- **Reset all:** the 26px main button with a count ([Labels](#labels)).
- **Filter by tags:** 13px tags; a filtering tag wears the accent tint.
- **Filter cards:** search 30px; reset and remove icons 26px and always
  shown; rows 30px. The 200px scroll area shows about 6 rows.
- **Bars:** accent fill at `--blockr-mark-weight-supporting`, value end
  rounded by `--blockr-mark-radius`.
- **Settings:** in the gear tray.
- **Type badges** in the column search are neutral badges (B4).
- Literal `#fff`, rgba accents and Bootstrap reds in its stylesheet
  become tokens.

### Code editor

The code, function and composer blocks share one CodeMirror editor.

- The editor is a large input: `bg-field` background, 13px mono.

- Syntax colours come from four local tokens:

  | Token                   | Points at      |
  |-------------------------|----------------|
  | `--blockr-code-keyword` | `text-accent`  |
  | `--blockr-code-string`  | `text-success` |
  | `--blockr-code-number`  | `text-warning` |
  | `--blockr-code-comment` | `text-muted`   |

- Line numbers `text-muted`; the active line `bg-hover`; the input-line
  band is mixed from `border-accent`.

- Footer: Run and Accept all are 26px main buttons; Reject all is a 26px
  secondary button. The syntax message is `text-danger`.

- Code is 13px on every code surface; dplyr’s expression input moves
  from 14px.

- The green and red line backgrounds of an AI review diff are diff
  meaning and stay.

### Composer

The composer keeps its busy cue (after 300ms the body dims to 0.45 and a
clock runs under the sentence) and the “composed in” time that fades
after 2s, both in `text-muted`. Every block gets the same cue: past
300ms of computing, its output dims to 0.45 and a clock (“computing… 1.2
s”) runs under it; nothing shows for a faster run. blockr.core applies
it from outside the block.

Drawn in the [design system
page](https://bristolmyerssquibb.github.io/blockr.ui/articles/design-system/index.html#special).

------------------------------------------------------------------------

## 10. For package authors

Read meaning tokens only. No `--blockr-grey-*`, `--blockr-blue-*` or hex
in a component.

Tints via the tint tokens or `color-mix()` from a meaning token, never
from a palette token or an rgba literal.

Local settings are named `--blockr-<package>-*` and read only in that
package’s stylesheet.

Text in one of three levels: default, muted, disabled (disabled only for
disabled controls). No muted text on a hover or selected fill.

Field labels 12px regular muted, 4px above; section titles in the one
section-title class; no heading larger than the output title inside a
block.

Fields in the one grid: numbers and checkboxes small, selects and text
large, tags and expressions full. Size steps on the panel, never the
viewport.

Controls by the decision table: checkbox, segmented control, Select
(single or tags), pill with a menu, icon tiles, pressed icon button. No
switches, radio buttons, cycle pills or shinyWidgets.

Text and numbers commit on Enter or blur; nothing has an Apply button.

Buttons: main (one per view), secondary, quiet, destructive, at 42, 30
or 26px. No Bootstrap solid buttons.

Header row: 26px tools, gear last, `.blockr-gear-btn` on the gear only.
No gear if there are no options. Every option is in the gear.

Menus through `Blockr.Select` / `Select.menu()` and the shared placement
routine. No popovers. Tooltips on icon-only buttons and cut-off labels
only, using the light card; no native `title`.

Badges are capsules and neutral unless they state status or “on”.

Columns: the name with its label as meta where a column is picked, the
label alone in an exhibit; read labels with the shared reader.

Focus: a field takes the accent border and the ring; everything else
takes the focus outline, on `:focus-visible` only.

Check the block in the dark scheme.

Drawn in the [design system
page](https://bristolmyerssquibb.github.io/blockr.ui/articles/design-system/index.html#authors).

------------------------------------------------------------------------

## Appendix

### Where the code lives

blockr.ui owns the tokens (`inst/assets/css/blockr-tokens.css`,
`blockr-tokens-dark.css`) and the shared components: the params grid
(moving from blockr.extra), `Blockr.Select` and its placement routine,
`Blockr.checkbox`, the badge, tag, pill and count classes, the slot and
offer rules (today declared twice, in blockr.viz `chart.css` and
blockr.dm `crossfilter-block.css`), the gear button, and the
column-label reader. blockr.dock owns the dock header, the “…” menu, the
block status dot, the category colours (`blk_color()`), the status dot’s
`--blockr-dock-status-*` tokens and the rule that hides the gear in
simplified mode. blockr.theme owns data colours.

To do: blockr.ui’s token tests check that the vocabulary changes only
deliberately and that consumers’ fallbacks agree with it. Two tests are
still to write: one that enforces the grammar, and one that warns on
every read of a legacy alias.

### Legacy token aliases

Old names stay in `blockr-tokens.css` for a release and each keeps its
old value. Packages move over when convenient; the alias goes when
nothing reads it.

| Old name | Points at |
|----|----|
| `--blockr-color-text-primary` | `color-text-default` |
| `--blockr-color-text-secondary` | grey-700 (by job: labels and status lines go to `text-muted`; headings, content and hover states to `text-default`) |
| `--blockr-color-text-subtle` | grey-400 (by job: placeholders, meta and icons go to `text-muted`; disabled controls to `text-disabled`; a border to `border-strong`) |
| `--blockr-color-text-meta` | `color-text-muted` |
| `--blockr-color-border` | `color-border-default` |
| `--blockr-color-border-hover` | `color-border-strong` |
| `--blockr-color-bg-input` | `color-bg-field` |
| `--blockr-color-primary` | `color-bg-accent` |
| `--blockr-color-primary-hover` | `color-bg-accent-hover` |
| `--blockr-color-primary-bg` | accent-50 (keeps its old value; the accent tint `color-bg-accent-subtle` is a 7% mix, nearly the same colour) |
| `--blockr-color-error`, `--blockr-color-danger` | `color-border-danger` |
| `--blockr-color-success` | `color-border-success` |
| `--blockr-color-warning` | `color-border-warning` |
| `--blockr-color-warning-bg` | `color-bg-warning` |
| `--blockr-color-warning-text` | `color-text-warning` |
| `--blockr-font-size-section` | `font-size-lg` |
| `--blockr-font-size-title` | `font-size-xl` |
| `--blockr-shadow` | `shadow-sm` |
| `--blockr-shadow-dropdown` | `shadow-md` |

One new token reuses an old name: `--blockr-color-bg-hover` was
grey-100, and it stays grey-100 in light until the fallbacks that write
`#f3f4f6` move to the text wash. The dark scheme uses the wash already.

Removed from the UI tokens: `--blockr-color-negative` (data colour,
belongs in blockr.theme). Local names move under their owner: dock’s
`spinner-delay`, `sidebar-*`, `stack-height` and header settings to
`--blockr-dock-*`; viz’s `rank-*` to `--blockr-viz-rank-*`.
blockr.outline drops its own `--md-*` variables and reads the blockr
tokens.

### Not specified yet

Tables (header, row height, number alignment, selected row, sort cue),
dialogs (surface, width, title, button order), dock tabs beyond their
selected state, the DAG view, notifications (Shiny’s
`showNotification`), and the icon set with its sizes.

### Decision record

The reasoning behind each rule, the options that were compared and the
questions they answered were in `naming.md`, `questions.html` and
`topics/` in `dev/design-system/`. They were removed once this document
was complete; git history has them
(`git log --all -- dev/design-system/naming.md`).
