# Brief: the consolidated blockr design system (spec + page)

Every design question is decided (2026-09-23). The decisions are in
`dev/design-system/naming.md` as a log, in the order they were made, with
reasoning. Two deliverables turn that into one clean system:

1. `dev/design-system/design-system.md`, the written spec.
2. `dev/design-system/index.html`, the one page that shows the whole system as decided.

Both follow the SAME table of contents (section numbers and titles below), so
the page and the spec can link to each other by anchor (`#controls`, etc.).

## Where things are
- Worktree `/workspace/_worktrees/ui-design-system` (blockr.ui, branch `feat/design-system`).
  Pages served at http://127.0.0.1:3840/dev/design-system/ (server running; do not start another).
- Decision record: `dev/design-system/naming.md` (read ALL of it; later sections win over earlier ones where they conflict,
  e.g. "Decided on questions.html", topic sections, checkbox, badges, tooltips, no popovers, gear band, sentence slots).
- Tokens (source of truth for values): `inst/assets/css/blockr-tokens.css`, `blockr-tokens-dark.css`
  (just updated: bg-hover is a text-colour wash, dark bg-selected, `--blockr-mark-font-size: 11px`).
- Drawings of every decided option (copy their look, they are what Christoph approved):
  `buttons.html` (section 3 = decided buttons), `block-layout.html`, `questions.html` (parts/*.html, each question with
  its options; the answer boxes say which option is settled), `topics/*.html` (01 gear tray B, 02 gear band behaviour
  B+A, 03 checkbox A, 04 badges B, 05 popovers C + tooltips C, 06 grid B, 06b checkbox in grid C, 16 sentence slots).
- Shared page CSS: `ds-page.css`; icons: use the real gear (Bootstrap gear-fill path, see `parts/_icons.svg.html`), never a sun-like icon.
- Older written spec for context only: `/workspace/blockr.docs/design-system/` (components/*.md, pinned-controls.md).

## Table of contents (both deliverables)
1. Principles (`#principles`)
2. Foundations (`#foundations`): colour (palette, meaning tokens, three text levels, surfaces, borders, status, focus),
   dark scheme, theming (accent ramp), type (Open Sans, scale, weights, mono), shape and size (radii, control heights
   42/30/26), elevation, motion, data marks, block category colours (Okabe-Ito, fixed)
3. The block (`#block`): anatomy (dock header; header row with output title, tools and gear; the gear tray; controls on
   the face; the output: body, caption, status line), the sentence and its slots
4. Layout (`#layout`): the grid and field sizes, spacing, section titles, field labels, rows
5. Controls (`#controls`): text and number inputs and their states, select (single; multi with tags), checkbox,
   segmented control, a pill that opens a menu, builder/code switch
6. Actions (`#actions`): buttons (main, secondary, quiet, destructive; sizes), icon buttons, the gear button, small icons
7. Floating layer (`#floating`): menus (surface, rows, states, group titles, placement), tooltips; no popovers
8. Labels and status (`#labels`): badges, tags, pills, counts, messages, status line, showing that a block filters
9. Special blocks (`#special`): charts, crossfilter, code editor
10. For package authors (`#authors`): a short checklist
Appendix: where the code lives (blockr.ui owns tokens and shared components), legacy token aliases, decision record link.

## Writing rules (strict, Christoph reads everything)
Plain English, short. No em dashes. No "not X, it is Y", no rule of three, no rhetorical questions, no aphorisms at the
end of paragraphs, no sentence that only comments on the previous one. Every sentence states a fact or a rule. Use
exact values (px, token names). Say "decided" facts as rules ("A badge has round ends."), not as history. Keep the
reasoning to one clause where it helps a reader apply the rule; the long reasoning stays in naming.md.

## Do not
- Edit files other than your deliverable (and /tmp). Do not commit. Do not touch package code or the token files.
- Invent rules. If naming.md does not decide something, leave it out or mark it "open" in one line.
