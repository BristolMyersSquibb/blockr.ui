# Brief for a design-system discussion part

You are writing ONE part of a single HTML discussion page for the blockr design
system. Christoph (product owner) decides; the page must let him decide each
question by looking, in one pass. Other agents write the other parts in
parallel, in the same format.

## Where things are
- Worktree: /workspace/_worktrees/ui-design-system (blockr.ui, branch feat/design-system).
  Pages live in dev/design-system/. Served at http://127.0.0.1:3840/dev/design-system/
  (server already running; do not start another).
- Read first, as the model to copy in tone, structure and density:
  dev/design-system/block-layout.html (the last part written) and
  dev/design-system/buttons.html. Decisions so far: dev/design-system/naming.md.
- Shared CSS: dev/design-system/ds-page.css (page chrome + token-only component
  classes: .panel .ctrl .prev .hrow .tool .gearb .band .sec .lbl .ctl .car .ph .tag
  .flag .box .gridhost .grid.wide/.knobs (--n) .rows .r .pill .add .o-title .o-sub
  .o-cap .o-foot .inv .cols .c2 .c3 .opt .t .d .rec .qtag .k .note .lede, buttons
  .b.m/.s/.xs + .main/.sec/.quiet/.danger). Tokens: inst/assets/css/blockr-tokens.css.
- Icon sprite already on the page: #i-gear #i-search #i-dl #i-check #i-plus #i-x
  (use <svg width=14 height=14><use href="#i-gear"/></svg>). Need other icons? Put
  an inline <svg> in your part (no external icon fonts).
- Package sources: /workspace/blockr.<pkg>/ (viz, dplyr, dm, extra, pharma, stats,
  io, dock, ui, sandbox, core ...). Git is /usr/lib/git-core/git. Earlier decided
  specs: /workspace/_blockr.design/done/ (e.g. shared-controls/select-controls.html,
  shared-controls/boolean-controls-proposals.html, shared-controls/gear-panel-proposals.html,
  block-config-ui/, chart-theming/, blockr-select-portal/). Old written spec:
  /workspace/blockr.docs/design-system/ (components/blockr-select.md,
  blockr-settings.md, blockr-row.md, blockr-input.md, pinned-controls.md,
  primary-item.md, spacing-and-sizing.md, ux-principles.md).
- Memory notes with past decisions (read the ones relevant to you):
  /home/dev/.claude/projects/-workspace/memory/ (MEMORY.md is the index).

## Already decided (treat as given; your proposals must fit them)
- Font Open Sans. Three text levels: default / muted / disabled. Labels 12px
  regular muted, 4px above the control. Controls 42px, field background, soft
  border, radius 8. Focus ring token. Tinted accent = the "main" button style.
- Buttons: main = tinted accent (no solid accent anywhere), secondary = surface +
  soft border, quiet = muted text, destructive = tinted red; sizes 42/30/26.
- Small icons (handles, remove): thin, muted, shown on hover/focus; remove turns
  red on hover. Row handle sits in the left padding.
- Proposed on block-layout.html (assume accepted): section titles = 12px 600
  uppercase tracked muted; output title 16px 600; ONE field grid (params grid,
  equal tracks, panel-width ladder, "wide" and "knobs"); rows for add/reorder lists.
  Anatomy: header row (26px tools, gear last) > gear band > controls on the face >
  output (title, body, caption, status line).
- From earlier sessions: every configurable option is in the gear (locking,
  per app, decides who can touch it); no gear when a block has no options;
  Blockr.Select for any number of choices incl. two (2026-07-22); Blockr.checkbox
  for booleans; chart blocks can "expose" mapping rows onto the block face
  (2026-09-08). Gear button canon 26px (viz drifted to 30px).

## What to produce
A single HTML FRAGMENT at dev/design-system/parts/<topic>.html (no <html>,
<head> or <body>; it is inserted into a page that already loads Open Sans, the
tokens, the dark tokens and ds-page.css). Structure:

<section class="part" id="<topic>">
  <h2 class="ptitle"><TITLE></h2>
  <p class="deps">Builds on: ... (which earlier decisions it assumes)</p>
  1. What exists today: an .inv table (job / today / where, with file paths and
     class names). Specimens of today's look copied with the package's real
     values (hex OK there, it is the inventory). Keep it tight.
  2. Questions, numbered with your prefix (<P>1, <P>2 ... inside <span class="qtag">).
     Per question: one short paragraph of why it is a question, then 2-3 options
     side by side (.cols .c2/.c3 with .opt) drawn realistically inside block
     panels, then <div class="rec">Proposal: <b>X</b> ... Runner-up: Y.</div>
     Only real questions: things that differ between packages, or that nobody has
     decided. Skip what is already consistent; list it at the end instead.
  3. "Recorded as they are": a short list of what is consistent and simply
     becomes the rule.
</section>

Scoped extra CSS goes in a <style> at the top of the fragment, every selector
prefixed with #<topic>. Proposed components use tokens only (var(--blockr-...)),
so the page's dark switch works. No <script> (it will not run). Hover/focus
states via CSS only.

Aim for 3-6 questions. Quality over quantity: each option must look like the
real app would, at real sizes.

## Writing rules (strict)
Plain English, short. No em dashes. No "not X, it is Y" antithesis, no rule of
three, no rhetorical questions, no aphorisms at paragraph ends, no sentence that
only comments on the previous one. Every sentence delivers a fact. One
recommendation per question plus a runner-up clause. Say what exists with file
paths; never invent usage you did not find in the code.

## Verify before finishing
Render it: http://127.0.0.1:3840/dev/design-system/part.html?p=<topic> (and
&dark=1). Screenshot with chromote in R, e.g.
  b <- chromote::ChromoteSession$new(width=1250,height=900)
  b$Page$navigate(url); Sys.sleep(3)
  h <- b$Runtime$evaluate("document.body.scrollHeight")$result$value
  b$Emulation$setDeviceMetricsOverride(width=1250,height=h,deviceScaleFactor=1,mobile=FALSE)
  s <- b$Page$captureScreenshot(); writeBin(jsonlite::base64_dec(s$data), "/tmp/<topic>.png")
then look at the image (crop with python PIL into ~1300px slices and Read them).
Fix overflow, clipping, misalignment, dark-mode breakage.

## Do not
- Do not edit any file other than parts/<topic>.html (and /tmp files).
- Do not commit, push, or touch package code. Do not start servers or Shiny apps.
- Do not use git's default `git` (use /usr/lib/git-core/git, read-only).

## Report back (final message, under 200 words)
The question list (id, one-line question, your proposal), anything you could
not verify, and any dependency on another part (e.g. "C2 assumes the gear band
from G1").
