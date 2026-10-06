/**
 * Types for the shared controls in this directory: blockr-ui.js (the
 * Blockr namespace and the small controls) and blockr-select.js.
 *
 * Dev tooling only: type-checked via tsconfig.json / `tsc`, never served to
 * the browser. A package that adds to the namespace (blockr.dplyr's block
 * protocol) declares its members in its own `interface BlockrNamespace`,
 * which TypeScript merges with this one.
 */

/* --- Blockr.Select (blockr-select.js) --- */

/** Option entry: a bare value string, or {value, label} for a muted label. */
type BlockrSelectOption = string | { value: string; label?: string };

interface BlockrSelectConfigBase {
  options?: BlockrSelectOption[];
  /** The field's label, as screen readers announce the select. A menu falls back to its title. */
  label?: string;
  /** Select.menu(): find the anchor again after it was redrawn (see Blockr.place). */
  reanchor?: () => HTMLElement | null;
  /** Shown when nothing is selected (single) / no tags (multi). */
  placeholder?: string;
  /**
   * Start with the dropdown showing "Loading…" until setLoading(false).
   * Pairs with onOpen to lazily fetch the option list on first open.
   */
  loading?: boolean;
  /** Fires on every dropdown open. */
  onOpen?: () => void;
  /**
   * Server-search hook: fires (debounced 250ms) with the current query
   * while the component is in server-search mode — i.e. after
   * setSearchInfo({truncated: true}). The consumer fetches a matching
   * option page and applies it via updateOptions + setSearchInfo.
   */
  onSearch?: (query: string) => void;
  /**
   * A standalone field (in the field grid, in the gear tray): 42px with a
   * border. Leave it off inside a row, where the select is bare.
   */
  bordered?: boolean;
  /**
   * Show the filter box once the list has more than 8 options (default
   * true). `false` for a short fixed list (operators, join types).
   */
  search?: boolean;
  /**
   * Start greyed out: text-disabled, never opens, not a tab stop (default
   * false). setDisabled() changes it later.
   */
  disabled?: boolean;
}

interface BlockrSelectSingleConfig extends BlockrSelectConfigBase {
  /** Initial value (null/undefined: first option, or '' if none). */
  selected?: string | null;
  /**
   * Opt out of the first-option fallback: `''` means "nothing selected" and
   * survives `setOptions()`, so the placeholder keeps showing. Use when a
   * silent auto-pick would change results (default false).
   */
  allowEmpty?: boolean;
  onChange?: (value: string) => void;
}

interface BlockrSelectMultiConfig extends BlockrSelectConfigBase {
  /** Initial values (copied, not aliased). */
  selected?: string[];
  /** Tags can be drag-reordered (default true). */
  reorderable?: boolean;
  /**
   * Keep the tags on one row and collapse the overflow into a "+N" chip,
   * instead of wrapping and growing the control (default false).
   */
  singleLine?: boolean;
  /**
   * Shorten tag labels longer than this many characters, cutting the MIDDLE so
   * both ends survive; the full value stays on the tag's title (default 0, off).
   */
  maxTagChars?: number;
  /** Receives a copy of the selected values, in tag order. */
  onChange?: (value: string[]) => void;
}

/** Internal union as consumed by createSelect (mode picks the shape). */
interface BlockrSelectConfig extends BlockrSelectConfigBase {
  selected?: string | string[] | null;
  allowEmpty?: boolean;
  reorderable?: boolean;
  singleLine?: boolean;
  maxTagChars?: number;
  /** `any` so both per-mode signatures are assignable under strict variance. */
  onChange?: (value: any) => void;
  title?: string;
  labelFirst?: boolean;
  searchPlaceholder?: string;
  onClose?: () => void;
}

/** Blockr.Select.menu(): the list alone, hung under a word or a pill. */
interface BlockrSelectMenuConfig extends BlockrSelectConfigBase {
  /** 'multi' keeps the menu open across picks; picks show in its head. */
  mode?: 'single' | 'multi';
  selected?: string | string[] | null;
  /** Names the setting the menu sets (the word that opened it names only the value). */
  title?: string;
  /** Lead each option with its label, the value muted after it. */
  labelFirst?: boolean;
  searchPlaceholder?: string;
  onChange?: (value: any) => void;
  onClose?: () => void;
}

interface BlockrSelectHandleBase {
  /** Root element (already appended to the container). */
  el: HTMLDivElement;
  /**
   * Replace the option list and reconcile the selection: single falls back
   * to the first option when `sel` is absent/unknown; multi keeps only
   * values present in the new options.
   */
  setOptions(
    opts: BlockrSelectOption[] | BlockrSelectOption | null | undefined,
    sel?: string | string[] | null
  ): void;
  /**
   * Swap the option list without reconciling the selection against it
   * (setOptions would drop chips whose value list hasn't arrived yet). Lazy
   * loading, and — with `sel` — a caller that owns the value and is only
   * borrowing the widget to display it: `sel` is forced, option list or not.
   * See Blockr.reconcileColumn.
   */
  updateOptions(
    opts: BlockrSelectOption[] | BlockrSelectOption | null | undefined,
    sel?: string | string[] | null
  ): void;
  /**
   * Set the selection from the owner (a restore, a mode switch) without
   * firing onChange; it is reconciled against the options as setOptions()
   * does.
   */
  setValue(value: string | string[] | null): void;
  /** Toggle the "Loading…" dropdown state. */
  setLoading(flag: boolean): void;
  /**
   * Grey the control out and take it out of the tab order, or put it back.
   * Disabling an open select closes it first.
   */
  setDisabled(flag: boolean): void;
  /**
   * Enter/leave server-search mode from a column-values response:
   * `truncated` activates the onSearch hook and the "N values — type to
   * search" footer; `total` is the full distinct count.
   */
  setSearchInfo(info: { total?: number; truncated?: boolean } | null | undefined): void;
  destroy(): void;
}

interface BlockrSelectSingleHandle extends BlockrSelectHandleBase {
  getValue(): string;
}

interface BlockrSelectMultiHandle extends BlockrSelectHandleBase {
  /** Copy of the selected values, in tag order. */
  getValue(): string[];
}

interface BlockrSelectStatic {
  single(
    container: HTMLElement,
    config: BlockrSelectSingleConfig
  ): BlockrSelectSingleHandle;
  multi(
    container: HTMLElement,
    config: BlockrSelectMultiConfig
  ): BlockrSelectMultiHandle;
  /**
   * The dropdown on its own, hung under `anchor`. Opens immediately and
   * destroys itself on close.
   */
  menu(
    anchor: HTMLElement,
    config: BlockrSelectMenuConfig
  ): { close: () => void };
  /**
   * Capability flag read by the packages that paint the words a menu opens
   * from: a build without it has a single-only menu.
   */
  menuMulti: boolean;
  /** Exposed for tests. */
  fitCount(widths: number[], avail: number, gap: number, chipWidth: number): number;
  midTruncate(value: string, cap: number): string;
}

/* --- Blockr.layer, the dismiss stack (blockr-ui.js) --- */

interface BlockrLayerOptions {
  /** What opened the layer: a pointerdown on it is inside, not outside. */
  from?: Element;
  /**
   * The layer sits in the page, not over it (the gear tray, a dirty field):
   * Escape reaches it only from inside it.
   */
  inPage?: boolean;
  /** What Escape does when the layer is on top; without it, Escape passes it by. */
  escape?: (e: KeyboardEvent) => void;
  /** What a pointerdown outside does; without it, the layer stays open. */
  outside?: (e: PointerEvent) => void;
}

/** Handle returned by Blockr.layer. */
interface BlockrLayerHandle {
  /** Take the layer off the stack; the control calls it when it closes. */
  remove(): void;
}

/** A layer as the stack holds it. */
interface BlockrLayerEntry {
  /** The layer's elements and what opened it: a pointerdown in any is inside. */
  els: Element[];
  inPage: boolean;
  escape: ((e: KeyboardEvent) => void) | null;
  outside: ((e: PointerEvent) => void) | null;
}

interface BlockrLayer {
  (el: Element | Element[], opts?: BlockrLayerOptions): BlockrLayerHandle;
  /** How many layers are open. */
  count(): number;
}

/** Handle returned by Blockr.place (blockr-ui.js). */
interface BlockrPlaceHandle {
  /** Recompute the position now (it also follows scroll, resize and size changes). */
  update(): void;
  /** Remove every listener and observer. */
  stop(): void;
}

/** Handle returned by Blockr.portal (blockr-ui.js). */
interface BlockrPortalHandle {
  /** Put the panel back where it was, or unbind and remove it if that left the page. */
  restore(): void;
}

interface BlockrPlaceOptions {
  /** Span the anchor (default), or size to content within bounds. */
  width?: 'anchor' | { min: number; max: number };
  /** With `width: 'anchor'`: never narrower than this (default 0). */
  minWidth?: number;
  /**
   * Line up with the anchor's left edge (default) or its right edge, for a
   * trigger in the header row.
   */
  align?: 'start' | 'end';
  /** Pixels between anchor and panel (default 4). */
  gap?: number;
  /** Distance kept from the viewport edges, both ways (default 8). */
  margin?: number;
  /**
   * The element that now stands for the anchor, once the anchor has left
   * the page (redrawn under the open panel). Without it, or while it returns
   * null, the panel stays where it is.
   */
  reanchor?: () => HTMLElement | null;
  /** Called on every placement with whether the panel sits above the anchor. */
  onFlip?: (above: boolean) => void;
}

/* --- Blockr.tooltip (blockr-ui.js) --- */

/** One tooltip line: plain text, or a column shown as its name, then its label muted. */
type BlockrTooltipLine = string | { name: string; label?: string; badge?: string };

/** A tooltip's content: one line, or several (the "+N" chip lists its hidden tags). */
type BlockrTooltipContent = BlockrTooltipLine | BlockrTooltipLine[];

interface BlockrTooltip {
  /** Give `el` a tooltip; with `overflow`, only while it or a child is cut off. */
  set(
    el: Element,
    content: BlockrTooltipContent | (() => BlockrTooltipContent),
    opts?: { overflow?: boolean }
  ): void;
  clear(el: Element): void;
  /** Plain text of `el`'s tooltip ("AGE · Age", lines joined by newlines), '' if none. */
  text(el: Element): string;
}

/** A row of Blockr.menu(). */
interface BlockrMenuItem {
  label: string;
  /** Drawn before the label: a Blockr.icons name ('trash') or an SVG/HTML string. */
  icon?: string;
  /** Grey text after the label (a block ID, a shortcut). */
  meta?: string;
  /** Set `meta` in the code face. */
  mono?: boolean;
  /** The item in use (the active view): weight 600. */
  current?: boolean;
  /** A toggle row; true shows a check (role menuitemcheckbox). */
  checked?: boolean;
  /** A destructive action: red under the pointer. */
  danger?: boolean;
  /** A muted row, such as "Manage pages". */
  quiet?: boolean;
  disabled?: boolean;
  /** Tooltip on a disabled row saying why. */
  reason?: string;
  onSelect?: () => void;
  /**
   * A block's mark before the label: its glyph (SVG) in its category's colour
   * on a tint of it, or in a `color` of its own (a stack's).
   */
  mark?: { icon?: string; category?: string; color?: string };
  /** A neutral badge at the end of the row (a package). */
  badge?: string;
  /** More text the filter box matches. */
  keywords?: string;
}

type BlockrMenuEntry = BlockrMenuItem | { gap: true } | { divider: true } | { title: string };

interface BlockrMenuConfig {
  items: BlockrMenuEntry[];
  /** Text above the rows: a title with an optional badge, then a line. */
  head?: { title: string; badge?: string; text?: string };
  /** 'end' lines the menu up with the trigger's right edge. */
  align?: 'start' | 'end';
  /** One muted line on top, saying what the menu is for. */
  caption?: string;
  /** A filter box on top (true, or its placeholder). */
  filter?: boolean | string;
  /** The panel's least width in px (default 180). */
  minWidth?: number;
  onClose?: () => void;
}

interface BlockrMenu {
  (anchor: HTMLElement, config: BlockrMenuConfig): { el: HTMLDivElement; close: () => void };
  /** Wire `trigger` to open and close its menu; a function config is read on each open. */
  bind(trigger: HTMLElement, config: BlockrMenuConfig | (() => BlockrMenuConfig)): void;
}

/** What drives an open menu of actions, of either kind (blockr-ui.js). */
interface BlockrMenuDrive {
  /** The floating surface, on <body> already. */
  panel: HTMLElement;
  /** Holds the rows and carries the menu role: the panel itself for markup from R. */
  list: HTMLElement;
  /** Takes the focus: the filter box, else the list. */
  focus?: HTMLElement;
  /** The trigger. */
  anchor: HTMLElement;
  width: { min: number; max: number };
  align: 'start' | 'end';
  /** Opened from the keyboard: the first row is the keyboard row. */
  byKeyboard?: boolean;
  /** A click on a usable row, by the pointer or by Enter and Space. */
  onPick(row: HTMLElement, e: MouseEvent): void;
  /** Takes the panel down once the menu has closed. */
  detach(): void;
  /** Runs last, once, whichever way the menu closed. */
  onClose?: () => void;
}

/* --- Blockr.actionMenu (blockr-ui.js) --- */

/** The document-level controller of the menus action_menu() builds in R. */
interface BlockrActionMenu {
  /** The trigger of the open menu, or null. */
  current(): HTMLElement | null;
  /** Close the open menu, if any. */
  close(): void;
}

/* --- Blockr.dropdown (blockr-ui.js) --- */

/** The document-level controller of the .blockr-dropdown panels. */
interface BlockrDropdown {
  /** Close the dropdown holding `el`, or whichever is open when `el` is omitted. */
  hide(el?: Element): void;
  /** Open the dropdown that is, or holds, `el`. */
  show(el: Element): void;
  /** The open dropdown's wrapper, or null. */
  current(): HTMLElement | null;
}

/* --- Blockr.Input (blockr-input.js) --- */

interface BlockrInputConfig {
  /** Initial field value. */
  value?: string;
  /** The field's name, as screen readers announce it. */
  label?: string;
  /** Column names offered as completions (backticked when non-syntactic). */
  columns?: string[];
  /**
   * Function completions: category label (shown as meta) -> function names.
   * Functions insert with trailing "()", cursor between the parens.
   */
  categories?: Record<string, string[]>;
  placeholder?: string;
  /** Render a <textarea> instead of <input>; disables Enter -> onConfirm. */
  multiline?: boolean;
  /** Fires on every edit and on completion insert (no arguments). */
  onChange?: () => void;
  /**
   * Single-line only: fires with the trimmed value on Enter while the
   * completion popup is closed.
   */
  onConfirm?: (value: string) => void;
}

interface BlockrInputHandle {
  /** Root element (already appended to the container). */
  el: HTMLDivElement;
  /** Trimmed field value. */
  getValue(): string;
  setValue(v: string | null | undefined): void;
  /** Replace the column completions (function categories are fixed). */
  setColumns(cols: string[] | null | undefined): void;
  focus(): void;
  destroy(): void;
}

interface BlockrInputStatic {
  create(container: HTMLElement, config: BlockrInputConfig): BlockrInputHandle;
}

interface BlockrNamespace {
  tooltip: BlockrTooltip;
  menu: BlockrMenu;

  actionMenu: BlockrActionMenu;
  dropdown: BlockrDropdown;
  uid(prefix?: string): string;
  /** Whether `el`, or anything in it, is cut off by its box. */
  cutOff(el: Element): boolean;
  escapeHtml(s: string): string;
  removeNode(node: Node | null | undefined): void;
  contentWidth(el: Element): number;
  _measureEl?: HTMLDivElement;
  /** The small icons by name, from the files small_icon() reads in R (inst/assets/icons). */
  icons: Record<string, string>;
  /** The dismiss stack: what Escape and a click outside close (blockr-ui.js). */
  layer: BlockrLayer;
  /**
   * Hang a fixed-position, body-portalled panel under an anchor and keep it
   * there: flips above when there is no room below, follows scroll, resize
   * and size changes of anchor and panel (blockr-ui.js).
   */
  place(panel: HTMLElement, anchor: HTMLElement, opts?: BlockrPlaceOptions): BlockrPlaceHandle;
  /**
   * Carry a panel rendered in R to <body> while it floats; `gone` runs if
   * `anchor` leaves the page meanwhile (blockr-ui.js).
   */
  portal(panel: HTMLElement, anchor: HTMLElement, gone: () => void): BlockrPortalHandle;
  /** Blockr.Select (blockr-select.js). */
  Select?: BlockrSelectStatic;
  /** Blockr.Input, the code field with completions (blockr-input.js). */
  Input?: BlockrInputStatic;
  /** Design-system checkbox factory (blockr-ui.js). */
  checkbox(
    label: string,
    checked: boolean,
    onChange: (checked: boolean) => void
  ): BlockrCheckboxHandle;
  /** Design-system segmented control (blockr-ui.js). */
  segmented(
    options: { value: string; label: string; title?: string }[],
    selected: string,
    onChange: (value: string) => void,
    opts?: { size?: 'xs'; label?: string }
  ): BlockrSegmentedHandle;
  /** The gear tray behaviour (blockr-ui.js): the gear toggles the band,
   *  which slides open and closed; Escape inside the band or on the gear
   *  closes it. `open` starts it open, without the slide. */
  gearTray(
    band: HTMLElement,
    gear: HTMLButtonElement,
    opts?: { label?: string; open?: boolean }
  ): BlockrGearTrayHandle;
  /** Toggle the canonical required-empty amber cue on a field wrapper. */
  setRequiredEmpty(el: Element, empty: boolean): void;
  /** Whether this is a Mac; decided once, with `.blockr-mac` on the root. */
  isMac: boolean;
  /** A shortcut written for this platform: "Mod+S" is "⌘S" or "Ctrl+S". */
  keys(keys: string): string;
  /** Commit-on-Enter text input with the ↵ button (§5.5). */
  textCommit(
    input: HTMLInputElement,
    opts: { onCommit: (value: string) => void }
  ): BlockrTextCommitHandle;
}

/** Handle returned by Blockr.textCommit (blockr-ui.js). */
interface BlockrTextCommitHandle {
  chip: HTMLButtonElement;
  commit(): void;
  sync(value: string): void;
}

/** Handle returned by Blockr.segmented (blockr-ui.js). */
interface BlockrSegmentedHandle {
  el: HTMLDivElement;
  set(value: string): void;
  get(): string;
}

/** Handle returned by Blockr.gearTray (blockr-ui.js). */
interface BlockrGearTrayHandle {
  set(open: boolean): void;
  toggle(): void;
  isOpen(): boolean;
}

/** Handle returned by Blockr.checkbox (blockr-ui.js). */
interface BlockrCheckboxHandle {
  el: HTMLLabelElement;
  input: HTMLInputElement;
  set(v: boolean): void;
  get(): boolean;
}

declare var Blockr: BlockrNamespace;

interface Window {
  Blockr: BlockrNamespace;
}
