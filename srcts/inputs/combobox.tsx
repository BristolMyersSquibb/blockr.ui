// The one select of blockr.ui, which core's own UIs, frontends, the UIs
// of plugins and those of blocks all draw with: React Aria's ComboBox
// (https://react-aria.adobe.com/ComboBox), drawn with the theme variables.
// It picks one item, or several where its selectionMode is "multiple", out of
// a list that the user filters by typing into its field.
// It is drawn after the ComboBox of React Aria's Tailwind starter kit
// (https://github.com/adobe/react-spectrum/blob/react-aria-components%401.22.1/starters/tailwind/src/ComboBox.tsx),
// and takes what that takes: the props of React Aria's ComboBox, and the
// label, the description, the error message and the placeholder that the
// starter kit adds.
// It hands its items to React Aria's ComboBox, as the starter kit of React
// Aria's docs does, so that React Aria filters `defaultItems` by what the user
// types, and leaves `items` to its caller to filter.
// It takes the renderEmptyState of React Aria's ListBox too, for what its
// list says where it has no item at all.
// Its items are ComboBoxItem and ComboBoxSection below, as in the starter kit.
//
// The field of several shows each pick as a chip, whose button removes it,
// as Backspace in the empty field removes the last one.
// The chips wrap onto more lines, but each cuts its own text off where the
// field is too narrow for it. A chip whose text is cut off shows it whole in
// a tooltip of blockr.ui, as the pointer rests on its text, and at once as the
// chip takes the focus from the keyboard.
// A FieldButton at the field's right, before the button that opens the list,
// clears the pick, or every pick, where there is one, but where the select
// `isRequired`. It says so in a tooltip of blockr.ui as the pointer rests on
// it.
// It is outside the order of Tab, as the button that opens the list is, since
// the keyboard clears the field itself, and it keeps the focus in the field.
// Bound, a select of one unsets its value so, through the unset() of its
// `bound`, as a block's new select of an arg without a default starts.
// A press in the field opens the list, as typing into it does.
// Where the select is disabled or read-only, the field changes no pick, as
// React Aria's ComboBox changes none then.
// Its chips show no button and remove nothing on a key, Backspace removes
// nothing, no button clears, and a press in the field opens no list.
// The chips of a disabled select take no focus either, as its text field
// takes none.
// The list hangs below the field, or above it where the window has no room
// below, as wide as the field.
// React Aria draws it in a portal at the end of the page's body, above a modal
// of blockr.ui that holds the select, as every overlay of blockr.ui sits in
// such a portal, at the z-index of the overlays, --bk-z-index-overlay
// (../overlays/index.ts).
// The list draws its rows through React Aria's Virtualizer, which puts on the
// page only the rows in view, however many items the select holds.
// The list says "Loading…" while it has no items yet, or while its items are
// `outdated` and there are none, "No matches" where React Aria's filter keeps none of `defaultItems`,
// and otherwise what `renderEmptyState` says, "No options" by default, as
// wherever the `items` that its caller filters are none.
//
// Given `bound`, it shows and edits the value that its `bound` gives, as
// blockr.ui's other inputs do (./inputs.tsx): one string, or an array of
// strings where it picks several, by the ids of its items.
// It shows a value that its items lack, such as a column that the data no
// longer has, by its id, as it shows every value, marks itself invalid, draws
// the value's chip in the colour of an error, and says below itself which values
// are not available, as "Not available: wt" (./inputs.tsx).
// While it has no items yet, as `items` and `defaultItems` are undefined while
// its children draw an item, it marks no value.
// While it is `outdated`, as while the request that gives its items asks again,
// the items may be for older data, so it marks none either.
// So its items must hold every value it takes, not a part of them.
// One pick it submits as the user picks it.
// Several it submits once, as the list closes, built from the value that the
// host holds then, through the get() of its `bound`, so that it keeps a string
// that another user added in the meantime.
// It submits them in the order the user picked them, after those the value
// holds, or where `ordered`, in the order of its items.
// Where the select goes while its list is open, as where its module unmounts,
// the picks go with it, as though the user never made them.
// A chip that the user removes while the list is closed it submits at once.
// It says below itself why its last submit was refused, the `error` of its
// `bound`.

import { useContext, useEffect, useEffectEvent, useLayoutEffect, useMemo, useRef, useState } from "react";
import type { ContextType, KeyboardEvent, ReactElement, ReactNode } from "react";
import { Icon } from "../icon/index.ts";
import { OVERLAY_Z_INDEX, Tooltip, TooltipTrigger } from "../overlays/index.ts";
import { styles } from "../styles/index.ts";
import { Check, ChevronDown, X } from "../vendor/index.ts";
import type { Bound } from "./bound.ts";
import { strings } from "./json.ts";
import {
  Button,
  Collection,
  ComboBox as AriaComboBox,
  ComboBoxStateContext,
  composeRenderProps,
  FieldError,
  Group,
  Header,
  Input,
  Label,
  ListBox,
  ListBoxItem,
  ListBoxSection,
  ListLayout,
  Popover,
  Tag,
  TagGroup,
  TagList,
  Text,
  TooltipTriggerStateContext,
  Virtualizer,
} from "react-aria-components";
import type {
  ComboBoxProps as AriaComboBoxProps,
  Key,
  ListBoxItemProps,
  ListBoxSectionProps,
  ValidationResult,
} from "react-aria-components";
import { FieldButton } from "./button.tsx";
import { NotAvailable, Refusal } from "./inputs.tsx";
import { inOrder, merged } from "./items.ts";

type Mode = "single" | "multiple";

export interface ComboBoxProps<T extends object, M extends Mode = "single">
  extends Omit<AriaComboBoxProps<T, M>, "children"> {
  // What the starter kit adds.
  readonly label?: string;
  readonly description?: string | null;
  readonly errorMessage?: string | ((validation: ValidationResult) => string);
  readonly placeholder?: string;
  readonly children: ReactNode | ((item: T) => ReactNode);
  // What the list says where it has no item at all, as in React Aria's
  // ListBox.
  readonly renderEmptyState?: () => ReactNode;
  // The value that it shows and submits.
  readonly bound?: Bound;
  readonly outdated?: boolean;
  readonly ordered?: boolean;
}

export function ComboBox<T extends object, M extends Mode = "single">(props: ComboBoxProps<T, M>): ReactElement {
  const { bound, ordered = false, ...rest } = props;
  if (bound === undefined) return <Drawn {...(rest as DrawnProps)} />;
  return props.selectionMode === "multiple" ? (
    <BoundSeveral {...(rest as DrawnProps)} bound={bound} ordered={ordered} />
  ) : (
    <BoundOne {...(rest as DrawnProps)} bound={bound} />
  );
}

// What the select draws with, of either mode, whether it marks a value that
// its items lack, and whether its field may offer to clear the pick.
type DrawnProps = Omit<ComboBoxProps<object, Mode>, "bound" | "ordered"> & {
  readonly marking?: boolean;
  // What clears the pick in place of the select's own clearing, as where its
  // `bound` unsets the value.
  readonly onClear?: () => void;
};

function BoundOne({ bound, outdated = false, ...props }: DrawnProps & { readonly bound: Bound }): ReactElement {
  const current = typeof bound.value === "string" ? bound.value : null;
  return (
    <>
      <Drawn
        {...props}
        selectionMode="single"
        outdated={outdated}
        marking={!outdated && known(props)}
        onClear={() => {
          void bound.unset();
        }}
        value={current}
        onChange={(key) => {
          if (key !== null && !Array.isArray(key)) void bound.commit(String(key));
        }}
      />
      <Refusal error={bound.error} />
    </>
  );
}

function BoundSeveral({
  bound,
  outdated = false,
  ordered,
  onOpenChange,
  ...props
}: DrawnProps & { readonly bound: Bound; readonly ordered: boolean }): ReactElement {
  const items = props.items ?? props.defaultItems;
  const held = strings(bound.value);
  // The picks while the list is open, and the value as it opened, which the
  // list submits as it closes, and which events read as they come.
  const [picks, setPicks] = useState<readonly string[] | null>(null);
  const pending = useRef<{ readonly picks: readonly string[]; readonly opened: readonly string[] } | null>(null);

  // Builds the submit from the value that the host holds now: the strings the
  // user picked since the list opened, after those the value holds, without
  // those the user removed.
  const submit = (next: readonly string[], opened: readonly string[]): void => {
    const value = merged(strings(bound.get()), opened, next);
    if (value === null) return;
    void bound.commit(ordered && items !== undefined ? inOrder(value, ids(items)) : value);
  };

  return (
    <>
      <Drawn
        {...props}
        selectionMode="multiple"
        outdated={outdated}
        marking={!outdated && known(props)}
        value={picks ?? held}
        onOpenChange={(isOpen, trigger) => {
          if (isOpen) {
            pending.current ??= { picks: held, opened: held };
            setPicks(pending.current.picks);
          } else {
            const left = pending.current;
            pending.current = null;
            setPicks(null);
            if (left !== null) submit(left.picks, left.opened);
          }
          onOpenChange?.(isOpen, trigger);
        }}
        onChange={(keys) => {
          if (!Array.isArray(keys)) return;
          const next = keys.map(String);
          if (pending.current === null) {
            submit(next, held);
            return;
          }
          pending.current = { ...pending.current, picks: next };
          setPicks(next);
        }}
      />
      <Refusal error={bound.error} />
    </>
  );
}

// Whether the select has its items, rather than a function that draws an item
// with no items to draw yet.
function known(props: DrawnProps): boolean {
  return (props.items ?? props.defaultItems) !== undefined || typeof props.children !== "function";
}

// The ids of the items, as React Aria keys an item by its id.
function ids(items: Iterable<object>): string[] {
  return [...items].flatMap((item) => {
    const id = (item as { id?: unknown }).id;
    return typeof id === "string" || typeof id === "number" ? [String(id)] : [];
  });
}

// The label, the field, the description, the error message and the list.
function Drawn({
  label,
  description,
  errorMessage,
  placeholder,
  children,
  renderEmptyState,
  outdated = false,
  marking = false,
  onClear,
  className,
  allowsEmptyCollection = true,
  ...props
}: DrawnProps): ReactElement {
  const multiple = props.selectionMode === "multiple";
  const isDisabled = props.isDisabled === true;
  const editable = !isDisabled && props.isReadOnly !== true;
  const given = props.items ?? props.defaultItems;
  // Whether it has no items yet, as its children draw an item with none given.
  const unknown = given === undefined && typeof children === "function";
  const none = given === undefined ? typeof children === "function" || !hasChildren(children) : [...given].length === 0;
  const empty = none && (outdated || unknown) ? "Loading…" : none ? (renderEmptyState?.() ?? "No options") : "No matches";
  const value = useSameValue(props.value);
  return (
    <AriaComboBox
      {...props}
      value={value}
      defaultItems={unknown ? [] : props.defaultItems}
      allowsEmptyCollection={allowsEmptyCollection}
      className={composeRenderProps(className, (given) => `bk:group ${styles.field} ${given ?? ""}`)}
    >
      {label === undefined ? null : <Label className={styles.label}>{label}</Label>}
      <Field
        placeholder={placeholder}
        multiple={multiple}
        marking={marking}
        isDisabled={isDisabled}
        editable={editable}
        clearable={props.isRequired !== true && editable}
        onClear={onClear}
        label={label}
      />
      {/* React Aria names the description of the field only where the
          element is there from the start, so it is there while empty too. */}
      <Text slot="description" className={`${styles.hint} bk:empty:hidden`}>
        {description}
      </Text>
      <FieldError className={styles.alert}>{errorMessage}</FieldError>
      <Lacking multiple={multiple} marking={marking} />
      <Popover offset={4} className={LIST} style={{ zIndex: OVERLAY_Z_INDEX }}>
        <Rows empty={empty}>{children}</Rows>
      </Popover>
    </AriaComboBox>
  );
}

// The rows of the list, which React Aria's Virtualizer lays out one below the
// other at the heights that ROW and HEADING give them, 8 and 7 times the
// spacing, inside a padding of the spacing.
// The Virtualizer takes those heights in pixels, so the rows measure the
// spacing as the list opens, through an element as wide as --bk-spacing.
// So the rows follow a frontend's spacing, and the browser's size of text,
// which the spacing's rem follows.
// Where nothing measures it, as in happy-dom, they take the default spacing.
function Rows({ empty, children }: { readonly empty: ReactNode; readonly children: DrawnProps["children"] }): ReactElement {
  const probe = useRef<HTMLSpanElement>(null);
  const [unit, setUnit] = useState(SPACING);
  useLayoutEffect(() => {
    const measured = probe.current?.getBoundingClientRect().width ?? 0;
    if (measured > 0) setUnit(measured);
  }, []);
  const layout = useMemo(() => ({ rowHeight: 8 * unit, headingHeight: 7 * unit, padding: unit }), [unit]);
  return (
    <>
      <span ref={probe} aria-hidden="true" className={PROBE} style={{ width: "var(--bk-spacing)" }} />
      <Virtualizer layout={ListLayout} layoutOptions={layout}>
        <ListBox renderEmptyState={() => <p className={NOTE}>{empty}</p>} className={LISTBOX}>
          {children}
        </ListBox>
      </Virtualizer>
    </>
  );
}

// The value given, or the array of the keys that the select drew before where
// it holds the same keys.
// React Aria takes a new array of picks for a change of the value, and then
// empties the field, so that a caller that builds the picks afresh as it draws
// would empty the field as the user types into it.
// The Select takes its picks so too (./select.tsx).
export function useSameValue<V>(value: V): V {
  const [drawn, setDrawn] = useState(value);
  const same = drawn === value || (Array.isArray(drawn) && Array.isArray(value) && sameKeys(drawn, value));
  if (!same) setDrawn(value);
  return same ? drawn : value;
}

function sameKeys(a: readonly unknown[], b: readonly unknown[]): boolean {
  return a.length === b.length && a.every((key, i) => key === b[i]);
}

function hasChildren(children: ReactNode): boolean {
  return Array.isArray(children) ? children.some((child) => hasChildren(child as ReactNode)) : children !== null && children !== undefined && children !== false;
}

// The field: the chips of the picks of several, the text field, which takes
// the filter, and the button that opens and closes the list.
// It shows a pick that the items lack by its id, as the text of its chip, or
// as the text of the field of one.
// Where `marking`, it marks itself invalid while it holds such a pick, and
// draws the pick's chip in the colour of an error.
// The text field of one shows the pick's text while the user is not in it,
// as React draws the pick, where React Aria would set it a step later, from
// an effect, so that the select shows a value as its `bound` gives it, with
// no wait for React, as core's controls(), which bind inputs for a module
// written without React, need.
function Field({
  placeholder,
  multiple,
  marking,
  isDisabled,
  editable,
  clearable,
  onClear,
  label,
}: {
  readonly placeholder: string | undefined;
  readonly multiple: boolean;
  readonly marking: boolean;
  readonly isDisabled: boolean;
  // Whether the field changes the pick, as not where the select is disabled
  // or read-only.
  readonly editable: boolean;
  readonly clearable: boolean;
  readonly onClear: (() => void) | undefined;
  readonly label: string | undefined;
}): ReactElement {
  const { state, keys, one, known, lacked } = usePicks(multiple);
  // The text of the pick of one, where the items hold it.
  const item = one === null ? undefined : known.get(one);
  const invalid = marking && lacked.length > 0;
  const remove = (gone: ReadonlySet<Key>): void => {
    state?.setValue(keys.filter((key) => !gone.has(key)));
  };
  // The chip of each pick: its key, its text, and whether the items lack it.
  const chips = keys.map((key) => ({ key, text: known.get(key) ?? String(key), gone: marking && !known.has(key) }));
  const picks = label === undefined ? "Picks" : `Picks of ${label}`;
  return (
    <Group isInvalid={invalid} className={FIELD}>
      <div className={VALUES}>
        {keys.length === 0 ? null : isDisabled ? (
          // The chips of a disabled select take no focus, as its text field
          // takes none.
          // React Aria's TagGroup keeps a tab stop on its list even where
          // every tag is disabled, so a disabled select draws its chips as
          // a plain list.
          <div role="list" aria-label={picks} className="bk:flex bk:min-w-0 bk:max-w-full bk:flex-wrap bk:gap-1">
            {chips.map(({ key, text, gone }) => (
              <PlainChip key={key} text={text} gone={gone} />
            ))}
          </div>
        ) : (
          <TagGroup aria-label={picks} onRemove={editable ? remove : undefined} className="bk:flex bk:min-w-0 bk:max-w-full">
            <TagList className="bk:flex bk:min-w-0 bk:flex-wrap bk:gap-1">
              {chips.map(({ key, text, gone }) => (
                <Tag key={key} id={key} textValue={text} data-missing={gone || undefined} className={CHIP}>
                  {({ isFocusVisible }) => (
                    <>
                      <ChipText text={text} focused={isFocusVisible} />
                      {editable ? (
                        <Button slot="remove" aria-label={`Remove ${text}`} className={CHIP_REMOVE}>
                          <Icon icon={X} size={10} />
                        </Button>
                      ) : null}
                    </>
                  )}
                </Tag>
              ))}
            </TagList>
          </TagGroup>
        )}
        <Input
          value={multiple || state === null || state.isFocused || state.isOpen ? undefined : (item ?? (one === null ? "" : String(one)))}
          placeholder={one !== null && item === undefined ? String(one) : keys.length > 0 ? undefined : placeholder}
          aria-invalid={invalid || undefined}
          className={INPUT}
          onClick={() => {
            if (editable && state !== null && !state.isOpen) state.open(null, "manual");
          }}
          onKeyDown={(event: KeyboardEvent<HTMLInputElement>) => {
            // An Escape that closes the list closes nothing else, such as the
            // dialog or the form that holds the select, which React Aria lets
            // it reach.
            if (event.key === "Escape" && state?.isOpen === true) event.preventDefault();
            if (!editable || event.key !== "Backspace" || event.currentTarget.value !== "" || keys.length === 0) return;
            state?.setValue(keys.slice(0, -1));
          }}
        />
      </div>
      {clearable && (keys.length > 0 || one !== null) ? (
        // A FieldButton whose slot is null, which React Aria's ComboBox then
        // leaves alone, rather than take it for the button that opens the
        // list. It is outside the order of Tab, and keeps the focus in the
        // text field, as a press in the list does.
        // It is the trigger of its tooltip, as a button of React Aria is.
        <TooltipTrigger>
          <FieldButton
            slot={null}
            excludeFromTabOrder
            preventFocusOnPress
            aria-label={label === undefined ? "Clear" : `Clear ${label}`}
            onPress={() => {
              if (onClear !== undefined) onClear();
              else state?.setValue(multiple ? [] : null);
            }}
          >
            <Icon icon={X} size={10} />
          </FieldButton>
          <Tooltip>{multiple ? "Clear every pick" : "Clear the pick"}</Tooltip>
        </TooltipTrigger>
      ) : null}
      <Button className={OPENER}>
        <span className={CHEVRON}>
          <Icon icon={ChevronDown} size={12} />
        </span>
      </Button>
    </Group>
  );
}

// A chip of a disabled select, which takes no focus.
function PlainChip({ text, gone }: { text: string; gone: boolean }): ReactElement {
  return (
    <div role="listitem" data-missing={gone || undefined} className={CHIP}>
      <ChipText text={text} focused={false} />
    </div>
  );
}

// The text of a chip, which the chip cuts off where it is too narrow for it.
// It then shows the text whole in a tooltip while the pointer rests on the
// text, and while the chip has the focus from the keyboard, as `focused`
// says.
// The text follows the pointer itself, since React Aria counts a chip as
// hovered only where the select's chips can be picked or act.
// The tooltip opens and closes as the state of a TooltipTrigger has it, with
// React Aria's delays: after the pointer rests a while, at once while another
// tooltip shows, and at once on the focus.
// The chip's own name holds the text whole for assistive technology, so the
// tooltip names nothing to it.
function ChipText({ text, focused }: { text: string; focused: boolean }): ReactElement {
  return (
    <TooltipTrigger>
      <CutText text={text} focused={focused} />
    </TooltipTrigger>
  );
}

// The tooltip closes at once as the chip loses the focus, and after React
// Aria's delay as the pointer leaves it.
function CutText({ text, focused }: { text: string; focused: boolean }): ReactElement {
  const tip = useContext(TooltipTriggerStateContext);
  const ref = useRef<HTMLSpanElement>(null);
  const [pointed, setPointed] = useState(false);
  const shown = useRef({ pointed: false, focused: false });
  const follow = useEffectEvent((now: { pointed: boolean; focused: boolean }) => {
    const before = shown.current;
    shown.current = now;
    const el = ref.current;
    const cut = el !== null && el.scrollWidth > el.clientWidth;
    if (tip === null) return;
    if (cut && now.focused) tip.open(true);
    else if (cut && now.pointed) tip.open();
    else if (before.pointed || before.focused) tip.close(before.focused);
  });
  useEffect(() => {
    follow({ pointed, focused });
  }, [pointed, focused]);
  return (
    <>
      <span
        ref={ref}
        className="bk:truncate"
        onPointerEnter={(event) => {
          if (event.pointerType !== "touch") setPointed(true);
        }}
        onPointerLeave={() => {
          setPointed(false);
        }}
      >
        {text}
      </span>
      <Tooltip triggerRef={ref}>{text}</Tooltip>
    </>
  );
}

// The picks that the select holds, as React Aria's state of the ComboBox has
// them: the keys of several, or the key of one, the text of each pick that
// the items hold, and the picks that they lack.
function usePicks(multiple: boolean): {
  readonly state: ContextType<typeof ComboBoxStateContext>;
  readonly keys: readonly Key[];
  readonly one: Key | null;
  readonly known: ReadonlyMap<Key, string>;
  readonly lacked: readonly Key[];
} {
  const state = useContext(ComboBoxStateContext);
  const value = state?.value ?? null;
  const keys = multiple && Array.isArray(value) ? (value as readonly Key[]) : [];
  const one = !multiple && value !== null && !Array.isArray(value) ? (value as Key) : null;
  const known = new Map((state?.selectedItems ?? []).map((item) => [item.key, item.textValue]));
  const lacked = (one === null ? keys : [one]).filter((key) => !known.has(key));
  return { state, keys, one, known, lacked };
}

// What the select says below itself, where `marking`, of the picks that its
// items lack. The user removes such a pick by its chip, or picks another in
// place of a pick of one.
function Lacking({ multiple, marking }: { readonly multiple: boolean; readonly marking: boolean }): ReactElement | null {
  const { lacked } = usePicks(multiple);
  return marking ? <NotAvailable values={lacked.map(String)} /> : null;
}

// A row of the list: the item, after a tick where the select picks several,
// in weight 600 where the select picks one and it is the pick.
// A description, which a child Text in the slot "description" holds, shows
// muted after it.
export function ComboBoxItem(props: ListBoxItemProps): ReactElement {
  const textValue = props.textValue ?? (typeof props.children === "string" ? props.children : undefined);
  return (
    <ListBoxItem {...props} textValue={textValue} className={ROW}>
      {composeRenderProps(props.children, (children, { isSelected, selectionMode }) => (
        <>
          {selectionMode === "multiple" ? (
            <span className="bk:flex bk:size-3.5 bk:shrink-0 bk:text-primary">{isSelected ? <Icon icon={Check} /> : null}</span>
          ) : null}
          <span
            className={`bk:flex bk:min-w-0 bk:flex-1 bk:items-center bk:gap-2 bk:truncate ${
              selectionMode === "single" && isSelected ? "bk:font-semibold" : ""
            }`}
          >
            {children}
          </span>
        </>
      ))}
    </ListBoxItem>
  );
}

export interface ComboBoxSectionProps<T> extends ListBoxSectionProps<T> {
  readonly title?: string;
  readonly items?: Iterable<T>;
}

// A group of rows under a heading.
// The Virtualizer lays the heading out as it lays out a row, so the heading
// scrolls away with the rows of its group.
export function ComboBoxSection<T extends object>({ title, items, children, ...props }: ComboBoxSectionProps<T>): ReactElement {
  return (
    <ListBoxSection {...props}>
      <Header className={HEADING}>{title}</Header>
      <Collection items={items}>{children}</Collection>
    </ListBoxSection>
  );
}

// The classes of the field and its parts, of the list and of its parts.
// The field is a box like blockr.ui's other inputs, which holds the chips, the text
// field and the button, and draws the outline of the focus while the focus is
// in it.
const FIELD =
  "bk:flex bk:min-h-7.5 bk:min-w-0 bk:items-center bk:gap-1 bk:justify-self-stretch bk:rounded-md bk:border bk:border-input bk:bg-transparent bk:py-0.5 bk:pr-0.5 bk:pl-2 bk:font-sans bk:text-sm bk:data-focus-within:outline-2 bk:data-focus-within:outline-offset-1 bk:data-focus-within:outline-ring bk:data-invalid:border-destructive bk:data-disabled:opacity-50";
// The box of the chips and the text field, which wraps the chips onto more
// lines, while the buttons stay at the field's right.
const VALUES = "bk:flex bk:min-w-0 bk:flex-1 bk:flex-wrap bk:items-center bk:gap-1";
// The chevron of the button that opens the list, in the muted colour of the
// text, which turns up while the list is open, as the select marks itself
// open.
const CHEVRON = "bk:flex bk:shrink-0 bk:text-muted-foreground bk:transition-transform bk:group-data-open:rotate-180";
const INPUT =
  "bk:min-w-16 bk:flex-1 bk:border-0 bk:bg-transparent bk:p-0 bk:font-sans bk:text-sm bk:text-inherit bk:outline-none bk:placeholder:text-muted-foreground bk:disabled:cursor-default";
const OPENER =
  "bk:ml-auto bk:flex bk:size-6 bk:shrink-0 bk:cursor-pointer bk:items-center bk:justify-center bk:rounded-sm bk:border-0 bk:bg-transparent bk:p-0 bk:text-inherit bk:data-disabled:cursor-default";
// A chip of a pick that the items lack shows in the colour of danger, as the
// field then does, through the attributes that mark them, whose classes win
// over those of their default colours.
// A chip with no button, as where the select is disabled or read-only, pads
// its right as it pads its left.
const CHIP =
  "bk:inline-flex bk:max-w-full bk:min-w-0 bk:items-center bk:gap-0.5 bk:rounded-sm bk:border bk:border-border bk:bg-muted bk:py-0 bk:pr-1.5 bk:pl-1.5 bk:data-allows-removing:pr-0.5 bk:text-xs bk:leading-5 bk:outline-none bk:data-focus-visible:outline-2 bk:data-focus-visible:outline-ring bk:data-missing:border-destructive bk:data-missing:text-destructive-text";
const CHIP_REMOVE =
  "bk:flex bk:size-4 bk:shrink-0 bk:cursor-pointer bk:items-center bk:justify-center bk:rounded-sm bk:border-0 bk:bg-transparent bk:p-0 bk:text-muted-foreground bk:data-hovered:bg-accent bk:data-hovered:text-accent-foreground";
// The list takes the field's width, and the box of the rows scrolls within
// the height that React Aria leaves the list in the window, its border
// included.
// The box fits the list to the pixel: where it stood out of the list by its
// border, React Aria's scroll to the pick on opening moved the list by the
// border, took that for a list out of view and scrolled the page, which
// closes the list, or moves a row under a pointer still pressed, which picks
// it as the pointer goes up.
// The Virtualizer lays the rows out one below the other, inside a padding of
// the spacing, which the box of the rows thus holds none of in its CSS.
// A row is one line, as ROW truncates what it holds, so the Virtualizer takes
// each row to be 8 times the spacing high and each heading 7 times, the
// heights that ROW and HEADING give them, rather than measure each as it draws
// it (Rows).
// The default spacing is 4 pixels, which a page whose rem is 16 pixels draws.
const SPACING = 4;
// The element that measures the spacing, which nothing sees and which takes
// no place.
const PROBE = "bk:pointer-events-none bk:invisible bk:absolute bk:h-0";
const LIST =
  "bk:box-border bk:w-(--trigger-width) bk:overflow-hidden bk:rounded-lg bk:bg-popover bk:font-sans bk:text-sm bk:text-popover-foreground bk:shadow-md bk:outline-none";
const LISTBOX =
  "bk:box-border bk:max-h-[inherit] bk:overflow-y-auto bk:rounded-lg bk:border bk:border-border bk:outline-none";
const HEADING =
  "bk:box-border bk:h-7 bk:bg-popover bk:px-2.5 bk:pt-2 bk:pb-1 bk:text-xs bk:font-semibold bk:uppercase bk:tracking-wider bk:text-muted-foreground";
const ROW =
  "bk:box-border bk:flex bk:h-8 bk:cursor-pointer bk:items-center bk:gap-2 bk:rounded-sm bk:px-2.5 bk:text-sm bk:outline-none bk:data-hovered:bg-accent bk:data-hovered:text-accent-foreground bk:data-focused:bg-accent bk:data-focused:text-accent-foreground bk:data-focus-visible:outline bk:data-focus-visible:outline-1 bk:data-focus-visible:outline-ring bk:data-disabled:cursor-default bk:data-disabled:opacity-50 bk:[&_[slot=description]]:min-w-0 bk:[&_[slot=description]]:flex-1 bk:[&_[slot=description]]:truncate bk:[&_[slot=description]]:text-sm bk:[&_[slot=description]]:text-muted-foreground bk:data-hovered:[&_[slot=description]]:text-accent-foreground bk:data-focused:[&_[slot=description]]:text-accent-foreground";
const NOTE = "bk:m-0 bk:px-2.5 bk:py-2 bk:text-center bk:text-sm bk:italic bk:text-muted-foreground";
