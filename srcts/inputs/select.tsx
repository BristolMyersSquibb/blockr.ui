// The select of blockr.ui that has no field to type into: React Aria's
// Select (https://react-aria.adobe.com/Select), drawn with the theme variables, for a pick
// of a few values, which a list shows whole, where blockr.ui's ComboBox
// (./combobox.tsx) filters a long list by what the user types.
// It picks one item, or several where its selectionMode is "multiple", out
// of a list that opens from its button.
// It is drawn after the Select of React Aria's Tailwind starter kit
// (https://github.com/adobe/react-spectrum/blob/react-aria-components%401.22.1/starters/tailwind/src/Select.tsx),
// and takes what that takes: the props of React Aria's Select, and the label,
// the description, the error message and the items that the starter kit adds.
// Its items are SelectItem and SelectSection below, the rows of the
// ComboBox's list.
// Its button shows the pick, or the picks of several, and the placeholder
// while there is none.
// React Aria draws the list in a portal at the end of the page's body, above
// a modal of blockr.ui that holds the select, as every overlay of blockr.ui sits
// in such a portal (../overlays/index.ts).
//
// Given `bound`, it shows and edits the value that its `bound` gives, as
// blockr.ui's other inputs do (./inputs.tsx): one string, or an array of
// strings where it picks several, by the ids of its items.
// It submits as the ComboBox does: one pick as the user picks it, and several
// once, as the list closes, built from the value that the host holds then, in
// the order the user picked them, after those the value holds, or where
// `ordered`, in the order of its items.
// Its button shows a value that its items lack, such as a column that the data
// no longer has, by its id, after the picks that the items hold.
// The select then marks itself invalid and says below itself which values are
// not available, as the ComboBox does, but not while it is `outdated`, or while
// it has no items yet, as `items` is undefined while its children draw an item.
// Its list holds no row for such a value.
// So the user drops it from a select of one by picking an item, and from a
// select of several by the button that the line below holds, Remove, which
// submits the value without them.
// It says below itself why its last submit was refused, the `error` of its
// `bound`.

import { useContext, useLayoutEffect, useMemo, useRef, useState } from "react";
import type { ReactElement, ReactNode } from "react";
import {
  Button,
  Collection,
  composeRenderProps,
  ListBox,
  Popover,
  Select as AriaSelect,
  SelectStateContext,
  SelectValue,
  useLocale,
} from "react-aria-components";
import type { Key, ListBoxItemProps, SelectProps as AriaSelectProps, ValidationResult } from "react-aria-components";
import { Icon } from "../icon/index.ts";
import { styles } from "../styles/index.ts";
import { ChevronDown } from "../vendor/index.ts";
import type { Bound } from "./bound.ts";
import { strings } from "./json.ts";
import { ComboBoxItem, ComboBoxSection, useSameValue } from "./combobox.tsx";
import type { ComboBoxSectionProps } from "./combobox.tsx";
import { Description, FieldError, Label, NotAvailable, Refusal } from "./inputs.tsx";
import { held, inOrder, merged } from "./items.ts";

type Mode = "single" | "multiple";

export interface SelectProps<T extends object, M extends Mode = "single"> extends Omit<AriaSelectProps<T, M>, "children"> {
  // What the starter kit adds.
  readonly label?: string;
  readonly description?: string | null;
  readonly errorMessage?: string | ((validation: ValidationResult) => string);
  readonly items?: Iterable<T>;
  readonly children: ReactNode | ((item: T) => ReactNode);
  // The value that it shows and submits.
  readonly bound?: Bound;
  readonly outdated?: boolean;
  readonly ordered?: boolean;
}

export function Select<T extends object, M extends Mode = "single">(props: SelectProps<T, M>): ReactElement {
  const { bound, outdated = false, ordered = false, ...rest } = props;
  if (bound === undefined) return <Drawn {...(rest as DrawnProps)} />;
  return <BoundSelect {...(rest as DrawnProps)} bound={bound} outdated={outdated} ordered={ordered} />;
}

// A row of the list, as a row of the ComboBox's list.
export function SelectItem(props: ListBoxItemProps): ReactElement {
  return <ComboBoxItem {...props} />;
}

export type SelectSectionProps<T> = ComboBoxSectionProps<T>;

// A group of rows under a heading.
export function SelectSection<T extends object>(props: SelectSectionProps<T>): ReactElement {
  return <ComboBoxSection {...props} />;
}

type DrawnProps = Omit<SelectProps<object, Mode>, "bound" | "outdated" | "ordered"> & {
  // The values that the button shows by their ids, after the picks that the
  // items hold, and what reads the ids of the items.
  readonly asIs?: readonly string[];
  readonly onKnown?: (ids: readonly string[]) => void;
};

function BoundSelect({
  bound,
  outdated,
  ordered,
  onChange,
  onOpenChange,
  ...props
}: DrawnProps & { readonly bound: Bound; readonly outdated: boolean; readonly ordered: boolean }): ReactElement {
  const several = props.selectionMode === "multiple";
  const values = several ? strings(bound.value) : typeof bound.value === "string" ? [bound.value] : [];
  // The ids of the items, as React Aria holds them, null until it has, and
  // while the items are unknown, as while the select's children draw an item
  // with none given.
  const [known, setKnown] = useState<readonly string[] | null>(null);
  const unknown = props.items === undefined && typeof props.children === "function";
  const ids = unknown ? null : known;
  // The picks of several while the list is open, and the value as it opened,
  // which the list submits as it closes, and which events read as they come.
  const [picks, setPicks] = useState<readonly string[] | null>(null);
  const pending = useRef<{ readonly picks: readonly string[]; readonly opened: readonly string[] } | null>(null);
  const shown = picks ?? values;
  // The values that the button shows by their ids: those that the items lack,
  // and every value until the items are known.
  const asIs = ids === null ? values : values.filter((value) => !ids.includes(value));
  const marking = !outdated && ids !== null && asIs.length > 0;

  const submit = (next: readonly string[], opened: readonly string[]): void => {
    const value = merged(strings(bound.get()), opened, next);
    if (value === null) return;
    void bound.commit(ordered && ids !== null ? inOrder(value, ids) : value);
  };

  const common = {
    ...props,
    isInvalid: props.isInvalid === true || marking,
    asIs,
    onKnown: (next: readonly string[]) => {
      setKnown((last) => (last !== null && sameIds(last, next) ? last : next));
    },
  };
  const line = marking ? (
    <NotAvailable
      values={asIs}
      onRemove={
        several
          ? () => {
              void bound.commit(held(strings(bound.get()), ids));
            }
          : undefined
      }
    />
  ) : null;

  if (!several) {
    return (
      <>
        <Drawn
          {...common}
          selectionMode="single"
          value={values[0] ?? null}
          onChange={(key) => {
            if (key !== null && !Array.isArray(key)) void bound.commit(String(key));
            onChange?.(key);
          }}
          onOpenChange={onOpenChange}
        />
        {line}
        <Refusal error={bound.error} />
      </>
    );
  }
  return (
    <>
      <Drawn
        {...common}
        selectionMode="multiple"
        value={[...shown]}
        onOpenChange={(isOpen) => {
          if (isOpen) {
            pending.current ??= { picks: values, opened: values };
            setPicks(pending.current.picks);
          } else {
            const left = pending.current;
            pending.current = null;
            setPicks(null);
            if (left !== null) submit(left.picks, left.opened);
          }
          onOpenChange?.(isOpen);
        }}
        onChange={(keys) => {
          if (Array.isArray(keys)) {
            const next = (keys as readonly Key[]).map(String);
            if (pending.current === null) {
              submit(next, values);
            } else {
              pending.current = { ...pending.current, picks: next };
              setPicks(next);
            }
          }
          onChange?.(keys);
        }}
      />
      {line}
      <Refusal error={bound.error} />
    </>
  );
}

function sameIds(a: readonly string[], b: readonly string[]): boolean {
  return a.length === b.length && a.every((id, i) => id === b[i]);
}

// The label, the button, the description, the error message and the list.
function Drawn({ label, description, errorMessage, items, children, asIs = [], onKnown, className, ...props }: DrawnProps): ReactElement {
  const value = useSameValue(props.value);
  const { locale } = useLocale();
  const list = useMemo(() => new Intl.ListFormat(locale), [locale]);
  return (
    <AriaSelect
      {...props}
      value={value}
      className={composeRenderProps(className, (given) => `bk:group ${styles.field} ${given ?? ""}`)}
    >
      {label === undefined ? null : <Label>{label}</Label>}
      <Button className={TRIGGER}>
        {/* React Aria shows the picks that the items hold, and the values
            that they lack follow by their ids. */}
        <SelectValue className={({ isPlaceholder }) => (isPlaceholder && asIs.length === 0 ? `${VALUE} ${PLACEHOLDER}` : VALUE)}>
          {({ state, selectedText, defaultChildren, isPlaceholder }) =>
            asIs.length > 0
              ? list.format([...state.selectedItems.map((item) => item.textValue), ...asIs])
              : isPlaceholder
                ? defaultChildren
                : selectedText || defaultChildren
          }
        </SelectValue>
        <span className={CHEVRON}>
          <Icon icon={ChevronDown} size={12} />
        </span>
      </Button>
      {/* React Aria names the description of the select only where the
          element is there from the start, so it is there while empty too. */}
      <Description>{description}</Description>
      <FieldError>{errorMessage}</FieldError>
      <Popover offset={4} className={LIST}>
        <ListBox renderEmptyState={() => <p className={NOTE}>No options</p>} className={LISTBOX}>
          {items === undefined ? (typeof children === "function" ? null : children) : <Collection items={items}>{children}</Collection>}
        </ListBox>
      </Popover>
      {onKnown === undefined ? null : <Known onKnown={onKnown} />}
    </AriaSelect>
  );
}

// Hands `onKnown` the ids of the items in the select's collection, in their
// order, each time the select has drawn.
// React Aria draws the select's children a second time, hidden, as it builds
// the collection, where no state of the select is there to read, and that
// draw hands nothing.
function Known({ onKnown }: { onKnown: (ids: readonly string[]) => void }): null {
  const state = useContext(SelectStateContext);
  useLayoutEffect(() => {
    if (state === null) return;
    const { collection } = state;
    const ids = [...collection.getKeys()].flatMap((key) =>
      collection.getItem(key)?.type === "item" ? [String(key)] : [],
    );
    onKnown(ids);
  });
  return null;
}

// The classes of the button, of the pick it shows and of the list.
// The button is a box like blockr.ui's other inputs, which shows the pick and the
// chevron, and takes the colour of danger where the select is invalid, as
// where it holds a value that its items lack.
// The placeholder takes the muted colour.
const TRIGGER =
  "bk:flex bk:min-h-7.5 bk:w-full bk:min-w-0 bk:cursor-pointer bk:items-center bk:gap-2 bk:justify-self-stretch bk:rounded-md bk:border bk:border-input bk:bg-transparent bk:py-1 bk:pr-1.5 bk:pl-2 bk:text-left bk:font-sans bk:text-sm bk:text-inherit bk:outline-none bk:data-focus-visible:outline-2 bk:data-focus-visible:outline-offset-1 bk:data-focus-visible:outline-ring bk:group-data-invalid:border-destructive bk:data-disabled:cursor-default bk:data-disabled:opacity-50";
const VALUE = "bk:min-w-0 bk:flex-1 bk:truncate bk:group-data-invalid:text-destructive-text";
const PLACEHOLDER = "bk:text-muted-foreground";
// The chevron of the button that opens the list, in the muted colour of the
// text, which turns up while the list is open, as the select marks itself
// open.
const CHEVRON = "bk:flex bk:shrink-0 bk:text-muted-foreground bk:transition-transform bk:group-data-open:rotate-180";
const LIST =
  "bk:box-border bk:min-w-(--trigger-width) bk:overflow-hidden bk:rounded-lg bk:bg-popover bk:font-sans bk:text-sm bk:text-popover-foreground bk:shadow-md bk:outline-none";
const LISTBOX =
  "bk:box-border bk:max-h-[inherit] bk:overflow-y-auto bk:rounded-lg bk:border bk:border-border bk:p-1 bk:outline-none";
const NOTE = "bk:m-0 bk:px-2.5 bk:py-2 bk:text-center bk:text-sm bk:italic bk:text-muted-foreground";
