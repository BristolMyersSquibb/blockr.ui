// The toggle button and the toggle button group of blockr.ui: React
// Aria's ToggleButton and ToggleButtonGroup
// (https://react-aria.adobe.com/ToggleButton, https://react-aria.adobe.com/ToggleButtonGroup),
// drawn with the theme variables, after those of React Aria's Tailwind starter kit
// (https://github.com/adobe/react-spectrum/blob/react-aria-components%401.22.1/starters/tailwind/src/ToggleButton.tsx,
// https://github.com/adobe/react-spectrum/blob/react-aria-components%401.22.1/starters/tailwind/src/ToggleButtonGroup.tsx).
// Each takes the props of its part of React Aria.
// A group draws its buttons side by side in one box, as a field's box, and
// takes the label and the description of blockr.ui's fields, which a group
// needs to name what it picks, as a select names it.
// A group holds its buttons, each with the id that it picks.
//
// Given `bound`, a toggle button shows and edits the boolean that its `bound`
// gives, as blockr.ui's other inputs do (./inputs.tsx), and a group a string,
// or an array of strings where its selectionMode is "multiple", by the ids of
// its buttons.
// Each submits the user's pick at once, a group of several built from the value
// that the host holds then, through the get() of its `bound`, so that it keeps
// a string that another user added in the meantime.
// A group of one unsets its value as the user unpicks its pick, unless it
// sets disallowEmptySelection.
// A group of several submits its picks in the order the user picked them, after
// those the value holds, or where `ordered`, in the order of its buttons.
// A group draws no button for a value that none of its buttons holds.
// It marks itself invalid and says below itself which values are not available,
// as the select does (./combobox.tsx), but not while it is `outdated`.
// The user drops such a value from a group of one by picking a button, and from
// a group of several by the button that the line holds, Remove, which submits
// the value without them.
// Each says below itself why its last submit was refused, the `error` of its
// `bound`.

import { createContext, useContext, useId, useRef } from "react";
import type { ReactElement, ReactNode } from "react";
import { strings } from "./json.ts";
import { composeRenderProps, ToggleButton as AriaToggleButton, ToggleButtonGroup as AriaToggleButtonGroup } from "react-aria-components";
import type { ToggleButtonGroupProps as AriaToggleButtonGroupProps, ToggleButtonProps as AriaToggleButtonProps } from "react-aria-components";
import type { Bound } from "./bound.ts";
import { styles } from "../styles/index.ts";
import { NotAvailable, Refusal } from "./inputs.tsx";
import { held, inOrder, ItemsContext, lacking, merged, useItem, useItems } from "./items.ts";

export interface ToggleButtonProps extends AriaToggleButtonProps {
  // The value that it shows and submits, for a button outside a group.
  readonly bound?: Bound;
}

export function ToggleButton({ bound, onChange, ...props }: ToggleButtonProps): ReactElement {
  if (bound === undefined) return <DrawnButton {...props} onChange={onChange} />;
  return (
    <>
      <DrawnButton
        {...props}
        isSelected={bound.value === true}
        onChange={(selected) => {
          void bound.commit(selected);
          onChange?.(selected);
        }}
      />
      <Refusal error={bound.error} />
    </>
  );
}

// Whether a button sits in a group, whose box holds it.
const InGroup = createContext(false);

// A button that tells its group its id.
// Outside a group, it has a border of its own.
function DrawnButton({ className, ...props }: AriaToggleButtonProps): ReactElement {
  const ref = useRef<HTMLButtonElement>(null);
  const inGroup = useContext(InGroup);
  useItem(props.id === undefined ? undefined : String(props.id), ref);
  return (
    <AriaToggleButton
      {...props}
      ref={ref}
      className={composeRenderProps(className, (given) => `${TOGGLE} ${inGroup ? "" : ALONE} ${given ?? ""}`)}
    />
  );
}

export interface ToggleButtonGroupProps extends Omit<AriaToggleButtonGroupProps, "children"> {
  // What blockr.ui's fields take.
  readonly label?: string;
  readonly description?: string;
  // Its buttons.
  readonly children?: ReactNode;
  // The value that it shows and submits.
  readonly bound?: Bound;
  readonly outdated?: boolean;
  readonly ordered?: boolean;
}

export function ToggleButtonGroup({ bound, outdated = false, ordered = false, ...props }: ToggleButtonGroupProps): ReactElement {
  return bound === undefined ? (
    <DrawnGroup {...props} />
  ) : (
    <BoundGroup {...props} bound={bound} outdated={outdated} ordered={ordered} />
  );
}

type DrawnGroupProps = Omit<ToggleButtonGroupProps, "bound" | "outdated" | "ordered"> & {
  // Whether it holds a value that its buttons lack.
  readonly invalid?: boolean;
};

function BoundGroup({
  bound,
  outdated,
  ordered,
  onSelectionChange,
  children,
  ...props
}: DrawnGroupProps & { readonly bound: Bound; readonly outdated: boolean; readonly ordered: boolean }): ReactElement {
  const { known, items } = useItems();
  const several = props.selectionMode === "multiple";
  const shown = several ? strings(bound.value) : typeof bound.value === "string" ? [bound.value] : [];
  const gone = lacking(shown, known);
  const marking = !outdated && gone.length > 0;
  return (
    <>
      <DrawnGroup
        {...props}
        selectedKeys={shown}
        invalid={marking}
        onSelectionChange={(keys) => {
          const picks = [...keys].map(String);
          if (several) {
            const value = merged(strings(bound.get()), shown, picks);
            if (value !== null) void bound.commit(ordered && known !== null ? inOrder(value, known) : value);
          } else {
            const [pick] = picks;
            void (pick === undefined ? bound.unset() : bound.commit(pick));
          }
          onSelectionChange?.(keys);
        }}
      >
        <ItemsContext.Provider value={items}>{children}</ItemsContext.Provider>
      </DrawnGroup>
      {marking ? (
        <NotAvailable
          values={gone}
          onRemove={
            several
              ? () => {
                  void bound.commit(held(strings(bound.get()), known));
                }
              : undefined
          }
        />
      ) : null}
      <Refusal error={bound.error} />
    </>
  );
}

// The label, the box of the buttons and the description.
// React Aria's group has no label of its own, so the label names the group
// through aria-labelledby, and the description through aria-describedby.
function DrawnGroup({ label, description, invalid = false, children, className, ...props }: DrawnGroupProps): ReactElement {
  const labelId = useId();
  const descriptionId = useId();
  const described = description !== undefined && description !== "";
  const group = (
    <AriaToggleButtonGroup
      {...props}
      aria-labelledby={props["aria-labelledby"] ?? (label === undefined ? undefined : labelId)}
      aria-describedby={described ? descriptionId : undefined}
      data-invalid={invalid || undefined}
      className={composeRenderProps(className, (given) => `${GROUP} ${given ?? ""}`)}
    >
      <InGroup.Provider value={true}>{children}</InGroup.Provider>
    </AriaToggleButtonGroup>
  );
  if (label === undefined && !described) return group;
  return (
    <div className={styles.field}>
      {label === undefined ? null : (
        <span id={labelId} className={styles.label}>
          {label}
        </span>
      )}
      {group}
      {described ? (
        <p id={descriptionId} className={styles.hint}>
          {description}
        </p>
      ) : null}
    </div>
  );
}

// The classes of the box of a group's buttons, and of a button, which takes
// the brand colour where it is picked, and the fill of a hover under the
// pointer otherwise.
// The box is outlined as a secondary button is, and outside a group, a button
// has the border of the box.
// The box takes the colour of an error where the group holds a value that its
// buttons lack.
const GROUP =
  "bk:flex bk:w-fit bk:max-w-full bk:flex-wrap bk:gap-0.5 bk:rounded-md bk:border bk:border-border bk:p-0.5 bk:font-sans bk:data-[orientation=vertical]:flex-col bk:data-invalid:border-destructive";
const TOGGLE =
  "bk:inline-flex bk:min-h-6 bk:cursor-pointer bk:items-center bk:justify-center bk:gap-1 bk:rounded-sm bk:border-0 bk:bg-transparent bk:px-2 bk:py-0.5 bk:font-sans bk:text-sm bk:text-inherit bk:outline-none bk:data-hovered:bg-accent bk:data-hovered:text-accent-foreground bk:data-focus-visible:outline-2 bk:data-focus-visible:outline-offset-1 bk:data-focus-visible:outline-ring bk:data-selected:bg-primary bk:data-selected:text-primary-foreground bk:data-disabled:cursor-default bk:data-disabled:opacity-50";
const ALONE = "bk:rounded-md bk:border bk:border-solid bk:border-border bk:data-selected:border-primary";
