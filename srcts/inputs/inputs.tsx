// What the inputs of blockr.ui share, each of which shows and edits one value
// that its `bound` gives (./bound.ts), such as a block's state arg that core's
// useValue() binds, so that each meets the contract of a block's UI as the
// binding does
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.7#the-contract):
// the line below an input that names the values it lacks, the line that says
// why the last submit was refused, and what the user typed into a field until
// the binding submits it.
// Here too are the layout of a form's controls and the field, a label that
// holds its input, so that it needs no id, as a block's UI may be mounted in
// several places at once.
// Each input sits in a file of its own beside this one.
// React Aria gives their parts ids, which React makes anew for each mount.

import { useState } from "react";
import type { ReactElement, ReactNode } from "react";
import { styles } from "../styles/index.ts";
import { Button } from "./button.tsx";
import type { Bound, Json } from "./bound.ts";

// The controls of a UI, one below the other, as Shopify's Polaris names the
// component that does the same
// (https://github.com/Shopify/polaris/blob/3f7954ae42fabf26d63cee68c23ceebfd7ef0972/polaris-react/src/components/FormLayout/FormLayout.tsx#L18).
// It pairs with Form without being it: Form is React Aria's form element with
// its validation (./form.tsx), while FormLayout only arranges controls, so a
// block's UI uses it without a form.
export function FormLayout({ children }: { children: ReactNode }): ReactElement {
  return <div className={styles.formLayout}>{children}</div>;
}

// A label that holds its input.
export function Field({ label, children }: { label: string; children: ReactNode }): ReactElement {
  return (
    <label className={styles.field}>
      <span className={styles.label}>{label}</span>
      {children}
    </label>
  );
}

// Says below an input which of its values it lacks, such as picks that none
// of its items holds or a number outside a slider's range, by their ids or
// as numbers, as "Not available: wt", since no item gives them a text.
// The input shows such a value as it is wherever it shows its value, and
// marks itself invalid.
// Where no item shows such a value, as in a group of several or a Select of
// several, the line offers a button, Remove, through which the user drops
// them, `onRemove`.
export function NotAvailable({
  values,
  onRemove,
}: {
  readonly values: readonly string[];
  readonly onRemove?: () => void;
}): ReactElement | null {
  if (values.length === 0) return null;
  const named = values.join(", ");
  return (
    <div className="bk:flex bk:flex-wrap bk:items-center bk:gap-x-2">
      <p role="alert" className={styles.alert}>{`Not available: ${named}`}</p>
      {onRemove === undefined ? null : (
        <Button variant="quiet" aria-label={`Remove ${named}`} onPress={onRemove}>
          Remove
        </Button>
      )}
    </div>
  );
}

// Says why the last submit of an input was refused, as its `bound` gives it.
export function Refusal({ error }: { error: string | null }): ReactElement | null {
  return error === null ? null : (
    <p role="alert" className={styles.alert}>
      {error}
    </p>
  );
}

// What the user typed into an input since it last showed the value of its
// `bound`, and the setter of it, which drops it given null,
// so that the input shows the value of its `bound` again.
// The binding gives the input no value while the user types,
// so a value that it gives ends what the user typed, as the host took it or
// announced another.
export function useTyped(bound: Bound): [string | null, (text: string | null) => void] {
  const [text, setText] = useState<string | null>(null);
  const [seen, setSeen] = useState<Json | undefined>(bound.value);
  if (!Object.is(seen, bound.value)) {
    setSeen(bound.value);
    setText(null);
  }
  return [text, setText];
}
