// The list box of blockr.ui: React Aria's ListBox
// (https://react-aria.adobe.com/ListBox), drawn with the theme variables, after the ListBox
// of React Aria's Tailwind starter kit
// (https://github.com/adobe/react-spectrum/blob/react-aria-components%401.22.1/starters/tailwind/src/ListBox.tsx).
// It takes the props of React Aria's ListBox.
// It shows a list of items in place, in a box like a field's, which picks one
// item, or several, where its selectionMode does.
// Its rows are ListBoxItem below, the rows of the ComboBox's list
// (./combobox.tsx), and its sections those of that list.
// An Autocomplete around it, with a SearchField, narrows its rows to those
// that hold what the user types, as React Aria builds a select's list alone.
// A list box takes no `bound`: a caller that picks with it holds what it
// picks itself.

import type { ReactElement } from "react";
import { composeRenderProps, ListBox as AriaListBox } from "react-aria-components";
import type { ListBoxItemProps, ListBoxProps } from "react-aria-components";
import { ComboBoxItem } from "./combobox.tsx";

export function ListBox<T extends object>({ className, ...props }: ListBoxProps<T>): ReactElement {
  return <AriaListBox {...props} className={composeRenderProps(className, (given) => `${LISTBOX} ${given ?? ""}`)} />;
}

export function ListBoxItem(props: ListBoxItemProps): ReactElement {
  return <ComboBoxItem {...props} />;
}

// The box of the rows, which scrolls them within its height where the caller
// gives it one.
const LISTBOX =
  "bk:box-border bk:grid bk:min-w-0 bk:content-start bk:overflow-y-auto bk:rounded-md bk:border bk:border-input bk:p-1 bk:font-sans bk:text-sm bk:outline-none bk:data-focus-visible:outline-2 bk:data-focus-visible:outline-offset-1 bk:data-focus-visible:outline-ring bk:data-empty:px-2.5 bk:data-empty:py-2 bk:data-empty:italic bk:data-empty:text-muted-foreground";
