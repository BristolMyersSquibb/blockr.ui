// What blockr.ui ships for the tests of packages, as `@blockr/ui/stubs`, as
// core ships its stubs of the hosts as `@blockr/core/stubs`
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.7.3#testing):
// - The stub of Bound, stubBound(), with useStubBound(), through which a
//   component takes the stub's value as it changes (./bound.ts).
// - The helpers that act on blockr.ui's components as a user does, and find
//   their parts by what the user reads: in a field, a select, an overlay and
//   the rows of a table, a tree or a grid list (./inputs.ts, ./select.ts,
//   ./overlays.ts, ./rows.ts), and step() and draw(), which act and draw
//   through React's act() (./react.ts).
//   They know where React Aria draws each part, such as a select's list at
//   the end of the page's body, so that a change to a component and the
//   change to its helper ship in the same release of blockr.ui.
// - The helpers expectedErrors() and expectedWarnings(), with which a test
//   takes the errors and the warnings that it sets off on purpose off the
//   console (./console.ts).
// Loading them makes happy-dom, the DOM that the tests run on, do as a
// browser does where blockr.ui's components need it (./dom.ts).
// The setup that fails a test on an error or a warning that it prints is an
// entry of its own, `@blockr/ui/stubs/setup`, which a package lists in
// `setupFiles`, so that loading the stubs sets up nothing (./setup.ts).

import "./dom.ts";

export { stubBound, useStubBound } from "./bound.ts";
export type { StubBound } from "./bound.ts";
export { expectedErrors, expectedWarnings } from "./console.ts";
export {
  choose,
  drop,
  fieldError,
  find,
  numberSaid,
  picking,
  press,
  pressAndRelease,
  texts,
  toggle,
  toggleButton,
  toggleGroup,
  toggles,
  type,
} from "./inputs.ts";
export {
  inert,
  itemDescribed,
  menuGroup,
  menuItem,
  menuItems,
  openDialog,
  openMenu,
  pointerPress,
  pointTo,
  shownTip,
  tipOnFocus,
} from "./overlays.ts";
export { draw, step } from "./react.ts";
export { rowCheckbox, rowOf, rows, rowTexts } from "./rows.ts";
export {
  chipFromLabel,
  chipLabels,
  cleared,
  clearButton,
  labelled,
  listed,
  listFromField,
  listNote,
  markedLabel,
  notAvailable,
  optionFromLabel,
  optionLabels,
  picked,
  pickedLabels,
  removeChip,
  selectButton,
  selectShows,
} from "./select.ts";
export type { Act } from "./select.ts";
