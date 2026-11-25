// The libraries that blockr.ui's components draw with beside React and React
// Aria, the one place that imports them.
// The build bundles what this file exports, minified, into blockr.ui's vendor
// file, vendor-<hash>.js, and leaves each import of this file an import of
// the vendor file (../build/build.ts).
// So the file of the components, blockr-ui.js, holds blockr.ui's own code
// alone, as the vendor check asks.
//
// - The functions of @internationalized/date
//   (https://react-aria.adobe.com/internationalized/date/), with which the
//   dates and times read the strings of a value and write those that they
//   submit (../inputs/dates.ts).
//   React Aria's files hold a copy of their own inside, which they do not
//   export, and the package holds no React context, so a second copy does no
//   harm.
// - The icons of Lucide that the components draw, from the lucide that
//   blockr.ui pins and offers packages as @blockr/ui/icons
//   (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.7.3#the-icons),
//   each where React Aria's starter kit draws it: ChevronDown on a select,
//   ChevronUp and ChevronDown on the steppers of a number field, ChevronLeft
//   and ChevronRight on the buttons that page a calendar, Calendar on the
//   button that opens a date picker, Check and Minus in a checkbox, Check and
//   ChevronRight in a menu, ChevronRight before a disclosure's title, a tree's
//   item and the next breadcrumb, X where a button clears or removes, Search in
//   a search field, ArrowUp on the column that a table is sorted by,
//   CircleExclamationPoint and Info on a dialog that asks,
//   TriangleExclamationPoint on a meter from 80%, and GripVertical on the
//   button that drags a row.
//   An icon that a component starts to draw goes here.

export { parseDate, parseDateTime, parseTime, toCalendarDate, toCalendarDateTime, toTime } from "@internationalized/date";
export {
  ArrowUp,
  Calendar,
  Check,
  ChevronDown,
  ChevronLeft,
  ChevronRight,
  ChevronUp,
  CircleExclamationPoint,
  GripVertical,
  Info,
  Minus,
  Search,
  TriangleExclamationPoint,
  X,
} from "lucide";
