// The disclosure of blockr.ui: React Aria's Disclosure, DisclosureGroup
// and DisclosurePanel (https://react-aria.adobe.com/Disclosure), with the
// starter kit's DisclosureHeader, drawn with the theme variables, after the Disclosure and
// the DisclosureGroup of React Aria's Tailwind starter kit
// (https://github.com/adobe/react-spectrum/blob/react-aria-components%401.22.1/starters/tailwind/src/Disclosure.tsx,
// https://github.com/adobe/react-spectrum/blob/react-aria-components%401.22.1/starters/tailwind/src/DisclosureGroup.tsx).
// Each takes the props of its part of React Aria, such as `defaultExpanded`,
// or `isExpanded` and `onExpandedChange` for a caller that keeps whether the
// disclosure is expanded.
// A disclosure holds its header, whose button expands and collapses it, and
// its panel, which shows while it is expanded, and may hold more between
// them, such as what shows whether it is expanded or not.
// The header is a heading, of the `level` given, 3 by default, which holds
// the button: its title after a chevron, Lucide's ChevronRight, as in the
// starter kit, which points at the title while the disclosure is collapsed,
// and down while it is expanded, as a browser draws a triangle before the
// summary of a <details>. The button takes the heading's face,
// in semibold at text-base, or in what the header's `className` gives the
// heading in place of that.
// The panel's height moves as it expands and collapses, but not while the
// user prefers reduced motion. A collapsed panel keeps what it holds on the
// page, hidden, where the browser's search in the page still finds it, and a
// match there expands the disclosure.
// The panel sets no padding of its own, since its height moves to 0: what it
// holds sets its own.
// A group of disclosures keeps one expanded at a time, unless it
// `allowsMultipleExpanded`.

import { useContext } from "react";
import type { ReactElement, ReactNode } from "react";
import {
  Button,
  composeRenderProps,
  Disclosure as AriaDisclosure,
  DisclosurePanel as AriaDisclosurePanel,
  DisclosureStateContext,
  Heading,
} from "react-aria-components";
import type { DisclosurePanelProps, DisclosureProps } from "react-aria-components";
import { Icon } from "../icon/index.ts";
import { ChevronRight } from "../vendor/index.ts";

export function Disclosure({ className, ...props }: DisclosureProps): ReactElement {
  return <AriaDisclosure {...props} className={composeRenderProps(className, (given) => `${DISCLOSURE} ${given ?? ""}`)} />;
}

export interface DisclosureHeaderProps {
  readonly children: ReactNode;
  readonly level?: number;
  readonly className?: string;
}

export function DisclosureHeader({ children, level = 3, className }: DisclosureHeaderProps): ReactElement {
  const expanded = useContext(DisclosureStateContext)?.isExpanded ?? false;
  return (
    <Heading level={level} className={`${HEADING} ${className ?? FACE}`}>
      <Button slot="trigger" className={TRIGGER}>
        <Marker expanded={expanded} />
        {children}
      </Button>
    </Heading>
  );
}

export function DisclosurePanel({ className, ...props }: DisclosurePanelProps): ReactElement {
  return <AriaDisclosurePanel {...props} className={composeRenderProps(className, (given) => `${PANEL} ${given ?? ""}`)} />;
}

// The chevron before the header's title.
function Marker({ expanded }: { expanded: boolean }): ReactElement {
  return (
    <span className={`bk:flex bk:shrink-0 bk:transition-transform bk:motion-reduce:transition-none ${expanded ? "bk:rotate-90" : ""}`}>
      <Icon icon={ChevronRight} size={12} />
    </span>
  );
}

const DISCLOSURE = "bk:font-sans";
const HEADING = "bk:m-0 bk:font-sans";
const FACE = "bk:text-base bk:font-semibold";
// The button takes the face of the heading, and fills its width.
const TRIGGER =
  "bk:flex bk:w-full bk:cursor-pointer bk:items-center bk:gap-1.5 bk:rounded-sm bk:border-0 bk:bg-transparent bk:p-0 bk:text-left bk:text-inherit bk:[font:inherit] bk:outline-none bk:data-focus-visible:outline-2 bk:data-focus-visible:outline-offset-2 bk:data-focus-visible:outline-ring bk:data-disabled:cursor-default bk:data-disabled:opacity-50";
const PANEL = "bk:h-(--disclosure-panel-height) bk:overflow-clip bk:motion-safe:transition-[height]";
