// The breadcrumbs of blockr.ui: React Aria's Breadcrumbs and
// Breadcrumb (https://react-aria.adobe.com/Breadcrumbs), drawn with the theme variables,
// after the Breadcrumbs of React Aria's Tailwind starter kit
// (https://github.com/adobe/react-spectrum/blob/react-aria-components%401.22.1/starters/tailwind/src/Breadcrumbs.tsx).
// The breadcrumbs take the props of React Aria's Breadcrumbs, and a
// breadcrumb those of React Aria's Breadcrumb and those of blockr.ui's Link,
// in which it draws its text, in the colour of the text around it.
// The breadcrumbs lay out their breadcrumbs in a row, which wraps, with a
// chevron after each but the last, which is the current one.
// A breadcrumb with an `href` goes there. Without one, the breadcrumbs' `onAction`
// acts on it, given its `id`. The current breadcrumb does nothing.
// A UI puts the breadcrumbs in a landmark of the role navigation, which names
// what they lead through, as in <nav aria-label="Directory">.

import type { ReactElement } from "react";
import { Breadcrumb as AriaBreadcrumb, Breadcrumbs as AriaBreadcrumbs, composeRenderProps } from "react-aria-components";
import { Icon } from "../icon/index.ts";
import { ChevronRight } from "../vendor/index.ts";
import type { BreadcrumbProps as AriaBreadcrumbProps, BreadcrumbsProps } from "react-aria-components";
import { Link } from "./link.tsx";
import type { LinkProps } from "./link.tsx";

export function Breadcrumbs<T extends object>({ className, ...props }: BreadcrumbsProps<T>): ReactElement {
  return <AriaBreadcrumbs {...props} className={`${BREADCRUMBS} ${className ?? ""}`} />;
}

export type BreadcrumbProps = AriaBreadcrumbProps & Omit<LinkProps, "className" | "variant" | "id">;

// The breadcrumb's `id` names it to the breadcrumbs, as the key of an item of
// a collection, and goes on no element, so that two breadcrumbs of one id on
// the page do not share an id there.
export function Breadcrumb({ className, id, ...props }: BreadcrumbProps): ReactElement {
  return (
    <AriaBreadcrumb {...props} id={id} className={composeRenderProps(className, (given) => `${BREADCRUMB} ${given ?? ""}`)}>
      {({ isCurrent }) => (
        <>
          <Link variant="secondary" {...props} />
          {isCurrent ? null : (
            <span className="bk:flex bk:shrink-0 bk:text-muted-foreground">
              <Icon icon={ChevronRight} size={12} />
            </span>
          )}
        </>
      )}
    </AriaBreadcrumb>
  );
}

// The breadcrumbs are a list, which blockr.ui's stylesheet leaves the browser's
// margin, padding and numbers, since it holds no reset of the page.
const BREADCRUMBS = "bk:m-0 bk:flex bk:list-none bk:flex-wrap bk:items-center bk:gap-1 bk:p-0 bk:font-sans bk:text-sm";
const BREADCRUMB = "bk:flex bk:items-center bk:gap-1";
