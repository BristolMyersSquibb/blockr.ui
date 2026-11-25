// The form of blockr.ui: React Aria's Form
// (https://react-aria.adobe.com/Form), drawn with the theme variables, after the Form of
// React Aria's Tailwind starter kit
// (https://github.com/adobe/react-spectrum/blob/react-aria-components%401.22.1/starters/tailwind/src/Form.tsx),
// which draws its fields one below the other.
// It takes the props of React Aria's Form, whose validationBehavior the
// fields in it take: "native", React Aria's default, keeps the form from
// being submitted while a field is invalid, and "aria" marks the field only,
// for a form that checks its fields itself.
// Its validationErrors show below the fields that they name, as errors of a
// server.
// A form takes no `bound`: it holds fields that the caller binds or holds the
// values of itself.

import type { ReactElement } from "react";
import { Form as AriaForm } from "react-aria-components";
import type { FormProps } from "react-aria-components";

export function Form({ className, ...props }: FormProps): ReactElement {
  return <AriaForm {...props} className={`bk:grid bk:gap-3 ${className ?? ""}`} />;
}
