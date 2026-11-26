// The switch of blockr.ui: React Aria's SwitchField and SwitchButton,
// drawn as one (https://react-aria.adobe.com/Switch), drawn with the theme variables, after
// the Switch of React Aria's Tailwind starter kit
// (https://github.com/adobe/react-spectrum/blob/react-aria-components%401.22.1/starters/tailwind/src/Switch.tsx).
// It takes what that takes: the props of React Aria's parts, and the
// description and the error message that the starter kit adds.
// It holds its label as its children.
//
// Given `bound`, it shows and edits the boolean that the `bound` gives, as
// blockr.ui's other inputs do (./inputs.tsx).
// It submits the user's pick at once, and says below itself why the submit
// was refused, where it was.

import type { ReactElement, ReactNode } from "react";
import { composeRenderProps, SwitchButton, SwitchField } from "react-aria-components";
import type { SwitchFieldProps, ValidationResult } from "react-aria-components";
import type { Bound } from "./bound.ts";
import { Description, FieldError, Refusal } from "./inputs.tsx";

export interface SwitchProps extends Omit<SwitchFieldProps, "children"> {
  // Its label.
  readonly children?: ReactNode;
  // What the starter kit adds.
  readonly description?: string;
  readonly errorMessage?: string | ((validation: ValidationResult) => string);
  // The value that it shows and submits.
  readonly bound?: Bound;
}

export function Switch({ bound, onChange, ...props }: SwitchProps): ReactElement {
  if (bound === undefined) return <Drawn {...props} onChange={onChange} />;
  return (
    <>
      <Drawn
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

// The track with its handle and the label, the description and the error
// message.
function Drawn({ children, description, errorMessage, className, ...props }: Omit<SwitchProps, "bound">): ReactElement {
  return (
    <SwitchField {...props} className="bk:grid bk:gap-1 bk:font-sans">
      <SwitchButton className={composeRenderProps(className, (given) => `bk:group ${SWITCH} ${given ?? ""}`)}>
        <span className={TRACK}>
          <span className={HANDLE} />
        </span>
        {children}
      </SwitchButton>
      {/* React Aria names the description of the switch only where the
          element is there from the start, so it is there while empty too. */}
      <Description>{description}</Description>
      <FieldError>{errorMessage}</FieldError>
    </SwitchField>
  );
}

// The classes of the switch, of its track and of the handle.
// The track is outlined as a field is, on a quiet fill.
// Where the switch is on, the track takes the brand colour, and the handle
// slides to the right and takes the colour of what sits on the brand colour.
const SWITCH =
  "bk:flex bk:w-fit bk:cursor-pointer bk:items-center bk:gap-2 bk:text-sm bk:data-disabled:cursor-default bk:data-disabled:opacity-50";
const TRACK =
  "bk:box-border bk:flex bk:h-5 bk:w-9 bk:shrink-0 bk:items-center bk:rounded-full bk:border bk:border-input bk:bg-muted bk:px-px bk:outline-offset-2 bk:outline-ring bk:transition-colors bk:group-data-focus-visible:outline-2 bk:group-data-selected:border-primary bk:group-data-selected:bg-primary";
const HANDLE =
  "bk:size-4 bk:rounded-full bk:bg-muted-foreground bk:shadow-sm bk:transition-transform bk:group-data-selected:translate-x-full bk:group-data-selected:bg-primary-foreground";
