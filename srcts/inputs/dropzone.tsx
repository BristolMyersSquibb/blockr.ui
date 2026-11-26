// The drop zone of blockr.ui: React Aria's DropZone
// (https://react-aria.adobe.com/DropZone), drawn with the theme variables, after the
// DropZone of React Aria's Tailwind starter kit
// (https://github.com/adobe/react-spectrum/blob/react-aria-components%401.22.1/starters/tailwind/src/DropZone.tsx).
// It takes the props of React Aria's DropZone, whose onDrop hears what the
// user drops on it, such as a file, and holds what it shows, such as a
// FileTrigger with a Button that picks a file the browser's way, and a Text
// in the slot "label", which names the zone.
// It marks itself while the user drags something over it that it takes.
// A drop zone takes no `bound`: its caller sends a file on itself, as a
// block's UI sends one to R through the request of its block class that
// receives it
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.7#the-contract).

import type { ReactElement } from "react";
import { composeRenderProps, DropZone as AriaDropZone } from "react-aria-components";
import type { DropZoneProps } from "react-aria-components";

export function DropZone({ className, ...props }: DropZoneProps): ReactElement {
  return <AriaDropZone {...props} className={composeRenderProps(className, (given) => `${ZONE} ${given ?? ""}`)} />;
}

// The zone is a box with a dashed border, the outline of a field, which takes
// the brand colour and the fill of a hover while the user drags something
// over it that it takes, and the focus ring while it has the focus of the
// keyboard, through which the user pastes a file onto it.
const ZONE =
  "bk:box-border bk:flex bk:min-h-20 bk:flex-col bk:items-center bk:justify-center bk:gap-2 bk:rounded-lg bk:border bk:border-dashed bk:border-input bk:p-4 bk:text-center bk:font-sans bk:text-sm bk:outline-none bk:data-focus-visible:outline-2 bk:data-focus-visible:outline-offset-1 bk:data-focus-visible:outline-ring bk:data-drop-target:border-solid bk:data-drop-target:border-primary bk:data-drop-target:bg-accent bk:data-drop-target:text-accent-foreground bk:data-disabled:opacity-50";
