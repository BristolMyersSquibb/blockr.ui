// What blockr.ui offers to drag the items of its Table, GridList and Tree, and
// to drop onto them:
// React Aria's useDragAndDrop and DropIndicator
// (https://react-aria.adobe.com/dnd), the latter drawn with the theme
// variables.
// A caller hands what useDragAndDrop returns to the list as its
// `dragAndDropHooks`.
// The list then shows where a dragged item would land by a DropIndicator: a
// line in the brand colour.
// React Aria's own indicator has no style.
// So useDragAndDrop draws blockr.ui's DropIndicator, unless the caller's
// `renderDropIndicator` draws another.
// Each item that drags starts with a button that drags it, which draws a grip,
// Lucide's GripVertical, as in the starter kit.
// This module gives that button to the Table, the GridList and the Tree.
// A keyboard user drags the item from the button too.

import type { ReactElement } from "react";
import { composeRenderProps, DropIndicator as AriaDropIndicator, useDragAndDrop as useAriaDragAndDrop } from "react-aria-components";
import type { DragAndDropHooks, DragAndDropOptions, DropIndicatorProps } from "react-aria-components";
import { Icon } from "../icon/index.ts";
import { Button } from "../inputs/index.ts";
import { GripVertical } from "../vendor/index.ts";

export type { DragAndDropHooks, DragAndDropOptions, DropIndicatorProps };

export function useDragAndDrop<T = object>(options: DragAndDropOptions<T>): { dragAndDropHooks: DragAndDropHooks<T> } {
  return useAriaDragAndDrop<T>({ renderDropIndicator: (target) => <DropIndicator target={target} />, ...options });
}

export function DropIndicator(props: DropIndicatorProps): ReactElement {
  return <AriaDropIndicator {...props} className={composeRenderProps(props.className, (given) => `${INDICATOR} ${given ?? ""}`)} />;
}

// The button that drags an item.
// React Aria labels it, and hands it its handlers through the slot "drag".
export function DragButton(): ReactElement {
  return (
    <Button slot="drag" variant="quiet">
      <Icon icon={GripVertical} size={12} />
    </Button>
  );
}

// React Aria lays the indicator out between two items, where it takes no
// height, so its outline draws a line there.
const INDICATOR = "bk:outline-none bk:data-drop-target:outline-1 bk:data-drop-target:outline-primary bk:forced-colors:data-drop-target:outline-[Highlight]";
