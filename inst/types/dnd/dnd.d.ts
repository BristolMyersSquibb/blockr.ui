import type { ReactElement } from "react";
import type { DragAndDropHooks, DragAndDropOptions, DropIndicatorProps } from "react-aria-components";
export type { DragAndDropHooks, DragAndDropOptions, DropIndicatorProps };
export declare function useDragAndDrop<T = object>(options: DragAndDropOptions<T>): {
    dragAndDropHooks: DragAndDropHooks<T>;
};
export declare function DropIndicator(props: DropIndicatorProps): ReactElement;
export declare function DragButton(): ReactElement;
