import type { ReactElement } from "react";
import type { TreeItemProps as AriaTreeItemProps, TreeProps } from "react-aria-components";
export declare function Tree<T extends object>(props: TreeProps<T>): ReactElement;
export interface TreeItemProps extends Partial<AriaTreeItemProps> {
    readonly title: string;
}
export declare function TreeItem({ title, children, ...props }: TreeItemProps): ReactElement;
export declare function Chevron({ expanded }: {
    expanded: boolean;
}): ReactElement;
