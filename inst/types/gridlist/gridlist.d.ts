import type { HTMLAttributes, ReactElement } from "react";
import { GridListSection } from "react-aria-components";
import type { GridListItemProps, GridListProps } from "react-aria-components";
export { GridListSection };
export declare function GridList<T extends object>(props: GridListProps<T>): ReactElement;
export declare function GridListItem({ children, ...props }: GridListItemProps): ReactElement;
export declare function GridListHeader({ className, ...props }: HTMLAttributes<HTMLElement>): ReactElement;
