import type { ReactElement, ReactNode } from "react";
import type { TagGroupProps as AriaTagGroupProps, TagListProps, TagProps as AriaTagProps } from "react-aria-components";
declare const colors: {
    readonly gray: "bk:border-border bk:bg-muted";
    readonly accent: "bk:border-primary bk:bg-muted";
};
type Color = keyof typeof colors;
export interface TagGroupProps<T> extends Omit<AriaTagGroupProps, "children">, Pick<TagListProps<T>, "items" | "children" | "renderEmptyState"> {
    readonly color?: Color;
    readonly label?: string;
    readonly description?: string;
    readonly errorMessage?: string;
}
export declare function TagGroup<T extends object>({ label, description, errorMessage, items, children, renderEmptyState, color, className, ...props }: TagGroupProps<T>): ReactElement;
export interface TagProps extends AriaTagProps {
    readonly color?: Color;
    readonly children?: ReactNode;
}
export declare function Tag({ children, color, className, ...props }: TagProps): ReactElement;
export {};
