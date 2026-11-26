import type { ReactElement, ReactNode } from "react";
import type { ListBoxItemProps, SelectProps as AriaSelectProps, ValidationResult } from "react-aria-components";
import type { Bound } from "./bound.ts";
import type { ComboBoxSectionProps } from "./combobox.tsx";
type Mode = "single" | "multiple";
export interface SelectProps<T extends object, M extends Mode = "single"> extends Omit<AriaSelectProps<T, M>, "children"> {
    readonly label?: string;
    readonly description?: string | null;
    readonly errorMessage?: string | ((validation: ValidationResult) => string);
    readonly items?: Iterable<T>;
    readonly children: ReactNode | ((item: T) => ReactNode);
    readonly bound?: Bound;
    readonly outdated?: boolean;
    readonly ordered?: boolean;
}
export declare function Select<T extends object, M extends Mode = "single">(props: SelectProps<T, M>): ReactElement;
export declare function SelectItem(props: ListBoxItemProps): ReactElement;
export type SelectSectionProps<T> = ComboBoxSectionProps<T>;
export declare function SelectSection<T extends object>(props: SelectSectionProps<T>): ReactElement;
export {};
