import type { ReactElement, ReactNode } from "react";
import type { DateFieldProps as AriaDateFieldProps, DateValue, TimeFieldProps as AriaTimeFieldProps, TimeValue, ValidationResult } from "react-aria-components";
import type { Bound } from "./bound.ts";
export interface DateFieldProps<T extends DateValue> extends AriaDateFieldProps<T> {
    readonly label?: string;
    readonly description?: string | null;
    readonly errorMessage?: string | ((validation: ValidationResult) => string);
    readonly bound?: Bound;
}
export interface TimeFieldProps<T extends TimeValue> extends AriaTimeFieldProps<T> {
    readonly label?: string;
    readonly description?: string | null;
    readonly errorMessage?: string | ((validation: ValidationResult) => string);
    readonly bound?: Bound;
}
export declare function DateField<T extends DateValue>({ bound, ...props }: DateFieldProps<T>): ReactElement;
export declare function TimeField<T extends TimeValue>({ bound, ...props }: TimeFieldProps<T>): ReactElement;
export declare function Segments({ onKey, children }: {
    readonly onKey: ((key: string) => void) | undefined;
    readonly children: ReactNode;
}): ReactElement;
export declare function DateInput({ slot, className }: {
    readonly slot?: "start" | "end";
    readonly className: string;
}): ReactElement;
export declare const INLINE = "bk:flex bk:min-w-0 bk:items-center bk:whitespace-nowrap bk:py-1 bk:font-sans bk:text-sm bk:cursor-text bk:data-disabled:cursor-default";
