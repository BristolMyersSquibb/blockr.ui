import type { ReactElement, ReactNode } from "react";
import type { FieldErrorProps, LabelProps, TextProps } from "react-aria-components";
import type { Bound } from "./bound.ts";
export declare function FormLayout({ children }: {
    children: ReactNode;
}): ReactElement;
export declare function Field({ label, children }: {
    label: string;
    children: ReactNode;
}): ReactElement;
export declare function Label({ className, ...props }: LabelProps): ReactElement;
export declare function Description({ className, ...props }: TextProps): ReactElement;
export declare function FieldError({ className, ...props }: FieldErrorProps): ReactElement;
export declare function NotAvailable({ values, onRemove, }: {
    readonly values: readonly string[];
    readonly onRemove?: () => void;
}): ReactElement | null;
export declare function Refusal({ error }: {
    error: string | null;
}): ReactElement | null;
export declare function useTyped(bound: Bound): [string | null, (text: string | null) => void];
