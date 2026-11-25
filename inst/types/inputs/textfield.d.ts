import type { ReactElement } from "react";
import type { TextFieldProps as AriaTextFieldProps, ValidationResult } from "react-aria-components";
import type { Bound } from "./bound.ts";
export interface TextFieldProps extends AriaTextFieldProps {
    readonly label?: string;
    readonly description?: string | null;
    readonly errorMessage?: string | ((validation: ValidationResult) => string);
    readonly placeholder?: string;
    readonly lines?: number;
    readonly bound?: Bound;
}
export declare function TextField({ bound, ...props }: TextFieldProps): ReactElement;
