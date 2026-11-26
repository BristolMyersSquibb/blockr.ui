import type { ReactElement, ReactNode } from "react";
import type { SwitchFieldProps, ValidationResult } from "react-aria-components";
import type { Bound } from "./bound.ts";
export interface SwitchProps extends Omit<SwitchFieldProps, "children"> {
    readonly children?: ReactNode;
    readonly description?: string;
    readonly errorMessage?: string | ((validation: ValidationResult) => string);
    readonly bound?: Bound;
}
export declare function Switch({ bound, onChange, ...props }: SwitchProps): ReactElement;
