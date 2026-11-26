import type { ReactElement } from "react";
import type { SearchFieldProps as AriaSearchFieldProps, ValidationResult } from "react-aria-components";
import type { Bound } from "./bound.ts";
export interface SearchFieldProps extends AriaSearchFieldProps {
    readonly label?: string;
    readonly description?: string | null;
    readonly errorMessage?: string | ((validation: ValidationResult) => string);
    readonly placeholder?: string;
    readonly bound?: Bound;
}
export declare function SearchField({ bound, ...props }: SearchFieldProps): ReactElement;
