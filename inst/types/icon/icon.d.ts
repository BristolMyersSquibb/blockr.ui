import type { ReactElement } from "react";
import type { IconNode } from "lucide";
interface IconProps {
    icon: IconNode;
    size?: number;
    label?: string;
}
export declare function Icon({ icon, size, label }: IconProps): ReactElement;
export declare function iconMarkup(icon: IconNode, size?: number): string;
export {};
