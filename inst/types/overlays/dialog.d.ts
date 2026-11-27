import type { ReactElement, ReactNode } from "react";
import type { DialogProps, ModalOverlayProps } from "react-aria-components";
export declare function Modal({ className, isDismissable, isKeyboardDismissDisabled, isOpen, defaultOpen, onOpenChange, shouldCloseOnInteractOutside, isEntering, isExiting, ...props }: Omit<ModalOverlayProps, "UNSTABLE_portalContainer">): ReactElement;
export declare function Dialog({ className, ...props }: DialogProps): ReactElement;
export interface AlertDialogProps extends Omit<DialogProps, "children"> {
    readonly title: string;
    readonly children: ReactNode;
    readonly variant?: "info" | "destructive";
    readonly actionLabel: string;
    readonly cancelLabel?: string;
    readonly onAction?: () => void;
}
export declare function AlertDialog({ title, variant, actionLabel, cancelLabel, onAction, children, ...props }: AlertDialogProps): ReactElement;
