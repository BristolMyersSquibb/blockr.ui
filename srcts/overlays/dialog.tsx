// The modal and the dialog of blockr.ui: React Aria's ModalOverlay and
// Modal (https://react-aria.adobe.com/Modal), drawn as one, and React Aria's
// Dialog (https://react-aria.adobe.com/Dialog), drawn with the theme variables, after the
// Modal and the Dialog of React Aria's Tailwind starter kit
// (https://github.com/adobe/react-spectrum/blob/react-aria-components%401.22.1/starters/tailwind/src/Modal.tsx,
// https://github.com/adobe/react-spectrum/blob/react-aria-components%401.22.1/starters/tailwind/src/Dialog.tsx),
// and the dialog that asks, the starter kit's AlertDialog
// (https://github.com/adobe/react-spectrum/blob/react-aria-components%401.22.1/starters/tailwind/src/AlertDialog.tsx).
// Each takes the props of its part of React Aria.
// The modal covers the page with a backdrop and holds the dialog in the middle
// of the window.
// React Aria draws it in a portal at the end of the page's body, keeps the
// focus inside it and makes the rest of the page inert while it is open, as the
// browser's own modal dialog does, so that the user reaches the dialog alone.
// It stacks at the z-index of the overlays, --bk-z-index-overlay
// (./stacking.ts).
// A select or a menu in the dialog opens its list in a portal of its own, above
// the modal, which React Aria leaves within the user's reach.
// Escape closes the modal, as does a press outside the dialog where the modal
// `isDismissable`.
// The dialog's title is React Aria's Heading in the slot "title", which names
// the dialog.
// The modal draws in the colours of an overlay, `popover` and
// `popover-foreground`, with the edge of an outline, `border`.
//
// The dialog that asks goes in the modal, as the dialog does. It is a dialog
// of the role alertdialog, which asks the user to confirm what its `title`
// and its text say, with the starter kit's props: `actionLabel`, the label of
// the button that confirms, which has the focus as the dialog opens,
// `onAction`, which that button runs before it closes the dialog, and
// `cancelLabel`, "Cancel" by default, the label of the button that closes it
// alone. Its `variant` "destructive" draws the button that confirms in the
// colour of what removes, for what cannot be undone, and marks the dialog
// with an exclamation mark in a circle, Lucide's CircleExclamationPoint, in
// the text of that colour, where "info", the default, draws the button in the
// brand colour and marks the dialog with Lucide's Info in the text of that
// colour.

import type { ReactElement, ReactNode } from "react";
import { Button, composeRenderProps, Dialog as AriaDialog, Heading, Modal as AriaModal, ModalOverlay } from "react-aria-components";
import type { DialogProps, ModalOverlayProps } from "react-aria-components";
import { Icon } from "../icon/index.ts";
import { buttons } from "../styles/index.ts";
import { CircleExclamationPoint, Info } from "../vendor/index.ts";

// Inside a ModalOverlay, React Aria's Modal warns of each prop that opens or
// closes the modal, such as isOpen and isDismissable.
// Only the ModalOverlay reads those props.
// So the ModalOverlay takes them, and the Modal takes the rest.
// React Aria's own Modal splits them so too, where it draws a ModalOverlay of
// its own.
// The starter kit's Modal hands every prop to both, and so warns
// (https://github.com/adobe/react-spectrum/issues/10026).
// The modal leaves out the prop UNSTABLE_portalContainer.
// React Aria deprecates that prop, and names a parent UNSAFE_PortalProvider in
// its place.
export function Modal({
  className,
  isDismissable,
  isKeyboardDismissDisabled,
  isOpen,
  defaultOpen,
  onOpenChange,
  shouldCloseOnInteractOutside,
  isEntering,
  isExiting,
  ...props
}: Omit<ModalOverlayProps, "UNSTABLE_portalContainer">): ReactElement {
  return (
    <ModalOverlay
      isDismissable={isDismissable}
      isKeyboardDismissDisabled={isKeyboardDismissDisabled}
      isOpen={isOpen}
      defaultOpen={defaultOpen}
      onOpenChange={onOpenChange}
      shouldCloseOnInteractOutside={shouldCloseOnInteractOutside}
      isEntering={isEntering}
      isExiting={isExiting}
      className={OVERLAY}
    >
      <div className={CENTER}>
        <AriaModal {...props} className={composeRenderProps(className, (given) => `${MODAL} ${given ?? ""}`)} />
      </div>
    </ModalOverlay>
  );
}

export function Dialog({ className, ...props }: DialogProps): ReactElement {
  return <AriaDialog {...props} className={`${DIALOG} ${className ?? ""}`} />;
}

export interface AlertDialogProps extends Omit<DialogProps, "children"> {
  readonly title: string;
  readonly children: ReactNode;
  readonly variant?: "info" | "destructive";
  readonly actionLabel: string;
  readonly cancelLabel?: string;
  readonly onAction?: () => void;
}

export function AlertDialog({
  title,
  variant = "info",
  actionLabel,
  cancelLabel = "Cancel",
  onAction,
  children,
  ...props
}: AlertDialogProps): ReactElement {
  const destructive = variant === "destructive";
  return (
    <Dialog role="alertdialog" {...props}>
      {({ close }) => (
        <>
          <Heading slot="title" className={TITLE}>
            {title}
          </Heading>
          <span className={`bk:absolute bk:top-4 bk:right-4 bk:flex ${destructive ? "bk:text-destructive-text" : "bk:text-primary-text"}`}>
            <Icon icon={destructive ? CircleExclamationPoint : Info} size={20} />
          </span>
          <p className="bk:mt-3 bk:mb-0 bk:font-sans bk:text-sm bk:text-muted-foreground">{children}</p>
          <div className="bk:mt-6 bk:flex bk:justify-end bk:gap-2">
            <Button className={buttons.secondary} onPress={close}>
              {cancelLabel}
            </Button>
            <Button
              className={destructive ? buttons.destructive : buttons.primary}
              autoFocus
              onPress={() => {
                onAction?.();
                close();
              }}
            >
              {actionLabel}
            </Button>
          </div>
        </>
      )}
    </Dialog>
  );
}

// The backdrop covers the whole page, and the box inside it the window, as
// the variables that React Aria sets on the backdrop measure them, so that
// the dialog stays in the middle of the window as the page scrolls.
const OVERLAY = "bk:absolute bk:top-0 bk:left-0 bk:isolate bk:z-overlay bk:h-(--page-height) bk:w-full bk:bg-black/40";
const CENTER =
  "bk:sticky bk:top-0 bk:left-0 bk:box-border bk:flex bk:h-(--visual-viewport-height) bk:w-full bk:items-center bk:justify-center bk:p-4";
const MODAL =
  "bk:box-border bk:max-h-[calc(var(--visual-viewport-height)*.9)] bk:w-full bk:max-w-144 bk:overflow-auto bk:rounded-lg bk:border bk:border-border bk:bg-popover bk:font-sans bk:text-popover-foreground bk:shadow-lg bk:outline-none";
const DIALOG = "bk:relative bk:box-border bk:max-h-[inherit] bk:p-4 bk:outline-none";
const TITLE = "bk:m-0 bk:pr-8 bk:font-sans bk:text-lg bk:font-semibold";
