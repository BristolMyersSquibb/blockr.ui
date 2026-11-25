// The classes that blockr.ui's components share: those of a form's layout, a
// field, its label, the box of a text field, the hint and the alert below an
// input, and the looks of a button.
// They are Tailwind CSS utilities under blockr.ui's prefix, `bk`, which
// blockr.ui's stylesheet holds, and they draw with the theme variables alone
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.7.2).
// A box and a button take the text's colour of what holds them, through
// `bk:text-inherit`, since a browser passes its colour on to no field and no
// button, while a frontend gives its page, its cards and its overlays the
// colour of their text.
// The build preset leaves out Tailwind's reset of the page, so a box fills
// its label by stretching in the label's grid, border included, rather than
// by its width.

export const styles = {
  formLayout: "bk:grid bk:gap-3 bk:font-sans",
  field: "bk:grid bk:gap-1 bk:font-sans",
  label: "bk:text-sm bk:font-medium",
  box: "bk:min-w-0 bk:justify-self-stretch bk:rounded-md bk:border bk:border-input bk:bg-transparent bk:px-2 bk:py-1 bk:font-sans bk:text-sm bk:text-inherit bk:placeholder:text-muted-foreground bk:focus:outline-2 bk:focus:outline-offset-1 bk:focus:outline-ring bk:aria-invalid:border-destructive",
  hint: "bk:m-0 bk:text-sm bk:text-muted-foreground",
  alert: "bk:m-0 bk:text-sm bk:text-destructive-text bk:whitespace-pre-wrap",
} as const;

// The looks of a button: "primary", in the brand colour, "secondary", with a
// border, "destructive", in the colour of what removes, for what cannot be
// undone, and "quiet", with no border, which Button takes by its `variant`.
// A browser passes its font and colour on to no button, so each look sets
// them itself.
// A hovered button that draws no fill of its own takes the fill of a hover,
// `accent`, and its text, since muted text reads too faint on it.
export const buttons = {
  primary:
    "bk:inline-flex bk:items-center bk:gap-1 bk:rounded-md bk:border bk:border-primary bk:bg-primary bk:px-2 bk:py-1 bk:font-sans bk:text-sm bk:text-primary-foreground bk:cursor-pointer bk:disabled:cursor-default bk:disabled:opacity-50",
  secondary:
    "bk:inline-flex bk:items-center bk:gap-1 bk:rounded-md bk:border bk:border-border bk:bg-transparent bk:px-2 bk:py-1 bk:font-sans bk:text-sm bk:text-inherit bk:cursor-pointer bk:hover:bg-accent bk:hover:text-accent-foreground bk:disabled:cursor-default bk:disabled:opacity-50",
  destructive:
    "bk:inline-flex bk:items-center bk:gap-1 bk:rounded-md bk:border bk:border-destructive bk:bg-destructive bk:px-2 bk:py-1 bk:font-sans bk:text-sm bk:text-destructive-foreground bk:cursor-pointer bk:hover:bg-destructive/90 bk:disabled:cursor-default bk:disabled:opacity-50",
  quiet:
    "bk:inline-flex bk:items-center bk:gap-1 bk:rounded-md bk:border-0 bk:bg-transparent bk:px-1.5 bk:py-0.5 bk:font-sans bk:text-xs bk:text-muted-foreground bk:cursor-pointer bk:hover:bg-accent bk:hover:text-accent-foreground",
} as const;
