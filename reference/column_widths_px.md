# Estimate fixed-layout column widths

Per-column width in px, from the header parts and the formatted cell
strings, for tables rendered with `table-layout: fixed` (the blockr
table preview and blockr.viz's html tables). Computing widths
server-side keeps the layout independent of measuring the DOM - the
measure-and-lock approach reads 0 whenever a render lands while a dock
panel is hidden or mid-relayout, crushing the columns.

## Usage

``` r
column_widths_px(
  col_names,
  col_labels = character(length(col_names)),
  col_types = character(length(col_names)),
  formatted = rep(list(character(0)), length(col_names)),
  col_na = lapply(formatted, is.na),
  wrap_names = FALSE
)
```

## Arguments

- col_names:

  Character vector of column header texts (plain text, strip any HTML
  before calling).

- col_labels:

  Optional secondary header line per column (the preview's ADaM labels);
  `""` for none.

- col_types:

  Optional type-tag line per column (`"<dbl>"` etc.); `""` for none.

- formatted:

  List of character vectors: the display strings of each column's cells
  (only the rows being rendered).

- col_na:

  List of logical vectors flagging cells rendered as the `NA` marker
  instead of their display string.

- wrap_names:

  Measure each header by its longest *word* rather than its full length.
  A header is free to wrap (nothing pins it to one line), it simply
  never has to when the estimate buys it a full line - so a long title
  sets the column width outright and the table sprawls. Measuring the
  longest word instead gives the narrowest column that still never
  breaks a word, and the header wraps into it by itself. `FALSE` (the
  default) keeps the historical one-line estimate.

## Value

Integer vector of pixel widths, one per column, including the two 16px
cell paddings, clamped to \[60, 320\].

## Details

Deliberately px, not font-relative units: `ch` tracks the width of the
digit 0, which in some font stacks runs 40% narrower than average text,
so ch-sized columns clip hard exactly where fonts differ. 8px/char at
the 14px table font (the constant the header min-width heuristic used
for years) over-estimates almost every UI font, so the estimate degrades
to slightly roomy columns - or, at worst, mild ellipsis with the title
tooltip - never to crushed ones.
