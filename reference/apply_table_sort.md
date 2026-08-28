# Sort a data frame for the table preview

Legacy whole-frame sort kept for direct callers (blockr.dm) during the
transition to
[`table_page()`](https://bristolmyerssquibb.github.io/blockr.ui/reference/table_page.md),
which sorts via a cached order index instead. Same semantics: `asc` /
`desc` put NA last, `na` puts NA first.

## Usage

``` r
apply_table_sort(data, sort_col, sort_dir)
```

## Arguments

- data:

  A data frame.

- sort_col:

  Column name to sort by.

- sort_dir:

  `"asc"`, `"desc"`, `"na"` or `"none"`.

## Value

The sorted data frame.
