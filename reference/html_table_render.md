# Render a result as the HTML table preview

`html_table_render()` returns a
[`shiny::renderUI()`](https://rdrr.io/pkg/shiny/man/renderUI.html) that
shows `result` as the paginated, sortable table preview.
`html_table_result()` is the block-flavored wrapper that reads the
`page_size` board option first.

## Usage

``` r
html_table_render(result, session, page_size = 5L)

html_table_result(result, block, session)

html_table_display
```

## Arguments

- result:

  A data frame or lazy table (`tbl_lazy`).

- session:

  The Shiny session.

- page_size:

  Rows per page.

- block:

  The block object (used to read board options).

## Value

A [`shiny::renderUI()`](https://rdrr.io/pkg/shiny/man/renderUI.html)
object.

## Details

`html_table_display` is the
[blockr.core::tabular_display](https://bristolmyerssquibb.github.io/blockr.core/reference/tabular-display.html)
that wires this preview into data, parser and transform blocks. Opt in
per app with
`options(blockr.tabular_display = blockr.ui::html_table_display)`; the
display declares and triggers on a single board option, `page_size`,
which the HTML table reads to paginate.

Sort and page state live in the browser and arrive via Shiny inputs
whose ids are derived from the output name (via
[`shiny::getCurrentOutputInfo()`](https://rdrr.io/pkg/shiny/man/getCurrentOutputInfo.html)),
so several previews in one module session stay independent. Sorting and
pagination are resolved by
[`table_page()`](https://bristolmyerssquibb.github.io/blockr.ui/reference/table_page.md):
in-memory frames use a cached order index, lazy dbplyr tables are
counted and paged in the database.
