# Skip the empty input messages Shiny sends after deferred inputs

Shiny's input batcher checks whether a send is already queued but, since
1.7.5, never records that one is, so every deferred `setInput` queues
its own send. The first carries all pending inputs and the rest send an
empty update. The server runs a full input cycle for each: it walks
every output of the session to update its hidden state, then flushes.
Mounting a block card sets about a dozen inputs, so a dock view sends
mostly empty messages while its cards mount, and the dock's own messages
queue behind them.

## Usage

``` r
shiny_input_batch_dep()
```

## Value

An
[htmltools::htmlDependency](https://rstudio.github.io/htmltools/reference/htmlDependency.html).

## Details

The script wraps `Shiny.shinyapp.sendInput` to return early on an empty
object. Nothing is lost: the inputs such a send would have carried went
out with the first one. Once Shiny records the queued send itself
(<https://github.com/rstudio/shiny/issues/4436>), an empty batch is left
only where an event-priority input sent the batch while its send was
queued, which is rare enough that the dependency can go.

Attach it once, at the page level, like
[`shiny_has_perf_dep()`](https://bristolmyerssquibb.github.io/blockr.ui/reference/shiny_has_perf_dep.md).

## Examples

``` r
shiny::fluidPage(shiny_input_batch_dep())
#> <div class="container-fluid"></div>
```
