# Runs the shipped script against a stand-in for `Shiny.shinyapp`, so the test
# covers what the browser executes.
input_batch_ctx <- function() {

  testthat::skip_if_not_installed("V8")

  ctx <- V8::v8()

  ctx$eval("
    var sent = [];
    var document = { addEventListener: function () {} };
    var window = {
      Shiny: {
        shinyapp: {
          sendInput: function (values) { sent.push(JSON.stringify(values)); }
        }
      }
    };
  ")

  ctx$source(
    system.file("assets", "js", "shiny-input-batch.js", package = "blockr.ui")
  )

  ctx
}

test_that("empty input batches are dropped, others pass through", {

  ctx <- input_batch_ctx()

  ctx$eval("
    var app = window.Shiny.shinyapp;
    app.sendInput({ a: 1, b: 'x' });
    app.sendInput({});
    app.sendInput({});
    app.sendInput({ c: null });
  ")

  expect_identical(
    ctx$get("sent"),
    c('{"a":1,"b":"x"}', '{"c":null}')
  )
})

test_that("the wrapper is applied once", {

  ctx <- input_batch_ctx()

  ctx$source(
    system.file("assets", "js", "shiny-input-batch.js", package = "blockr.ui")
  )

  ctx$eval("window.Shiny.shinyapp.sendInput({ a: 1 });")

  expect_identical(ctx$get("sent"), '{"a":1}')
})

test_that("shiny_input_batch_dep ships the script it documents", {

  dep <- shiny_input_batch_dep()

  expect_s3_class(dep, "html_dependency")
  expect_identical(dep$script, "js/shiny-input-batch.js")
  expect_true(
    file.exists(
      system.file("assets", dep$script, package = "blockr.ui")
    )
  )
})
