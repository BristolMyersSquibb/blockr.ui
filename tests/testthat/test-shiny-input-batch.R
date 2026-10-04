# Runs the shipped script against a stand-in for Shiny, so the test covers
# what the browser executes. The stand-in keeps the order a page sees: the
# script loads before `Shiny.shinyapp` exists, and `connect()` creates it and
# fires `shiny:connected`, as Shiny does once the socket opens. Its
# `sendInput()` reaches `$sendMsg()` through `this`, like Shiny's.
input_batch_ctx <- function() {

  testthat::skip_if_not_installed("V8")

  ctx <- V8::v8()

  ctx$eval("
    var sent = [];
    var connected = [];
    var document = {};
    var window = {
      Shiny: {},
      jQuery: function () {
        return {
          on: function (type, fn) {
            if (type === 'shiny:connected') connected.push(fn);
          }
        };
      }
    };
    function connect() {
      window.Shiny.shinyapp = window.Shiny.shinyapp || {
        $sendMsg: function (msg) { sent.push(msg); },
        sendInput: function (values) { this.$sendMsg(JSON.stringify(values)); }
      };
      connected.forEach(function (fn) { fn(); });
    }
  ")

  ctx
}

load_input_batch <- function(ctx) {
  ctx$source(
    system.file("assets", "js", "shiny-input-batch.js", package = "blockr.ui")
  )
}

test_that("once Shiny connects, empty batches are dropped, others sent", {

  ctx <- input_batch_ctx()
  load_input_batch(ctx)

  ctx$eval("
    connect();
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

test_that("a script loaded after Shiny connects patches at once", {

  ctx <- input_batch_ctx()
  ctx$eval("connect();")
  load_input_batch(ctx)

  ctx$eval("
    window.Shiny.shinyapp.sendInput({});
    window.Shiny.shinyapp.sendInput({ a: 1 });
  ")

  expect_identical(ctx$get("sent"), '{"a":1}')
})

test_that("the wrapper is applied once", {

  ctx <- input_batch_ctx()
  load_input_batch(ctx)
  ctx$eval("connect(); var first = window.Shiny.shinyapp.sendInput;")

  # A second copy of the script, and a reconnect, find the wrapper in place.
  load_input_batch(ctx)
  ctx$eval("connect();")

  expect_true(ctx$get("window.Shiny.shinyapp.sendInput === first"))
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
