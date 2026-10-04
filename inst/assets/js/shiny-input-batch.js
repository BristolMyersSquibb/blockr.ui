// Drop the empty input messages Shiny sends after every deferred setInput.
//
// Shiny's InputBatchSender (shiny 1.14.0, srcts/src/inputPolicies/
// inputBatchSender.ts) batches deferred inputs into one message per task. It
// checks `sendIsEnqueued` before enqueueing the send, but never sets it, so
// each deferred setInput enqueues its own send. The first carries every
// pending input; the rest find the batch drained and send
// `{"method":"update","data":{}}`.
//
// The server treats each of those as a full input cycle: manageInputs() walks
// every output of the session to update its hidden state, then a reactive
// flush runs. Mounting a block card sets about a dozen inputs, so a first
// visit to a 15-block view sent 218 messages, 177 of them empty, at 3 to 11ms
// each. They also sit in the websocket queue ahead of the messages that
// matter: the dock's `initialized` report waited 1.9 to 3.7s behind them, and
// nothing on the view evaluates before it is handled.
//
// An empty update carries nothing, so skipping it is behaviour-neutral: the
// batch it would have sent was already sent by the first task. Once Shiny sets
// the flag itself, no empty batch reaches this wrapper and it does nothing.
(function () {
  function patch() {
    var app = window.Shiny && window.Shiny.shinyapp;

    if (!app || typeof app.sendInput !== "function" || app.sendInput.blockrSkipsEmpty) {
      return;
    }

    var sendInput = app.sendInput;

    var wrapped = function (values) {
      if (values && typeof values === "object" && Object.keys(values).length === 0) {
        return;
      }
      return sendInput.apply(this, arguments);
    };

    wrapped.blockrSkipsEmpty = true;
    app.sendInput = wrapped;
  }

  // Shiny creates `shinyapp` when it initialises, in a timeout after document
  // ready, so a script in the page head runs before it exists. It does exist
  // by `shiny:connected`, which Shiny fires before it starts running queued
  // tasks, so patching then catches every deferred send. Patching now covers
  // a script that arrives after connect, through renderUI() for example. The
  // wrapper is applied once either way.
  patch();

  if (window.jQuery) {
    window.jQuery(document).on("shiny:connected", patch);
  }
})();
