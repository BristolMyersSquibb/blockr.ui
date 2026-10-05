/* The input batch script, shiny-input-batch.js: the empty input messages
 * Shiny sends after deferred inputs are dropped, and nothing else is.
 *
 * Shiny is a stand-in that keeps the order a page sees: the script loads
 * before Shiny.shinyapp exists, and connect() creates it and fires
 * shiny:connected, as Shiny does once the socket opens. Its sendInput()
 * reaches $sendMsg() through `this`, like Shiny's. A stub jQuery records the
 * shiny:connected handler.
 */
'use strict';

const assert = require('node:assert');
const fs = require('node:fs');
const path = require('node:path');
const { test } = require('node:test');
const { Window } = require('happy-dom');

const script = fs.readFileSync(
  path.join(__dirname, '..', '..', 'inst', 'assets', 'js', 'shiny-input-batch.js'), 'utf8');

/** A page with the stand-in for Shiny, which records what it sends. */
const page = () => {
  const win = new Window({ url: 'http://localhost/' });
  const sent = [];
  const connected = [];
  win.Shiny = {};
  win.jQuery = () => ({
    on: (type, fn) => { if (type === 'shiny:connected') connected.push(fn); }
  });
  const connect = () => {
    win.Shiny.shinyapp = win.Shiny.shinyapp || {
      $sendMsg(msg) { sent.push(msg); },
      sendInput(values) { this.$sendMsg(JSON.stringify(values)); }
    };
    connected.forEach((fn) => fn());
  };
  return { win, sent, connect, load: () => win.eval(script) };
};

test('once Shiny connects, empty batches are dropped and others sent', () => {
  const { win, sent, connect, load } = page();
  load();
  connect();
  const app = win.Shiny.shinyapp;
  app.sendInput({ a: 1, b: 'x' });
  app.sendInput({});
  app.sendInput({});
  app.sendInput({ c: null });
  assert.deepStrictEqual(sent, ['{"a":1,"b":"x"}', '{"c":null}']);
  win.close();
});

test('a script loaded after Shiny connects patches at once', () => {
  const { win, sent, connect, load } = page();
  connect();
  load();
  win.Shiny.shinyapp.sendInput({});
  win.Shiny.shinyapp.sendInput({ a: 1 });
  assert.deepStrictEqual(sent, ['{"a":1}']);
  win.close();
});

test('the wrapper is applied once', () => {
  const { win, connect, load } = page();
  load();
  connect();
  const first = win.Shiny.shinyapp.sendInput;
  // A second copy of the script, and a reconnect, find the wrapper in place.
  load();
  connect();
  assert.strictEqual(win.Shiny.shinyapp.sendInput, first);
  win.close();
});
