// Drop Shiny's `:has(> *)` pass-through rules for uiOutput()/conditionalPanel().
//
// Shiny 1.8.1 (rstudio/shiny#3957, #3960) styles those two containers
// `display: contents` so their children lay out as direct children of the
// parent, which is the right default. The guard it uses to apply that only
// when the container is non-empty is not:
//
//   div:where(.shiny-html-output):has(> *) { display: contents }
//
// `:has(> *)` takes the universal selector as its argument, so any element
// appearing or disappearing anywhere in the document could flip some
// ancestor's match. Chrome cannot narrow that into an invalidation set and
// falls back to restyling the whole document on every DOM mutation. On a
// large board that is tens of milliseconds of style recalc for every block
// re-render, every keystroke in a picker, every streamed chat token.
//
// Deleting the rules is the fix. Shiny's documented escape hatch (override
// `display` on `.shiny-html-output`) does not help: the selector stays in the
// sheet and keeps being tracked. Rewriting it as `:not(:empty)` does not work
// either -- it applies to containers holding only whitespace, which collapses
// the dock host.
//
// Runs once the stylesheets are parsed. Same origin, so `cssRules` is
// readable; a sheet that is not (a CDN font, say) is skipped rather than
// throwing.
(function () {
  function isShinyPassThrough(selector) {
    return selector.includes(":has(> *)") &&
      (selector.includes("shiny-html-output") ||
       selector.includes("shiny-conditional--shown"));
  }

  function dropRules() {
    for (const sheet of document.styleSheets) {
      let rules;
      try {
        rules = sheet.cssRules;
      } catch (e) {
        continue;
      }
      // Backwards, so deleting does not shift the indices still to visit.
      for (let i = rules.length - 1; i >= 0; i--) {
        const selector = rules[i].selectorText;
        if (selector && isShinyPassThrough(selector)) {
          sheet.deleteRule(i);
        }
      }
    }
  }

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", dropRules);
  } else {
    dropRules();
  }
})();
