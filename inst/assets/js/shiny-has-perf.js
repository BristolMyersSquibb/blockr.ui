// Strip a redundant `:has(> *)` guard out of Shiny's recalculating-fade rule.
//
// Shiny 1.8.1 styles uiOutput()/conditionalPanel() containers `display:
// contents` once they are non-empty (rstudio/shiny#3957), so their children
// lay out as direct children of the parent. That is the right default. A
// pass-through container generates no box, though, so it can no longer carry
// the `.recalculating` fade, and Shiny pushes the opacity down to the
// children with a companion rule:
//
//   div:where(.shiny-html-output):has(> *).recalculating > * { opacity: ... }
//
// The companion is what costs. Its `:has()` sits in non-subject position with
// a universal subject, and Chrome answers that by restyling the whole
// document on every DOM insertion -- 107ms per insertion on a 40-block dock
// board, linear in element count, paid by every block re-render, every
// keystroke in a picker and every streamed chat token. The `display:
// contents` rule beside it, whose `:has()` is in subject position, measures
// free: deleting it changes nothing.
//
// The guard on the companion is also redundant. A selector of the shape
// `X:has(> *)... > *` picks a descendant of X, so X necessarily has an
// element child wherever the subject exists and the guard never changes the
// match set. Removing it keeps the pass-through layout and keeps the fade,
// and takes the same 107ms to 3ms.
//
// This has to go through the CSSOM. The cost is bound to the selector being
// present in the active index, which Chrome consults when deciding which
// elements to restyle at all -- before the cascade runs. A rule emptied to
// `{ }`, or aimed at a class present nowhere in the document, still costs
// full price. No stylesheet can reach it, including Shiny's documented
// `display: block` escape hatch, which addresses layout and leaves the cost
// untouched.
//
// Runs once the stylesheets are parsed. Same origin, so `cssRules` is
// readable; a sheet that is not (a CDN font, say) is skipped rather than
// throwing.
(function () {
  // Redundant exactly when the subject is a descendant of the guarded
  // element: reaching it proves the element has an element child. Two shapes
  // are therefore excluded. The guard in subject position -- as on the
  // `display: contents` rule -- is load-bearing, because dropping it there
  // would make empty containers pass through and collapse the dock host. And
  // a sibling combinator (`~`, `+`) after the guard walks out of the element
  // entirely, so nothing is proven about its children.
  var REDUNDANT_GUARD = /:has\(> \*\)[^>~+]*>\s*\*\s*$/;

  // Scoped to the two containers Shiny styles, rather than every sheet in
  // the page: the rewrite is provably safe for any selector of that shape,
  // but there is no measured win elsewhere to justify the blast radius.
  function isShinyPassThrough(selector) {
    return selector.includes("shiny-html-output") ||
      selector.includes("shiny-conditional--shown");
  }

  function stripGuard(selector) {
    return selector
      .split(",")
      .map(function (part) {
        var trimmed = part.trim();
        if (!REDUNDANT_GUARD.test(trimmed) || !isShinyPassThrough(trimmed)) {
          return trimmed;
        }
        return trimmed.split(":has(> *)").join("");
      })
      .join(", ");
  }

  function stripGuards() {
    for (const sheet of document.styleSheets) {
      let rules;
      try {
        rules = sheet.cssRules;
      } catch (e) {
        continue;
      }
      for (const rule of rules) {
        const selector = rule.selectorText;
        if (!selector || !selector.includes(":has(> *)")) {
          continue;
        }
        const stripped = stripGuard(selector);
        if (stripped !== selector) {
          rule.selectorText = stripped;
        }
      }
    }
  }

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", stripGuards);
  } else {
    stripGuards();
  }
})();
