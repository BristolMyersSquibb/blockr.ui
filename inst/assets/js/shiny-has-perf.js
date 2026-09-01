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
// elements to restyle at all -- before the cascade runs. A rule stripped of
// every declaration, or aimed at a class present nowhere in the document,
// still costs full price. No stylesheet can reach it, including Shiny's
// documented `display: block` escape hatch, which addresses layout and
// leaves the cost untouched.
//
// Editing a stylesheet we do not own means the shape we match is Shiny's to
// change, so the failure mode that matters is not breaking the page -- it is
// silently doing nothing and handing back the 30x. Three things guard that,
// in order of how early they catch it: a test that fails when Shiny stops
// nesting the fade inside the guard, a read-back after each rewrite, and the
// scan below, which re-checks the invariant we actually care about and warns
// if any universal `:has()` is left in non-subject position. That scan is
// deliberately not keyed on Shiny's class names, so a rename upstream
// surfaces in the console rather than in a profiler.
//
// Runs once the stylesheets are parsed. Same origin, so `cssRules` is
// readable; a sheet that is not (a CDN font, say) is skipped rather than
// throwing.
(function () {
  // Chrome does not normalise the inside of `:has()` -- Shiny's own
  // busy-indicators sheet ships `:has(>*)` while its Bootstrap 5 sheet ships
  // `:has(> *)` -- so match the spacing loosely rather than by literal.
  var REWRITABLE = /:has\(\s*>\s*\*\s*\)[^>~+]*>\s*\*\s*$/;
  var NON_SUBJECT = /:has\(\s*>\s*\*\s*\)[^ >+~]*[ >+~]/;
  var GUARD = /:has\(\s*>\s*\*\s*\)/g;

  // Rewriting is scoped to the two containers Shiny styles even though the
  // transform is provably safe for any selector of that shape: there is no
  // measured win elsewhere to justify editing another library's sheet.
  // Detection below is not scoped, on purpose.
  function isShinyPassThrough(part) {
    return part.includes("shiny-html-output") ||
      part.includes("shiny-conditional--shown");
  }

  function stripGuard(selector) {
    return selector
      .split(",")
      .map(function (part) {
        var trimmed = part.trim();
        if (!REWRITABLE.test(trimmed) || !isShinyPassThrough(trimmed)) {
          return trimmed;
        }
        return trimmed.replace(GUARD, "");
      })
      .join(", ");
  }

  function eachRule(fn) {
    for (const sheet of document.styleSheets) {
      let rules;
      try {
        rules = sheet.cssRules;
      } catch (e) {
        continue;
      }
      for (const rule of rules) {
        if (rule.selectorText) {
          fn(rule);
        }
      }
    }
  }

  function survivingNonSubject() {
    const found = [];
    eachRule(function (rule) {
      for (const part of rule.selectorText.split(",")) {
        if (NON_SUBJECT.test(part.trim())) {
          found.push(part.trim());
          break;
        }
      }
    });
    return found;
  }

  function stripGuards() {
    eachRule(function (rule) {
      const selector = rule.selectorText;
      const stripped = stripGuard(selector);
      if (stripped === selector) {
        return;
      }
      rule.selectorText = stripped;
      // Assignment reparses, and a selector the browser cannot parse is
      // ignored -- so a mangled rewrite is inert rather than harmful, but it
      // is also invisible. Read it back.
      if (rule.selectorText !== stripped) {
        console.warn(
          "[blockr.ui] the browser rejected a selector rewrite, so the " +
            "style-invalidation cost of this rule remains:",
          selector
        );
      }
    });

    const left = survivingNonSubject();
    if (left.length) {
      console.warn(
        "[blockr.ui] a universal `:has()` is still in non-subject position, " +
          "which makes Chrome restyle the whole document on every DOM " +
          "insertion. Either Shiny's stylesheet changed shape and this " +
          "workaround no longer matches it, or another library has grown " +
          "the same pattern. See BristolMyersSquibb/blockr.ui#41.",
        left
      );
    }
  }

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", stripGuards);
  } else {
    stripGuards();
  }
})();
