// blockr table preview: sort, pagination and horizontal-scroll restore.
//
// Loaded once per page via the blockr-table-preview htmlDependency. Column
// widths are computed server-side (see build_html_table) and rendered as
// table-layout: fixed from the first paint, so this script never measures
// or mutates layout - it only wires clicks and restores scroll position.
// Scroll restore hooks Shiny's own output lifecycle ('shiny:value' fires
// exactly once per render of an output): a sort or page click saves the
// wrapper's scrollLeft, and the handler below re-applies it right after
// the next render of that output lands. Columns and widths are identical
// before and after a sort/page change (widths are memoized per result on
// the server), so the absolute scrollLeft is exact.

window.blockrScrollRestore = window.blockrScrollRestore || {};

if (!window.blockrShinyValueInit) {
  window.blockrShinyValueInit = true;
  $(document).on('shiny:value', function(e) {
    var name = e.name;
    if (!name) return;
    var saved = window.blockrScrollRestore[name];
    if (!saved || !saved.scrollLeft) return;
    // 'shiny:value' fires just before the DOM swap; run after it.
    requestAnimationFrame(function() {
      var output = document.getElementById(name);
      if (!output) return;
      var wrapper = output.querySelector('.blockr-table-wrapper');
      if (!wrapper) return;
      void wrapper.scrollWidth; // flush pending layout
      wrapper.scrollLeft = saved.scrollLeft;
      delete window.blockrScrollRestore[name];
    });
  });
}

if (!window.blockrSortInit) {
  window.blockrSortInit = true;
  document.addEventListener('click', function(e) {
    if (e.target.closest('.blockr-col-name')) return;
    var header = e.target.closest('.blockr-sortable');
    if (!header) return;
    // Where the sort click happened, so the redrawn header can find out
    // whether the pointer is still on it (see blockrHoverTip).
    window.blockrLastSortPoint = {x: e.clientX, y: e.clientY, t: Date.now()};
    e.preventDefault();
    e.stopPropagation();
    var container = header.closest('.blockr-table-container');
    var inputId = container ? container.dataset.sortInput : null;
    if (!inputId) return;
    var col = header.dataset.column;
    var wrapper = container.querySelector('.blockr-table-wrapper');
    var output = container.closest('.shiny-html-output');
    if (wrapper && output) {
      window.blockrScrollRestore[output.id] = {
        scrollLeft: wrapper.scrollLeft,
        t: Date.now()
      };
    }
    var currentDir = header.classList.contains('blockr-sort-asc') ? 'asc' :
                     header.classList.contains('blockr-sort-desc') ? 'desc' :
                     header.classList.contains('blockr-sort-na') ? 'na' : 'none';
    var newDir = currentDir === 'none' ? 'asc' :
                 currentDir === 'asc' ? 'desc' :
                 currentDir === 'desc' ? 'na' : 'none';
    // NB: no page-reset input here. The server resets to page 1 when the
    // sort state changes; a second setInputValue would trigger a second
    // render of the same output (and break restore-once scroll handling).
    Shiny.setInputValue(inputId, {col: col, dir: newDir}, {priority: 'event'});
  });
}

if (!window.blockrPaginationInit) {
  window.blockrPaginationInit = true;
  document.addEventListener('click', function(e) {
    var btn = e.target.closest('.blockr-nav-btn');
    if (!btn || btn.classList.contains('disabled')) return;
    e.preventDefault();
    e.stopPropagation();
    var container = btn.closest('.blockr-table-container');
    var inputId = container ? container.dataset.pageInput : null;
    if (!inputId) return;
    var wrapper = container.querySelector('.blockr-table-wrapper');
    var output = container.closest('.shiny-html-output');
    if (wrapper && output) {
      window.blockrScrollRestore[output.id] = {
        scrollLeft: wrapper.scrollLeft,
        t: Date.now()
      };
    }
    var currentPage = parseInt(container.dataset.currentPage) || 1;
    var maxPage = parseInt(container.dataset.maxPage) || 1;
    var direction = btn.dataset.direction;
    var newPage = direction === 'prev' ? Math.max(1, currentPage - 1) :
                  Math.min(maxPage, currentPage + 1);
    Shiny.setInputValue(inputId, newPage, {priority: 'event'});
  });
}

// The sorted header's tooltip (data-sort-tip, set by build_html_table()),
// and the cut-off-only tooltips of cells and labels (data-blockr-tooltip),
// registered with the shared light tooltip whenever a table lands.
if (!window.blockrSortTipInit) {
  window.blockrSortTipInit = true;
  var blockrSortTips = function(node) {
    if (!window.Blockr || !Blockr.tooltip || !node || node.nodeType !== 1) return;
    var cut = node.querySelectorAll('.blockr-table [data-blockr-tooltip-overflow]');
    for (var k = 0; k < cut.length; k++) {
      Blockr.tooltip.set(cut[k], cut[k].getAttribute('data-blockr-tooltip'), {overflow: true});
    }
    var ths = node.matches('th[data-sort-tip]') ? [node] :
      node.querySelectorAll('th[data-sort-tip]');
    for (var i = 0; i < ths.length; i++) {
      Blockr.tooltip.set(ths[i], ths[i].getAttribute('data-sort-tip'));
      // A sort click redraws the table under a pointer that has not moved,
      // so no pointerover follows; when the pointer is already on the
      // sorted header, hand the tooltip the event it waits for.
      blockrHoverTip(ths[i]);
    }
  };
  // The browser's :hover is not a test here: Safari does not update it for
  // a node drawn under a pointer that has not moved. What is under the point
  // of the last sort click is.
  // It waits for the scroll restore above: setting the wrapper's scrollLeft
  // fires a scroll event, and the tooltip closes on scroll.
  var blockrHoverTip = function(th) {
    setTimeout(function() {
      var pt = window.blockrLastSortPoint;
      if (!th.isConnected || !pt || Date.now() - pt.t > 10000) return;
      var under = document.elementFromPoint(pt.x, pt.y);
      if (under && th.contains(under)) {
        th.dispatchEvent(new PointerEvent('pointerover', {bubbles: true}));
      }
    }, 150);
  };
  new MutationObserver(function(muts) {
    muts.forEach(function(m) { m.addedNodes.forEach(blockrSortTips); });
  }).observe(document.documentElement, {childList: true, subtree: true});
  blockrSortTips(document.documentElement);
}
