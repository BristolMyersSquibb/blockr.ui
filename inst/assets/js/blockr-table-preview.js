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
