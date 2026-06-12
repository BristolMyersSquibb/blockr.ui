// blockr table preview: sort, pagination, horizontal-scroll restore and
// column-width locking.
//
// Loaded once per page via the blockr-table-preview htmlDependency. Scroll
// restore and width locking hook Shiny's own output lifecycle
// ('shiny:value' fires exactly once per render of an output), so no DOM
// observation and no quiet-timer heuristics are needed: a sort or page
// click saves the wrapper's scrollLeft, and the handler below re-applies
// it right after the next render of that output lands.
//
// The window.blockr* stores and init flags are shared with the legacy
// inline copies of this script (pre-migration blockr.extra / blockr.dm):
// if such a copy is on the page too, the click handlers bind only once and
// both regimes read the same state.

window.blockrScrollRestore = window.blockrScrollRestore || {};
window.blockrColumnWidths = window.blockrColumnWidths || {};

if (!window.blockrShinyValueInit) {
  window.blockrShinyValueInit = true;
  $(document).on('shiny:value', function(e) {
    var name = e.name;
    if (!name) return;
    // 'shiny:value' fires just before the DOM swap; run after it.
    requestAnimationFrame(function() {
      var output = document.getElementById(name);
      if (!output) return;
      var wrapper = output.querySelector('.blockr-table-wrapper');
      if (!wrapper) return;
      var table = wrapper.querySelector('.blockr-table');
      if (!table) return;

      // Columns and their locked widths are identical before and after a
      // sort/page change, so the absolute scrollLeft is exact.
      var saved = window.blockrScrollRestore[name];
      if (saved && saved.scrollLeft) {
        void wrapper.scrollWidth; // flush pending layout
        wrapper.scrollLeft = saved.scrollLeft;
        delete window.blockrScrollRestore[name];
      }

      if (table.dataset.widthsLocked) return;
      var allThs = table.querySelectorAll('thead th');
      if (allThs.length === 0) return;
      var dataThs = table.querySelectorAll('thead th[data-column]');
      var colKey = Array.from(dataThs).map(function(th) {
        return th.dataset.column;
      }).join(',');
      var stored = window.blockrColumnWidths[name];
      if (stored && stored.colKey === colKey) {
        // Same columns re-rendered: freeze the measured widths so sorting
        // or paging never reflows the columns.
        table.style.tableLayout = 'fixed';
        table.style.width = stored.totalWidth + 'px';
        allThs.forEach(function(th, i) {
          th.style.width = stored.widths[i] + 'px';
        });
        table.dataset.widthsLocked = '1';
      } else {
        // First sight of these columns: measure now (layout is settled,
        // we run post-render), lock on the next render.
        var widths = Array.from(allThs).map(function(th) {
          return th.offsetWidth;
        });
        window.blockrColumnWidths[name] = {
          colKey: colKey,
          widths: widths,
          totalWidth: table.offsetWidth
        };
      }
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
