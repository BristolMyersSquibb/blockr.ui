// blockr table preview: sort, pagination, horizontal-scroll restore and
// column-width locking.
//
// Loaded once per page via the blockr-table-preview htmlDependency. Scroll
// restore and width locking hook Shiny's own output lifecycle
// ('shiny:value' fires exactly once per render of an output), so no DOM
// observation and no quiet-timer heuristics are needed: a sort or page
// click saves the wrapper's scrollLeft, and the handler below re-applies
// it right after the next render of that output lands.

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
      // A render can land while the table is hidden or detached (e.g. the
      // dock parks the card during a relayout, or the view is inactive):
      // offsetWidth is 0 for every cell then, and locking those values
      // would crush all columns to their padding for the rest of the
      // session. Skip entirely; an unlocked table simply auto-lays-out
      // when shown, and the next visible render measures/locks it.
      if (table.offsetWidth === 0) return;
      var allThs = table.querySelectorAll('thead th');
      if (allThs.length === 0) return;
      var dataThs = table.querySelectorAll('thead th[data-column]');
      var colKey = Array.from(dataThs).map(function(th) {
        return th.dataset.column;
      }).join(',');
      var stored = window.blockrColumnWidths[name];
      // A visible rendered th can never legitimately measure 0 (padding
      // alone is wider), so zeros mean the entry was measured while
      // hidden. Drop it and fall through to a fresh measurement.
      if (stored && !(stored.totalWidth > 0 && stored.widths.every(
        function(w) { return w > 0; }
      ))) {
        delete window.blockrColumnWidths[name];
        stored = null;
      }
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
        // Widths measured in the fallback font would ellipsize once the
        // real webfont swaps in; wait for a render that happens after
        // fonts settle (unlocked auto layout is the safe interim state).
        if (document.fonts && document.fonts.status === 'loading') return;
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
