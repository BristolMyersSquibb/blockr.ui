if (!window.blockrTableInit) {
      window.blockrTableInit = true;
      window.blockrScrollRestore = {};
      window.blockrColumnWidths = {};
      new MutationObserver(function(mutations) {
        mutations.forEach(function(m) {
          var output = m.target.closest('.shiny-html-output');
          if (!output || !output.id) return;
          var key = output.id;
          var wrapper = output.querySelector('.blockr-table-wrapper');
          if (!wrapper) return;
          var table = wrapper.querySelector('.blockr-table');
          if (!table) return;

          // A single sort or page change makes Shiny re-render this output
          // several times in a row (input flush + recalculating placeholder
          // + final result); each replaces the table element and resets the
          // horizontal scroll to 0. The renders can arrive in any order and
          // any of them may reuse an already width-locked table, so we keep
          // re-applying the saved scrollLeft on every render and only release
          // it once the burst goes quiet (no further render for 1.5s). This
          // is also robust to a slow server delaying the final render.
          // Columns and their locked widths are identical before and after a
          // sort/page change, so the absolute scrollLeft is exact.
          var saved = window.blockrScrollRestore[key];
          if (saved && saved.scrollLeft) {
            if (saved.applied && Date.now() - saved.t > 1500) {
              delete window.blockrScrollRestore[key];
            } else {
              void wrapper.scrollWidth; // flush pending layout
              wrapper.scrollLeft = saved.scrollLeft;
              saved.applied = true;
              saved.t = Date.now();
            }
          }

          if (table.dataset.widthsLocked) return;
          var allThs = table.querySelectorAll('thead th');
          if (allThs.length === 0) return;
          var dataThs = table.querySelectorAll('thead th[data-column]');
          var colKey = Array.from(dataThs).map(function(th) {
            return th.dataset.column;
          }).join(',');
          var stored = window.blockrColumnWidths[key];
          if (stored && stored.colKey === colKey) {
            table.style.tableLayout = 'fixed';
            table.style.width = stored.totalWidth + 'px';
            allThs.forEach(function(th, i) {
              th.style.width = stored.widths[i] + 'px';
            });
            table.dataset.widthsLocked = '1';
          } else {
            requestAnimationFrame(function() {
              var widths = Array.from(allThs).map(function(th) {
                return th.offsetWidth;
              });
              window.blockrColumnWidths[key] = {
                colKey: colKey,
                widths: widths,
                totalWidth: table.offsetWidth
              };
            });
          }
        });
      }).observe(document.body, { childList: true, subtree: true });
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
        Shiny.setInputValue(inputId, {col: col, dir: newDir}, {priority: 'event'});
        var pageInputId = container.dataset.pageInput;
        if (pageInputId) {
          Shiny.setInputValue(pageInputId, 1, {priority: 'event'});
        }
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

