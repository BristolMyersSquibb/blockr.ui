export const setupApp = () => {
  $.busyLoadFull('show', {
    background: '#5e626b',
    spinner: 'circles',
    animation: 'slide'
  });

  Shiny.addCustomMessageHandler('app-ready', (m) => {
    setTimeout(() => {
      $.busyLoadFull('hide');
    }, 1000);
  });

  // Handle dashboard zoom
  Shiny.addCustomMessageHandler('update-dashboard-zoom', (m) => {
    $(m.id).css('zoom', m.zoom);
  });

  Shiny.addCustomMessageHandler('select-scoutbar-page', (m) => {
    let scoutbarChoice;
    switch (m.value) {
      case 'links':
        scoutbarChoice = 0;
        break;
      case 'serialize':
        scoutbarChoice = 1;
        break;
    }
    $('.scoutbar-cell-item')[scoutbarChoice].click();
  })

  // Move block UI to offcanvas
  Shiny.addCustomMessageHandler('hide-block', (m) => {
    $(m.offcanvas).find('.offcanvas-body').append($(m.block_id).find('.card'));
  })

  // Move block UI back to panel
  Shiny.addCustomMessageHandler('show-block', (m) => {
    $(m.panel_id).append($(m.block_id));
  })

  Shiny.addCustomMessageHandler('setup-remove-els-keyboard', (m) => {
    $(document).on('keydown', function (e) {
      if (e.key == m.key && e.ctrlKey) { // Ctrl + Delete
        e.preventDefault();
        let graph = HTMLWidgets.find(`#${m.ns}-network`).getWidget();
        // Selected edge
        let selectedEdge = Shiny.shinyapp.$inputValues[[`${m.ns}-network-selected_edge`]];
        let selectedNodes = Shiny.shinyapp.$inputValues[[`${m.ns}-network-selected_node`]];

        if (selectedEdge !== undefined && selectedEdge !== null) {
          Shiny.setInputValue(`${m.ns}-remove_edge`, selectedEdge);
          Shiny.setInputValue(`${m.ns}-network-selected_edge`, null);
          graph.removeEdgeData([selectedEdge]);
          graph.draw();
        }
        if (selectedNodes !== undefined && selectedNodes !== null) {
          Shiny.setInputValue(`${m.ns}-remove_node`, selectedNodes);
          Shiny.setInputValue(`${m.ns}-network-selected_node`, null);
          // Node is removed in another place.
        }
      }
    });
  })
} 
