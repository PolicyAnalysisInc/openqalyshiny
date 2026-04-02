/* OQGrid — Sync Action Handler (full-data dispatch for analysis param grids) */
(function() {
  "use strict";

  var OQGrid = window.OQGrid = window.OQGrid || {};
  OQGrid.actions = OQGrid.actions || {};

  OQGrid.actions.sync = {
    onCellEdited: function(controller, cell) {
      OQGrid.relayout(controller.table);
      this.syncData(controller);
    },

    // Send the full grid data to Shiny
    syncData: function(controller) {
      var spec = controller.spec;
      var data = controller.table.getData();
      var payload = spec.actions && spec.actions.serialize
        ? spec.actions.serialize(data)
        : data;
      OQGrid.shiny.dispatch(controller.inputId, payload);
    }
  };
})();
