/* OQGrid — CRUD Action Handler (per-cell dispatch for model editor grids) */
(function() {
  "use strict";

  var OQGrid = window.OQGrid = window.OQGrid || {};
  OQGrid.actions = OQGrid.actions || {};

  OQGrid.actions.crud = {
    onCellEdited: function(controller, cell) {
      var field = cell.getField();
      var data = cell.getRow().getData();
      var spec = controller.spec;

      // Skip new rows — they dispatch on confirm, not on edit
      if (data._isNew) {
        OQGrid.relayout(controller.table);
        return;
      }

      // Skip no-op edits
      if (cell.getOldValue() === cell.getValue()) {
        OQGrid.relayout(controller.table);
        return;
      }

      // Handle var__ columns (strategies/groups dual dispatch pattern)
      if (spec.actions.editVariable && field.indexOf("var__") === 0) {
        var payload = spec.actions.editVariable(data, field, cell.getValue(), cell.getOldValue());
        if (payload) {
          OQGrid.shiny.dispatch(controller.inputId, payload);
        }
        OQGrid.relayout(controller.table);
        return;
      }

      // Standard edit dispatch
      if (spec.actions.edit) {
        var payload = spec.actions.edit(data, field, cell.getValue(), cell.getOldValue());
        if (payload) {
          OQGrid.shiny.dispatch(controller.inputId, payload);
        }
      }

      OQGrid.relayout(controller.table);
    }
  };
})();
