/* OQGrid — Keyboard Handler */
(function() {
  "use strict";

  var OQGrid = window.OQGrid = window.OQGrid || {};

  OQGrid.keyboard = {
    _activeController: null,
    _documentPointerdownAttached: false,

    init: function(controller) {
      var table = controller.table;
      var containerDiv = controller.containerDiv;

      // --- Pointerdown on container: set as active grid ---
      controller._pointerdownHandler = function() {
        OQGrid.keyboard._activeController = controller;
      };
      containerDiv.addEventListener("pointerdown", controller._pointerdownHandler);

      // --- Document-level pointerdown: clear active when clicking outside grids ---
      if (!OQGrid.keyboard._documentPointerdownAttached) {
        OQGrid.keyboard._documentPointerdownAttached = true;
        document.addEventListener("pointerdown", function(e) {
          if (!e.target.closest("[data-input-id]")) {
            OQGrid.keyboard._activeController = null;
          }
        });
      }

      // --- Keydown on table element ---
      controller._keydownHandler = function(e) {
        // Only handle keys for the active grid
        if (OQGrid.keyboard._activeController !== controller) return;

        // Don't interfere with active editors
        if (table.element.querySelector(".tabulator-editing")) return;

        // Don't interfere with input elements, Ace editors, or modals
        if (e.target.tagName === "INPUT" || e.target.tagName === "TEXTAREA" || e.target.tagName === "SELECT") return;
        if (e.target.closest(".ace_editor") || e.target.closest(".modal")) return;

        // --- Enter / F2: start editing ---
        if (e.key === "Enter" || e.key === "F2") {
          var ranges = table.getRanges();
          if (!ranges || !ranges.length) return;
          var cells = ranges[0].getStructuredCells();
          if (!cells || !cells.length || !cells[0] || !cells[0].length) return;
          var cell = cells[0][0];
          var def = cell.getColumn().getDefinition();
          if (!def.editor) return;
          if (typeof def.editable === "function" && !def.editable(cell)) return;
          e.preventDefault();
          cell.edit();
          return;
        }

        // --- Delete / Backspace: clear selected cells ---
        if (e.key === "Delete" || e.key === "Backspace") {
          var ranges = table.getRanges();
          if (!ranges || !ranges.length) return;
          e.preventDefault();
          e.stopPropagation();
          OQGrid.keyboard._clearSelectedCells(controller, ranges);
          return;
        }
      };
      table.element.addEventListener("keydown", controller._keydownHandler);
    },

    destroy: function(controller) {
      if (controller._keydownHandler && controller.table && controller.table.element) {
        controller.table.element.removeEventListener("keydown", controller._keydownHandler);
        controller._keydownHandler = null;
      }
      if (controller._pointerdownHandler && controller.containerDiv) {
        controller.containerDiv.removeEventListener("pointerdown", controller._pointerdownHandler);
        controller._pointerdownHandler = null;
      }
      if (OQGrid.keyboard._activeController === controller) {
        OQGrid.keyboard._activeController = null;
      }
    },

    // --- Clearability check ---
    _isCellClearable: function(cell, spec) {
      var field = cell.getField();

      // Skip _-prefixed internal fields
      if (field.charAt(0) === "_") return false;

      // Skip var__ columns (formula fields, would cause noop dispatch)
      if (field.indexOf("var__") === 0) return false;

      // Skip cells without an editor
      var def = cell.getColumn().getDefinition();
      if (!def.editor) return false;

      // Skip clipboard-excluded columns
      if (def.clipboard === false) return false;

      // Skip conditionally non-editable cells
      if (typeof def.editable === "function" && !def.editable(cell)) return false;

      // Skip _isNew rows (pending confirmation)
      var rowData = cell.getRow().getData();
      if (rowData._isNew) return false;

      // Skip non-clearable fields defined by the spec
      if (spec.nonClearableFields && spec.nonClearableFields.indexOf(field) !== -1) return false;

      return true;
    },

    // --- Clear selected cells with batching ---
    _clearSelectedCells: function(controller, ranges) {
      var spec = controller.spec;
      var table = controller.table;

      // 1. SNAPSHOT PHASE: collect clearable cells with pre-clear data
      var changes = [];
      for (var ri = 0; ri < ranges.length; ri++) {
        var cells2d = ranges[ri].getStructuredCells();
        for (var r = 0; r < cells2d.length; r++) {
          for (var c = 0; c < cells2d[r].length; c++) {
            var cell = cells2d[r][c];
            if (!cell) continue;
            if (!OQGrid.keyboard._isCellClearable(cell, spec)) continue;
            var oldValue = cell.getValue();
            if (oldValue === "") continue;
            changes.push({
              cell: cell,
              field: cell.getField(),
              oldValue: oldValue,
              rowSnapshot: JSON.parse(JSON.stringify(cell.getRow().getData()))
            });
          }
        }
      }

      if (changes.length === 0) return;

      // 2. CLEAR PHASE: batch all setValue calls
      controller._batchClearing = true;
      table.blockRedraw();
      for (var i = 0; i < changes.length; i++) {
        changes[i].cell.setValue("");
      }
      table.restoreRedraw();
      controller._batchClearing = false;

      // 3. DISPATCH PHASE
      var dispatchMode = spec.dispatchMode || "event";

      if (dispatchMode === "sync") {
        // Pure SYNC (threshold): send full grid state
        // Hybrid SYNC (DSA, PSA, scenario, TWSA): all fields are non-clearable,
        // so changes array will be empty and we never reach here.
        // But as a safety net, call syncData if we do.
        OQGrid.actions.sync.syncData(controller);
      } else {
        // EVENT mode: dispatch individual edit payloads using snapshots
        for (var i = 0; i < changes.length; i++) {
          var ch = changes[i];
          if (spec.actions && spec.actions.edit) {
            var payload = spec.actions.edit(ch.rowSnapshot, ch.field, "", ch.oldValue);
            if (payload) {
              OQGrid.shiny.dispatch(controller.inputId, payload);
            }
          }
        }
      }

      // 4. Relayout
      OQGrid.relayout(table);
    }
  };
})();
