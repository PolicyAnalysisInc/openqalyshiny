/* OQGrid — Grid Controller */
(function() {
  "use strict";

  var OQGrid = window.OQGrid = window.OQGrid || {};

  // =========================================================================
  // Controller constructor
  // Creates a Tabulator grid from a spec object
  // =========================================================================
  OQGrid.Controller = function(containerDiv, spec) {
    var self = this;
    this.containerDiv = containerDiv;
    this.spec = spec;
    this.inputId = containerDiv.dataset.inputId;
    if (!this.inputId) return;

    // Parse data from container's data-* attributes
    this.data = spec.parseData(containerDiv);

    // Build column definitions from spec
    var columnDefs = spec.getColumnDefs(this.data, this);

    // Add delete column if spec supports row removal
    if (spec.actions && spec.actions.remove) {
      columnDefs.push(OQGrid.columns.deleteColumn(this));
    }

    // Build tabulator options
    var tabulatorOpts = {
      index: "_id",
      data: this.data.initial || [],
      columns: columnDefs,
      layout: "fitData",
      layoutColumnsOnNewData: true,
      height: "100%",
      selectableRange: true,
      selectableRangeColumns: true,
      selectableRangeRows: false,
      selectableRangeClearCells: true,
      editTriggerEvent: "dblclick",
      clipboard: true,
      clipboardCopyStyled: false,
      clipboardCopyRowRange: "range",
      clipboardPasteParser: "range",
      clipboardPasteAction: "range",
      clipboardCopyConfig: { columnHeaders: false },
      headerSortClickElement: "icon"
    };

    // EVENT-mode: intercept paste so the grid is NOT modified directly.
    // Build correct edit payloads from original row data and dispatch to R.
    var dispatchMode = spec.dispatchMode || "event";
    if (dispatchMode === "event" && spec.actions && (spec.actions.edit || spec.actions.editVariable)) {
      tabulatorOpts.clipboardPasteAction = function(rowData) {
        // rowData = array of objects keyed by field name (from "range" parser)
        // `this` = Clipboard module; use this.table for the Tabulator instance
        var table = this.table;
        var ranges = table.getRanges();
        if (!ranges || !ranges.length || !rowData || !rowData.length) return;

        var cells2d = ranges[0].getStructuredCells();
        if (!cells2d || !cells2d.length) return;

        var edits = [];
        var numRows = Math.min(cells2d.length, rowData.length);

        for (var r = 0; r < numRows; r++) {
          var rowCells = cells2d[r];
          var clipObj = rowData[r % rowData.length];
          if (!rowCells || !rowCells.length || !clipObj) continue;

          // Original row data is intact — grid hasn't been modified
          var originalRow = rowCells[0].getRow().getData();

          for (var c = 0; c < rowCells.length; c++) {
            var cell = rowCells[c];
            var field = cell.getField();
            if (!field || field.charAt(0) === "_") continue;
            var def = cell.getColumn().getDefinition();
            if (!def.editor) continue;
            if (def.clipboard === false) continue;

            // Parser returns objects keyed by field name
            if (!(field in clipObj)) continue;
            var newVal = clipObj[field];
            if (newVal === undefined || newVal === null) continue;

            var oldVal = cell.getValue();
            if (String(newVal) === String(oldVal)) continue;

            var payload;
            if (spec.actions.editVariable && field.indexOf("var__") === 0) {
              payload = spec.actions.editVariable(originalRow, field, newVal, oldVal);
            } else if (spec.actions.edit) {
              payload = spec.actions.edit(originalRow, field, newVal, oldVal);
            }
            if (payload) edits.push(payload);
          }
        }

        if (edits.length > 0) {
          OQGrid.shiny.dispatch(self.inputId, {
            type: "batch_edit",
            edits: edits
          });
        }
      };
    }

    // Allow spec to override/extend tabulator options
    if (spec.tabulatorOptions) {
      var extra = typeof spec.tabulatorOptions === "function"
        ? spec.tabulatorOptions(this.data)
        : spec.tabulatorOptions;
      var keys = Object.keys(extra);
      for (var i = 0; i < keys.length; i++) {
        tabulatorOpts[keys[i]] = extra[keys[i]];
      }
    }

    // Create Tabulator instance
    this.table = new Tabulator(containerDiv, tabulatorOpts);

    // Setup resize observer for tab visibility
    this._setupResizeObserver();

    // Setup add button (if spec defines addRow and it's not custom)
    if (spec.addRow && !spec.addRow.custom) {
      this._setupAddButton();
    }

    // Setup cell edited handler
    this._setupCellEdited();

    // Setup clipboard paste handler
    this._setupClipboardPaste();

    // Setup keyboard handler (Enter/F2 to edit, Delete to clear)
    OQGrid.keyboard.init(this);

    // Register custom message handlers
    if (spec.messageHandler) {
      spec.messageHandler(this);
    }

    // Post-init hook for custom setup (multi-button, etc.)
    if (spec.onInit) {
      spec.onInit(this);
    }

    containerDiv.setAttribute("data-initialized", "true");
  };

  // =========================================================================
  // Resize observer — redraw when tab becomes visible
  // =========================================================================
  OQGrid.Controller.prototype._setupResizeObserver = function() {
    var self = this;
    if (typeof ResizeObserver !== "undefined") {
      var ro = new ResizeObserver(function(entries) {
        for (var i = 0; i < entries.length; i++) {
          if (entries[i].contentRect.width > 0) {
            OQGrid.relayout(self.table);
            ro.disconnect();
            break;
          }
        }
      });
      ro.observe(this.containerDiv);
    }
  };

  // =========================================================================
  // Add button setup
  // =========================================================================
  OQGrid.Controller.prototype._setupAddButton = function() {
    var self = this;
    var spec = this.spec;
    var addRow = spec.addRow;

    var addBtn = document.createElement("button");
    addBtn.type = "button";
    addBtn.className = "oq-btn oq-btn-sm";
    addBtn.style.marginTop = "8px";
    addBtn.style.marginBottom = "8px";
    addBtn.style.alignSelf = "flex-start";
    addBtn.textContent = addRow.buttonText || "+ Add";

    // Determine where to place the button
    if (addRow.buttonContainer) {
      var target = addRow.buttonContainer(this.containerDiv);
      if (target) {
        var existing = target.querySelectorAll('.oq-btn');
        for (var i = 0; i < existing.length; i++) existing[i].remove();
        target.appendChild(addBtn);
      } else {
        this.containerDiv.parentNode.insertBefore(addBtn, this.containerDiv);
      }
    } else {
      this.containerDiv.parentNode.insertBefore(addBtn, this.containerDiv);
    }

    // Check if adding is possible and disable button if not
    if (addRow.canAdd) {
      addBtn.disabled = !addRow.canAdd(this.data.initial || [], this.data);
    }

    if (addRow.useModal) {
      // Modal pattern: dispatch Shiny event to open modal
      addBtn.addEventListener("click", function() {
        OQGrid.shiny.dispatch(self.inputId, {
          type: addRow.modalAction
        });
      });
    } else {
      // Immediate dispatch: build a complete row with defaults and send to Shiny
      addBtn.addEventListener("click", function() {
        var row = typeof addRow.emptyRow === "function"
          ? addRow.emptyRow(self.data)
          : JSON.parse(JSON.stringify(addRow.emptyRow || {}));

        delete row._isNew;

        // Let spec fill in valid defaults
        if (addRow.generateDefaults) {
          addRow.generateDefaults(row, self.table.getData(), self.data);
        }

        // Dispatch add action immediately
        if (spec.actions && spec.actions.add) {
          var payload = spec.actions.add(row);
          if (payload) {
            OQGrid.shiny.dispatch(self.inputId, payload);
          }
        }
      });
    }
  };

  // =========================================================================
  // Clipboard paste handler — SYNC-mode only.
  // EVENT-mode paste is handled by the custom clipboardPasteAction set in
  // the constructor, which dispatches edit payloads without modifying the grid.
  // =========================================================================
  OQGrid.Controller.prototype._setupClipboardPaste = function() {
    var self = this;
    var spec = this.spec;
    var dispatchMode = spec.dispatchMode || "event";

    // EVENT-mode paste is fully handled by custom clipboardPasteAction
    if (dispatchMode === "event") return;

    // SYNC-mode: sync full grid on paste
    this.table.on("clipboardPasted", function() {
      OQGrid.actions.sync.syncData(self);
    });
  };

  // =========================================================================
  // Cell edited handler — routes to appropriate action handler
  // =========================================================================
  OQGrid.Controller.prototype._setupCellEdited = function() {
    var self = this;
    var spec = this.spec;
    var dispatchMode = spec.dispatchMode || "event";

    this.table.on("cellEdited", function(cell) {
      if (self._batchClearing) return;
      if (dispatchMode === "event") {
        OQGrid.actions.crud.onCellEdited(self, cell);
      } else if (dispatchMode === "sync") {
        OQGrid.actions.sync.onCellEdited(self, cell);
      }

      // Post-edit hook
      if (spec.onCellEdited) {
        spec.onCellEdited(self, cell);
      }
    });
  };
})();
