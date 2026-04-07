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
      headerSortClickElement: "icon"
    };

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
  // Cell edited handler — routes to appropriate action handler
  // =========================================================================
  OQGrid.Controller.prototype._setupCellEdited = function() {
    var self = this;
    var spec = this.spec;
    var dispatchMode = spec.dispatchMode || "event";

    this.table.on("cellEdited", function(cell) {
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
