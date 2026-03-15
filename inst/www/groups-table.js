/* Groups Table — Tabulator (individual edit/remove actions) */
(function() {
  "use strict";
  console.log("[Groups Table] JS version 1.0.0 loaded");

  // =========================================================================
  // Tabulator CDN loader (shared — guards double-load)
  // =========================================================================
  var TABULATOR_CDN = "https://cdn.jsdelivr.net/npm/tabulator-tables@6.3.1/dist/js/tabulator.min.js";
  var TABULATOR_CSS = "https://cdn.jsdelivr.net/npm/tabulator-tables@6.3.1/dist/css/tabulator_bootstrap5.min.css";
  var _tabulatorCallbacks = [];
  var _tabulatorLoading = false;

  function ensureTabulator(callback) {
    if (typeof Tabulator !== "undefined") {
      callback();
      return;
    }
    _tabulatorCallbacks.push(callback);
    if (_tabulatorLoading) return;
    _tabulatorLoading = true;

    var cssLink = document.createElement("link");
    cssLink.rel = "stylesheet";
    cssLink.href = TABULATOR_CSS;
    document.head.appendChild(cssLink);

    var script = document.createElement("script");
    script.src = TABULATOR_CDN;
    script.onload = function() {
      console.log("[Groups Table] Tabulator loaded");
      var cbs = _tabulatorCallbacks.slice();
      _tabulatorCallbacks = [];
      for (var i = 0; i < cbs.length; i++) cbs[i]();
    };
    script.onerror = function() {
      console.error("[Groups Table] Failed to load Tabulator from CDN");
    };
    document.head.appendChild(script);
  }

  // =========================================================================
  // Helpers
  // =========================================================================

  function emdashIfEmpty(cell) {
    var val = cell.getValue();
    if (val === null || val === undefined || val === "") return "\u2014";
    return val;
  }

  // =========================================================================
  // Column definitions
  // =========================================================================

  function buildColumnDefs(inputId) {
    return [
      // Name
      {
        title: "Name",
        field: "name",
        widthGrow: 1,
        minWidth: 120,
        editor: "input",
        formatter: emdashIfEmpty
      },

      // Display Name
      {
        title: "Display Name",
        field: "display_name",
        widthGrow: 1,
        minWidth: 120,
        editor: "input",
        formatter: emdashIfEmpty
      },

      // Description
      {
        title: "Description",
        field: "description",
        widthGrow: 1,
        minWidth: 120,
        editor: "input",
        formatter: emdashIfEmpty
      },

      // Weight
      {
        title: "Weight",
        field: "weight",
        width: 80,
        editor: "input",
        formatter: emdashIfEmpty
      },

      // Enabled
      {
        title: "Enabled",
        field: "enabled",
        width: 80,
        editor: "list",
        editorParams: {
          values: [
            { label: "Yes", value: 1 },
            { label: "No", value: 0 }
          ]
        },
        formatter: function(cell) {
          var val = cell.getValue();
          return (val === 1 || val === "1" || val === true) ? "Yes" : "No";
        }
      },

      // Delete column
      {
        title: "",
        field: "_delete",
        width: 50,
        hozAlign: "center",
        headerSort: false,
        editor: false,
        clipboard: false,
        formatter: function(cell) {
          var data = cell.getRow().getData();

          if (data._isNew) {
            var wrapper = document.createElement("span");

            var confirmBtn = document.createElement("button");
            confirmBtn.type = "button";
            confirmBtn.className = "grp-confirm-btn";
            confirmBtn.textContent = "\u2713";
            confirmBtn.addEventListener("click", function(e) {
              e.stopPropagation();
              var rowData = cell.getRow().getData();
              var name = (rowData.name || "").trim();
              if (!name) {
                alert("Name is required.");
                return;
              }
              cell.getRow().delete();
              if (typeof Shiny !== "undefined") {
                Shiny.setInputValue(inputId, {
                  type: "add_group",
                  name: name,
                  display_name: (rowData.display_name || "").trim(),
                  description: (rowData.description || "").trim(),
                  weight: (rowData.weight || "1").toString().trim(),
                  enabled: rowData.enabled === 0 || rowData.enabled === "0" ? 0 : 1
                }, { priority: "event" });
              }
            });

            var cancelBtn = document.createElement("button");
            cancelBtn.type = "button";
            cancelBtn.className = "grp-cancel-btn";
            cancelBtn.textContent = "\u00d7";
            cancelBtn.addEventListener("click", function(e) {
              e.stopPropagation();
              cell.getRow().delete();
            });

            wrapper.appendChild(confirmBtn);
            wrapper.appendChild(cancelBtn);
            return wrapper;
          }

          var btn = document.createElement("button");
          btn.type = "button";
          btn.className = "grp-delete-btn";
          btn.textContent = "\u00d7";
          btn.addEventListener("click", function(e) {
            e.stopPropagation();
            var data = cell.getRow().getData();
            if (typeof Shiny !== "undefined") {
              Shiny.setInputValue(inputId, {
                type: "remove_group",
                name: data.name
              }, { priority: "event" });
            }
          });
          return btn;
        }
      }
    ];
  }

  // =========================================================================
  // Grid initialization
  // =========================================================================

  var _activeTables = {};
  var _pendingEdit = null;

  function initGrid(containerDiv) {
    var inputId = containerDiv.dataset.inputId;
    if (!inputId) return;

    // Destroy previous table for this inputId if it exists
    if (_activeTables[inputId]) {
      try { _activeTables[inputId].destroy(); } catch (e) {}
      delete _activeTables[inputId];
    }

    var initialData = [];
    try {
      initialData = JSON.parse(containerDiv.dataset.initial || "[]");
    } catch (e) {
      initialData = [];
    }

    var columnDefs = buildColumnDefs(inputId);

    var table = new Tabulator(containerDiv, {
      data: initialData,
      columns: columnDefs,
      layout: "fitColumns",
      height: "auto",
      editTriggerEvent: "dblclick",
      headerSortClickElement: "icon"
    });

    _activeTables[inputId] = table;

    // "Add Group" button above the table
    var addBtn = document.createElement("button");
    addBtn.type = "button";
    addBtn.className = "grp-add-btn";
    addBtn.textContent = "+ Add Group";
    containerDiv.parentNode.insertBefore(addBtn, containerDiv);
    addBtn.addEventListener("click", function() {
      table.addRow(
        { _isNew: true, name: "", display_name: "", description: "", weight: "1", enabled: 1 },
        false
      ).then(function(row) {
        row.getElement().scrollIntoView({ behavior: "smooth", block: "nearest" });
        var nameCell = row.getCell("name");
        if (nameCell) {
          setTimeout(function() { nameCell.edit(); }, 50);
        }
      });
    });

    // Cell edited — fire individual edit_group action
    table.on("cellEdited", function(cell) {
      var field = cell.getField();
      var data = cell.getRow().getData();
      if (data._isNew) return;
      if (cell.getOldValue() === cell.getValue()) return;

      var name = data.name;
      if (field === "name") name = cell.getOldValue() || "";

      // Store pending edit for potential revert
      _pendingEdit = { cell: cell };

      if (typeof Shiny !== "undefined") {
        Shiny.setInputValue(inputId, {
          type: "edit_group",
          name: name,
          field: field,
          value: cell.getValue()
        }, { priority: "event" });
      }
    });

    containerDiv.setAttribute("data-initialized", "true");
  }

  // =========================================================================
  // Error revert handler
  // =========================================================================

  if (typeof Shiny !== "undefined") {
    Shiny.addCustomMessageHandler("groups_table_revert", function(msg) {
      if (_pendingEdit && _pendingEdit.cell) {
        try { _pendingEdit.cell.restoreOldValue(); } catch (e) {}
      }
      _pendingEdit = null;
    });
  }

  // =========================================================================
  // Lifecycle — listen for renderUI re-renders
  // =========================================================================

  function initAllGrids() {
    var containers = document.querySelectorAll(".groups-table-container:not([data-initialized])");
    if (containers.length === 0) return;
    ensureTabulator(function() {
      containers.forEach(initGrid);
    });
  }

  if (typeof Shiny !== "undefined") {
    $(document).on("shiny:connected", function() {
      setTimeout(initAllGrids, 100);
    });

    $(document).on("shiny:value", function() {
      setTimeout(initAllGrids, 100);
    });

    setTimeout(initAllGrids, 100);
  }
})();
