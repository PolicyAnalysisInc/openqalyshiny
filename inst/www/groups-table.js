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

  function relayout(table) {
    var holder = table.element.querySelector(".tabulator-tableholder");
    var scrollLeft = holder ? holder.scrollLeft : 0;
    var scrollTop = holder ? holder.scrollTop : 0;

    table.redraw(true);

    requestAnimationFrame(function() {
      var containerWidth = table.element.clientWidth;
      var totalWidth = 0;
      var growCols = [];
      var totalGrow = 0;

      table.getColumns().forEach(function(col) {
        var def = col.getDefinition();
        var w = col.getWidth();
        totalWidth += w;
        if (def.widthGrow && def.widthGrow > 0) {
          growCols.push({ col: col, grow: def.widthGrow, width: w });
          totalGrow += def.widthGrow;
        }
      });

      if (totalWidth < containerWidth && growCols.length > 0) {
        var extra = containerWidth - totalWidth;
        growCols.forEach(function(c) {
          c.col.setWidth(c.width + Math.floor(extra * c.grow / totalGrow));
        });
      }

      holder = table.element.querySelector(".tabulator-tableholder");
      if (holder) {
        holder.scrollLeft = scrollLeft;
        holder.scrollTop = scrollTop;
      }
    });
  }

  function emdashIfEmpty(cell) {
    var val = cell.getValue();
    if (val === null || val === undefined || val === "") return "\u2014";
    return val;
  }

  // =========================================================================
  // Custom formula editor (copied from strategies-table.js)
  // =========================================================================

  function formulaEditor(terms, suggestions) {
    return function(cell, onRendered, success, cancel) {
      var placeholder = document.createElement("div");
      placeholder.className = "var-formula-placeholder";
      placeholder.addEventListener("focusout", function(e) { e.stopPropagation(); });

      var currentValue = cell.getValue() || "";
      var committed = false;
      var overlay = null;
      var aceEditor = null;
      var onDocMouseDown = null;
      var cellFocusRedirect = null;
      var editCellEl = null;

      var cellEl = cell.getElement();
      var cellRect = cellEl.getBoundingClientRect();

      function commit(val) {
        if (committed) return;
        committed = true;
        cleanup();
        try { success(val); } catch (e) {}
      }
      function doCancel() {
        if (committed) return;
        committed = true;
        cleanup();
        try { cancel(); } catch (e) {}
      }
      function cleanup() {
        if (onDocMouseDown) {
          document.removeEventListener("mousedown", onDocMouseDown, true);
          onDocMouseDown = null;
        }
        if (cellFocusRedirect && editCellEl) {
          editCellEl.removeEventListener("focus", cellFocusRedirect, true);
          cellFocusRedirect = null;
        }
        if (aceEditor) {
          try { aceEditor.destroy(); } catch (e) {}
          aceEditor = null;
        }
        if (overlay && overlay.parentNode) {
          overlay.parentNode.removeChild(overlay);
          overlay = null;
        }
      }

      onRendered(function() {
        if (typeof ace === "undefined") {
          var input = document.createElement("input");
          input.type = "text";
          input.className = "var-input-editor";
          input.value = currentValue;
          input.addEventListener("keydown", function(e) {
            if (e.key === "Enter") { commit(input.value); e.preventDefault(); }
            if (e.key === "Escape") { doCancel(); e.preventDefault(); }
          });
          input.addEventListener("blur", function() { commit(input.value); });
          placeholder.appendChild(input);
          input.focus();
          input.select();
          return;
        }

        var lineH = 18;
        var innerH = cellRect.height - 4;
        var vPad = Math.max(0, Math.round((innerH - lineH) / 2));

        overlay = document.createElement("div");
        overlay.className = "var-formula-overlay";
        overlay.style.position = "fixed";
        overlay.style.left = cellRect.left + "px";
        overlay.style.top = cellRect.top + "px";
        overlay.style.width = cellRect.width + "px";
        overlay.style.height = cellRect.height + "px";
        overlay.style.zIndex = "10000";

        var aceContainer = document.createElement("div");
        aceContainer.className = "var-ace-container";
        aceContainer.style.position = "absolute";
        aceContainer.style.left = "0";
        aceContainer.style.right = "0";
        aceContainer.style.top = vPad + "px";
        aceContainer.style.bottom = "0";
        overlay.appendChild(aceContainer);
        document.body.appendChild(overlay);

        onDocMouseDown = function(e) {
          if (!overlay || committed) {
            document.removeEventListener("mousedown", onDocMouseDown, true);
            return;
          }
          var isAce = overlay.contains(e.target);
          var isAutocomplete = false;
          try { isAutocomplete = e.target.closest && e.target.closest(".ace_autocomplete"); } catch (x) {}
          if (!isAce && !isAutocomplete) {
            var val = currentValue;
            try { if (aceEditor) val = aceEditor.getValue(); } catch (x) {}
            commit(val);
          }
        };
        document.addEventListener("mousedown", onDocMouseDown, true);

        ["mousedown", "pointerdown", "click", "mouseup", "pointerup", "focusin"].forEach(function(evt) {
          overlay.addEventListener(evt, function(e) { e.stopPropagation(); });
        });

        ace.require("ace/ext/language_tools");
        aceEditor = ace.edit(aceContainer);
        aceEditor.setTheme("ace/theme/chrome");
        aceEditor.session.setMode("ace/mode/r");
        aceEditor.setOptions({
          showGutter: false,
          showPrintMargin: false,
          highlightActiveLine: false,
          showFoldWidgets: false,
          displayIndentGuides: false,
          scrollPastEnd: 0,
          useSoftTabs: true,
          tabSize: 2,
          enableBasicAutocompletion: true,
          enableLiveAutocompletion: true,
          enableSnippets: false
        });
        aceEditor.setValue(currentValue, -1);

        try {
          if (terms && typeof FormulaInputMode !== "undefined") {
            FormulaInputMode.injectDefaultStyles();
            var hl = new FormulaInputMode.FormulaHighlighter(aceEditor);
            hl.setTerms(terms);
          }
          if (suggestions && typeof FormulaInputAutocomplete !== "undefined") {
            var cmp = new FormulaInputAutocomplete.FormulaCompleter(aceEditor, suggestions);
            aceEditor.completers = [cmp];
          }
        } catch (e) {
          console.warn("[Groups Table] Term highlighting/autocomplete init failed:", e.message);
        }

        overlay.addEventListener("keydown", function(e) {
          if (committed || !aceEditor) return;
          if (e.key === "Enter") {
            e.preventDefault();
            e.stopPropagation();
            if (aceEditor.completer && aceEditor.completer.popup &&
                aceEditor.completer.popup.isOpen) {
              aceEditor.completer.detach();
            }
            commit(aceEditor.getValue());
          } else if (e.key === "Escape") {
            e.preventDefault();
            e.stopPropagation();
            if (aceEditor.completer && aceEditor.completer.popup &&
                aceEditor.completer.popup.isOpen) {
              aceEditor.completer.detach();
              return;
            }
            doCancel();
          }
        }, true);

        aceEditor.commands.addCommand({
          name: "acceptCompletionAndCommit",
          bindKey: { win: "Tab", mac: "Tab" },
          exec: function(ed) {
            if (ed.completer && ed.completer.popup &&
                ed.completer.popup.isOpen) {
              ed.completer.insertMatch();
            }
            commit(ed.getValue());
            setTimeout(function() { cell.navigateNext(); }, 0);
            return true;
          }
        });

        aceEditor.container.addEventListener("paste", function(e) {
          e.preventDefault();
          e.stopPropagation();
          var text = (e.clipboardData || window.clipboardData).getData("text");
          aceEditor.insert(text.replace(/[\r\n]+/g, " "));
        }, true);

        aceEditor.on("blur", function() {
          setTimeout(function() {
            if (!committed && aceEditor && !aceEditor.isFocused()) {
              commit(aceEditor.getValue());
            }
          }, 300);
        });

        aceEditor.resize();

        editCellEl = cellEl;
        var savedTabindex = cellEl.getAttribute("tabindex");
        cellEl.setAttribute("tabindex", "-1");
        cellEl.style.pointerEvents = "none";
        cellFocusRedirect = function() {
          if (committed || !aceEditor) return;
          aceEditor.focus();
        };
        cellEl.addEventListener("focus", cellFocusRedirect, true);

        var origCleanup = cleanup;
        cleanup = function() {
          if (editCellEl) {
            editCellEl.removeEventListener("focus", cellFocusRedirect, true);
            if (savedTabindex !== null) {
              editCellEl.setAttribute("tabindex", savedTabindex);
            } else {
              editCellEl.removeAttribute("tabindex");
            }
            editCellEl.style.pointerEvents = "";
          }
          origCleanup();
        };

        cellEl.blur();
        aceEditor.focus();
        var focusTimer = setInterval(function() {
          if (committed || !aceEditor) { clearInterval(focusTimer); return; }
          aceEditor.focus();
        }, 30);
        setTimeout(function() { clearInterval(focusTimer); }, 300);
      });

      return placeholder;
    };
  }

  // =========================================================================
  // Column definitions
  // =========================================================================

  function buildColumnDefs(inputId, varColumns, terms, suggestions) {
    var cols = [
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
        minWidth: 80,
        editor: "input",
        formatter: emdashIfEmpty
      },

      // Enabled
      {
        title: "Enabled",
        field: "enabled",
        minWidth: 120,
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
      }
    ];

    // Add one column per group-specific variable
    if (varColumns && varColumns.length > 0) {
      var fEditor = formulaEditor(terms, suggestions);
      for (var i = 0; i < varColumns.length; i++) {
        cols.push({
          title: varColumns[i].display_name || varColumns[i].name,
          field: "var__" + varColumns[i].name,
          widthGrow: 1,
          minWidth: 450,
          editor: fEditor,
          formatter: emdashIfEmpty
        });
      }
    }

    // Delete column
    cols.push({
      title: "",
      field: "_delete",
      width: 50,
      widthGrow: 0,
      hozAlign: "center",
      headerSort: false,
      editor: false,
      clipboard: false,
      formatter: function(cell) {
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
    });

    return cols;
  }

  // =========================================================================
  // Grid initialization
  // =========================================================================

  var _activeTables = {};
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

    var varColumns = [];
    try { varColumns = JSON.parse(containerDiv.dataset.varColumns || "[]"); } catch (e) {}
    var terms = null;
    try { terms = JSON.parse(containerDiv.dataset.terms || "null"); } catch (e) {}
    var suggestions = null;
    try { suggestions = JSON.parse(containerDiv.dataset.suggestions || "null"); } catch (e) {}

    var columnDefs = buildColumnDefs(inputId, varColumns, terms, suggestions);

    var table = new Tabulator(containerDiv, {
      index: "_id",
      data: initialData,
      columns: columnDefs,
      layout: "fitData",
      layoutColumnsOnNewData: true,
      height: "100%",
      selectableRange: true,
      selectableRangeColumns: true,
      selectableRangeRows: true,
      selectableRangeClearCells: true,
      editTriggerEvent: "dblclick",
      headerSortClickElement: "icon"
    });

    // Fix: redraw when tab becomes visible (container goes from 0 to non-zero width)
    if (typeof ResizeObserver !== "undefined") {
      var ro = new ResizeObserver(function(entries) {
        for (var i = 0; i < entries.length; i++) {
          if (entries[i].contentRect.width > 0) {
            relayout(table);
            ro.disconnect();
            break;
          }
        }
      });
      ro.observe(containerDiv);
    }

    _activeTables[inputId] = table;

    // "Add Group" button above the table
    var addBtn = document.createElement("button");
    addBtn.type = "button";
    addBtn.className = "grp-add-btn";
    addBtn.textContent = "+ Add Group";
    containerDiv.parentNode.insertBefore(addBtn, containerDiv);
    addBtn.addEventListener("click", function() {
      if (typeof Shiny !== "undefined") {
        Shiny.setInputValue(inputId, {
          type: "show_add_group_modal"
        }, { priority: "event" });
      }
    });

    // Cell edited — handle variable edits vs group edits
    table.on("cellEdited", function(cell) {
      var field = cell.getField();
      var data = cell.getRow().getData();
      if (cell.getOldValue() === cell.getValue()) {
        relayout(table);
        return;
      }

      if (field.indexOf("var__") === 0) {
        // Variable formula edit
        var varName = field.substring(5);
        var groupName = data.name;
        var oldValue = cell.getOldValue() || "";

        if (typeof Shiny !== "undefined") {
          if (!oldValue) {
            // No existing entry — add_variable
            Shiny.setInputValue(inputId, {
              type: "add_variable",
              name: varName,
              group: groupName,
              formula: cell.getValue()
            }, { priority: "event" });
          } else {
            // Existing entry — edit_variable
            Shiny.setInputValue(inputId, {
              type: "edit_variable",
              name: varName,
              group: groupName,
              field: "formula",
              value: cell.getValue()
            }, { priority: "event" });
          }
        }
      } else {
        // Group metadata edit
        var name = data.name;
        if (field === "name") name = cell.getOldValue() || "";

        if (typeof Shiny !== "undefined") {
          Shiny.setInputValue(inputId, {
            type: "edit_group",
            name: name,
            field: field,
            value: cell.getValue()
          }, { priority: "event" });
        }
      }

      relayout(table);
    });

    containerDiv.setAttribute("data-initialized", "true");
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
