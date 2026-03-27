/* Variables Table — Tabulator (individual edit/remove actions) */
(function() {
  "use strict";
  console.log("[Variables Table] JS version 1.0.0 loaded");

  // Prevent Tabulator's internal focus() calls from triggering browser auto-scroll
  (function() {
    var origFocus = HTMLElement.prototype.focus;
    HTMLElement.prototype.focus = function(opts) {
      if (this.closest && this.closest('.tabulator')) {
        return origFocus.call(this, Object.assign({}, opts, { preventScroll: true }));
      }
      return origFocus.call(this, opts);
    };
  })();

  // =========================================================================
  // Tabulator CDN loader (shared with dsa-params.js — guards double-load)
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
      console.log("[Variables Table] Tabulator loaded");
      var cbs = _tabulatorCallbacks.slice();
      _tabulatorCallbacks = [];
      for (var i = 0; i < cbs.length; i++) cbs[i]();
    };
    script.onerror = function() {
      console.error("[Variables Table] Failed to load Tabulator from CDN");
    };
    document.head.appendChild(script);
  }

  // =========================================================================
  // Helpers
  // =========================================================================

  // Reverse-lookup map: value -> display name
  function buildDisplayMap(obj) {
    var map = {};
    var keys = Object.keys(obj);
    for (var i = 0; i < keys.length; i++) {
      map[obj[keys[i]]] = keys[i];
    }
    return map;
  }

  // =========================================================================
  // Custom formula editor (same pattern as dsa-params.js)
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
          console.warn("[Variables Table] Term highlighting/autocomplete init failed:", e.message);
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

  function buildColumnDefs(strategies, groups, inputId, terms, suggestions) {
    var strategyDisplay = buildDisplayMap(strategies);
    var groupDisplay = buildDisplayMap(groups);

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

      // Strategy
      {
        title: "Strategy",
        field: "strategy",
        minWidth: 120,
        editor: "list",
        editorParams: {
          values: [{ label: "\u2014 (None)", value: "" }].concat(
            Object.keys(strategies).map(function(displayName) {
              return { label: displayName, value: strategies[displayName] };
            })
          )
        },
        formatter: function(cell) {
          var val = cell.getValue();
          if (!val || val === "") return "\u2014";
          return strategyDisplay[val] || val;
        }
      },

      // Group
      {
        title: "Group",
        field: "group",
        minWidth: 120,
        editor: "list",
        editorParams: {
          values: [{ label: "\u2014 (None)", value: "" }].concat(
            Object.keys(groups).map(function(displayName) {
              return { label: displayName, value: groups[displayName] };
            })
          )
        },
        formatter: function(cell) {
          var val = cell.getValue();
          if (!val || val === "") return "\u2014";
          return groupDisplay[val] || val;
        }
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

      // Formula
      {
        title: "Formula",
        field: "formula",
        widthGrow: 2,
        minWidth: 450,
        editor: formulaEditor(terms, suggestions),
        formatter: emdashIfEmpty
      },

      // Delete column
      {
        title: "",
        field: "_delete",
        width: 60,
        widthGrow: 0,
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
            confirmBtn.className = "var-confirm-btn";
            confirmBtn.textContent = "\u2713";
            confirmBtn.addEventListener("click", function(e) {
              e.stopPropagation();
              var rowData = cell.getRow().getData();
              var name = (rowData.name || "").trim();
              var formula = (rowData.formula || "").trim();
              if (!name || !formula) {
                alert("Name and formula are required.");
                return;
              }
              cell.getRow().delete();
              relayout(table);
              if (typeof Shiny !== "undefined") {
                Shiny.setInputValue(inputId, {
                  type: "add_variable",
                  name: name,
                  formula: formula,
                  display_name: (rowData.display_name || "").trim(),
                  description: (rowData.description || "").trim()
                }, { priority: "event" });
              }
            });

            var cancelBtn = document.createElement("button");
            cancelBtn.type = "button";
            cancelBtn.className = "var-cancel-btn";
            cancelBtn.textContent = "\u00d7";
            cancelBtn.addEventListener("click", function(e) {
              e.stopPropagation();
              cell.getRow().delete();
              relayout(table);
            });

            wrapper.appendChild(confirmBtn);
            wrapper.appendChild(cancelBtn);
            return wrapper;
          }

          var btn = document.createElement("button");
          btn.type = "button";
          btn.className = "var-delete-btn";
          btn.textContent = "\u00d7";
          btn.addEventListener("click", function(e) {
            e.stopPropagation();
            var data = cell.getRow().getData();
            cell.getRow().delete();
            relayout(table);
            if (typeof Shiny !== "undefined") {
              Shiny.setInputValue(inputId, {
                type: "remove_variable",
                name: data.name,
                strategy: data.strategy || "",
                group: data.group || ""
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
  function initGrid(containerDiv) {
    var inputId = containerDiv.dataset.inputId;
    if (!inputId) return;

    // Destroy previous table for this inputId if it exists
    if (_activeTables[inputId]) {
      try { _activeTables[inputId].destroy(); } catch (e) {}
      delete _activeTables[inputId];
    }

    var strategies = {};
    try { strategies = JSON.parse(containerDiv.dataset.strategies || "{}"); } catch (e) {}

    var groups = {};
    try { groups = JSON.parse(containerDiv.dataset.groups || "{}"); } catch (e) {}

    var terms = null;
    try { terms = JSON.parse(containerDiv.dataset.terms || "null"); } catch (e) {}

    var suggestions = null;
    try { suggestions = JSON.parse(containerDiv.dataset.suggestions || "null"); } catch (e) {}

    var initialData = [];
    try {
      initialData = JSON.parse(containerDiv.dataset.initial || "[]");
    } catch (e) {
      initialData = [];
    }

    var columnDefs = buildColumnDefs(strategies, groups, inputId, terms, suggestions);

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

      clipboard: true,
      clipboardCopyStyled: false,

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

    // "Add Variable" button above the table
    var addBtn = document.createElement("button");
    addBtn.type = "button";
    addBtn.className = "var-add-btn";
    addBtn.textContent = "+ Add Variable";
    containerDiv.parentNode.insertBefore(addBtn, containerDiv);
    addBtn.addEventListener("click", function() {
      table.addRow(
        { _isNew: true, name: "", formula: "", display_name: "", description: "", strategy: "", group: "" },
        false
      ).then(function(row) {
        relayout(table);
        row.getElement().scrollIntoView({ behavior: "smooth", block: "nearest" });
        // Auto-open the name editor on the new row
        var nameCell = row.getCell("name");
        if (nameCell) {
          setTimeout(function() { nameCell.edit(); }, 50);
        }
      });
    });

    // Cell edited — fire individual edit_variable action
    table.on("cellEdited", function(cell) {
      var field = cell.getField();
      var data = cell.getRow().getData();
      if (data._isNew) {
        relayout(table);
        return;
      }
      if (cell.getOldValue() === cell.getValue()) {
        relayout(table);
        return;
      }

      // For strategy/group/name edits, use OLD value for lookup since
      // data already reflects the new value but edit_variable needs
      // the old targeting/name to find the correct variable
      var name = data.name;
      var strategy = data.strategy || "";
      var group = data.group || "";
      if (field === "name") name = cell.getOldValue() || "";
      if (field === "strategy") strategy = cell.getOldValue() || "";
      if (field === "group") group = cell.getOldValue() || "";

      if (typeof Shiny !== "undefined") {
        Shiny.setInputValue(inputId, {
          type: "edit_variable",
          name: name,
          strategy: strategy,
          group: group,
          field: field,
          value: cell.getValue()
        }, { priority: "event" });
      }

      relayout(table);
    });

    containerDiv.setAttribute("data-initialized", "true");
  }


  // =========================================================================
  // Lifecycle — listen for renderUI re-renders
  // =========================================================================

  function initAllGrids() {
    var containers = document.querySelectorAll(".variables-table-container:not([data-initialized])");
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
