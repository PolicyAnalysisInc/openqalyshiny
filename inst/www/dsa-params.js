/* DSA Parameter Table — Tabulator */
(function() {
  "use strict";
  console.log("[DSA Params] JS version 3.0.0 loaded");

  // =========================================================================
  // Tabulator CDN loader
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

    // Inject CSS
    var cssLink = document.createElement("link");
    cssLink.rel = "stylesheet";
    cssLink.href = TABULATOR_CSS;
    document.head.appendChild(cssLink);

    // Load JS
    var script = document.createElement("script");
    script.src = TABULATOR_CDN;
    script.onload = function() {
      console.log("[DSA Params] Tabulator loaded");
      var cbs = _tabulatorCallbacks.slice();
      _tabulatorCallbacks = [];
      for (var i = 0; i < cbs.length; i++) cbs[i]();
    };
    script.onerror = function() {
      console.error("[DSA Params] Failed to load Tabulator from CDN");
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
    table.setData(table.getData());
    if (holder) {
      requestAnimationFrame(function() {
        holder.scrollLeft = scrollLeft;
        holder.scrollTop = scrollTop;
      });
    }
  }

  // Reverse-lookup map: settings value -> display name
  function buildSettingsDisplayMap(settingsObj) {
    var map = {};
    var keys = Object.keys(settingsObj);
    for (var i = 0; i < keys.length; i++) {
      map[settingsObj[keys[i]]] = keys[i];
    }
    return map;
  }

  // Get targeting info for a variable name
  function getTargeting(choices, name) {
    var t = choices.variableTargeting || {};
    return t[name] || { strategies: null, groups: null };
  }

  // Set strategy/group on row data based on variable targeting
  function applyTargetingDefaults(data, choices) {
    var targeting = getTargeting(choices, data.name);
    if (targeting.strategies) {
      data.strategy = targeting.strategies[0] || "";
    } else {
      data.strategy = "";
    }
    if (targeting.groups) {
      data.group = targeting.groups[0] || "";
    } else {
      data.group = "";
    }
  }

  // =========================================================================
  // Custom editors
  // =========================================================================

  // Factory: returns a Tabulator custom editor that renders an Ace formula editor
  // as a fixed-position overlay on document.body, positioned over the cell.
  // This isolates the Ace editor from Tabulator's DOM and event handling,
  // avoiding focus theft and height collapse issues with inline editors.
  function formulaEditor(terms, suggestions) {
    return function(cell, onRendered, success, cancel) {
      // Placeholder returned to Tabulator — keeps editing state active
      var placeholder = document.createElement("div");
      placeholder.className = "dsa-formula-placeholder";
      placeholder.addEventListener("focusout", function(e) { e.stopPropagation(); });

      var currentValue = cell.getValue() || "";
      var committed = false;
      var overlay = null;
      var aceEditor = null;
      var onDocMouseDown = null;
      var cellFocusRedirect = null;
      var editCellEl = null;

      // Capture cell dimensions BEFORE Tabulator modifies the cell for editing.
      // Inside onRendered, the cell may have expanded due to the placeholder.
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
        // Guard: if Ace isn't loaded, fall back to plain input
        if (typeof ace === "undefined") {
          var input = document.createElement("input");
          input.type = "text";
          input.className = "dsa-input-editor";
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

        // Position overlay over cell (using pre-captured rect).
        // Ace always puts its first line ~2px from the container top.
        // To vertically center text, we create a wrapper inside the overlay
        // with top padding that pushes Ace's content to the center.
        var lineH = 18; // approximate line height at 0.85rem
        var innerH = cellRect.height - 4; // subtract 2px border each side
        var vPad = Math.max(0, Math.round((innerH - lineH) / 2));

        overlay = document.createElement("div");
        overlay.className = "dsa-formula-overlay";
        overlay.style.position = "fixed";
        overlay.style.left = cellRect.left + "px";
        overlay.style.top = cellRect.top + "px";
        overlay.style.width = cellRect.width + "px";
        overlay.style.height = cellRect.height + "px";
        overlay.style.zIndex = "10000";

        var aceContainer = document.createElement("div");
        aceContainer.className = "dsa-ace-container";
        aceContainer.style.position = "absolute";
        aceContainer.style.left = "0";
        aceContainer.style.right = "0";
        aceContainer.style.top = vPad + "px";
        aceContainer.style.bottom = "0";
        overlay.appendChild(aceContainer);
        document.body.appendChild(overlay);

        // Click outside overlay → commit (registered early for safety)
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

        // Block all events from reaching Tabulator
        ["mousedown", "pointerdown", "click", "mouseup", "pointerup", "focusin"].forEach(function(evt) {
          overlay.addEventListener(evt, function(e) { e.stopPropagation(); });
        });

        // Initialize Ace with same config as standalone formulaInput
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

        // Term highlighting + autocomplete (same as standalone)
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
          console.warn("[DSA Params] Term highlighting/autocomplete init failed:", e.message);
        }

        // Enter/Escape at capture phase — must fire before Ace's autocomplete
        // popup intercepts these keys. Ace's completer adds its own keyBinding
        // that handles Enter (to accept completion) at higher priority than
        // editor commands, so we catch it at the DOM level.
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

        // Tab accepts autocomplete completion
        aceEditor.commands.addCommand({
          name: "acceptCompletion",
          bindKey: { win: "Tab", mac: "Tab" },
          exec: function(ed) {
            if (ed.completer && ed.completer.popup &&
                ed.completer.popup.isOpen) {
              ed.completer.insertMatch();
              return true;
            }
            return false;
          }
        });

        // Paste: strip newlines to keep single-line
        aceEditor.container.addEventListener("paste", function(e) {
          e.preventDefault();
          e.stopPropagation();
          var text = (e.clipboardData || window.clipboardData).getData("text");
          aceEditor.insert(text.replace(/[\r\n]+/g, " "));
        }, true);

        // Blur: commit after delay (allows autocomplete clicks)
        aceEditor.on("blur", function() {
          setTimeout(function() {
            if (!committed && aceEditor && !aceEditor.isFocused()) {
              commit(aceEditor.getValue());
            }
          }, 300);
        });

        aceEditor.resize();
        // Tabulator's selectableRange actively re-focuses the cell element via tabindex.
        // Temporarily make the cell non-focusable and redirect any focus attempts to Ace.
        editCellEl = cellEl;
        var savedTabindex = cellEl.getAttribute("tabindex");
        cellEl.setAttribute("tabindex", "-1");
        cellEl.style.pointerEvents = "none";
        cellFocusRedirect = function() {
          if (committed || !aceEditor) return;
          aceEditor.focus();
        };
        cellEl.addEventListener("focus", cellFocusRedirect, true);

        // Restore cell focusability on cleanup
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

        // Force focus to Ace
        cellEl.blur();
        aceEditor.focus();
        // Retry in case Tabulator re-asserts before our changes take effect
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
  // Shiny sync
  // =========================================================================

  function syncToShiny(table, inputId) {
    var data = table.getData().map(function(d) {
      return {
        type: d.type,
        name: d.name,
        display_name: d.display_name || d.name,
        strategy: d.strategy || "",
        group: d.group || "",
        low: d.low || "",
        high: d.high || ""
      };
    });
    if (typeof Shiny !== "undefined") {
      Shiny.setInputValue(inputId, data, { priority: "event" });
    }
  }

  // =========================================================================
  // Column definitions
  // =========================================================================

  function buildColumnDefs(choices, inputId, terms, suggestions) {
    var settingsDisplay = buildSettingsDisplayMap(choices.settings);
    var strategyKeys = Object.keys(choices.strategies);
    var groupKeys = Object.keys(choices.groups);

    return [
      // Type column
      {
        title: "Type",
        field: "type",
        width: 100,
        editor: "list",
        editorParams: {
          values: [
            { label: "Variable", value: "variable" },
            { label: "Setting", value: "setting" }
          ]
        },
        formatter: function(cell) {
          var val = cell.getValue();
          if (val === "variable") return "Variable";
          if (val === "setting") return "Setting";
          return val || "";
        }
      },

      // Name column
      {
        title: "Name",
        field: "name",
        widthGrow: 1,
        minWidth: 120,
        editor: "list",
        editorParams: function(cell) {
          var data = cell.getRow().getData();
          if (data.type === "variable") {
            return { values: choices.variables.map(function(v) { return { label: v, value: v }; }) };
          } else {
            var sKeys = Object.keys(choices.settings);
            return { values: sKeys.map(function(k) { return { label: k, value: choices.settings[k] }; }) };
          }
        },
        formatter: function(cell) {
          var data = cell.getRow().getData();
          if (data.type === "setting") {
            return settingsDisplay[cell.getValue()] || cell.getValue() || "";
          }
          return cell.getValue() || "";
        }
      },

      // Strategy column
      {
        title: "Strategy",
        field: "strategy",
        minWidth: 120,
        editor: "list",
        editorParams: function(cell) {
          var data = cell.getRow().getData();
          var targeting = getTargeting(choices, data.name);
          if (!targeting.strategies) return { values: [] };
          return {
            values: targeting.strategies.map(function(sVal) {
              var sLabel = sVal;
              for (var sk = 0; sk < strategyKeys.length; sk++) {
                if (choices.strategies[strategyKeys[sk]] === sVal) {
                  sLabel = strategyKeys[sk];
                  break;
                }
              }
              return { label: sLabel, value: sVal };
            })
          };
        },
        editable: function(cell) {
          var data = cell.getRow().getData();
          if (data.type === "setting") return false;
          var targeting = getTargeting(choices, data.name);
          return targeting.strategies !== null;
        },
        formatter: function(cell) {
          var data = cell.getRow().getData();
          if (data.type === "setting") return "\u2014";
          var targeting = getTargeting(choices, data.name);
          if (!targeting.strategies) return "\u2014";
          var val = cell.getValue();
          if (!val || val === "") return "";
          for (var sk = 0; sk < strategyKeys.length; sk++) {
            if (choices.strategies[strategyKeys[sk]] === val) {
              return strategyKeys[sk];
            }
          }
          return val;
        },
        cellClick: function(e, cell) {
          // Style disabled cells
          var data = cell.getRow().getData();
          if (data.type === "setting" || !getTargeting(choices, data.name).strategies) {
            cell.getElement().style.color = "var(--bs-secondary, #6c757d)";
            cell.getElement().style.fontStyle = "italic";
          }
        }
      },

      // Group column
      {
        title: "Group",
        field: "group",
        minWidth: 120,
        editor: "list",
        editorParams: function(cell) {
          var data = cell.getRow().getData();
          var targeting = getTargeting(choices, data.name);
          if (!targeting.groups) return { values: [] };
          return {
            values: targeting.groups.map(function(gVal) {
              var gLabel = gVal;
              for (var gk = 0; gk < groupKeys.length; gk++) {
                if (choices.groups[groupKeys[gk]] === gVal) {
                  gLabel = groupKeys[gk];
                  break;
                }
              }
              return { label: gLabel, value: gVal };
            })
          };
        },
        editable: function(cell) {
          var data = cell.getRow().getData();
          if (data.type === "setting") return false;
          var targeting = getTargeting(choices, data.name);
          return targeting.groups !== null;
        },
        formatter: function(cell) {
          var data = cell.getRow().getData();
          if (data.type === "setting") return "\u2014";
          var targeting = getTargeting(choices, data.name);
          if (!targeting.groups) return "\u2014";
          var val = cell.getValue();
          if (!val || val === "") return "";
          for (var gk = 0; gk < groupKeys.length; gk++) {
            if (choices.groups[groupKeys[gk]] === val) {
              return groupKeys[gk];
            }
          }
          return val;
        },
        cellClick: function(e, cell) {
          var data = cell.getRow().getData();
          if (data.type === "setting" || !getTargeting(choices, data.name).groups) {
            cell.getElement().style.color = "var(--bs-secondary, #6c757d)";
            cell.getElement().style.fontStyle = "italic";
          }
        }
      },

      // Low column
      {
        title: "Low",
        field: "low",
        widthGrow: 1,
        minWidth: 450,
        editor: formulaEditor(terms, suggestions)
      },

      // High column
      {
        title: "High",
        field: "high",
        widthGrow: 1,
        minWidth: 450,
        editor: formulaEditor(terms, suggestions)
      },

      // Delete column
      {
        title: "",
        field: "_delete",
        width: 45,
        hozAlign: "center",
        headerSort: false,
        editor: false,
        clipboard: false,
        formatter: function(cell) {
          var btn = document.createElement("button");
          btn.type = "button";
          btn.className = "dsa-delete-btn";
          btn.textContent = "\u00d7";
          btn.addEventListener("click", function(e) {
            e.stopPropagation();
            var tbl = cell.getTable();
            cell.getRow().delete();
            relayout(tbl);
            syncToShiny(tbl, inputId);
          });
          return btn;
        }
      }
    ];
  }

  // =========================================================================
  // Grid initialization
  // =========================================================================

  // Track active table instances so we can destroy on re-render
  var _activeTables = {};

  function initGrid(containerDiv) {
    var inputId = containerDiv.dataset.inputId;
    if (!inputId) return;

    // Destroy previous table for this inputId if it exists
    if (_activeTables[inputId]) {
      try { _activeTables[inputId].destroy(); } catch (e) {}
      delete _activeTables[inputId];
    }

    // Parse data attributes
    var variableTargeting = {};
    try {
      variableTargeting = JSON.parse(containerDiv.dataset.variableTargeting || "{}");
    } catch (e) {}

    var choices = {
      variables: JSON.parse(containerDiv.dataset.variables || "[]"),
      settings: JSON.parse(containerDiv.dataset.settings || "{}"),
      strategies: JSON.parse(containerDiv.dataset.strategies || "{}"),
      groups: JSON.parse(containerDiv.dataset.groups || "{}"),
      variableTargeting: variableTargeting
    };

    // Parse formula editor terms and suggestions for Low/High columns
    var terms = null;
    var suggestions = null;
    try {
      terms = JSON.parse(containerDiv.dataset.terms || "null");
    } catch (e) {}
    try {
      suggestions = JSON.parse(containerDiv.dataset.suggestions || "null");
    } catch (e) {}

    var initialData = [];
    try {
      initialData = JSON.parse(containerDiv.dataset.initial || "[]");
    } catch (e) {
      initialData = [];
    }

    // Ensure display_name is set on initial data
    var settingsDisplay = buildSettingsDisplayMap(choices.settings);
    for (var i = 0; i < initialData.length; i++) {
      var row = initialData[i];
      if (!row.display_name) {
        if (row.type === "setting") {
          row.display_name = settingsDisplay[row.name] || row.name;
        } else {
          row.display_name = row.name;
        }
      }
    }

    var columnDefs = buildColumnDefs(choices, inputId, terms, suggestions);

    var table = new Tabulator(containerDiv, {
      data: initialData,
      columns: columnDefs,
      layout: "fitDataStretch",
      layoutColumnsOnNewData: true,
      height: "100%",

      // Cell range selection
      selectableRange: true,
      selectableRangeColumns: true,
      selectableRangeRows: true,
      selectableRangeClearCells: true,

      // Editing — double-click only
      editTriggerEvent: "dblclick",

      // Clipboard — built-in
      clipboard: true,
      clipboardCopyRowRange: "range",
      clipboardPasteParser: "range",
      clipboardPasteAction: "range",
      clipboardCopyConfig: { rowHeaders: false, columnHeaders: false },
      clipboardCopyStyled: false,

      // Sort by header icon only (prevents conflict with column selection)
      headerSortClickElement: "icon",

      // Row formatter to style disabled strategy/group cells
      rowFormatter: function(row) {
        var data = row.getData();
        var cells = row.getCells();
        for (var ci = 0; ci < cells.length; ci++) {
          var field = cells[ci].getColumn().getField();
          if (field === "strategy" || field === "group") {
            var el = cells[ci].getElement();
            if (data.type === "setting") {
              el.style.color = "var(--bs-secondary, #6c757d)";
              el.style.fontStyle = "italic";
            } else {
              var targeting = getTargeting(choices, data.name);
              var isDisabled = (field === "strategy" && !targeting.strategies) ||
                               (field === "group" && !targeting.groups);
              if (isDisabled) {
                el.style.color = "var(--bs-secondary, #6c757d)";
                el.style.fontStyle = "italic";
              } else {
                el.style.color = "";
                el.style.fontStyle = "";
              }
            }
          }
        }
      }
    });

    _activeTables[inputId] = table;

    // Event: cell edited — handle type/name cascading + sync
    table.on("cellEdited", function(cell) {
      var field = cell.getField();
      var data = cell.getRow().getData();

      if (field === "type") {
        // Type changed: reset name, strategy, group
        if (data.type === "variable") {
          var firstName = choices.variables[0] || "";
          cell.getRow().update({
            name: firstName,
            display_name: firstName,
            strategy: "",
            group: ""
          });
          // Apply targeting defaults for the new variable
          var rowData = cell.getRow().getData();
          applyTargetingDefaults(rowData, choices);
          cell.getRow().update({
            strategy: rowData.strategy,
            group: rowData.group
          });
        } else {
          var settingKeys = Object.keys(choices.settings);
          var firstSetting = settingKeys.length > 0 ? choices.settings[settingKeys[0]] : "";
          var firstSettingDisplay = settingKeys.length > 0 ? settingKeys[0] : "";
          cell.getRow().update({
            name: firstSetting,
            display_name: firstSettingDisplay,
            strategy: "",
            group: ""
          });
        }
      } else if (field === "name") {
        // Name changed: update display_name and targeting defaults
        if (data.type === "setting") {
          cell.getRow().update({
            display_name: settingsDisplay[data.name] || data.name
          });
        } else {
          cell.getRow().update({
            display_name: data.name
          });
          // Update strategy/group based on new variable's targeting
          var updatedData = cell.getRow().getData();
          applyTargetingDefaults(updatedData, choices);
          cell.getRow().update({
            strategy: updatedData.strategy,
            group: updatedData.group
          });
        }
      }

      syncToShiny(table, inputId);
      relayout(table);
    });

    // Event: clipboard paste — sync pasted data to Shiny
    table.on("clipboardPasted", function(clipboard, rowData, rows) {
      var pasteSettingsDisplay = buildSettingsDisplayMap(choices.settings);
      rows.forEach(function(row) {
        var d = row.getData();
        if (!d.display_name) {
          if (d.type === "setting") {
            row.update({ display_name: pasteSettingsDisplay[d.name] || d.name });
          } else {
            row.update({ display_name: d.name || "" });
          }
        }
      });
      syncToShiny(table, inputId);
    });

    // Wire up the add-row button (sibling of container)
    var addBtn = containerDiv.parentElement
      ? containerDiv.parentElement.querySelector(".dsa-add-row-btn")
      : null;
    if (addBtn) {
      // Remove old listener if any (re-render scenario)
      var newBtn = addBtn.cloneNode(true);
      addBtn.parentNode.replaceChild(newBtn, addBtn);

      newBtn.addEventListener("click", function() {
        var firstVar = choices.variables[0] || "";
        var newRow = {
          type: "variable",
          name: firstVar,
          display_name: firstVar,
          strategy: "",
          group: "",
          low: "",
          high: ""
        };
        applyTargetingDefaults(newRow, choices);
        table.addRow(newRow);
        relayout(table);
        syncToShiny(table, inputId);
      });
    }

    // Always sync so Shiny has data (even if empty array)
    syncToShiny(table, inputId);

    // Bundle grid data with run trigger — eliminates race condition
    var runBtn = document.querySelector(".dsa-run-btn");
    if (runBtn) {
      runBtn.addEventListener("click", function() {
        var data = table.getData().map(function(d) {
          return {
            type: d.type,
            name: d.name,
            display_name: d.display_name || d.name,
            strategy: d.strategy || "",
            group: d.group || "",
            low: d.low || "",
            high: d.high || ""
          };
        });
        Shiny.setInputValue("run_dsa_action", {
          nonce: Date.now(),
          params: data
        }, { priority: "event" });
      });
    }

    containerDiv.setAttribute("data-initialized", "true");
  }

  // =========================================================================
  // Lifecycle — listen for renderUI re-renders
  // =========================================================================

  function initAllGrids() {
    var containers = document.querySelectorAll(".dsa-params-container:not([data-initialized])");
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

    // Initialize grids already in the DOM (script may load after shiny:value fired)
    setTimeout(initAllGrids, 100);
  }
})();
