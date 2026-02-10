/* DSA Parameter Table — AG Grid Community Edition */
(function() {
  "use strict";

  // =========================================================================
  // AG Grid CDN loader (same pattern as override-manager.js SortableJS loader)
  // =========================================================================
  var AG_GRID_CDN = "https://cdn.jsdelivr.net/npm/ag-grid-community@32.3.3/dist/ag-grid-community.min.js";
  var AG_GRID_CSS = "https://cdn.jsdelivr.net/npm/ag-grid-community@32.3.3/styles/ag-grid.min.css";
  var AG_GRID_THEME_CSS = "https://cdn.jsdelivr.net/npm/ag-grid-community@32.3.3/styles/ag-theme-quartz.min.css";
  var _agGridCallbacks = [];
  var _agGridLoading = false;

  function ensureAgGrid(callback) {
    if (typeof agGrid !== "undefined") {
      callback();
      return;
    }
    _agGridCallbacks.push(callback);
    if (_agGridLoading) return;
    _agGridLoading = true;

    // Inject theme CSS
    var cssLink = document.createElement("link");
    cssLink.rel = "stylesheet";
    cssLink.href = AG_GRID_CSS;
    document.head.appendChild(cssLink);

    var themeCssLink = document.createElement("link");
    themeCssLink.rel = "stylesheet";
    themeCssLink.href = AG_GRID_THEME_CSS;
    document.head.appendChild(themeCssLink);

    // Load JS
    var script = document.createElement("script");
    script.src = AG_GRID_CDN;
    script.onload = function() {
      console.log("[DSA Params] AG Grid loaded");
      var cbs = _agGridCallbacks.slice();
      _agGridCallbacks = [];
      for (var i = 0; i < cbs.length; i++) cbs[i]();
    };
    script.onerror = function() {
      console.error("[DSA Params] Failed to load AG Grid from CDN");
    };
    document.head.appendChild(script);
  }

  // =========================================================================
  // FormulaEditor — popup cell editor wrapping Ace
  // =========================================================================
  function FormulaEditor() {}

  FormulaEditor.prototype.init = function(params) {
    this.params = params;
    this.value = params.value || "";

    // Container div
    this.container = document.createElement("div");
    this.container.className = "dsa-formula-editor";

    // Ace editor div
    this.editorDiv = document.createElement("div");
    this.editorDiv.style.width = "100%";
    this.editorDiv.style.height = "100%";
    this.container.appendChild(this.editorDiv);
  };

  FormulaEditor.prototype.getGui = function() {
    return this.container;
  };

  FormulaEditor.prototype.afterGuiAttached = function() {
    var self = this;

    // Check if Ace is available
    if (typeof ace === "undefined") {
      // Fallback: just show a text input
      this.container.innerHTML = "";
      var inp = document.createElement("input");
      inp.type = "text";
      inp.value = this.value;
      inp.style.width = "100%";
      inp.style.height = "100%";
      inp.style.border = "none";
      inp.style.outline = "none";
      inp.style.fontSize = "0.85rem";
      this.fallbackInput = inp;
      this.container.appendChild(inp);
      inp.focus();
      inp.select();

      inp.addEventListener("keydown", function(e) {
        if (e.key === "Enter") {
          self.value = inp.value;
          self.params.stopEditing(false);
          e.preventDefault();
          e.stopPropagation();
        } else if (e.key === "Escape") {
          self.params.stopEditing(true);
          e.preventDefault();
          e.stopPropagation();
        }
      });
      return;
    }

    // Load language tools extension
    ace.require("ace/ext/language_tools");

    var editor = ace.edit(this.editorDiv);
    this.editor = editor;
    editor.setTheme("ace/theme/chrome");
    editor.session.setMode("ace/mode/r");

    editor.setOptions({
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

    editor.setValue(this.value, -1);

    // Custom highlighting
    if (typeof FormulaInputMode !== "undefined" && this.params.colDef.cellEditorParams) {
      var editorParams = typeof this.params.colDef.cellEditorParams === "function"
        ? this.params.colDef.cellEditorParams(this.params)
        : this.params.colDef.cellEditorParams;
      if (editorParams && editorParams.terms) {
        try {
          FormulaInputMode.injectDefaultStyles();
          var highlighter = new FormulaInputMode.FormulaHighlighter(editor);
          highlighter.setTerms(editorParams.terms);
          this._highlighter = highlighter;
        } catch (e) {
          // Graceful degradation
        }
      }

      // Custom autocomplete
      if (typeof FormulaInputAutocomplete !== "undefined" && editorParams && editorParams.suggestions) {
        try {
          var completer = new FormulaInputAutocomplete.FormulaCompleter(editor, editorParams.suggestions);
          editor.completers = [completer];
          this._completer = completer;
        } catch (e) {
          // Graceful degradation
        }
      }
    }

    // Handle Enter → commit
    editor.commands.addCommand({
      name: "commitCell",
      bindKey: { win: "Enter", mac: "Enter" },
      exec: function() {
        if (editor.completer && editor.completer.popup && editor.completer.popup.isOpen) {
          editor.completer.detach();
        }
        self.value = editor.getValue();
        self.params.stopEditing(false);
      }
    });

    // Shift+Enter also commits
    editor.commands.addCommand({
      name: "commitCellShift",
      bindKey: { win: "Shift-Enter", mac: "Shift-Enter" },
      exec: function() {
        if (editor.completer && editor.completer.popup && editor.completer.popup.isOpen) {
          editor.completer.detach();
        }
        self.value = editor.getValue();
        self.params.stopEditing(false);
      }
    });

    // Escape → cancel
    editor.commands.addCommand({
      name: "cancelEdit",
      bindKey: { win: "Escape", mac: "Escape" },
      exec: function() {
        self.params.stopEditing(true);
      }
    });

    // Tab accepts autocomplete
    editor.commands.addCommand({
      name: "acceptCompletion",
      bindKey: { win: "Tab", mac: "Tab" },
      exec: function(ed) {
        if (ed.completer && ed.completer.popup && ed.completer.popup.isOpen) {
          ed.completer.insertMatch();
          return true;
        }
        return false;
      }
    });

    // Stop arrow key propagation so grid doesn't navigate
    this.container.addEventListener("keydown", function(e) {
      if (["ArrowLeft", "ArrowRight", "ArrowUp", "ArrowDown"].indexOf(e.key) !== -1) {
        e.stopPropagation();
      }
    }, true);

    // Handle paste — strip newlines
    editor.container.addEventListener("paste", function(e) {
      e.preventDefault();
      e.stopPropagation();
      var text = (e.clipboardData || window.clipboardData).getData("text");
      text = text.replace(/[\r\n]+/g, " ");
      editor.insert(text);
    }, true);

    editor.focus();
    editor.selectAll();
  };

  FormulaEditor.prototype.getValue = function() {
    if (this.editor) {
      return this.editor.getValue();
    }
    if (this.fallbackInput) {
      return this.fallbackInput.value;
    }
    return this.value;
  };

  FormulaEditor.prototype.isPopup = function() {
    return true;
  };

  FormulaEditor.prototype.destroy = function() {
    if (this.editor) {
      this.editor.destroy();
      this.editor = null;
    }
    this._highlighter = null;
    this._completer = null;
  };

  // =========================================================================
  // DynamicSelectEditor — custom select editor with runtime options
  // =========================================================================
  function DynamicSelectEditor() {}

  DynamicSelectEditor.prototype.init = function(params) {
    this.params = params;
    this.value = params.value;

    this.select = document.createElement("select");
    this.select.className = "dsa-select-editor";

    // Get options from cellEditorParams (may be a function)
    var editorParams = typeof params.colDef.cellEditorParams === "function"
      ? params.colDef.cellEditorParams(params)
      : (params.colDef.cellEditorParams || {});
    var options = editorParams.options || [];

    for (var i = 0; i < options.length; i++) {
      var opt = document.createElement("option");
      opt.value = options[i].value;
      opt.textContent = options[i].label;
      if (options[i].value === this.value) opt.selected = true;
      this.select.appendChild(opt);
    }

    var self = this;
    this.select.addEventListener("change", function() {
      self.value = self.select.value;
      self.params.stopEditing(false);
    });

    this.select.addEventListener("keydown", function(e) {
      if (e.key === "Escape") {
        self.params.stopEditing(true);
        e.preventDefault();
      }
    });
  };

  DynamicSelectEditor.prototype.getGui = function() {
    return this.select;
  };

  DynamicSelectEditor.prototype.afterGuiAttached = function() {
    this.select.focus();
  };

  DynamicSelectEditor.prototype.getValue = function() {
    return this.value;
  };

  DynamicSelectEditor.prototype.destroy = function() {
    this.select = null;
  };

  // =========================================================================
  // Grid configuration factory
  // =========================================================================

  // Reverse-lookup map: settings value → display name
  function buildSettingsDisplayMap(settingsObj) {
    var map = {};
    var keys = Object.keys(settingsObj);
    for (var i = 0; i < keys.length; i++) {
      map[settingsObj[keys[i]]] = keys[i];
    }
    return map;
  }

  // Helper: get targeting info for a variable name
  function getTargeting(choices, name) {
    var t = choices.variableTargeting || {};
    return t[name] || { strategies: null, groups: null };
  }

  // Helper: set strategy/group on row data based on variable targeting
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

  function buildColumnDefs(choices, formulaEditorParams) {
    var settingsDisplay = buildSettingsDisplayMap(choices.settings);

    // Build options arrays
    var typeOptions = [
      { value: "variable", label: "Variable" },
      { value: "setting", label: "Setting" }
    ];

    var strategyKeys = Object.keys(choices.strategies);

    var groupKeys = Object.keys(choices.groups);

    return [
      {
        headerName: "Type",
        field: "type",
        width: 100,
        cellEditor: DynamicSelectEditor,
        cellEditorParams: { options: typeOptions },
        valueFormatter: function(params) {
          if (params.value === "variable") return "Variable";
          if (params.value === "setting") return "Setting";
          return params.value || "";
        },
        valueSetter: function(params) {
          if (params.newValue === params.oldValue) return false;
          params.data.type = params.newValue;
          // Reset name to first option for new type
          if (params.newValue === "variable") {
            params.data.name = choices.variables[0] || "";
            params.data.display_name = params.data.name;
            applyTargetingDefaults(params.data, choices);
          } else {
            var settingKeys = Object.keys(choices.settings);
            params.data.name = settingKeys.length > 0 ? choices.settings[settingKeys[0]] : "";
            params.data.display_name = settingKeys.length > 0 ? settingKeys[0] : "";
            params.data.strategy = "";
            params.data.group = "";
          }
          // Refresh all cells in this row
          params.api.refreshCells({ rowNodes: [params.node], force: true });
          return true;
        }
      },
      {
        headerName: "Name",
        field: "name",
        flex: 1,
        minWidth: 120,
        cellEditor: DynamicSelectEditor,
        cellEditorParams: function(params) {
          var opts = [];
          if (params.data.type === "variable") {
            for (var vi = 0; vi < choices.variables.length; vi++) {
              opts.push({ value: choices.variables[vi], label: choices.variables[vi] });
            }
          } else {
            var sKeys = Object.keys(choices.settings);
            for (var ski = 0; ski < sKeys.length; ski++) {
              opts.push({ value: choices.settings[sKeys[ski]], label: sKeys[ski] });
            }
          }
          return { options: opts };
        },
        valueFormatter: function(params) {
          if (params.data.type === "setting") {
            return settingsDisplay[params.value] || params.value || "";
          }
          return params.value || "";
        },
        valueSetter: function(params) {
          if (params.newValue === params.oldValue) return false;
          params.data.name = params.newValue;
          // Update display_name
          if (params.data.type === "setting") {
            params.data.display_name = settingsDisplay[params.newValue] || params.newValue;
          } else {
            params.data.display_name = params.newValue;
            // Update strategy/group based on new variable's targeting
            applyTargetingDefaults(params.data, choices);
          }
          params.api.refreshCells({ rowNodes: [params.node], force: true });
          return true;
        }
      },
      {
        headerName: "Strategy",
        field: "strategy",
        width: 110,
        cellEditor: DynamicSelectEditor,
        cellEditorParams: function(params) {
          var targeting = getTargeting(choices, params.data.name);
          var opts = [];
          if (targeting.strategies) {
            for (var si = 0; si < targeting.strategies.length; si++) {
              var sVal = targeting.strategies[si];
              // Find display name
              var sLabel = sVal;
              for (var sk = 0; sk < strategyKeys.length; sk++) {
                if (choices.strategies[strategyKeys[sk]] === sVal) {
                  sLabel = strategyKeys[sk];
                  break;
                }
              }
              opts.push({ value: sVal, label: sLabel });
            }
          }
          return { options: opts };
        },
        editable: function(params) {
          if (params.data.type === "setting") return false;
          var targeting = getTargeting(choices, params.data.name);
          return targeting.strategies !== null;
        },
        valueFormatter: function(params) {
          if (params.data.type === "setting") return "\u2014";
          var targeting = getTargeting(choices, params.data.name);
          if (!targeting.strategies) return "\u2014";
          if (!params.value || params.value === "") return "";
          // Look up display name from strategies
          for (var sk = 0; sk < strategyKeys.length; sk++) {
            if (choices.strategies[strategyKeys[sk]] === params.value) {
              return strategyKeys[sk];
            }
          }
          return params.value;
        },
        cellStyle: function(params) {
          if (params.data.type === "setting") {
            return { color: "var(--bs-secondary, #6c757d)", fontStyle: "italic" };
          }
          var targeting = getTargeting(choices, params.data.name);
          if (!targeting.strategies) {
            return { color: "var(--bs-secondary, #6c757d)", fontStyle: "italic" };
          }
          return null;
        }
      },
      {
        headerName: "Group",
        field: "group",
        width: 110,
        cellEditor: DynamicSelectEditor,
        cellEditorParams: function(params) {
          var targeting = getTargeting(choices, params.data.name);
          var opts = [];
          if (targeting.groups) {
            for (var gi = 0; gi < targeting.groups.length; gi++) {
              var gVal = targeting.groups[gi];
              // Find display name
              var gLabel = gVal;
              for (var gk = 0; gk < groupKeys.length; gk++) {
                if (choices.groups[groupKeys[gk]] === gVal) {
                  gLabel = groupKeys[gk];
                  break;
                }
              }
              opts.push({ value: gVal, label: gLabel });
            }
          }
          return { options: opts };
        },
        editable: function(params) {
          if (params.data.type === "setting") return false;
          var targeting = getTargeting(choices, params.data.name);
          return targeting.groups !== null;
        },
        valueFormatter: function(params) {
          if (params.data.type === "setting") return "\u2014";
          var targeting = getTargeting(choices, params.data.name);
          if (!targeting.groups) return "\u2014";
          if (!params.value || params.value === "") return "";
          // Look up display name from groups
          for (var gk = 0; gk < groupKeys.length; gk++) {
            if (choices.groups[groupKeys[gk]] === params.value) {
              return groupKeys[gk];
            }
          }
          return params.value;
        },
        cellStyle: function(params) {
          if (params.data.type === "setting") {
            return { color: "var(--bs-secondary, #6c757d)", fontStyle: "italic" };
          }
          var targeting = getTargeting(choices, params.data.name);
          if (!targeting.groups) {
            return { color: "var(--bs-secondary, #6c757d)", fontStyle: "italic" };
          }
          return null;
        }
      },
      {
        headerName: "Low",
        field: "low",
        flex: 1,
        minWidth: 100,
        cellEditor: FormulaEditor,
        cellEditorPopup: true,
        cellEditorParams: formulaEditorParams
      },
      {
        headerName: "High",
        field: "high",
        flex: 1,
        minWidth: 100,
        cellEditor: FormulaEditor,
        cellEditorPopup: true,
        cellEditorParams: formulaEditorParams
      },
      {
        headerName: "",
        field: "_delete",
        width: 45,
        editable: false,
        sortable: false,
        filter: false,
        cellRenderer: function(params) {
          var btn = document.createElement("button");
          btn.type = "button";
          btn.className = "dsa-delete-btn";
          btn.textContent = "\u00d7";
          btn.addEventListener("click", function(e) {
            e.stopPropagation();
            params.api.applyTransaction({ remove: [params.node.data] });
            syncToShiny(params.api, params.context.inputId);
          });
          return btn;
        }
      }
    ];
  }

  // =========================================================================
  // Shiny sync
  // =========================================================================

  function syncToShiny(api, inputId) {
    var data = [];
    api.forEachNode(function(node) {
      var d = node.data;
      data.push({
        type: d.type,
        name: d.name,
        display_name: d.display_name || d.name,
        strategy: d.strategy || "",
        group: d.group || "",
        low: d.low || "",
        high: d.high || ""
      });
    });
    if (typeof Shiny !== "undefined") {
      Shiny.setInputValue(inputId, data, { priority: "event" });
    }
  }

  // =========================================================================
  // Grid initialization
  // =========================================================================

  // Track active grid instances so we can destroy on re-render
  var _activeGrids = {};

  function initGrid(containerDiv) {
    var inputId = containerDiv.dataset.inputId;
    if (!inputId) return;

    // Destroy previous grid for this inputId if it exists
    if (_activeGrids[inputId]) {
      try { _activeGrids[inputId].destroy(); } catch (e) {}
      delete _activeGrids[inputId];
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

    var initialData = [];
    try {
      initialData = JSON.parse(containerDiv.dataset.initial || "[]");
    } catch (e) {
      initialData = [];
    }

    var terms = null;
    try { terms = JSON.parse(containerDiv.dataset.terms || "null"); } catch (e) {}

    var suggestions = null;
    try { suggestions = JSON.parse(containerDiv.dataset.suggestions || "null"); } catch (e) {}

    var formulaEditorParams = {};
    if (terms) formulaEditorParams.terms = terms;
    if (suggestions) formulaEditorParams.suggestions = suggestions;

    var columnDefs = buildColumnDefs(choices, formulaEditorParams);

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

    var gridOptions = {
      columnDefs: columnDefs,
      rowData: initialData,
      singleClickEdit: true,
      stopEditingWhenCellsLoseFocus: true,
      domLayout: "autoHeight",
      defaultColDef: {
        editable: true,
        resizable: false,
        suppressMovable: true
      },
      context: {
        inputId: inputId,
        choices: choices
      },
      onCellValueChanged: function(event) {
        syncToShiny(event.api, inputId);
      }
    };

    var api = agGrid.createGrid(containerDiv, gridOptions);
    _activeGrids[inputId] = api;

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
        api.applyTransaction({ add: [newRow] });
        syncToShiny(api, inputId);
      });
    }

    // Initial sync so Shiny has the data from pre-populated rows
    if (initialData.length > 0) {
      syncToShiny(api, inputId);
    }

    containerDiv.setAttribute("data-initialized", "true");
  }

  // =========================================================================
  // Lifecycle — listen for renderUI re-renders
  // =========================================================================

  function initAllGrids() {
    var containers = document.querySelectorAll(".dsa-params-container:not([data-initialized])");
    if (containers.length === 0) return;
    ensureAgGrid(function() {
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
