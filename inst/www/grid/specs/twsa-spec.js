/* OQGrid Spec — TWSA (Two-Way Sensitivity Analysis) Parameters Table */
(function() {
  "use strict";

  var targeting = OQGrid.helpers.targeting;
  var combo = OQGrid.helpers.combo;
  var exclusions = OQGrid.helpers.exclusions;

  // =========================================================================
  // Page-level state machine for multi-analysis tabs
  // =========================================================================
  var _state = {
    analyses: [],
    selectedId: null,
    nextId: 1
  };

  function getSelectedAnalysis() {
    for (var i = 0; i < _state.analyses.length; i++) {
      if (_state.analyses[i].id === _state.selectedId) return _state.analyses[i];
    }
    return null;
  }

  function getAnalysisById(id) {
    for (var i = 0; i < _state.analyses.length; i++) {
      if (_state.analyses[i].id === id) return _state.analyses[i];
    }
    return null;
  }

  function addAnalysis(name) {
    var a = {
      id: _state.nextId++,
      name: name || "TWSA " + _state.nextId,
      description: "",
      parameters: []
    };
    _state.analyses.push(a);
    _state.selectedId = a.id;
    return a;
  }

  function removeAnalysis(id) {
    var idx = -1;
    for (var i = 0; i < _state.analyses.length; i++) {
      if (_state.analyses[i].id === id) { idx = i; break; }
    }
    if (idx === -1) return;
    _state.analyses.splice(idx, 1);
    if (_state.selectedId === id) {
      if (_state.analyses.length > 0) {
        var newIdx = Math.min(idx, _state.analyses.length - 1);
        _state.selectedId = _state.analyses[newIdx].id;
      } else {
        _state.selectedId = null;
      }
    }
  }

  function saveGridToState(table) {
    var analysis = getSelectedAnalysis();
    if (!analysis || !table) return;
    analysis.parameters = table.getData().map(function(d) {
      return {
        axis: d.axis || "x",
        param_type: d.param_type || "variable",
        name: d.name || "",
        display_name: d.display_name || d.name || "",
        strategy: d.strategy || "",
        group: d.group || "",
        type: d.type || "radius",
        data: d.data || {}
      };
    });
  }

  function loadStateToGrid(table, id) {
    var analysis = getAnalysisById(id);
    _state.selectedId = id;
    if (!analysis || !table) return;
    var params = analysis.parameters || [];
    // Ensure exactly 2 rows for TWSA
    while (params.length < 2) {
      var axis = params.length === 0 ? "x" : "y";
      params.push({
        axis: axis, param_type: "variable", name: "", display_name: "",
        strategy: "", group: "", type: "radius", data: {}
      });
    }
    analysis.parameters = params;
    table.setData(params);
    OQGrid.relayout(table);
  }

  // =========================================================================
  // Grid visibility
  // =========================================================================
  function updateGridVisibility(wrapperDiv) {
    var gridPanel = wrapperDiv ? wrapperDiv.querySelector(".twsa-grid-panel") : null;
    if (!gridPanel) return;
    if (_state.analyses.length === 0 || _state.selectedId === null) {
      gridPanel.style.display = "none";
    } else {
      gridPanel.style.display = "";
    }
  }

  // =========================================================================
  // Serialize ALL analyses for Shiny
  // =========================================================================
  function syncAllToShiny(table, inputId) {
    saveGridToState(table);
    var payload = _state.analyses.map(function(a) {
      var variableParams = [];
      var settingParams = [];
      (a.parameters || []).forEach(function(p) {
        if (p.param_type === "setting") {
          settingParams.push({
            axis: p.axis,
            name: p.name,
            type: p.type || "radius",
            data: p.data || {}
          });
        } else {
          variableParams.push({
            axis: p.axis,
            name: p.name,
            strategy: p.strategy || "",
            group: p.group || "",
            type: p.type || "radius",
            data: p.data || {}
          });
        }
      });
      return {
        name: a.name,
        description: a.description || "",
        variable_params: variableParams,
        setting_params: settingParams
      };
    });
    OQGrid.shiny.dispatch(inputId, payload);
  }

  // =========================================================================
  // Analysis list rendering
  // =========================================================================
  function renderAnalysisList(listContainer, table, inputId) {
    while (listContainer.firstChild) {
      listContainer.removeChild(listContainer.firstChild);
    }

    _state.analyses.forEach(function(a) {
      var item = document.createElement("div");
      item.className = "twsa-list-item" + (a.id === _state.selectedId ? " twsa-list-item--active" : "");
      item.setAttribute("data-id", a.id);

      var nameSpan = document.createElement("div");
      nameSpan.className = "twsa-list-item-name";
      nameSpan.textContent = a.name;
      item.appendChild(nameSpan);

      if (a.description) {
        var descSpan = document.createElement("div");
        descSpan.className = "twsa-list-item-desc";
        descSpan.textContent = a.description;
        item.appendChild(descSpan);
      }

      item.addEventListener("click", function() {
        if (a.id === _state.selectedId) return;
        saveGridToState(table);
        loadStateToGrid(table, a.id);
        renderAnalysisList(listContainer, table, inputId);
        syncAllToShiny(table, inputId);
      });

      item.addEventListener("dblclick", function(e) {
        e.stopPropagation();
        saveGridToState(table);
        OQGrid.shiny.dispatch("show_edit_twsa_modal", {
          nonce: Date.now(),
          id: a.id,
          name: a.name,
          description: a.description || ""
        });
      });

      listContainer.appendChild(item);
    });
  }

  // =========================================================================
  // Spec registration
  // =========================================================================
  OQGrid.registerSpec("twsa", function() {
    return {
      name: "twsa",
      containerSelector: ".twsa-params-container",
      wrapperSelector: ".twsa-params-wrapper",
      dispatchMode: "sync",

      // =====================================================================
      // Parse data attributes
      // =====================================================================
      parseData: function(el) {
        var variableTargeting = OQGrid.utils.parseDataAttr(el, "variableTargeting", {});
        return {
          initial: OQGrid.utils.parseDataAttr(el, "initial", []),
          variables: OQGrid.utils.parseDataAttr(el, "variables", []),
          settings: OQGrid.utils.parseDataAttr(el, "settings", {}),
          strategies: OQGrid.utils.parseDataAttr(el, "strategies", {}),
          groups: OQGrid.utils.parseDataAttr(el, "groups", {}),
          variableTargeting: variableTargeting,
          variableFormulas: OQGrid.utils.parseDataAttr(el, "variableFormulas", {}),
          settingValues: OQGrid.utils.parseDataAttr(el, "settingValues", {}),
          terms: OQGrid.utils.parseDataAttr(el, "terms", null),
          suggestions: OQGrid.utils.parseDataAttr(el, "suggestions", null),
          initialTwsa: OQGrid.utils.parseDataAttr(el, "initialTwsa", [])
        };
      },

      // =====================================================================
      // Column definitions
      // =====================================================================
      getColumnDefs: function(data, controller) {
        var choices = data;
        var settingsDisplay = OQGrid.utils.buildDisplayMap(choices.settings);
        var strategyKeys = Object.keys(choices.strategies);
        var groupKeys = Object.keys(choices.groups);
        var terms = choices.terms;
        var suggestions = choices.suggestions;

        // Active popup reference for cleanup
        var activePopup = null;

        return [
          // Axis column (read-only)
          {
            title: "Axis",
            field: "axis",
            width: 60,
            hozAlign: "center",
            headerSort: false,
            formatter: function(cell) {
              var val = cell.getValue();
              return val === "x" ? "X" : val === "y" ? "Y" : (val || "").toUpperCase();
            }
          },

          // Type column (read-only)
          {
            title: "Type",
            field: "param_type",
            width: 80,
            headerSort: false,
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
            widthGrow: 2,
            minWidth: 120,
            editor: "list",
            editorParams: function(cell) {
              var rowData = cell.getRow().getData();
              if (rowData.param_type === "variable") {
                var cid = combo.getComboId(cell);
                var usedCombos = combo.getUsedCombosForDropdown(cell.getTable(), cid);
                return {
                  values: choices.variables
                    .filter(function(v) {
                      if (v === rowData.name) return true;
                      var combos = combo.allVariableCombos(choices).filter(function(c) { return c.name === v; });
                      return combos.some(function(c) {
                        return !usedCombos.has(combo.compositeKey(c.name, c.strategy, c.group));
                      });
                    })
                    .map(function(v) { return { label: v, value: v }; })
                };
              } else {
                var currentName = cell.getValue();
                var usedSettingNames = new Set();
                cell.getTable().getData().forEach(function(r) {
                  if (r.param_type === "setting" && r.name !== currentName) {
                    usedSettingNames.add(r.name);
                  }
                });
                exclusions.expandWithExclusions(usedSettingNames);
                var sKeys = Object.keys(choices.settings);
                return {
                  values: sKeys
                    .filter(function(k) { return !usedSettingNames.has(choices.settings[k]); })
                    .map(function(k) { return { label: k, value: choices.settings[k] }; })
                };
              }
            },
            formatter: function(cell) {
              var rowData = cell.getRow().getData();
              if (rowData.param_type === "setting") {
                return settingsDisplay[cell.getValue()] || cell.getValue() || "";
              }
              return cell.getValue() || "";
            }
          },

          // Strategy column
          {
            title: "Strategy",
            field: "strategy",
            width: 100,
            editor: "list",
            editorParams: function(cell) {
              var rowData = cell.getRow().getData();
              var t = targeting.getTargeting(choices, rowData.name);
              if (!t.strategies) return { values: [] };
              var currentStrategy = cell.getValue();
              var currentGroup = rowData.group || "";
              var usedStrategies = new Set(
                cell.getTable().getData()
                  .filter(function(r) {
                    return r.param_type === "variable" &&
                      r.name === rowData.name &&
                      (r.group || "") === currentGroup &&
                      (r.strategy || "") !== currentStrategy;
                  })
                  .map(function(r) { return r.strategy || ""; })
              );
              return {
                values: t.strategies
                  .filter(function(sVal) { return !usedStrategies.has(sVal); })
                  .map(function(sVal) {
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
              var rowData = cell.getRow().getData();
              if (rowData.param_type === "setting") return false;
              var t = targeting.getTargeting(choices, rowData.name);
              return t.strategies !== null;
            },
            formatter: function(cell) {
              var rowData = cell.getRow().getData();
              if (rowData.param_type === "setting") return "\u2014";
              var t = targeting.getTargeting(choices, rowData.name);
              if (!t.strategies) return "\u2014";
              var val = cell.getValue();
              if (!val || val === "") return "";
              for (var sk = 0; sk < strategyKeys.length; sk++) {
                if (choices.strategies[strategyKeys[sk]] === val) {
                  return strategyKeys[sk];
                }
              }
              return val;
            }
          },

          // Group column
          {
            title: "Group",
            field: "group",
            width: 100,
            editor: "list",
            editorParams: function(cell) {
              var rowData = cell.getRow().getData();
              var t = targeting.getTargeting(choices, rowData.name);
              if (!t.groups) return { values: [] };
              var currentGroup = cell.getValue();
              var currentStrategy = rowData.strategy || "";
              var usedGroups = new Set(
                cell.getTable().getData()
                  .filter(function(r) {
                    return r.param_type === "variable" &&
                      r.name === rowData.name &&
                      (r.strategy || "") === currentStrategy &&
                      (r.group || "") !== currentGroup;
                  })
                  .map(function(r) { return r.group || ""; })
              );
              return {
                values: t.groups
                  .filter(function(gVal) { return !usedGroups.has(gVal); })
                  .map(function(gVal) {
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
              var rowData = cell.getRow().getData();
              if (rowData.param_type === "setting") return false;
              var t = targeting.getTargeting(choices, rowData.name);
              return t.groups !== null;
            },
            formatter: function(cell) {
              var rowData = cell.getRow().getData();
              if (rowData.param_type === "setting") return "\u2014";
              var t = targeting.getTargeting(choices, rowData.name);
              if (!t.groups) return "\u2014";
              var val = cell.getValue();
              if (!val || val === "") return "";
              for (var gk = 0; gk < groupKeys.length; gk++) {
                if (choices.groups[groupKeys[gk]] === val) {
                  return groupKeys[gk];
                }
              }
              return val;
            }
          },

          // Base Case column (read-only)
          {
            title: "Base Case",
            field: "_baseCase",
            widthGrow: 1,
            minWidth: 100,
            editor: false,
            headerSort: false,
            clipboard: false,
            formatter: function(cell) {
              var rowData = cell.getRow().getData();
              if (rowData.param_type === "setting") {
                var val = choices.settingValues[rowData.name];
                return val !== undefined ? String(val) : "";
              }
              var key = OQGrid.helpers.combo.compositeKey(rowData.name, rowData.strategy, rowData.group);
              return choices.variableFormulas[key] || "";
            }
          },

          // Method column
          {
            title: "Method",
            field: "type",
            width: 100,
            editor: "list",
            editorParams: {
              values: [
                { label: "Range", value: "range" },
                { label: "Radius", value: "radius" },
                { label: "Custom", value: "custom" }
              ]
            },
            formatter: function(cell) {
              var val = cell.getValue();
              if (val === "range") return "Range";
              if (val === "radius") return "Radius";
              if (val === "custom") return "Custom";
              return val || "";
            }
          },

          // Values column (popup editor)
          {
            title: "Values",
            field: "data",
            minWidth: 265,
            widthGrow: 2,
            editor: false,
            headerSort: false,
            formatter: function(cell) {
              var rowData = cell.getRow().getData();
              var text = OQGrid.editors.formatValuesDisplay(rowData.type, rowData.data);
              var container = document.createElement("div");
              container.style.display = "flex";
              container.style.alignItems = "center";
              container.style.gap = "4px";
              container.style.cursor = "pointer";
              container.style.width = "100%";

              var span = document.createElement("span");
              span.style.flex = "1";
              span.style.overflow = "hidden";
              span.style.textOverflow = "ellipsis";
              span.style.whiteSpace = "nowrap";
              span.textContent = text || "(click to edit)";
              if (!text) span.style.color = "#999";
              container.appendChild(span);

              var editIcon = document.createElement("span");
              editIcon.textContent = "\u270e";
              editIcon.style.fontSize = "0.75rem";
              editIcon.style.color = "#6c757d";
              container.appendChild(editIcon);

              return container;
            },
            cellClick: function(e, cell) {
              e.stopPropagation();
              if (activePopup) {
                activePopup.cleanup();
                activePopup = null;
              }
              var rowData = cell.getRow().getData();
              var analysis = getSelectedAnalysis();
              var twsaName = analysis ? analysis.name : "";
              var tbl = cell.getTable();

              activePopup = OQGrid.editors.createValuesPopup(cell, terms, suggestions,
                function onSave(result) {
                  activePopup = null;
                  cell.getRow().update({ data: result });
                  OQGrid.relayout(tbl);
                  saveGridToState(tbl);

                  // Dispatch action with flat fields
                  var d = cell.getRow().getData();
                  if (d.param_type === "variable") {
                    OQGrid.shiny.dispatchModelAction({
                      type: "edit_twsa_variable",
                      twsa_name: twsaName,
                      variable: d.name,
                      strategy: d.strategy || "",
                      group: d.group || "",
                      method_type: d.type || "radius",
                      min: result.min || null,
                      max: result.max || null,
                      radius: result.radius || null,
                      steps: result.steps || null,
                      values: result.values || null,
                      include_base_case: result.include_bc
                    });
                  } else {
                    OQGrid.shiny.dispatchModelAction({
                      type: "edit_twsa_setting",
                      twsa_name: twsaName,
                      setting: d.name,
                      method_type: d.type || "radius",
                      min: result.min || null,
                      max: result.max || null,
                      radius: result.radius || null,
                      steps: result.steps || null,
                      values: result.values || null,
                      include_base_case: result.include_bc
                    });
                  }
                  syncAllToShiny(tbl, controller.inputId);
                },
                function onCancel() {
                  activePopup = null;
                }
              );
            }
          }

          // Delete column is added automatically by grid-controller
        ];
      },

      // =====================================================================
      // Tabulator options overrides
      // =====================================================================
      tabulatorOptions: {
        clipboardCopyRowRange: "range",
        clipboardPasteParser: "range",
        clipboardPasteAction: "range",
        clipboardCopyConfig: { rowHeaders: false, columnHeaders: false },
        rowFormatter: function(row) {
          var data = row.getData();
          var cells = row.getCells();
          for (var ci = 0; ci < cells.length; ci++) {
            var field = cells[ci].getColumn().getField();
            if (field === "strategy" || field === "group") {
              var el = cells[ci].getElement();
              if (data.param_type === "setting") {
                el.style.color = "var(--bs-secondary, #6c757d)";
                el.style.fontStyle = "italic";
              } else {
                var t = targeting.getTargeting(row.getTable()._choices || {}, data.name);
                var isDisabled = (field === "strategy" && !t.strategies) ||
                                 (field === "group" && !t.groups);
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
      },

      // TWSA always has exactly 2 rows (X and Y), no manual add/remove
      addRow: { custom: true },

      // =====================================================================
      // Actions
      // =====================================================================
      actions: {
        // Serialize: full state sync sends ALL analyses, not just grid data
        serialize: function(data) {
          // This is called by sync handler but for TWSA we use syncAllToShiny instead.
          // Return current grid data as-is; the onInit replaces the sync mechanism.
          return data;
        },

      },

      // =====================================================================
      // Post cell-edit hook: dispatch model_action side-effects
      // =====================================================================
      onCellEdited: function(controller, cell) {
        var field = cell.getField();
        var data = cell.getRow().getData();
        var oldValue = cell.getOldValue();
        var choices = controller.table._choices;
        var settingsDisplay = OQGrid.utils.buildDisplayMap(choices.settings);
        var table = controller.table;
        var analysis = getSelectedAnalysis();
        var twsaName = analysis ? analysis.name : "";

        if (field === "name") {
          if (data.param_type === "setting") {
            cell.getRow().update({ display_name: settingsDisplay[data.name] || data.name, data: {} });
            var d3 = cell.getRow().getData();
            OQGrid.shiny.dispatchModelAction({
              type: "edit_twsa_setting",
              twsa_name: twsaName,
              setting: oldValue || "",
              new_setting: d3.name
            });
          } else {
            var oldStrategy = data.strategy || "";
            var oldGroup = data.group || "";
            var cid = combo.getComboId(cell);
            var usedCombos = combo.getUsedCombosForVariable(table, data.name, cid);
            cell.getRow().update({ display_name: data.name });
            targeting.applyTargetingDefaults(data, choices, usedCombos);
            cell.getRow().update({ strategy: data.strategy, group: data.group });
            combo.setRowCombo(table, cid, {
              type: "variable", name: data.name,
              strategy: data.strategy, group: data.group
            });
            var d4 = cell.getRow().getData();
            OQGrid.shiny.dispatchModelAction({
              type: "edit_twsa_variable",
              twsa_name: twsaName,
              variable: oldValue || "",
              new_variable: d4.name,
              strategy: oldStrategy,
              group: oldGroup,
              new_strategy: d4.strategy || "",
              new_group: d4.group || ""
            });
          }
        } else if (field === "type") {
          // Method changed — clear data
          cell.getRow().update({ data: {} });
          var d5 = cell.getRow().getData();
          if (d5.param_type === "variable") {
            OQGrid.shiny.dispatchModelAction({
              type: "edit_twsa_variable",
              twsa_name: twsaName,
              variable: d5.name,
              strategy: d5.strategy || "",
              group: d5.group || "",
              method_type: d5.type || "radius"
            });
          } else {
            OQGrid.shiny.dispatchModelAction({
              type: "edit_twsa_setting",
              twsa_name: twsaName,
              setting: d5.name,
              method_type: d5.type || "radius"
            });
          }
        } else if (field === "strategy" || field === "group") {
          var payload = {
            type: "edit_twsa_variable",
            twsa_name: twsaName,
            variable: data.name,
            strategy: field === "strategy" ? (oldValue || "") : (data.strategy || ""),
            group: field === "group" ? (oldValue || "") : (data.group || ""),
            axis: data.axis
          };
          if (field === "strategy") payload.new_strategy = data.strategy || "";
          if (field === "group") payload.new_group = data.group || "";
          OQGrid.shiny.dispatchModelAction(payload);
        }

        saveGridToState(table);
        OQGrid.relayout(table);
        syncAllToShiny(table, controller.inputId);
      },

      // =====================================================================
      // Post-delete hook
      // =====================================================================

      // =====================================================================
      // Post-init: build state, analysis list, two add buttons, etc.
      // =====================================================================
      onInit: function(controller) {
        var table = controller.table;
        var choices = controller.data;
        var inputId = controller.inputId;
        var settingsDisplay = OQGrid.utils.buildDisplayMap(choices.settings);

        // Store choices on table for rowFormatter and handlers
        table._choices = choices;

        // Find wrapper div (parent of container for analysis list etc.)
        var wrapperDiv = controller.containerDiv.closest(".twsa-params-wrapper") ||
                         controller.containerDiv.parentElement;

        // ---------------------------------------------------------------
        // Build state from initial TWSA data
        // ---------------------------------------------------------------
        var initialTwsa = choices.initialTwsa || [];
        _state.analyses = [];
        _state.nextId = 1;

        for (var ai = 0; ai < initialTwsa.length; ai++) {
          var tw = initialTwsa[ai];
          var parameters = [];

          // Variable params
          var varParams = tw.variable_params || [];
          for (var vi = 0; vi < varParams.length; vi++) {
            var v = varParams[vi];
            parameters.push({
              axis: v.axis || "x",
              param_type: "variable",
              name: v.name || "",
              display_name: v.name || "",
              strategy: v.strategy || "",
              group: v.group || "",
              type: v.type || "radius",
              data: v.data || {}
            });
          }

          // Setting params
          var setParams = tw.setting_params || [];
          for (var si = 0; si < setParams.length; si++) {
            var s = setParams[si];
            parameters.push({
              axis: s.axis || "x",
              param_type: "setting",
              name: s.name || "",
              display_name: settingsDisplay[s.name] || s.name || "",
              strategy: "",
              group: "",
              type: s.type || "radius",
              data: s.data || {}
            });
          }

          _state.analyses.push({
            id: _state.nextId++,
            name: tw.name || "TWSA " + _state.nextId,
            description: tw.description || "",
            parameters: parameters
          });
        }

        // Select first analysis if any
        if (_state.analyses.length > 0) {
          _state.selectedId = _state.analyses[0].id;
        } else {
          _state.selectedId = null;
        }

        // ---------------------------------------------------------------
        // Analysis list UI
        // ---------------------------------------------------------------
        var listContainer = wrapperDiv.querySelector(".twsa-list");

        if (listContainer) {
          renderAnalysisList(listContainer, table, inputId);
        }

        // Initial visibility check
        updateGridVisibility(wrapperDiv);

        // Load selected analysis into grid, ensuring exactly 2 rows
        var selected = getSelectedAnalysis();
        function ensureTwoRows(params) {
          while (params.length < 2) {
            var axis = params.length === 0 ? "x" : "y";
            var varName = (choices.variables && choices.variables.length > params.length)
              ? choices.variables[params.length]
              : (choices.variables && choices.variables.length > 0 ? choices.variables[0] : "");
            params.push({
              axis: axis,
              param_type: "variable",
              name: varName,
              display_name: varName,
              strategy: "",
              group: "",
              type: "radius",
              data: {}
            });
          }
          return params;
        }
        function loadSelectedData() {
          if (!selected) return;
          var params = ensureTwoRows(selected.parameters || []);
          selected.parameters = params;
          try {
            table.setData(params);
            OQGrid.relayout(table);
            combo.initRowCombos(table, params);
            syncAllToShiny(table, inputId);
          } catch (e) {
            // Tabulator ops can fail on hidden tabs;
            // ResizeObserver will relayout when visible
          }
        }
        // Try immediate load; also defer via tableBuilt for async init
        loadSelectedData();
        table.on("tableBuilt", function() {
          if (selected && table.getData().length === 0) {
            loadSelectedData();
          }
        });
        if (!selected) {
          combo.initRowCombos(table, []);
        }

        // ---------------------------------------------------------------
        // Add TWSA button — opens modal via Shiny
        // ---------------------------------------------------------------
        var addTwsaBtn = wrapperDiv.querySelector(".twsa-add-btn");
        if (addTwsaBtn) {
          var newAddBtn = addTwsaBtn.cloneNode(true);
          addTwsaBtn.parentNode.replaceChild(newAddBtn, addTwsaBtn);
          newAddBtn.addEventListener("click", function() {
            saveGridToState(table);
            OQGrid.shiny.dispatch("show_add_twsa_modal", {
              nonce: Date.now()
            });
          });
        }

        // Remove TWSA button
        var removeTwsaBtn = wrapperDiv.querySelector(".twsa-remove-btn");
        if (removeTwsaBtn) {
          var newRemBtn = removeTwsaBtn.cloneNode(true);
          removeTwsaBtn.parentNode.replaceChild(newRemBtn, removeTwsaBtn);
          newRemBtn.addEventListener("click", function() {
            var sel = getSelectedAnalysis();
            if (!sel) return;
            OQGrid.shiny.dispatch("remove_twsa_action", {
              nonce: Date.now(),
              name: sel.name
            });
          });
        }

        // ---------------------------------------------------------------
        // Clipboard paste handler
        // ---------------------------------------------------------------
        table.on("clipboardPasted", function(clipboard, rowData, rows) {
          var pasteSettingsDisplay = OQGrid.utils.buildDisplayMap(choices.settings);
          rows.forEach(function(row) {
            var d = row.getData();
            if (!d.display_name) {
              if (d.param_type === "setting") {
                row.update({ display_name: pasteSettingsDisplay[d.name] || d.name });
              } else {
                row.update({ display_name: d.name || "" });
              }
            }
          });
          saveGridToState(table);
          syncAllToShiny(table, inputId);
        });

        // ---------------------------------------------------------------
        // Run button
        // ---------------------------------------------------------------
        var runBtn = wrapperDiv.querySelector(".twsa-run-btn");
        if (runBtn) {
          var newRunBtn = runBtn.cloneNode(true);
          runBtn.parentNode.replaceChild(newRunBtn, runBtn);
          newRunBtn.addEventListener("click", function() {
            saveGridToState(table);
            OQGrid.shiny.dispatch("run_twsa_action", {
              nonce: Date.now()
            });
          });
        }

        // ---------------------------------------------------------------
        // Initial sync
        // ---------------------------------------------------------------
        syncAllToShiny(table, inputId);
      }
    };
  });

  // =========================================================================
  // Expose state helpers for external use (e.g., Shiny message handlers)
  // =========================================================================
  OQGrid.twsaState = {
    getState: function() { return _state; },
    getSelectedAnalysis: getSelectedAnalysis,
    getAnalysisById: getAnalysisById,
    addAnalysis: addAnalysis,
    removeAnalysis: removeAnalysis,
    saveGridToState: saveGridToState,
    loadStateToGrid: loadStateToGrid,
    updateGridVisibility: updateGridVisibility,
    syncAllToShiny: syncAllToShiny,
    renderAnalysisList: renderAnalysisList
  };
})();
