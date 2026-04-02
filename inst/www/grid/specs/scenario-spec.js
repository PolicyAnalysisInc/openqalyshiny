/* OQGrid Spec — Scenario Parameters Table (multi-scenario state machine) */
(function() {
  "use strict";

  var targeting = OQGrid.helpers.targeting;
  var combo = OQGrid.helpers.combo;
  var exclusions = OQGrid.helpers.exclusions;

  // =========================================================================
  // Scenario tab state machine (lives inside the spec, not in the grid system)
  // =========================================================================
  var _state = {
    scenarios: [],
    selectedId: null,
    nextId: 1
  };

  function getSelectedScenario() {
    for (var i = 0; i < _state.scenarios.length; i++) {
      if (_state.scenarios[i].id === _state.selectedId) return _state.scenarios[i];
    }
    return null;
  }

  function getScenarioById(id) {
    for (var i = 0; i < _state.scenarios.length; i++) {
      if (_state.scenarios[i].id === id) return _state.scenarios[i];
    }
    return null;
  }

  function addScenario(name) {
    var s = {
      id: _state.nextId++,
      name: name || "Scenario " + _state.nextId,
      description: "",
      overrides: []
    };
    _state.scenarios.push(s);
    _state.selectedId = s.id;
    return s;
  }

  function removeScenario(id) {
    var idx = -1;
    for (var i = 0; i < _state.scenarios.length; i++) {
      if (_state.scenarios[i].id === id) { idx = i; break; }
    }
    if (idx === -1) return;
    _state.scenarios.splice(idx, 1);
    if (_state.selectedId === id) {
      if (_state.scenarios.length > 0) {
        var newIdx = Math.min(idx, _state.scenarios.length - 1);
        _state.selectedId = _state.scenarios[newIdx].id;
      } else {
        _state.selectedId = null;
      }
    }
  }

  function saveGridToState(table) {
    var scenario = getSelectedScenario();
    if (!scenario || !table) return;
    scenario.overrides = table.getData().map(function(d) {
      return {
        type: d.type || "variable",
        name: d.name || "",
        display_name: d.display_name || d.name || "",
        strategy: d.strategy || "",
        group: d.group || "",
        value: d.value || ""
      };
    });
  }

  function loadStateToGrid(table, id) {
    var scenario = getScenarioById(id);
    _state.selectedId = id;
    if (!scenario || !table) return;
    table.setData(scenario.overrides || []);
    OQGrid.relayout(table);
  }

  // =========================================================================
  // Grid visibility — hide grid panel when no scenarios exist
  // =========================================================================
  function updateGridVisibility(wrapperDiv) {
    var gridPanel = wrapperDiv ? wrapperDiv.querySelector(".scenario-grid-panel") : null;
    if (!gridPanel) return;
    if (_state.scenarios.length === 0 || _state.selectedId === null) {
      gridPanel.style.display = "none";
    } else {
      gridPanel.style.display = "";
    }
  }

  // =========================================================================
  // Serialize all scenarios for Shiny (full sync)
  // =========================================================================
  function syncAllToShiny(table, inputId) {
    saveGridToState(table);
    var payload = _state.scenarios.map(function(s) {
      var variableOverrides = [];
      var settingOverrides = [];
      (s.overrides || []).forEach(function(o) {
        if (o.type === "setting") {
          settingOverrides.push({ name: o.name, value: o.value });
        } else {
          variableOverrides.push({
            name: o.name,
            value: o.value,
            strategy: o.strategy || "",
            group: o.group || ""
          });
        }
      });
      return {
        name: s.name,
        description: s.description || "",
        variable_overrides: variableOverrides,
        setting_overrides: settingOverrides
      };
    });
    OQGrid.shiny.dispatch(inputId, payload);
  }

  // =========================================================================
  // Scenario list renderer
  // =========================================================================
  function renderScenarioList(listContainer, table, inputId) {
    while (listContainer.firstChild) {
      listContainer.removeChild(listContainer.firstChild);
    }

    _state.scenarios.forEach(function(s) {
      var item = document.createElement("div");
      item.className = "scenario-list-item" + (s.id === _state.selectedId ? " scenario-list-item--active" : "");
      item.setAttribute("data-id", s.id);

      var nameSpan = document.createElement("div");
      nameSpan.className = "scenario-list-item-name";
      nameSpan.textContent = s.name;
      item.appendChild(nameSpan);

      if (s.description) {
        var descSpan = document.createElement("div");
        descSpan.className = "scenario-list-item-desc";
        descSpan.textContent = s.description;
        item.appendChild(descSpan);
      }

      // Click to select
      item.addEventListener("click", function() {
        if (s.id === _state.selectedId) return;
        saveGridToState(table);
        loadStateToGrid(table, s.id);
        renderScenarioList(listContainer, table, inputId);
        syncAllToShiny(table, inputId);
      });

      // Double-click to edit via modal
      item.addEventListener("dblclick", function(e) {
        e.stopPropagation();
        saveGridToState(table);
        OQGrid.shiny.dispatch("show_edit_scenario_modal", {
          nonce: Date.now(),
          id: s.id,
          name: s.name,
          description: s.description || ""
        });
      });

      listContainer.appendChild(item);
    });
  }

  // =========================================================================
  // Spec registration
  // =========================================================================
  OQGrid.registerSpec("scenario", function() {
    return {
      name: "scenario",
      containerSelector: ".scenario-params-container",
      dispatchMode: "sync",

      // =====================================================================
      // Parse data attributes from container element
      // =====================================================================
      parseData: function(el) {
        var variableTargeting = OQGrid.utils.parseDataAttr(el, "variableTargeting", {});
        return {
          initial: [], // scenarios use initialScenarios, not initial rows
          initialScenarios: OQGrid.utils.parseDataAttr(el, "initialScenarios", []),
          variables: OQGrid.utils.parseDataAttr(el, "variables", []),
          settings: OQGrid.utils.parseDataAttr(el, "settings", {}),
          strategies: OQGrid.utils.parseDataAttr(el, "strategies", {}),
          groups: OQGrid.utils.parseDataAttr(el, "groups", {}),
          variableTargeting: variableTargeting,
          variableFormulas: OQGrid.utils.parseDataAttr(el, "variableFormulas", {}),
          settingValues: OQGrid.utils.parseDataAttr(el, "settingValues", {}),
          terms: OQGrid.utils.parseDataAttr(el, "terms", null),
          suggestions: OQGrid.utils.parseDataAttr(el, "suggestions", null)
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

        return [
          // Type column (read-only)
          {
            title: "Type",
            field: "type",
            width: 100,
            formatter: function(cell) {
              var val = cell.getValue();
              if (val === "variable") return "Variable";
              if (val === "setting") return "Setting";
              return val || "";
            }
          },

          // Name column (list editor with dynamic editorParams)
          {
            title: "Name",
            field: "name",
            widthGrow: 1,
            minWidth: 120,
            editor: "list",
            editorParams: function(cell) {
              var rowData = cell.getRow().getData();
              var currentName = cell.getValue();
              if (rowData.type === "variable") {
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
                var usedSettingNames = new Set(
                  cell.getTable().getData()
                    .filter(function(r) { return r.type === "setting" && r.name !== currentName; })
                    .map(function(r) { return r.name; })
                );
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
              if (rowData.type === "setting") {
                return settingsDisplay[cell.getValue()] || cell.getValue() || "";
              }
              return cell.getValue() || "";
            }
          },

          // Strategy column (conditional on targeting)
          {
            title: "Strategy",
            field: "strategy",
            widthGrow: 1,
            minWidth: 120,
            editor: "list",
            editorParams: function(cell) {
              var rowData = cell.getRow().getData();
              var t = targeting.getTargeting(choices, rowData.name);
              if (!t.strategies) return { values: [] };
              return {
                values: t.strategies.map(function(sVal) {
                  return { label: targeting.strategyLabel(choices, sVal) || sVal, value: sVal };
                })
              };
            },
            editable: function(cell) {
              var rowData = cell.getRow().getData();
              if (rowData.type === "setting") return false;
              var t = targeting.getTargeting(choices, rowData.name);
              return t.strategies !== null;
            },
            formatter: function(cell) {
              var rowData = cell.getRow().getData();
              if (rowData.type === "setting") return "\u2014";
              var t = targeting.getTargeting(choices, rowData.name);
              if (!t.strategies) return "\u2014";
              var val = cell.getValue();
              if (!val || val === "") return "";
              return targeting.strategyLabel(choices, val) || val;
            }
          },

          // Group column (conditional on targeting)
          {
            title: "Group",
            field: "group",
            widthGrow: 1,
            minWidth: 120,
            editor: "list",
            editorParams: function(cell) {
              var rowData = cell.getRow().getData();
              var t = targeting.getTargeting(choices, rowData.name);
              if (!t.groups) return { values: [] };
              return {
                values: t.groups.map(function(gVal) {
                  return { label: targeting.groupLabel(choices, gVal) || gVal, value: gVal };
                })
              };
            },
            editable: function(cell) {
              var rowData = cell.getRow().getData();
              if (rowData.type === "setting") return false;
              var t = targeting.getTargeting(choices, rowData.name);
              return t.groups !== null;
            },
            formatter: function(cell) {
              var rowData = cell.getRow().getData();
              if (rowData.type === "setting") return "\u2014";
              var t = targeting.getTargeting(choices, rowData.name);
              if (!t.groups) return "\u2014";
              var val = cell.getValue();
              if (!val || val === "") return "";
              return targeting.groupLabel(choices, val) || val;
            }
          },

          // Base Case column (computed, read-only)
          {
            title: "Base Case",
            field: "_baseCase",
            widthGrow: 1,
            minWidth: 120,
            editor: false,
            headerSort: false,
            clipboard: false,
            formatter: function(cell) {
              var rowData = cell.getRow().getData();
              return targeting.getBaseCase(choices, rowData);
            }
          },

          // Value column (single formula, replaces Low/High)
          {
            title: "Value",
            field: "value",
            widthGrow: 2,
            minWidth: 450,
            editor: OQGrid.editors.formula(choices.terms, choices.suggestions)
          }

          // Delete column is added automatically by grid-controller
        ];
      },

      // =====================================================================
      // Tabulator options overrides
      // =====================================================================
      tabulatorOptions: function(data) {
        var choices = data;
        return {
          clipboardCopyRowRange: "range",
          clipboardPasteParser: "range",
          clipboardPasteAction: "range",
          clipboardCopyConfig: { rowHeaders: false, columnHeaders: false },
          data: getSelectedScenario() ? getSelectedScenario().overrides : [],
          rowFormatter: function(row) {
            var rowData = row.getData();
            var cells = row.getCells();
            for (var ci = 0; ci < cells.length; ci++) {
              var field = cells[ci].getColumn().getField();
              if (field === "strategy" || field === "group") {
                var el = cells[ci].getElement();
                if (rowData.type === "setting") {
                  el.style.color = "var(--bs-secondary, #6c757d)";
                  el.style.fontStyle = "italic";
                } else {
                  var t = targeting.getTargeting(choices, rowData.name);
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
        };
      },

      // =====================================================================
      // Custom add row setup (two buttons + scenario management)
      // =====================================================================
      addRow: { custom: true },

      // =====================================================================
      // Actions
      // =====================================================================
      actions: {
        // Serialize is not used for normal sync — we use syncAllToShiny instead.
        // But we need remove for the delete column.
        remove: function(row) {
          return { type: "remove" };
        }
      },

      // =====================================================================
      // Post-delete hook: dispatch model_action side-effect + update buttons
      // =====================================================================
      onRowDeleted: function(controller, rowData) {
        var table = controller.table;
        var scenario = getSelectedScenario();
        saveGridToState(table);
        if (scenario) {
          if (rowData.type === "variable") {
            OQGrid.shiny.dispatchModelAction({
              type: "remove_scenario_variable",
              scenario: scenario.name,
              variable: rowData.name || "",
              strategy: rowData.strategy || "",
              group: rowData.group || ""
            });
          } else if (rowData.type === "setting") {
            OQGrid.shiny.dispatchModelAction({
              type: "remove_scenario_setting",
              scenario: scenario.name,
              setting: rowData.name || ""
            });
          }
        }
        combo.updateButtonStates(table, table._choices, table._addVarBtn, table._addSettingBtn);
      },

      // =====================================================================
      // Post cell-edit hook: dispatch model_action side-effects + save state
      // =====================================================================
      onCellEdited: function(controller, cell) {
        var field = cell.getField();
        var data = cell.getRow().getData();
        var oldValue = cell.getOldValue();
        var choices = controller.table._choices;
        var settingsDisplay = OQGrid.utils.buildDisplayMap(choices.settings);
        var scenario = getSelectedScenario();
        var scenarioName = scenario ? scenario.name : "";

        if (field === "name") {
          if (data.type === "setting") {
            var newBcVal = targeting.getBaseCase(choices, { type: "setting", name: data.name });
            cell.getRow().update({
              display_name: settingsDisplay[data.name] || data.name,
              value: newBcVal
            });
            var d3 = cell.getRow().getData();
            OQGrid.shiny.dispatchModelAction({
              type: "edit_scenario_setting",
              scenario: scenarioName,
              setting: oldValue || "",
              new_setting: d3.name,
              value: d3.value || ""
            });
          } else {
            var oldStrategy = data.strategy || "";
            var oldGroup = data.group || "";
            var cid = combo.getComboId(cell);
            var usedCombos = combo.getUsedCombosForVariable(cell.getTable(), data.name, cid);
            cell.getRow().update({ display_name: data.name });
            targeting.applyTargetingDefaults(data, choices, usedCombos);
            cell.getRow().update({ strategy: data.strategy, group: data.group });
            combo.setRowCombo(cell.getTable(), cid, {
              type: "variable", name: data.name,
              strategy: data.strategy, group: data.group
            });
            var d4 = cell.getRow().getData();
            OQGrid.shiny.dispatchModelAction({
              type: "edit_scenario_variable",
              scenario: scenarioName,
              variable: oldValue || "",
              strategy: oldStrategy,
              group: oldGroup,
              new_variable: d4.name,
              new_strategy: d4.strategy || "",
              new_group: d4.group || "",
              value: d4.value || ""
            });
          }
        } else if (field === "value") {
          if (data.type === "variable") {
            OQGrid.shiny.dispatchModelAction({
              type: "edit_scenario_variable",
              scenario: scenarioName,
              variable: data.name,
              strategy: data.strategy || "",
              group: data.group || "",
              value: data.value || ""
            });
          } else {
            OQGrid.shiny.dispatchModelAction({
              type: "edit_scenario_setting",
              scenario: scenarioName,
              setting: data.name,
              value: data.value || ""
            });
          }
        } else if (field === "strategy" || field === "group") {
          var payload = {
            type: "edit_scenario_variable",
            scenario: scenarioName,
            variable: data.name,
            strategy: field === "strategy" ? (oldValue || "") : (data.strategy || ""),
            group: field === "group" ? (oldValue || "") : (data.group || "")
          };
          if (field === "strategy") payload.new_strategy = data.strategy || "";
          if (field === "group") payload.new_group = data.group || "";
          OQGrid.shiny.dispatchModelAction(payload);
        }

        // Save to scenario state and re-sync
        saveGridToState(controller.table);
      },

      // =====================================================================
      // Post-init: scenario state machine + buttons + combo tracking
      // =====================================================================
      onInit: function(controller) {
        var table = controller.table;
        var choices = controller.data;
        var wrapperDiv = controller.containerDiv.closest(".scenario-params-wrapper") ||
                         controller.containerDiv.parentElement;
        var inputId = controller.inputId;

        // Store choices on table
        table._choices = choices;

        // ---------------------------------------------------------------
        // Wire DOM buttons first (before Tabulator ops that can throw
        // when the container is on a hidden tab)
        // ---------------------------------------------------------------

        // Wire add-scenario button (opens modal via Shiny)
        var addScenarioBtn = wrapperDiv.querySelector(".scenario-add-btn");
        if (addScenarioBtn) {
          var newAddBtn = addScenarioBtn.cloneNode(true);
          addScenarioBtn.parentNode.replaceChild(newAddBtn, addScenarioBtn);
          newAddBtn.addEventListener("click", function() {
            saveGridToState(table);
            OQGrid.shiny.dispatch("show_add_scenario_modal", {
              nonce: Date.now()
            });
          });
        }

        // Wire remove-scenario button
        var removeScenarioBtn = wrapperDiv.querySelector(".scenario-remove-btn");
        if (removeScenarioBtn) {
          var newRemBtn = removeScenarioBtn.cloneNode(true);
          removeScenarioBtn.parentNode.replaceChild(newRemBtn, removeScenarioBtn);
          newRemBtn.addEventListener("click", function() {
            var selected = getSelectedScenario();
            if (!selected) return;
            OQGrid.shiny.dispatch("remove_scenario_action", {
              nonce: Date.now(),
              name: selected.name
            });
          });
        }

        // Wire existing R-created buttons
        var addVarBtn = wrapperDiv.querySelector(".scenario-add-variable-btn") || null;
        var addSettingBtn = wrapperDiv.querySelector(".scenario-add-setting-btn") || null;

        if (addVarBtn) addVarBtn.addEventListener("click", function() {
          var scenario = getSelectedScenario();
          if (!scenario) return;
          var unused = combo.findFirstUnusedVariable(table, choices);
          if (!unused) return;
          var newRow = {
            type: "variable",
            name: unused.name,
            display_name: unused.name,
            strategy: unused.strategy,
            group: unused.group,
            value: "bc"
          };
          OQGrid.shiny.dispatchModelAction({
            type: "add_scenario_variable",
            scenario: scenario.name,
            variable: newRow.name,
            value: "bc",
            strategy: newRow.strategy || "",
            group: newRow.group || ""
          });
          table.addRow(newRow).then(function(row) {
            OQGrid.relayout(table);
            combo.addRowCombo(table, row, {
              type: "variable", name: newRow.name,
              strategy: newRow.strategy, group: newRow.group
            });
            saveGridToState(table);
            combo.updateButtonStates(table, choices, addVarBtn, addSettingBtn);
          });
        });

        if (addSettingBtn) addSettingBtn.addEventListener("click", function() {
          var scenario = getSelectedScenario();
          if (!scenario) return;
          var unused = combo.findFirstUnusedSetting(table, choices);
          if (!unused) return;
          var bcVal = targeting.getBaseCase(choices, { type: "setting", name: unused.name });
          var newRow = {
            type: "setting",
            name: unused.name,
            display_name: unused.display,
            strategy: "",
            group: "",
            value: bcVal
          };
          OQGrid.shiny.dispatchModelAction({
            type: "add_scenario_setting",
            scenario: scenario.name,
            setting: newRow.name,
            value: bcVal
          });
          table.addRow(newRow).then(function() {
            OQGrid.relayout(table);
            saveGridToState(table);
            combo.updateButtonStates(table, choices, addVarBtn, addSettingBtn);
          });
        });

        // Store refs for delete handler
        table._addVarBtn = addVarBtn;
        table._addSettingBtn = addSettingBtn;

        // Clipboard paste handler
        table.on("clipboardPasted", function(clipboard, rowData, rows) {
          var pasteSettingsDisplay = OQGrid.utils.buildDisplayMap(choices.settings);
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
          syncAllToShiny(table, inputId);
        });

        // Run button — bundles all scenarios with nonce
        var runBtn = wrapperDiv.querySelector(".scenario-run-btn");
        if (runBtn) {
          var newRunBtn = runBtn.cloneNode(true);
          runBtn.parentNode.replaceChild(newRunBtn, runBtn);
          newRunBtn.addEventListener("click", function() {
            saveGridToState(table);
            var scenarios = _state.scenarios.map(function(s) {
              var variableOverrides = [];
              var settingOverrides = [];
              (s.overrides || []).forEach(function(o) {
                if (o.type === "setting") {
                  settingOverrides.push({ name: o.name, value: o.value });
                } else {
                  variableOverrides.push({
                    name: o.name,
                    value: o.value,
                    strategy: o.strategy || "",
                    group: o.group || ""
                  });
                }
              });
              return {
                name: s.name,
                description: s.description || "",
                variable_overrides: variableOverrides,
                setting_overrides: settingOverrides
              };
            });
            OQGrid.shiny.dispatch("run_scenario_action", {
              nonce: Date.now(),
              scenarios: scenarios
            });
          });
        }

        // ---------------------------------------------------------------
        // Build state and load data into grid (may fail on hidden tabs
        // due to Tabulator needing visible elements for layout)
        // ---------------------------------------------------------------
        var settingsDisplay = OQGrid.utils.buildDisplayMap(choices.settings);
        _state.scenarios = [];
        _state.nextId = 1;

        var initialScenarios = choices.initialScenarios || [];
        for (var si = 0; si < initialScenarios.length; si++) {
          var sc = initialScenarios[si];
          var overrides = [];

          // Variable overrides
          var varOvs = sc.variable_overrides || [];
          for (var vi = 0; vi < varOvs.length; vi++) {
            var v = varOvs[vi];
            overrides.push({
              type: "variable",
              name: v.name || "",
              display_name: v.name || "",
              strategy: v.strategy || "",
              group: v.group || "",
              value: String(v.value || "")
            });
          }

          // Setting overrides
          var setOvs = sc.setting_overrides || [];
          for (var sti = 0; sti < setOvs.length; sti++) {
            var st = setOvs[sti];
            overrides.push({
              type: "setting",
              name: st.name || "",
              display_name: settingsDisplay[st.name] || st.name || "",
              strategy: "",
              group: "",
              value: String(st.value || "")
            });
          }

          _state.scenarios.push({
            id: _state.nextId++,
            name: sc.name || "Scenario " + _state.nextId,
            description: sc.description || "",
            overrides: overrides
          });
        }

        // Select first scenario if any exist
        if (_state.scenarios.length > 0) {
          _state.selectedId = _state.scenarios[0].id;
        } else {
          _state.selectedId = null;
        }

        // Render scenario list
        var listContainer = wrapperDiv.querySelector(".scenario-list");
        if (listContainer) {
          renderScenarioList(listContainer, table, inputId);
        }

        // Initial visibility check
        updateGridVisibility(wrapperDiv);

        // Load selected scenario data into grid (can throw on hidden tabs)
        var selectedScenario = getSelectedScenario();
        try {
          if (selectedScenario) {
            table.setData(selectedScenario.overrides || []);
            OQGrid.relayout(table);
          }
          combo.initRowCombos(table, selectedScenario ? selectedScenario.overrides : []);
          combo.updateButtonStates(table, choices, addVarBtn, addSettingBtn);
          syncAllToShiny(table, inputId);
        } catch (e) {
          // Tabulator layout ops fail when container is hidden;
          // the ResizeObserver in grid-controller will relayout when visible
        }
      }
    };
  });
})();
