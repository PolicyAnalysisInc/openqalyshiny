/* OQGrid Spec — DSA Parameters Table */
(function() {
  "use strict";

  var targeting = OQGrid.helpers.targeting;
  var combo = OQGrid.helpers.combo;
  var exclusions = OQGrid.helpers.exclusions;

  OQGrid.registerSpec("dsa", function() {
    return {
      name: "dsa",
      containerSelector: ".dsa-params-container",
      dispatchMode: "sync",

      // =====================================================================
      // Parse data attributes from container element
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

        // Ensure display_name is set on initial data
        var initial = choices.initial || [];
        for (var i = 0; i < initial.length; i++) {
          var row = initial[i];
          if (!row.display_name) {
            if (row.type === "setting") {
              row.display_name = settingsDisplay[row.name] || row.name;
            } else {
              row.display_name = row.name;
            }
          }
        }

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
            },
            cellClick: function(e, cell) {
              var rowData = cell.getRow().getData();
              if (rowData.type === "setting" || !targeting.getTargeting(choices, rowData.name).strategies) {
                cell.getElement().style.color = "var(--bs-secondary, #6c757d)";
                cell.getElement().style.fontStyle = "italic";
              }
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
            },
            cellClick: function(e, cell) {
              var rowData = cell.getRow().getData();
              if (rowData.type === "setting" || !targeting.getTargeting(choices, rowData.name).groups) {
                cell.getElement().style.color = "var(--bs-secondary, #6c757d)";
                cell.getElement().style.fontStyle = "italic";
              }
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

          // Low column (formula editor)
          {
            title: "Low",
            field: "low",
            widthGrow: 1,
            minWidth: 450,
            editor: OQGrid.editors.formula(choices.terms, choices.suggestions)
          },

          // High column (formula editor)
          {
            title: "High",
            field: "high",
            widthGrow: 1,
            minWidth: 450,
            editor: OQGrid.editors.formula(choices.terms, choices.suggestions)
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
              if (data.type === "setting") {
                el.style.color = "var(--bs-secondary, #6c757d)";
                el.style.fontStyle = "italic";
              } else {
                // Need to access choices from closure
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

      // =====================================================================
      // Custom add row setup (two buttons)
      // =====================================================================
      addRow: { custom: true },

      // =====================================================================
      // Actions
      // =====================================================================
      actions: {
        // Serialize grid data for sync dispatch
        serialize: function(data) {
          return data.map(function(d) {
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
        },

        // Remove action — used by delete column
        remove: function(row) {
          // Return non-null to allow deletion (sync handler will dispatch data)
          return { type: "remove" };
        }
      },

      // =====================================================================
      // Post-delete hook: dispatch model_action side-effect + update buttons
      // =====================================================================
      onRowDeleted: function(controller, rowData) {
        var table = controller.table;
        // Dispatch side-effect via model_action
        if (rowData.type === "variable") {
          OQGrid.shiny.dispatchModelAction({
            type: "remove_dsa_variable",
            variable: rowData.name || "",
            strategy: rowData.strategy || "",
            group: rowData.group || ""
          });
        } else if (rowData.type === "setting") {
          OQGrid.shiny.dispatchModelAction({
            type: "remove_dsa_setting",
            setting: rowData.name || ""
          });
        }
        combo.updateButtonStates(table, table._choices, table._addVarBtn, table._addSettingBtn);
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

        if (field === "name") {
          if (data.type === "setting") {
            var newBcVal = targeting.getBaseCase(choices, { type: "setting", name: data.name });
            cell.getRow().update({
              display_name: settingsDisplay[data.name] || data.name,
              low: newBcVal,
              high: newBcVal
            });
            var d3 = cell.getRow().getData();
            OQGrid.shiny.dispatchModelAction({
              type: "edit_dsa_setting",
              setting: oldValue || "",
              new_setting: d3.name,
              low: d3.low || "",
              high: d3.high || "",
              display_name: d3.display_name || d3.name
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
              type: "edit_dsa_variable",
              variable: oldValue || "",
              strategy: oldStrategy,
              group: oldGroup,
              new_variable: d4.name,
              new_strategy: d4.strategy || "",
              new_group: d4.group || "",
              low: d4.low || "",
              high: d4.high || "",
              display_name: d4.display_name || d4.name
            });
          }
        } else if (field === "low" || field === "high") {
          if (data.type === "variable") {
            var editPayload = {
              type: "edit_dsa_variable",
              variable: data.name,
              strategy: data.strategy || "",
              group: data.group || ""
            };
            editPayload[field] = data[field] || "";
            OQGrid.shiny.dispatchModelAction(editPayload);
          } else {
            var editPayload2 = { type: "edit_dsa_setting", setting: data.name };
            editPayload2[field] = data[field] || "";
            OQGrid.shiny.dispatchModelAction(editPayload2);
          }
        } else if (field === "strategy" || field === "group") {
          var editPayload3 = {
            type: "edit_dsa_variable",
            variable: data.name,
            strategy: field === "strategy" ? (oldValue || "") : (data.strategy || ""),
            group: field === "group" ? (oldValue || "") : (data.group || "")
          };
          if (field === "strategy") editPayload3.new_strategy = data.strategy || "";
          if (field === "group") editPayload3.new_group = data.group || "";
          OQGrid.shiny.dispatchModelAction(editPayload3);
        }

        // Re-sync after side-effects
        OQGrid.actions.sync.syncData(controller);
      },

      // =====================================================================
      // Post-init: create two add buttons + init combo tracking
      // =====================================================================
      onInit: function(controller) {
        var table = controller.table;
        var choices = controller.data;
        var inputId = controller.inputId;

        // Store choices on table for rowFormatter and delete handler
        table._choices = choices;

        // Init combo tracking
        combo.initRowCombos(table, choices.initial);

        // Wire existing R-created buttons
        var parent = controller.containerDiv.parentNode;
        var addVarBtn = parent ? parent.querySelector(".dsa-add-variable-btn") : null;
        var addSettingBtn = parent ? parent.querySelector(".dsa-add-setting-btn") : null;

        if (addVarBtn) addVarBtn.addEventListener("click", function() {
          var unused = combo.findFirstUnusedVariable(table, choices);
          if (!unused) return;
          var newRow = {
            type: "variable",
            name: unused.name,
            display_name: unused.name,
            strategy: unused.strategy,
            group: unused.group,
            low: "bc",
            high: "bc"
          };
          OQGrid.shiny.dispatchModelAction({
            type: "add_dsa_variable",
            variable: newRow.name,
            low: "bc", high: "bc",
            strategy: newRow.strategy || "",
            group: newRow.group || "",
            display_name: newRow.display_name || newRow.name
          });
          table.addRow(newRow).then(function(row) {
            OQGrid.relayout(table);
            combo.addRowCombo(table, row, {
              type: "variable", name: newRow.name,
              strategy: newRow.strategy, group: newRow.group
            });
            OQGrid.actions.sync.syncData(controller);
            combo.updateButtonStates(table, choices, addVarBtn, addSettingBtn);
          });
        });

        if (addSettingBtn) addSettingBtn.addEventListener("click", function() {
          var unused = combo.findFirstUnusedSetting(table, choices);
          if (!unused) return;
          var bcVal = targeting.getBaseCase(choices, { type: "setting", name: unused.name });
          var newRow = {
            type: "setting",
            name: unused.name,
            display_name: unused.display,
            strategy: "",
            group: "",
            low: bcVal,
            high: bcVal
          };
          OQGrid.shiny.dispatchModelAction({
            type: "add_dsa_setting",
            setting: newRow.name,
            low: bcVal, high: bcVal,
            display_name: newRow.display_name || newRow.name
          });
          table.addRow(newRow).then(function() {
            OQGrid.relayout(table);
            OQGrid.actions.sync.syncData(controller);
            combo.updateButtonStates(table, choices, addVarBtn, addSettingBtn);
          });
        });

        // Store refs for delete handler
        table._addVarBtn = addVarBtn;
        table._addSettingBtn = addSettingBtn;

        // Initial button state
        combo.updateButtonStates(table, choices, addVarBtn, addSettingBtn);

        // Initial sync
        OQGrid.actions.sync.syncData(controller);

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
          OQGrid.actions.sync.syncData(controller);
        });

        // Run button
        var runBtn = document.querySelector(".dsa-run-btn");
        if (runBtn) {
          runBtn.addEventListener("click", function() {
            var gridData = table.getData().map(function(d) {
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
            OQGrid.shiny.dispatch("run_dsa_action", {
              nonce: Date.now(),
              params: gridData
            });
          });
        }
      }
    };
  });
})();
