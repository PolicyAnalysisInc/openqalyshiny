/* OQGrid Spec — Threshold Analysis Parameters Table */
(function() {
  "use strict";

  var targeting = OQGrid.helpers.targeting;

  OQGrid.registerSpec("threshold", function() {
    return {
      name: "threshold",
      containerSelector: ".threshold-params-container",
      dispatchMode: "sync",

      // =====================================================================
      // Parse data attributes from container element
      // =====================================================================
      parseData: function(el) {
        var variableTargeting = OQGrid.utils.parseDataAttr(el, "variableTargeting", {});
        var outcomeSummariesRaw = OQGrid.utils.parseDataAttr(el, "outcomeSummaries", {});
        var costSummariesRaw = OQGrid.utils.parseDataAttr(el, "costSummaries", {});
        var strategies = OQGrid.utils.parseDataAttr(el, "strategies", {});
        var groups = OQGrid.utils.parseDataAttr(el, "groups", {});

        return {
          initial: OQGrid.utils.parseDataAttr(el, "initial", []),
          variables: OQGrid.utils.parseDataAttr(el, "variables", []),
          strategies: strategies,
          groups: groups,
          variableTargeting: variableTargeting,
          variableFormulas: OQGrid.utils.parseDataAttr(el, "variableFormulas", {}),
          states: OQGrid.utils.parseDataAttr(el, "states", []),
          outcomeValues: OQGrid.utils.parseDataAttr(el, "outcomeValues", []),
          costValues: OQGrid.utils.parseDataAttr(el, "costValues", []),
          // Pre-computed metadata for condition editor
          _metadata: {
            outcomeSummaries: Object.keys(outcomeSummariesRaw).map(function(k) {
              return { label: k, value: outcomeSummariesRaw[k] };
            }),
            costSummaries: Object.keys(costSummariesRaw).map(function(k) {
              return { label: k, value: costSummariesRaw[k] };
            }),
            strategies: Object.keys(strategies).map(function(k) {
              return { label: k, value: strategies[k] };
            }),
            groups: Object.keys(groups).map(function(k) {
              return { label: k, value: groups[k] };
            }),
            states: OQGrid.utils.parseDataAttr(el, "states", []),
            outcomeValues: OQGrid.utils.parseDataAttr(el, "outcomeValues", []),
            costValues: OQGrid.utils.parseDataAttr(el, "costValues", [])
          }
        };
      },

      // =====================================================================
      // Column definitions
      // =====================================================================
      getColumnDefs: function(data, controller) {
        var choices = data;
        var metadata = data._metadata;
        var strategyKeys = Object.keys(choices.strategies);
        var groupKeys = Object.keys(choices.groups);

        return [
          // Active column
          {
            title: "Active",
            field: "active",
            width: 65,
            hozAlign: "center",
            formatter: "tickCross",
            editor: true,
            editorParams: { tristate: false },
            cellEdited: function(cell) {
              cell.getRow().update({ active: !!cell.getValue() });
            }
          },

          // Name column
          {
            title: "Name",
            field: "name",
            widthGrow: 1,
            minWidth: 120,
            editor: "input"
          },

          // Variable column
          {
            title: "Variable",
            field: "variable",
            widthGrow: 1,
            minWidth: 120,
            editor: "list",
            editorParams: function() {
              return {
                values: choices.variables.map(function(v) {
                  return { label: v, value: v };
                })
              };
            }
          },

          // Strategy column (variable targeting)
          {
            title: "Strategy",
            field: "variable_strategy",
            widthGrow: 1,
            minWidth: 100,
            editor: "list",
            editorParams: function(cell) {
              var rowData = cell.getRow().getData();
              var t = targeting.getTargeting(choices, rowData.variable);
              if (!t.strategies) return { values: [] };
              return {
                values: t.strategies.map(function(sVal) {
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
              var t = targeting.getTargeting(choices, rowData.variable);
              return t.strategies !== null;
            },
            formatter: function(cell) {
              var rowData = cell.getRow().getData();
              var t = targeting.getTargeting(choices, rowData.variable);
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

          // Group column (variable targeting)
          {
            title: "Group",
            field: "variable_group",
            widthGrow: 1,
            minWidth: 100,
            editor: "list",
            editorParams: function(cell) {
              var rowData = cell.getRow().getData();
              var t = targeting.getTargeting(choices, rowData.variable);
              if (!t.groups) return { values: [] };
              return {
                values: t.groups.map(function(gVal) {
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
              var t = targeting.getTargeting(choices, rowData.variable);
              return t.groups !== null;
            },
            formatter: function(cell) {
              var rowData = cell.getRow().getData();
              var t = targeting.getTargeting(choices, rowData.variable);
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
              var key = OQGrid.helpers.combo.compositeKey(rowData.variable, rowData.variable_strategy, rowData.variable_group);
              return choices.variableFormulas[key] || "";
            }
          },

          // Lower bound
          {
            title: "Lower",
            field: "lower",
            widthGrow: 1,
            minWidth: 80,
            editor: "number",
            editorParams: { step: "any" }
          },

          // Upper bound
          {
            title: "Upper",
            field: "upper",
            widthGrow: 1,
            minWidth: 80,
            editor: "number",
            editorParams: { step: "any" }
          },

          // Condition column (click to open editor)
          {
            title: "Condition",
            field: "condition",
            widthGrow: 2,
            minWidth: 250,
            editor: false,
            headerSort: false,
            formatter: function(cell) {
              var el = document.createElement("span");
              el.className = "threshold-condition-cell";
              el.textContent = OQGrid.editors.getConditionDisplayString(cell.getValue());
              return el;
            },
            cellDblClick: function(e, cell) {
              var tbl = cell.getTable();
              OQGrid.editors.openConditionEditor(
                cell.getElement(),
                cell.getRow().getData().condition || {},
                metadata,
                function(newCondition) {
                  cell.getRow().update({ condition: newCondition });
                  OQGrid.relayout(tbl);
                  OQGrid.actions.sync.syncData(controller);
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
            if (field === "variable_strategy" || field === "variable_group") {
              var el = cells[ci].getElement();
              var t = targeting.getTargeting(row.getTable()._choices || {}, data.variable);
              var isDisabled = (field === "variable_strategy" && !t.strategies) ||
                               (field === "variable_group" && !t.groups);
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
      },

      // =====================================================================
      // Add row setup (single add button, no settings for threshold)
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
              active: d.active !== false,
              name: d.name || "",
              variable: d.variable || "",
              variable_strategy: d.variable_strategy || "",
              variable_group: d.variable_group || "",
              lower: d.lower,
              upper: d.upper,
              condition: d.condition || {}
            };
          });
        },

        // Remove action
        remove: function(row) {
          return { type: "remove" };
        }
      },

      // =====================================================================
      // Post cell-edit hook: apply targeting defaults when variable changes
      // =====================================================================
      onCellEdited: function(controller, cell) {
        var field = cell.getField();
        var data = cell.getRow().getData();
        var choices = controller.table._choices;

        if (field === "variable" && choices) {
          var t = targeting.getTargeting(choices, data.variable);
          var updates = {};
          if (t.strategies) {
            updates.variable_strategy = t.strategies[0] || "";
          } else {
            updates.variable_strategy = "";
          }
          if (t.groups) {
            updates.variable_group = t.groups[0] || "";
          } else {
            updates.variable_group = "";
          }
          cell.getRow().update(updates);
          OQGrid.relayout(controller.table);
        }

        OQGrid.actions.sync.syncData(controller);
      },

      // =====================================================================
      // Post-init: create add button, store choices, wire run button
      // =====================================================================
      onInit: function(controller) {
        var table = controller.table;
        var choices = controller.data;
        var metadata = choices._metadata;

        // Store choices on table for rowFormatter
        table._choices = choices;

        // Create single add button
        var addBtn = document.createElement("button");
        addBtn.type = "button";
        addBtn.className = "oq-grid-add-btn threshold-add-variable-btn";
        addBtn.textContent = "+ Add Variable";
        addBtn.addEventListener("click", function() {
          var defaultVar = choices.variables.length > 0 ? choices.variables[0] : "";
          var newRow = {
            active: true,
            name: "Analysis " + (table.getData().length + 1),
            variable: defaultVar,
            variable_strategy: "",
            variable_group: "",
            lower: 0,
            upper: 1,
            condition: {
              output: "ce",
              health_summary: metadata.outcomeSummaries.length > 0 ? metadata.outcomeSummaries[0].value : "",
              cost_summary: metadata.costSummaries.length > 0 ? metadata.costSummaries[0].value : "",
              referent: metadata.strategies.length > 0 ? metadata.strategies[0].value : "",
              comparator: metadata.strategies.length > 1 ? metadata.strategies[1].value :
                (metadata.strategies.length > 0 ? metadata.strategies[0].value : ""),
              discounted: true
            }
          };
          if (defaultVar) {
            var t = targeting.getTargeting(choices, defaultVar);
            if (t.strategies) {
              newRow.variable_strategy = t.strategies[0] || "";
            }
            if (t.groups) {
              newRow.variable_group = t.groups[0] || "";
            }
          }
          table.addRow(newRow).then(function(row) {
            OQGrid.relayout(table);
            row.getElement().scrollIntoView({ behavior: "smooth", block: "nearest" });
            OQGrid.actions.sync.syncData(controller);
          });
        });

        controller.containerDiv.parentNode.insertBefore(addBtn, controller.containerDiv);

        // Initial sync
        OQGrid.actions.sync.syncData(controller);

        // Run button
        var runBtn = document.querySelector(".threshold-run-btn");
        if (runBtn) {
          runBtn.addEventListener("click", function() {
            var analyses = table.getData().map(function(d) {
              return {
                active: d.active !== false,
                name: d.name || "",
                variable: d.variable || "",
                variable_strategy: d.variable_strategy || "",
                variable_group: d.variable_group || "",
                lower: d.lower,
                upper: d.upper,
                condition: d.condition || {}
              };
            });
            OQGrid.shiny.dispatch("run_threshold_action", {
              nonce: Date.now(),
              analyses: analyses
            });
          });
        }
      }
    };
  });
})();
