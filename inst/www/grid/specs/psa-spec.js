/* OQGrid Spec — PSA Parameters Tables (univariate + multivariate) */
(function() {
  "use strict";

  var OQGrid = window.OQGrid = window.OQGrid || {};

  // =========================================================================
  // PSA-specific instance helpers
  // PSA uses variableInstances (array of {name, strategy, group, sampling})
  // instead of the DSA variableTargeting map, so it needs its own helpers.
  // =========================================================================

  function normalizeSegmentValue(value) {
    if (value == null || value === "") return "";
    if (typeof value === "object") return "";
    return String(value);
  }

  function buildVariableKey(name, strategy, group) {
    return [
      normalizeSegmentValue(name),
      normalizeSegmentValue(strategy),
      normalizeSegmentValue(group)
    ].join("|");
  }

  function getInstanceKey(instance) {
    if (!instance) return "||";
    return buildVariableKey(instance.name, instance.strategy, instance.group);
  }

  function getInstancesForName(choices, name) {
    return (choices.variableInstances || []).filter(function(instance) {
      return normalizeSegmentValue(instance.name) === normalizeSegmentValue(name);
    });
  }

  function findInstance(choices, name, strategy, group) {
    var targetKey = buildVariableKey(name, strategy, group);
    var instances = choices.variableInstances || [];
    for (var i = 0; i < instances.length; i++) {
      if (getInstanceKey(instances[i]) === targetKey) return instances[i];
    }
    return null;
  }

  function setInstanceSampling(choices, name, strategy, group, sampling) {
    var instance = findInstance(choices, name, strategy, group);
    if (instance) instance.sampling = sampling || "";
  }

  function getTargeting(choices, name) {
    var instances = getInstancesForName(choices, name);
    var strategies = [];
    var groups = [];
    instances.forEach(function(instance) {
      var strategy = normalizeSegmentValue(instance.strategy);
      var group = normalizeSegmentValue(instance.group);
      if (strategy !== "" && strategies.indexOf(strategy) < 0) strategies.push(strategy);
      if (group !== "" && groups.indexOf(group) < 0) groups.push(group);
    });
    return {
      strategies: strategies.length > 0 ? strategies : null,
      groups: groups.length > 0 ? groups : null
    };
  }

  function getUsedInstanceKeys(table, excludeId) {
    var usedKeys = new Set();
    var combos = table._rowCombos || {};
    Object.keys(combos).forEach(function(id) {
      if (id !== excludeId) {
        var combo = combos[id];
        usedKeys.add(buildVariableKey(combo.name, combo.strategy, combo.group));
      }
    });
    return usedKeys;
  }

  function findFirstUnusedInstance(choices, name, usedKeys) {
    var instances = choices.variableInstances || [];
    for (var i = 0; i < instances.length; i++) {
      var instance = instances[i];
      if (name && normalizeSegmentValue(instance.name) !== normalizeSegmentValue(name)) continue;
      if (!usedKeys.has(getInstanceKey(instance))) return instance;
    }
    return null;
  }

  function getSelectableNames(choices, currentName, usedKeys) {
    return (choices.variables || []).filter(function(name) {
      if (name === currentName) return true;
      return !!findFirstUnusedInstance(choices, name, usedKeys);
    });
  }

  function findFirstUnusedVariable(table, choices) {
    var ck = OQGrid.helpers.combo.compositeKey;
    var usedKeys = new Set();
    table.getData().forEach(function(row) {
      usedKeys.add(ck(row.name, row.strategy, row.group));
    });
    var instances = choices.variableInstances || [];
    for (var i = 0; i < instances.length; i++) {
      var key = ck(instances[i].name, instances[i].strategy, instances[i].group);
      if (!usedKeys.has(key)) return instances[i];
    }
    return null;
  }

  function getBaseCase(choices, data) {
    var key = buildVariableKey(data.name, data.strategy, data.group);
    return choices.variableFormulas[key] || "";
  }

  // Row combo tracking (PSA-specific: no type field, just name/strategy/group)
  function initRowCombos(table, initialData) {
    table._rowCombos = {};
    table._nextComboId = 1;
    var rows = table.getRows();
    for (var i = 0; i < rows.length; i++) {
      var id = table._nextComboId++;
      rows[i].getElement().setAttribute("data-combo-id", id);
      var r = (initialData || [])[i] || {};
      table._rowCombos[id] = {
        name: r.name || "", strategy: r.strategy || "", group: r.group || ""
      };
    }
  }

  function getComboId(cell) {
    return cell.getRow().getElement().getAttribute("data-combo-id");
  }

  function setRowCombo(table, comboId, name, strategy, group) {
    table._rowCombos[comboId] = {
      name: name || "", strategy: strategy || "", group: group || ""
    };
  }

  function addRowCombo(table, row, name, strategy, group) {
    var id = table._nextComboId++;
    row.getElement().setAttribute("data-combo-id", id);
    table._rowCombos[id] = {
      name: name || "", strategy: strategy || "", group: group || ""
    };
  }

  function removeRowCombo(table, comboId) {
    delete table._rowCombos[comboId];
  }

  // =========================================================================
  // Shared table instance tracking (for run button to read both grids)
  // =========================================================================
  var _activeTables = {};
  OQGrid._psaActiveTables = _activeTables;

  // =========================================================================
  // Settings panel wiring
  // =========================================================================
  function initSettings(settingsDiv) {
    if (!settingsDiv || settingsDiv.hasAttribute("data-initialized")) return;
    var nSimInput = settingsDiv.querySelector(".psa-nsim-input");
    var seedInput = settingsDiv.querySelector(".psa-seed-input");
    function sendSettings() {
      OQGrid.shiny.dispatchModelAction({
        type: "set_psa_settings",
        n_sim: nSimInput ? parseInt(nSimInput.value, 10) || 1000 : 1000,
        seed: seedInput ? seedInput.value : ""
      });
    }
    if (nSimInput) nSimInput.addEventListener("change", sendSettings);
    if (seedInput) seedInput.addEventListener("change", sendSettings);
    settingsDiv.setAttribute("data-initialized", "true");
  }

  // =========================================================================
  // Run button wiring
  // =========================================================================
  function wireRunButton(wrapper) {
    var runBtn = wrapper.querySelector(".psa-run-btn");
    if (!runBtn) return;
    var newRunBtn = runBtn.cloneNode(true);
    runBtn.parentNode.replaceChild(newRunBtn, runBtn);
    runBtn = newRunBtn;
    var psaPage = document.getElementById("page_psa") || document;
    runBtn.addEventListener("click", function() {
      var nSimInput = psaPage.querySelector(".psa-nsim-input");
      var seedInput = psaPage.querySelector(".psa-seed-input");

      var univContainer = psaPage.querySelector(".psa-params-container");
      var univInputId = univContainer ? univContainer.dataset.inputId : null;
      var univTable = univInputId ? _activeTables[univInputId] : null;
      var univData = univTable ? univTable.getData().map(function(d) {
        return { name: d.name, strategy: d.strategy || "", group: d.group || "", sampling: d.sampling || "" };
      }) : [];

      var mvContainer = psaPage.querySelector(".psa-multivariate-container");
      var mvInputId = mvContainer ? mvContainer.dataset.inputId : null;
      var mvTable = mvInputId ? _activeTables[mvInputId] : null;
      var mvData = mvTable ? mvTable.getData().map(function(d) {
        return {
          name: d.name || "",
          variables: d.variables || [],
          strategy: d.strategy || "",
          group: d.group || "",
          type: d.type || "",
          n: d.n || null,
          covariance: d.covariance || null
        };
      }) : [];

      OQGrid.shiny.dispatch("run_psa_action", {
        nonce: Date.now(),
        n_sim: nSimInput ? parseInt(nSimInput.value, 10) || 1000 : 1000,
        seed: seedInput ? seedInput.value : "",
        params: univData,
        multivariate: mvData
      });
    });
  }

  // =========================================================================
  // Sync functions
  // =========================================================================
  function syncUnivariateToShiny(table, inputId) {
    var data = table.getData().map(function(d) {
      return {
        name: d.name,
        display_name: d.display_name || d.name,
        strategy: d.strategy || "",
        group: d.group || "",
        sampling: d.sampling || ""
      };
    });
    OQGrid.shiny.dispatch(inputId, data);
  }

  function syncMultivariateToShiny(table, inputId) {
    var data = table.getData().map(function(d) {
      return {
        name: d.name || "",
        variables: d.variables || [],
        strategy: d.strategy || "",
        group: d.group || "",
        type: d.type || "",
        n: d.n || null,
        covariance: d.covariance || null
      };
    });
    OQGrid.shiny.dispatch(inputId, data);
  }

  // =========================================================================
  // Distribution display formatter (shared between univariate and MV grids)
  // =========================================================================
  function distributionFormatter(cell) {
    var val = cell.getValue();
    if (!val || !val.trim()) {
      var span = document.createElement("span");
      span.className = "psa-sampling-empty";
      span.textContent = "Click to set";
      return span;
    }
    var span = document.createElement("span");
    span.className = "psa-sampling-display";
    var parenIdx = val.indexOf("(");
    if (parenIdx > 0) {
      var nameSpan = document.createElement("span");
      nameSpan.className = "psa-dist-name";
      nameSpan.textContent = val.substring(0, parenIdx);
      span.appendChild(nameSpan);
      span.appendChild(document.createTextNode(val.substring(parenIdx)));
    } else {
      span.textContent = val;
    }
    return span;
  }

  // =========================================================================
  // PSA UNIVARIATE spec registration
  // =========================================================================
  OQGrid.registerSpec("psa-univariate", function() {
    return {
      name: "psa-univariate",
      nonClearableFields: ["name", "strategy", "group", "sampling"],
      containerSelector: ".psa-params-container",
      dispatchMode: "sync",

      // =====================================================================
      // Parse data attributes from container element
      // =====================================================================
      parseData: function(el) {
        return {
          initial: OQGrid.utils.parseDataAttr(el, "initial", []),
          variables: OQGrid.utils.parseDataAttr(el, "variables", []),
          strategies: OQGrid.utils.parseDataAttr(el, "strategies", {}),
          groups: OQGrid.utils.parseDataAttr(el, "groups", {}),
          variableTargeting: OQGrid.utils.parseDataAttr(el, "variableTargeting", {}),
          variableInstances: OQGrid.utils.parseDataAttr(el, "variableInstances", []),
          variableFormulas: OQGrid.utils.parseDataAttr(el, "variableFormulas", {}),
          terms: OQGrid.utils.parseDataAttr(el, "terms", null),
          suggestions: OQGrid.utils.parseDataAttr(el, "suggestions", null)
        };
      },

      // =====================================================================
      // Column definitions
      // =====================================================================
      getColumnDefs: function(data, controller) {
        var choices = data;
        var strategyKeys = Object.keys(choices.strategies);
        var groupKeys = Object.keys(choices.groups);

        return [
          // Name column
          {
            title: "Name",
            field: "name",
            widthGrow: 1,
            minWidth: 120,
            editor: "list",
            editorParams: function(cell) {
              var currentData = cell.getRow().getData();
              var usedKeys = getUsedInstanceKeys(cell.getTable(), getComboId(cell));
              return {
                values: getSelectableNames(choices, currentData.name, usedKeys)
                  .map(function(name) { return { label: name, value: name }; })
              };
            }
          },

          // Strategy column
          {
            title: "Strategy",
            field: "strategy",
            widthGrow: 1,
            minWidth: 100,
            editor: "list",
            editorParams: function(cell) {
              var d = cell.getRow().getData();
              var targeting = getTargeting(choices, d.name);
              if (!targeting.strategies) return { values: [] };
              var currentGroup = d.group || "";
              var usedKeys = getUsedInstanceKeys(cell.getTable(), getComboId(cell));
              var strategyValues = [];
              getInstancesForName(choices, d.name).forEach(function(instance) {
                if (normalizeSegmentValue(instance.group) !== currentGroup) return;
                if (usedKeys.has(getInstanceKey(instance))) return;
                var strategyVal = normalizeSegmentValue(instance.strategy);
                if (strategyValues.indexOf(strategyVal) >= 0) return;
                strategyValues.push(strategyVal);
              });
              return {
                values: strategyValues.map(function(strategyVal) {
                  var sLabel = strategyVal;
                  for (var sk = 0; sk < strategyKeys.length; sk++) {
                    if (choices.strategies[strategyKeys[sk]] === strategyVal) { sLabel = strategyKeys[sk]; break; }
                  }
                  return { label: sLabel, value: strategyVal };
                })
              };
            },
            editable: function(cell) {
              return getTargeting(choices, cell.getRow().getData().name).strategies !== null;
            },
            formatter: function(cell) {
              var targeting = getTargeting(choices, cell.getRow().getData().name);
              if (!targeting.strategies) return "\u2014";
              var val = cell.getValue();
              if (!val || val === "") return "";
              for (var sk = 0; sk < strategyKeys.length; sk++) {
                if (choices.strategies[strategyKeys[sk]] === val) return strategyKeys[sk];
              }
              return val;
            }
          },

          // Group column
          {
            title: "Group",
            field: "group",
            widthGrow: 1,
            minWidth: 100,
            editor: "list",
            editorParams: function(cell) {
              var d = cell.getRow().getData();
              var targeting = getTargeting(choices, d.name);
              if (!targeting.groups) return { values: [] };
              var currentStrategy = d.strategy || "";
              var usedKeys = getUsedInstanceKeys(cell.getTable(), getComboId(cell));
              var groupValues = [];
              getInstancesForName(choices, d.name).forEach(function(instance) {
                if (normalizeSegmentValue(instance.strategy) !== currentStrategy) return;
                if (usedKeys.has(getInstanceKey(instance))) return;
                var groupVal = normalizeSegmentValue(instance.group);
                if (groupValues.indexOf(groupVal) >= 0) return;
                groupValues.push(groupVal);
              });
              return {
                values: groupValues.map(function(groupVal) {
                  var gLabel = groupVal;
                  for (var gk = 0; gk < groupKeys.length; gk++) {
                    if (choices.groups[groupKeys[gk]] === groupVal) { gLabel = groupKeys[gk]; break; }
                  }
                  return { label: gLabel, value: groupVal };
                })
              };
            },
            editable: function(cell) {
              return getTargeting(choices, cell.getRow().getData().name).groups !== null;
            },
            formatter: function(cell) {
              var targeting = getTargeting(choices, cell.getRow().getData().name);
              if (!targeting.groups) return "\u2014";
              var val = cell.getValue();
              if (!val || val === "") return "";
              for (var gk = 0; gk < groupKeys.length; gk++) {
                if (choices.groups[groupKeys[gk]] === val) return groupKeys[gk];
              }
              return val;
            }
          },

          // Base Case column (computed, read-only)
          {
            title: "Base Case",
            field: "_baseCase",
            titleFormatter: OQGrid.utils.infoTitle("Current value from the model definition."),
            widthGrow: 1,
            minWidth: 120,
            editor: false,
            headerSort: false,
            clipboard: false,
            formatter: function(cell) {
              return getBaseCase(choices, cell.getRow().getData());
            }
          },

          // Sampling column (distribution editor)
          {
            title: "Sampling",
            field: "sampling",
            titleFormatter: OQGrid.utils.infoTitle("Probability distribution to sample from during PSA. Specify distribution type and parameters."),
            widthGrow: 2,
            minWidth: 250,
            editor: OQGrid.editors.distribution(choices.terms, choices.suggestions),
            formatter: distributionFormatter
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
          row.getCells().forEach(function(c) {
            var field = c.getColumn().getField();
            if (field === "strategy" || field === "group") {
              var el = c.getElement();
              var targeting = getTargeting(row.getTable()._choices || {}, data.name);
              var isDisabled = (field === "strategy" && !targeting.strategies) ||
                               (field === "group" && !targeting.groups);
              el.style.color = isDisabled ? "var(--bs-secondary, #6c757d)" : "";
              el.style.fontStyle = isDisabled ? "italic" : "";
            }
          });
        }
      },

      // =====================================================================
      // Custom add row setup
      // =====================================================================
      addRow: { custom: true },

      // =====================================================================
      // Actions
      // =====================================================================
      actions: {
        serialize: function(data) {
          return data.map(function(d) {
            return {
              name: d.name,
              display_name: d.display_name || d.name,
              strategy: d.strategy || "",
              group: d.group || "",
              sampling: d.sampling || ""
            };
          });
        },
        remove: function(row) {
          return { type: "remove" };
        }
      },

      // =====================================================================
      // Post-delete hook
      // =====================================================================
      onRowDeleted: function(controller, rowData) {
        var table = controller.table;
        var choices = table._choices;
        var delComboId = null;
        // Clean up instance sampling
        setInstanceSampling(choices, rowData.name, rowData.strategy, rowData.group, "");
        OQGrid.shiny.dispatchModelAction({
          type: "remove_variable_sampling",
          variable: rowData.name || "",
          strategy: rowData.strategy || "",
          group: rowData.group || ""
        });
        syncUnivariateToShiny(table, controller.inputId);
      },

      // =====================================================================
      // Post cell-edit hook
      // =====================================================================
      onCellEdited: function(controller, cell) {
        var field = cell.getField();
        var data = cell.getRow().getData();
        var oldValue = cell.getOldValue();
        var choices = controller.table._choices;

        if (field === "name") {
          var cid = getComboId(cell);
          var oldName = oldValue || "";
          var oldStrategy = data.strategy || "";
          var oldGroup = data.group || "";
          if (data.name !== oldName) {
            var targetInstance = findFirstUnusedInstance(
              choices,
              data.name,
              getUsedInstanceKeys(cell.getTable(), cid)
            );
            if (targetInstance) {
              data.display_name = data.name;
              data.strategy = normalizeSegmentValue(targetInstance.strategy);
              data.group = normalizeSegmentValue(targetInstance.group);
              data.sampling = targetInstance.sampling || "";
              cell.getRow().update({
                display_name: data.display_name,
                strategy: data.strategy,
                group: data.group,
                sampling: data.sampling
              });
              setRowCombo(cell.getTable(), cid, data.name, data.strategy, data.group);
              setInstanceSampling(choices, oldName, oldStrategy, oldGroup, "");
              setInstanceSampling(choices, data.name, data.strategy, data.group, data.sampling);
              OQGrid.shiny.dispatchModelAction({
                type: "rename_variable_sampling",
                variable: oldName,
                strategy: oldStrategy,
                group: oldGroup,
                new_variable: data.name,
                new_strategy: data.strategy || "",
                new_group: data.group || "",
                sampling: data.sampling || ""
              });
            } else {
              data.name = oldName;
              cell.getRow().update({ name: oldName });
              setRowCombo(cell.getTable(), cid, oldName, oldStrategy, oldGroup);
            }
          }
        } else if (field === "sampling") {
          setInstanceSampling(choices, data.name, data.strategy, data.group, data.sampling || "");
          OQGrid.shiny.dispatchModelAction({
            type: "edit_variable_sampling",
            variable: data.name,
            strategy: data.strategy || "",
            group: data.group || "",
            sampling: data.sampling || ""
          });
        } else if (field === "strategy" || field === "group") {
          var priorStrategy = field === "strategy" ? (oldValue || "") : (data.strategy || "");
          var priorGroup = field === "group" ? (oldValue || "") : (data.group || "");
          var currentInstance = findInstance(choices, data.name, data.strategy, data.group);
          data.sampling = currentInstance && currentInstance.sampling ? currentInstance.sampling : "";
          cell.getRow().update({ sampling: data.sampling });
          setRowCombo(cell.getTable(), getComboId(cell), data.name, data.strategy, data.group);
          setInstanceSampling(choices, data.name, priorStrategy, priorGroup, "");
          setInstanceSampling(choices, data.name, data.strategy, data.group, data.sampling);
          if (priorStrategy !== (data.strategy || "") || priorGroup !== (data.group || "")) {
            OQGrid.shiny.dispatchModelAction({
              type: "rename_variable_sampling",
              variable: data.name,
              strategy: priorStrategy,
              group: priorGroup,
              new_variable: data.name,
              new_strategy: data.strategy || "",
              new_group: data.group || "",
              sampling: data.sampling || ""
            });
          }
        }

        syncUnivariateToShiny(controller.table, controller.inputId);
      },

      // =====================================================================
      // Post-init: add button + combo tracking + settings + run button
      // =====================================================================
      onInit: function(controller) {
        var table = controller.table;
        var choices = controller.data;
        var inputId = controller.inputId;

        // Store choices on table for rowFormatter and other hooks
        table._choices = choices;

        // Register in active tables for run button
        _activeTables[inputId] = table;

        // Init combo tracking
        initRowCombos(table, choices.initial);

        // Wire existing R-created button
        var addBtn = controller.containerDiv.parentNode
          ? controller.containerDiv.parentNode.querySelector(".psa-add-variable-btn")
          : null;

        if (addBtn) addBtn.addEventListener("click", function() {
          var unused = findFirstUnusedVariable(table, choices);
          if (!unused) return;
          var newRow = {
            name: unused.name,
            display_name: unused.name,
            strategy: normalizeSegmentValue(unused.strategy),
            group: normalizeSegmentValue(unused.group),
            sampling: unused.sampling || ""
          };
          table.addRow(newRow, false).then(function(row) {
            OQGrid.relayout(table);
            row.getElement().scrollIntoView({ behavior: "smooth", block: "nearest" });
            addRowCombo(table, row, newRow.name, newRow.strategy, newRow.group);
            syncUnivariateToShiny(table, inputId);
          });
        });

        // Initial sync
        syncUnivariateToShiny(table, inputId);

        // Wire settings panel
        var psaPage = document.getElementById("page_psa") || document;
        var settingsDiv = psaPage.querySelector(".psa-settings-container");
        if (settingsDiv) initSettings(settingsDiv);

        // Wire run button
        var wrapper = psaPage.querySelector(".psa-inputs-wrapper");
        if (wrapper && !wrapper.hasAttribute("data-run-wired")) {
          wireRunButton(wrapper);
          wrapper.setAttribute("data-run-wired", "true");
        }

        // Wire modal builder lifecycle
        OQGrid.editors._initAddMvModalBuilder();
        if (typeof $ !== "undefined") {
          $(document).on("shown.bs.modal", ".modal", function() {
            OQGrid.editors._initAddMvModalBuilder();
          });
          $(document).on("hidden.bs.modal", ".modal", function() {
            OQGrid.editors._cleanupAddMvModalBuilder();
          });
        }
      }
    };
  });

  // =========================================================================
  // PSA MULTIVARIATE spec registration
  // =========================================================================
  OQGrid.registerSpec("psa-multivariate", function() {
    return {
      name: "psa-multivariate",
      nonClearableFields: ["name", "variables", "strategy", "group"],
      containerSelector: ".psa-multivariate-container",
      dispatchMode: "sync",

      // =====================================================================
      // Parse data attributes from container element
      // =====================================================================
      parseData: function(el) {
        return {
          initial: OQGrid.utils.parseDataAttr(el, "initial", []),
          variables: OQGrid.utils.parseDataAttr(el, "variables", []),
          strategies: OQGrid.utils.parseDataAttr(el, "strategies", {}),
          groups: OQGrid.utils.parseDataAttr(el, "groups", {}),
          tables: OQGrid.utils.parseDataAttr(el, "tables", []),
          terms: OQGrid.utils.parseDataAttr(el, "terms", null),
          suggestions: OQGrid.utils.parseDataAttr(el, "suggestions", null)
        };
      },

      // =====================================================================
      // Column definitions
      // =====================================================================
      getColumnDefs: function(data, controller) {
        var choices = data;
        var strategyKeys = Object.keys(choices.strategies);
        var groupKeys = Object.keys(choices.groups);

        return [
          // Name column
          {
            title: "Name",
            field: "name",
            widthGrow: 1,
            minWidth: 120,
            editor: "input"
          },

          // Variables column (typeahead tag input with variable names)
          {
            title: "Variables",
            field: "variables",
            titleFormatter: OQGrid.utils.infoTitle("Variables that are jointly sampled from this multivariate distribution."),
            widthGrow: 2,
            minWidth: 200,
            editor: OQGrid.editors.tagInput(choices.variables || []),
            formatter: function(cell) {
              var val = cell.getValue();
              if (!val || (Array.isArray(val) && val.length === 0)) return "";
              if (Array.isArray(val)) return val.join(", ");
              return String(val);
            }
          },

          // Strategy column
          strategyKeys.length > 0 ? {
            title: "Strategy",
            field: "strategy",
            widthGrow: 1,
            minWidth: 100,
            editor: "list",
            editorParams: {
              values: [{ label: "(all)", value: "" }].concat(
                OQGrid.utils.buildDisplayMap(choices.strategies)
              )
            },
            formatter: function(cell) {
              var val = cell.getValue();
              if (!val) return "";
              var stratDisplay = OQGrid.utils.buildDisplayMap(choices.strategies);
              for (var i = 0; i < stratDisplay.length; i++) {
                if (stratDisplay[i].value === val) return stratDisplay[i].label;
              }
              return val;
            }
          } : null,

          // Group column
          groupKeys.length > 0 ? {
            title: "Group",
            field: "group",
            widthGrow: 1,
            minWidth: 100,
            editor: "list",
            editorParams: {
              values: [{ label: "(all)", value: "" }].concat(
                OQGrid.utils.buildDisplayMap(choices.groups)
              )
            },
            formatter: function(cell) {
              var val = cell.getValue();
              if (!val) return "";
              var grpDisplay = OQGrid.utils.buildDisplayMap(choices.groups);
              for (var i = 0; i < grpDisplay.length; i++) {
                if (grpDisplay[i].value === val) return grpDisplay[i].label;
              }
              return val;
            }
          } : null,

          // Parameters column (custom editor with type selector + type-specific params)
          {
            title: "Parameters",
            field: "_params",
            titleFormatter: OQGrid.utils.infoTitle("Distribution-specific parameters (e.g. alpha values for Dirichlet, covariance matrix for MV Normal)."),
            widthGrow: 2,
            minWidth: 250,
            editor: OQGrid.editors.mvDistribution(choices, choices.terms, choices.suggestions),
            formatter: function(cell) {
              var d = cell.getRow().getData();
              var type = d.type || "";
              if (!type) {
                var span = document.createElement("span");
                span.className = "psa-sampling-empty";
                span.textContent = "Click to set";
                return span;
              }
              var span = document.createElement("span");
              span.className = "psa-sampling-display";
              var nameSpan = document.createElement("span");
              nameSpan.className = "psa-dist-name";
              if (type === "dirichlet") {
                nameSpan.textContent = "Dirichlet";
                span.appendChild(nameSpan);
                if (d.n) {
                  span.appendChild(document.createTextNode("(n = " + d.n + ")"));
                }
              } else if (type === "mvnormal") {
                nameSpan.textContent = "MV Normal";
                span.appendChild(nameSpan);
                span.appendChild(document.createTextNode("(cov: " + (d.covariance || "?") + ")"));
              } else if (type === "multinomial") {
                nameSpan.textContent = "Multinomial";
                span.appendChild(nameSpan);
              } else {
                span.textContent = type;
              }
              return span;
            }
          }

          // Delete column is added automatically by grid-controller
        ].filter(Boolean);
      },

      // =====================================================================
      // Tabulator options overrides
      // =====================================================================
      tabulatorOptions: {
        clipboardCopyRowRange: "range",
        clipboardPasteParser: "range",
        clipboardPasteAction: "range",
        clipboardCopyConfig: { rowHeaders: false, columnHeaders: false }
      },

      // =====================================================================
      // Custom add row (opens modal via Shiny)
      // =====================================================================
      addRow: { custom: true },

      // =====================================================================
      // Actions
      // =====================================================================
      actions: {
        serialize: function(data) {
          return data.map(function(d) {
            return {
              name: d.name || "",
              variables: d.variables || [],
              strategy: d.strategy || "",
              group: d.group || "",
              type: d.type || "",
              n: d.n || null,
              covariance: d.covariance || null
            };
          });
        },
        remove: function(row) {
          return { type: "remove" };
        }
      },

      // =====================================================================
      // Post-delete hook
      // =====================================================================
      onRowDeleted: function(controller, rowData) {
        var table = controller.table;
        OQGrid.shiny.dispatchModelAction({
          type: "remove_multivariate_sampling",
          name: rowData.name || ""
        });
        syncMultivariateToShiny(table, controller.inputId);
      },

      // =====================================================================
      // Post cell-edit hook
      // =====================================================================
      onCellEdited: function(controller, cell) {
        var field = cell.getField();
        var data = cell.getRow().getData();
        var oldValue = cell.getOldValue();

        var action = {
          type: "edit_multivariate_sampling",
          name: data.name
        };

        if (field === "name" && oldValue) {
          action.name = oldValue;
          action.new_name = data.name;
        } else if (field === "_params") {
          // Parameters editor returns structured object — merge into row data
          var params = cell.getValue();
          if (params && typeof params === "object") {
            var row = cell.getRow();
            var updateData = {};
            if (params.type !== undefined) updateData.type = params.type;
            if (params.type === "dirichlet") {
              updateData.n = params.n || null;
              updateData.covariance = null;
            } else if (params.type === "mvnormal") {
              updateData.covariance = params.covariance || null;
              updateData.n = null;
            } else if (params.type === "multinomial") {
              updateData.n = null;
              updateData.covariance = null;
            }
            row.update(updateData);
            var updated = row.getData();
            action.type_value = updated.type;
            action.n = updated.n;
            action.covariance = updated.covariance;
          }
        } else if (field === "variables") {
          action.variables = data.variables || [];
        } else if (field === "strategy") {
          action.strategy = data.strategy || "";
        } else if (field === "group") {
          action.group = data.group || "";
        }

        OQGrid.shiny.dispatchModelAction(action);
        syncMultivariateToShiny(controller.table, controller.inputId);
      },

      // =====================================================================
      // Post-init: add button + register in active tables
      // =====================================================================
      onInit: function(controller) {
        var table = controller.table;
        var choices = controller.data;
        var inputId = controller.inputId;

        // Register in active tables for run button
        _activeTables[inputId] = table;

        // Wire existing R-created button
        var addMvBtn = controller.containerDiv.parentNode
          ? controller.containerDiv.parentNode.querySelector(".psa-add-mv-btn")
          : null;
        if (addMvBtn) addMvBtn.addEventListener("click", function() {
          OQGrid.shiny.dispatch("show_add_multivariate_modal", {
            nonce: Date.now()
          });
        });

        // Initial sync
        syncMultivariateToShiny(table, inputId);
      }
    };
  });
})();
