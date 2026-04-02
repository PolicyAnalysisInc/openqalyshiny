/* OQGrid — Analysis Param Helpers: Row Combo Tracking */
(function() {
  "use strict";

  var OQGrid = window.OQGrid = window.OQGrid || {};
  OQGrid.helpers = OQGrid.helpers || {};
  OQGrid.helpers.combo = {};

  var targeting = OQGrid.helpers.targeting;

  // =========================================================================
  // Composite key: canonical name|strategy|group key with null/undefined/"" normalization
  // =========================================================================
  function normalizeKeyPart(val) {
    if (val == null || val === "") return "";
    if (typeof val === "object") return "";
    return String(val);
  }

  OQGrid.helpers.combo.compositeKey = function(name, strategy, group) {
    return normalizeKeyPart(name) + "|" + normalizeKeyPart(strategy) + "|" + normalizeKeyPart(group);
  };

  // =========================================================================
  // Initialize combo tracking on a table from initial row data.
  // Stores { type, name, strategy, group } per row keyed by auto-incrementing id.
  // =========================================================================
  OQGrid.helpers.combo.initRowCombos = function(table, initialData) {
    table._rowCombos = {};
    table._nextComboId = 1;
    var rows = table.getRows();
    for (var i = 0; i < rows.length; i++) {
      var id = table._nextComboId++;
      rows[i].getElement().setAttribute("data-combo-id", id);
      var r = (initialData || [])[i] || {};
      table._rowCombos[id] = {
        type: r.type || "variable", name: r.name || "",
        strategy: r.strategy || "", group: r.group || ""
      };
    }
  };

  // =========================================================================
  // Get the combo tracking id from a cell's row element
  // =========================================================================
  OQGrid.helpers.combo.getComboId = function(cell) {
    return cell.getRow().getElement().getAttribute("data-combo-id");
  };

  // =========================================================================
  // Update the combo data for an existing row
  // =========================================================================
  OQGrid.helpers.combo.setRowCombo = function(table, comboId, data) {
    table._rowCombos[comboId] = {
      type: data.type || "variable", name: data.name || "",
      strategy: data.strategy || "", group: data.group || ""
    };
  };

  // =========================================================================
  // Add combo tracking for a newly added row
  // =========================================================================
  OQGrid.helpers.combo.addRowCombo = function(table, row, data) {
    var id = table._nextComboId++;
    row.getElement().setAttribute("data-combo-id", id);
    table._rowCombos[id] = {
      type: data.type || "variable", name: data.name || "",
      strategy: data.strategy || "", group: data.group || ""
    };
  };

  // =========================================================================
  // Remove combo tracking for a deleted row
  // =========================================================================
  OQGrid.helpers.combo.removeRowCombo = function(table, comboId) {
    delete table._rowCombos[comboId];
  };

  // =========================================================================
  // Get used strategy|group combos for a specific variable name,
  // excluding one row (by combo id) — used to filter strategy/group dropdowns.
  // =========================================================================
  OQGrid.helpers.combo.getUsedCombosForVariable = function(table, variableName, excludeId) {
    var usedCombos = new Set();
    var combos = table._rowCombos || {};
    Object.keys(combos).forEach(function(id) {
      if (id !== excludeId && combos[id].name === variableName) {
        usedCombos.add((combos[id].strategy || "") + "|" + (combos[id].group || ""));
      }
    });
    return usedCombos;
  };

  // =========================================================================
  // Get used name|strategy|group combos across all variable rows,
  // excluding one row — used to filter the name dropdown.
  // =========================================================================
  OQGrid.helpers.combo.getUsedCombosForDropdown = function(table, excludeId) {
    var ck = OQGrid.helpers.combo.compositeKey;
    var usedCombos = new Set();
    var combos = table._rowCombos || {};
    Object.keys(combos).forEach(function(id) {
      if (id !== excludeId) {
        var c = combos[id];
        if (c.type === "variable") {
          usedCombos.add(ck(c.name, c.strategy, c.group));
        }
      }
    });
    return usedCombos;
  };

  // =========================================================================
  // Build all possible (name, strategy, group) combos for every variable.
  // =========================================================================
  function isEmptyValue(val) {
    return val == null || val === "" || (typeof val === "object" && !Array.isArray(val));
  }

  OQGrid.helpers.combo.allVariableCombos = function(choices) {
    return choices.variables.flatMap(function(v) {
      var t = targeting.getTargeting(choices, v);
      var rawStrats = isEmptyValue(t.strategies) ? null : t.strategies;
      var rawGrps = isEmptyValue(t.groups) ? null : t.groups;
      var strats = Array.isArray(rawStrats) ? rawStrats : (rawStrats ? [rawStrats] : [""]);
      var grps = Array.isArray(rawGrps) ? rawGrps : (rawGrps ? [rawGrps] : [""]);
      return strats.flatMap(function(s) {
        return grps.map(function(g) { return { name: v, strategy: normalizeKeyPart(s), group: normalizeKeyPart(g) }; });
      });
    });
  };

  // =========================================================================
  // Find the first unused variable combo (name + strategy + group).
  // Keys all current table rows, keys all possible combos, returns the first
  // combo whose key is not in the used set. Returns null if all combos are used.
  // =========================================================================
  OQGrid.helpers.combo.findFirstUnusedVariable = function(table, choices) {
    var ck = OQGrid.helpers.combo.compositeKey;
    var usedKeys = new Set();
    table.getData().forEach(function(row) {
      usedKeys.add(ck(row.name, row.strategy, row.group));
    });
    var allCombos = OQGrid.helpers.combo.allVariableCombos(choices);
    return allCombos.find(function(c) {
      return !usedKeys.has(ck(c.name, c.strategy, c.group));
    }) || null;
  };

  // =========================================================================
  // Find the first unused setting (respecting mutual exclusions).
  // Returns { name, display } or null.
  // =========================================================================
  OQGrid.helpers.combo.findFirstUnusedSetting = function(table, choices) {
    var usedNames = new Set(
      table.getData()
        .filter(function(r) { return r.type === "setting"; })
        .map(function(r) { return r.name; })
    );
    OQGrid.helpers.exclusions.expandWithExclusions(usedNames);
    var settingKeys = Object.keys(choices.settings);
    var key = settingKeys.find(function(k) { return !usedNames.has(choices.settings[k]); });
    return key ? { name: choices.settings[key], display: key } : null;
  };

  // =========================================================================
  // Enable/disable add buttons based on available combos.
  // =========================================================================
  OQGrid.helpers.combo.updateButtonStates = function(table, choices, addVarBtn, addSettingBtn) {
    if (addVarBtn) addVarBtn.disabled = !OQGrid.helpers.combo.findFirstUnusedVariable(table, choices);
    if (addSettingBtn) addSettingBtn.disabled = !OQGrid.helpers.combo.findFirstUnusedSetting(table, choices);
  };
})();
