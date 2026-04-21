/* OQGrid — Analysis Param Helpers: Variable Targeting */
(function() {
  "use strict";

  var OQGrid = window.OQGrid = window.OQGrid || {};
  OQGrid.helpers = OQGrid.helpers || {};
  OQGrid.helpers.targeting = {};

  // Normalize a value that may be {} (R jsonlite NULL serialization) to null
  function normalizeField(val) {
    if (val == null || val === "") return null;
    if (typeof val === "object" && !Array.isArray(val)) return null;
    return val;
  }

  // Normalize a scalar value for key building — {} and null become ""
  function norm(val) {
    if (val == null || val === "") return "";
    if (typeof val === "object") return "";
    return String(val);
  }

  // =========================================================================
  // Get targeting info (strategies/groups arrays) for a variable name.
  // Normalizes single-element R vectors (auto_unbox) into arrays,
  // and empty objects ({} from R NULL serialization) into null.
  // =========================================================================
  OQGrid.helpers.targeting.getTargeting = function(choices, name) {
    var t = choices.variableTargeting || {};
    var raw = t[name] || { strategies: null, groups: null };
    var strategies = normalizeField(raw.strategies);
    var groups = normalizeField(raw.groups);
    if (typeof strategies === "string") strategies = [strategies];
    if (typeof groups === "string") groups = [groups];
    return { strategies: strategies, groups: groups };
  };

  // =========================================================================
  // Fill empty strategy/group fields on a data object using targeting info.
  // Picks the first unused combination.
  // =========================================================================
  OQGrid.helpers.targeting.applyTargetingDefaults = function(data, choices, usedCombos) {
    var targeting = OQGrid.helpers.targeting.getTargeting(choices, data.name);
    if (!targeting.strategies && !targeting.groups) {
      data.strategy = "";
      data.group = "";
      return;
    }
    var strats = targeting.strategies || [""];
    var grps = targeting.groups || [""];
    var found = false;
    for (var si = 0; si < strats.length && !found; si++) {
      for (var gi = 0; gi < grps.length && !found; gi++) {
        var key = norm(strats[si]) + "|" + norm(grps[gi]);
        if (!usedCombos.has(key)) {
          data.strategy = norm(strats[si]);
          data.group = norm(grps[gi]);
          found = true;
        }
      }
    }
    if (!found) {
      data.strategy = norm(strats[0]);
      data.group = norm(grps[0]);
    }
  };

  // =========================================================================
  // Compute the base case value for a row.
  // Settings use settingValues lookup; variables use the variableFormulas map.
  // =========================================================================
  OQGrid.helpers.targeting.getBaseCase = function(choices, data) {
    if (data.type === "setting") {
      var val = choices.settingValues[data.name];
      return val !== undefined ? String(val) : "";
    }
    var key = norm(data.name) + "|" + norm(data.strategy) + "|" + norm(data.group);
    return choices.variableFormulas[key] || "";
  };

  // =========================================================================
  // Reverse-lookup: strategy value -> display name
  // =========================================================================
  OQGrid.helpers.targeting.strategyLabel = function(choices, val) {
    if (!val || val === "") return "";
    var keys = Object.keys(choices.strategies || {});
    for (var i = 0; i < keys.length; i++) {
      if (choices.strategies[keys[i]] === val) return keys[i];
    }
    return val;
  };

  // =========================================================================
  // Reverse-lookup: group value -> display name
  // =========================================================================
  OQGrid.helpers.targeting.groupLabel = function(choices, val) {
    if (!val || val === "") return "";
    var keys = Object.keys(choices.groups || {});
    for (var i = 0; i < keys.length; i++) {
      if (choices.groups[keys[i]] === val) return keys[i];
    }
    return val;
  };
})();
