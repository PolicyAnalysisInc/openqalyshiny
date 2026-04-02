/* OQGrid — Distribution String Parser/Builder */
(function() {
  "use strict";

  var OQGrid = window.OQGrid = window.OQGrid || {};
  OQGrid.helpers = OQGrid.helpers || {};
  OQGrid.helpers.distributions = OQGrid.helpers.distributions || {};

  var DISTRIBUTIONS = OQGrid.helpers.distributions.DISTRIBUTIONS;
  var MV_DISTRIBUTIONS = OQGrid.helpers.distributions.MV_DISTRIBUTIONS;

  // =========================================================================
  // Split a string at top-level commas (respecting parentheses/brackets)
  // =========================================================================
  function splitTopLevel(str) {
    var result = [];
    var depth = 0;
    var start = 0;
    for (var i = 0; i < str.length; i++) {
      var ch = str.charAt(i);
      if (ch === "(" || ch === "[") depth++;
      else if (ch === ")" || ch === "]") depth--;
      else if (ch === "," && depth === 0) {
        result.push(str.substring(start, i));
        start = i + 1;
      }
    }
    result.push(str.substring(start));
    return result;
  }

  // =========================================================================
  // Get param names for a distribution config, optionally with parameterization
  // =========================================================================
  function getParamNames(config, parameterization) {
    if (config.params) return config.params;
    if (config.parameterizations) {
      if (parameterization && config.parameterizations[parameterization]) {
        var p = config.parameterizations[parameterization];
        return (p.params || []).concat(p.vectorParams || []).concat(p.scalarParams || []).concat(p.matrixParams || []);
      }
      var firstKey = Object.keys(config.parameterizations)[0];
      var first = config.parameterizations[firstKey];
      return (first.params || []).concat(first.vectorParams || []).concat(first.scalarParams || []).concat(first.matrixParams || []);
    }
    // Non-parameterized config
    return (config.params || []).concat(config.vectorParams || []).concat(config.scalarParams || []).concat(config.matrixParams || []);
  }

  // =========================================================================
  // Auto-detect parameterization from params object
  // =========================================================================
  function detectParameterization(type, params) {
    var config = DISTRIBUTIONS[type] || MV_DISTRIBUTIONS[type];
    if (!config || !config.parameterizations) return null;
    var keys = Object.keys(config.parameterizations);
    for (var i = 0; i < keys.length; i++) {
      var p = config.parameterizations[keys[i]];
      var pNames = (p.params || []).concat(p.vectorParams || []).concat(p.scalarParams || []).concat(p.matrixParams || []);
      var match = pNames.some(function(n) { return params.hasOwnProperty(n); });
      if (match) return keys[i];
    }
    return keys[0];
  }

  // =========================================================================
  // Parse a distribution string like "normal(mean = 0, sd = 1)"
  // Returns { type, parameterization, params } or null
  // =========================================================================
  OQGrid.helpers.distributions.parseDistributionString = function(str) {
    if (!str || !str.trim()) return null;
    str = str.trim();
    var parenIdx = str.indexOf("(");
    if (parenIdx < 0) return null;
    var type = str.substring(0, parenIdx).trim();
    var inner = str.substring(parenIdx + 1, str.length - 1).trim();
    var args = splitTopLevel(inner);
    var params = {};
    var positionalIdx = 0;
    var distConfig = DISTRIBUTIONS[type] || MV_DISTRIBUTIONS[type];
    if (!distConfig) return { type: type, parameterization: null, params: { _raw: inner } };

    for (var i = 0; i < args.length; i++) {
      var arg = args[i].trim();
      var eqIdx = arg.indexOf("=");
      if (eqIdx > 0 && /^[a-zA-Z_]/.test(arg.charAt(0))) {
        var name = arg.substring(0, eqIdx).trim();
        var val = arg.substring(eqIdx + 1).trim();
        params[name] = val;
      } else {
        var paramNames = getParamNames(distConfig, null);
        if (positionalIdx < paramNames.length) {
          params[paramNames[positionalIdx]] = arg;
        }
        positionalIdx++;
      }
    }
    var parameterization = detectParameterization(type, params);
    return { type: type, parameterization: parameterization, params: params };
  };

  // =========================================================================
  // Build a distribution string from type, parameterization, and params
  // =========================================================================
  OQGrid.helpers.distributions.buildDistributionString = function(type, parameterization, params) {
    var config = DISTRIBUTIONS[type] || MV_DISTRIBUTIONS[type];
    if (!config) return "";
    var pConfig;
    if (config.parameterizations && parameterization) {
      pConfig = config.parameterizations[parameterization];
    } else {
      pConfig = config;
    }
    if (!pConfig) return type + "()";
    var allNames = (pConfig.params || []).concat(pConfig.vectorParams || []).concat(pConfig.scalarParams || []).concat(pConfig.matrixParams || []);
    if (allNames.length === 0) return "";
    var parts = [];
    for (var i = 0; i < allNames.length; i++) {
      var name = allNames[i];
      var val = params[name];
      if (val !== undefined && val !== null && String(val).trim() !== "") {
        parts.push(name + " = " + val);
      }
    }
    return type + "(" + parts.join(", ") + ")";
  };

  // =========================================================================
  // Parse an R c() vector string into an array of values
  // =========================================================================
  OQGrid.helpers.distributions.parseCVector = function(str) {
    if (!str) return [];
    str = str.trim();
    if (str.substring(0, 2) === "c(" && str.charAt(str.length - 1) === ")") {
      var inner = str.substring(2, str.length - 1);
      return splitTopLevel(inner).map(function(s) { return s.trim(); });
    }
    return [str];
  };

  // =========================================================================
  // Build an R c() vector string from an array of values
  // =========================================================================
  OQGrid.helpers.distributions.buildCVector = function(arr) {
    if (!arr || arr.length === 0) return "";
    return "c(" + arr.join(", ") + ")";
  };

  // Expose splitTopLevel for internal use
  OQGrid.helpers.distributions._splitTopLevel = splitTopLevel;
})();
