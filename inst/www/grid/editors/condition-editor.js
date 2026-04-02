/* OQGrid — Threshold Condition Editor Overlay */
(function() {
  "use strict";

  var OQGrid = window.OQGrid = window.OQGrid || {};
  OQGrid.editors = OQGrid.editors || {};

  // =========================================================================
  // Active overlay tracking (one at a time)
  // =========================================================================
  var _activeOverlay = null;
  var _activeOverlayDocHandler = null;

  function closeActiveOverlay() {
    if (_activeOverlayDocHandler) {
      document.removeEventListener("mousedown", _activeOverlayDocHandler, true);
      _activeOverlayDocHandler = null;
    }
    if (_activeOverlay) {
      if (_activeOverlay.parentNode) _activeOverlay.parentNode.removeChild(_activeOverlay);
      _activeOverlay = null;
    }
  }

  // =========================================================================
  // Field definitions for the condition editor (all 18 fields)
  // =========================================================================
  var FIELD_DEFS = [
    {
      id: "output", label: "Output", field: "output", inputType: "select",
      getVisible: function() { return true; },
      getOptions: function() {
        return [
          { label: "Cost-Effectiveness", value: "ce" },
          { label: "Net Monetary Benefit", value: "nmb" },
          { label: "Outcomes", value: "outcomes" },
          { label: "Costs", value: "costs" },
          { label: "Trace", value: "trace" }
        ];
      }
    },
    {
      id: "summary_or_value", label: "Measure", field: "_summary_or_value", inputType: "select",
      getVisible: function(c) { return c.output === "outcomes" || c.output === "costs"; },
      getOptions: function() {
        return [
          { label: "Summary", value: "summary" },
          { label: "Individual Value", value: "value" }
        ];
      }
    },
    {
      id: "summary", label: "Summary", field: "summary", inputType: "select",
      getVisible: function(c) {
        return (c.output === "outcomes" || c.output === "costs") && c._summary_or_value === "summary";
      },
      getOptions: function(c, meta) {
        return c.output === "costs" ? meta.costSummaries : meta.outcomeSummaries;
      }
    },
    {
      id: "value", label: "Value", field: "value", inputType: "select",
      getVisible: function(c) {
        return (c.output === "outcomes" || c.output === "costs") && c._summary_or_value === "value";
      },
      getOptions: function(c, meta) {
        var vals = c.output === "costs" ? meta.costValues : meta.outcomeValues;
        return vals.map(function(v) { return { label: v, value: v }; });
      }
    },
    {
      id: "health_summary", label: "Health Summary", field: "health_summary", inputType: "select",
      getVisible: function(c) { return c.output === "ce" || c.output === "nmb"; },
      getOptions: function(c, meta) { return meta.outcomeSummaries; }
    },
    {
      id: "cost_summary", label: "Cost Summary", field: "cost_summary", inputType: "select",
      getVisible: function(c) { return c.output === "ce" || c.output === "nmb"; },
      getOptions: function(c, meta) { return meta.costSummaries; }
    },
    {
      id: "state", label: "State", field: "state", inputType: "select",
      getVisible: function(c) { return c.output === "trace"; },
      getOptions: function(c, meta) {
        return meta.states.map(function(s) { return { label: s, value: s }; });
      }
    },
    {
      id: "time", label: "Time", field: "time", inputType: "number",
      getVisible: function(c) { return c.output === "trace"; }
    },
    {
      id: "time_unit", label: "Time Unit", field: "time_unit", inputType: "select",
      getVisible: function(c) { return c.output === "trace"; },
      getOptions: function() {
        return [
          { label: "Cycle", value: "cycle" },
          { label: "Year", value: "year" },
          { label: "Month", value: "month" },
          { label: "Week", value: "week" },
          { label: "Day", value: "day" }
        ];
      }
    },
    {
      id: "type", label: "Type", field: "type", inputType: "select",
      getVisible: function(c) {
        return c.output === "outcomes" || c.output === "costs" || c.output === "trace";
      },
      getOptions: function() {
        return [
          { label: "Absolute", value: "absolute" },
          { label: "Difference", value: "difference" }
        ];
      }
    },
    {
      id: "strategy", label: "Strategy", field: "strategy", inputType: "select",
      getVisible: function(c) {
        return ((c.output === "outcomes" || c.output === "costs" || c.output === "trace") &&
                c.type === "absolute");
      },
      getOptions: function(c, meta) { return meta.strategies; }
    },
    {
      id: "referent", label: "Intervention", field: "referent", inputType: "select",
      getVisible: function(c) {
        if (c.output === "ce" || c.output === "nmb") return true;
        return (c.output === "outcomes" || c.output === "costs" || c.output === "trace") &&
               c.type === "difference";
      },
      getOptions: function(c, meta) { return meta.strategies; }
    },
    {
      id: "comparator", label: "Comparator", field: "comparator", inputType: "select",
      getVisible: function(c) {
        if (c.output === "ce" || c.output === "nmb") return true;
        return (c.output === "outcomes" || c.output === "costs" || c.output === "trace") &&
               c.type === "difference";
      },
      getOptions: function(c, meta) { return meta.strategies; }
    },
    {
      id: "discounted", label: "Discounted", field: "discounted", inputType: "select",
      getVisible: function(c) {
        return c.output === "outcomes" || c.output === "costs" ||
               c.output === "nmb" || c.output === "ce";
      },
      getOptions: function() {
        return [
          { label: "Yes", value: "true" },
          { label: "No", value: "false" }
        ];
      }
    },
    {
      id: "target_value", label: "Target Value", field: "target_value", inputType: "number",
      getVisible: function(c) {
        return c.output === "outcomes" || c.output === "costs" ||
               c.output === "nmb" || c.output === "trace";
      }
    },
    {
      id: "wtp", label: "WTP (optional)", field: "wtp", inputType: "number",
      getVisible: function(c) { return c.output === "nmb" || c.output === "ce"; }
    },
    {
      id: "group", label: "Group (optional)", field: "group", inputType: "select",
      getVisible: function() { return true; },
      getOptions: function(c, meta) {
        var opts = [{ label: "Overall", value: "" }];
        return opts.concat(meta.groups);
      }
    }
  ];

  // =========================================================================
  // Helpers
  // =========================================================================

  function deriveSummaryOrValue(cond) {
    if (cond.value && cond.value !== "") return "value";
    return "summary";
  }

  function parseFieldValue(def, rawValue) {
    if (def.inputType === "number") {
      return rawValue === "" ? null : Number(rawValue);
    }
    if (rawValue === "true") return true;
    if (rawValue === "false") return false;
    return rawValue;
  }

  function buildCleanCondition(cond) {
    var result = {};
    for (var key in cond) {
      if (key.charAt(0) !== "_") result[key] = cond[key];
    }
    if (result.output === "ce" || result.output === "nmb") {
      delete result.summary;
      delete result.value;
      delete result.type;
      delete result.strategy;
      delete result.state;
      delete result.time;
      delete result.time_unit;
      if (result.output === "ce") delete result.target_value;
    } else if (result.output === "outcomes" || result.output === "costs") {
      delete result.health_summary;
      delete result.cost_summary;
      delete result.state;
      delete result.time;
      delete result.time_unit;
      delete result.wtp;
      if (result.type === "absolute") {
        delete result.referent;
        delete result.comparator;
      } else {
        delete result.strategy;
      }
      if (cond._summary_or_value === "summary") {
        delete result.value;
      } else {
        delete result.summary;
      }
    } else if (result.output === "trace") {
      delete result.summary;
      delete result.value;
      delete result.health_summary;
      delete result.cost_summary;
      delete result.discounted;
      delete result.wtp;
      if (result.type === "absolute") {
        delete result.referent;
        delete result.comparator;
      } else {
        delete result.strategy;
      }
    }
    return result;
  }

  // =========================================================================
  // Condition display string (human-readable)
  // =========================================================================

  function getConditionDisplayString(condition) {
    if (!condition || !condition.output) return "\u2014";

    switch (condition.output) {
      case "ce":
        return "CE (" + (condition.health_summary || "?") + ", " +
          (condition.cost_summary || "?") + ") " +
          (condition.referent || "?") + " vs. " + (condition.comparator || "?");
      case "nmb":
        return "NMB (" + (condition.health_summary || "?") + ", " +
          (condition.cost_summary || "?") + ") " +
          (condition.referent || "?") + " vs. " + (condition.comparator || "?") +
          " = " + (condition.target_value != null ? condition.target_value : "?");
      case "outcomes":
      case "costs": {
        var measure = condition.summary || condition.value || "?";
        var disc = condition.discounted !== false ? "Disc." : "Undisc.";
        var strat = condition.type === "difference"
          ? (condition.referent || "?") + " vs. " + (condition.comparator || "?")
          : (condition.strategy || "?");
        return disc + " " + measure + " " + strat +
          " = " + (condition.target_value != null ? condition.target_value : "?");
      }
      case "trace": {
        var stratT = condition.type === "difference"
          ? (condition.referent || "?") + " vs. " + (condition.comparator || "?")
          : (condition.strategy || "?");
        return "Trace " + (condition.state || "?") + " @ t=" +
          (condition.time != null ? condition.time : "?") + " " +
          (condition.time_unit || "") + " " + stratT +
          " = " + (condition.target_value != null ? condition.target_value : "?");
      }
      default:
        return "\u2014";
    }
  }

  // =========================================================================
  // Open the condition editor overlay
  // =========================================================================

  function openConditionEditor(cellElement, currentCondition, metadata, onSave) {
    closeActiveOverlay();

    var cond = JSON.parse(JSON.stringify(currentCondition || {}));
    if (!cond.output) cond.output = "ce";
    if (cond._summary_or_value === undefined) {
      cond._summary_or_value = deriveSummaryOrValue(cond);
    }
    if (cond.discounted === undefined) cond.discounted = true;
    if (cond.type === undefined && (cond.output === "outcomes" || cond.output === "costs" || cond.output === "trace")) {
      cond.type = "absolute";
    }

    var overlay = document.createElement("div");
    overlay.className = "tc-editor-overlay";
    _activeOverlay = overlay;

    // Header
    var header = document.createElement("div");
    header.className = "tc-editor-header";
    var titleEl = document.createElement("span");
    titleEl.textContent = "Edit Condition";
    var closeBtn = document.createElement("button");
    closeBtn.className = "tc-editor-close-btn";
    closeBtn.type = "button";
    closeBtn.textContent = "\u00d7";
    closeBtn.addEventListener("click", function() { closeActiveOverlay(); });
    header.appendChild(titleEl);
    header.appendChild(closeBtn);
    overlay.appendChild(header);

    // Body
    var body = document.createElement("div");
    body.className = "tc-editor-body";
    overlay.appendChild(body);

    var fieldElements = {};
    for (var fi = 0; fi < FIELD_DEFS.length; fi++) {
      var def = FIELD_DEFS[fi];
      var row = document.createElement("div");
      row.className = "tc-field-row";
      row.dataset.fieldId = def.id;

      var lbl = document.createElement("label");
      lbl.textContent = def.label;
      row.appendChild(lbl);

      var input;
      if (def.inputType === "select") {
        input = document.createElement("select");
        input.className = "tc-field-select";
      } else {
        input = document.createElement("input");
        input.type = "number";
        input.className = "tc-field-input";
        input.step = "any";
      }
      input.dataset.fieldId = def.id;
      row.appendChild(input);
      body.appendChild(row);

      fieldElements[def.id] = { container: row, input: input, def: def };
    }

    // Footer
    var footer = document.createElement("div");
    footer.className = "tc-editor-footer";
    var cancelBtn = document.createElement("button");
    cancelBtn.type = "button";
    cancelBtn.className = "tc-editor-cancel-btn";
    cancelBtn.textContent = "Cancel";
    cancelBtn.addEventListener("click", function() { closeActiveOverlay(); });
    var saveBtn = document.createElement("button");
    saveBtn.type = "button";
    saveBtn.className = "tc-editor-save-btn";
    saveBtn.textContent = "Save";
    saveBtn.addEventListener("click", function() {
      var result = buildCleanCondition(cond);
      closeActiveOverlay();
      onSave(result);
    });
    footer.appendChild(cancelBtn);
    footer.appendChild(saveBtn);
    overlay.appendChild(footer);

    // Block events from reaching Tabulator
    ["mousedown", "pointerdown", "click", "mouseup", "pointerup", "focusin"].forEach(function(evt) {
      overlay.addEventListener(evt, function(e) { e.stopPropagation(); });
    });

    document.body.appendChild(overlay);

    // Position below the cell
    var cellRect = cellElement.getBoundingClientRect();
    overlay.style.left = cellRect.left + "px";
    overlay.style.top = (cellRect.bottom + 4) + "px";

    requestAnimationFrame(function() {
      var rect = overlay.getBoundingClientRect();
      if (rect.right > window.innerWidth - 8) {
        overlay.style.left = Math.max(8, window.innerWidth - rect.width - 8) + "px";
      }
      if (rect.bottom > window.innerHeight - 8) {
        overlay.style.top = Math.max(8, cellRect.top - rect.height - 4) + "px";
      }
    });

    // Click outside to close
    _activeOverlayDocHandler = function(e) {
      if (!overlay.contains(e.target)) {
        closeActiveOverlay();
      }
    };
    setTimeout(function() {
      document.addEventListener("mousedown", _activeOverlayDocHandler, true);
    }, 0);

    // =====================================================================
    // Internal field management
    // =====================================================================

    function populateOptions(fieldId) {
      var fe = fieldElements[fieldId];
      if (!fe || fe.def.inputType !== "select") return;
      var opts = fe.def.getOptions ? fe.def.getOptions(cond, metadata) : [];
      var selectEl = fe.input;
      var currentVal = cond[fe.def.field];

      // Remove existing options safely
      while (selectEl.firstChild) {
        selectEl.removeChild(selectEl.firstChild);
      }

      for (var oi = 0; oi < opts.length; oi++) {
        var opt = document.createElement("option");
        opt.value = opts[oi].value;
        opt.textContent = opts[oi].label;
        if (String(opts[oi].value) === String(currentVal)) {
          opt.selected = true;
        }
        selectEl.appendChild(opt);
      }
      if (selectEl.selectedIndex === -1 && opts.length > 0) {
        selectEl.selectedIndex = 0;
        cond[fe.def.field] = parseFieldValue(fe.def, selectEl.value);
      }
    }

    function updateAll() {
      for (var fid in fieldElements) {
        var fe = fieldElements[fid];
        var visible = fe.def.getVisible(cond);
        if (visible) {
          fe.container.classList.remove("tc-hidden");
          if (fe.def.inputType === "select" && fe.def.getOptions) {
            populateOptions(fid);
          } else if (fe.def.inputType === "number") {
            var val = cond[fe.def.field];
            fe.input.value = (val != null && val !== "") ? val : "";
          }
        } else {
          fe.container.classList.add("tc-hidden");
        }
      }
    }

    function onOutputChange(newOutput) {
      cond.output = newOutput;

      if (newOutput === "ce" || newOutput === "nmb") {
        if (!cond.health_summary && metadata.outcomeSummaries.length > 0) {
          cond.health_summary = metadata.outcomeSummaries[0].value;
        }
        if (!cond.cost_summary && metadata.costSummaries.length > 0) {
          cond.cost_summary = metadata.costSummaries[0].value;
        }
        if (!cond.referent && metadata.strategies.length > 0) {
          cond.referent = metadata.strategies[0].value;
        }
        if (!cond.comparator && metadata.strategies.length > 1) {
          cond.comparator = metadata.strategies[1].value;
        } else if (!cond.comparator && metadata.strategies.length > 0) {
          cond.comparator = metadata.strategies[0].value;
        }
        if (cond.discounted === undefined) cond.discounted = true;
        if (newOutput === "nmb" && cond.target_value === undefined) cond.target_value = 0;
      } else if (newOutput === "outcomes" || newOutput === "costs") {
        cond._summary_or_value = cond._summary_or_value || "summary";
        cond.type = cond.type || "absolute";
        if (cond.discounted === undefined) cond.discounted = true;
        if (cond.target_value === undefined) cond.target_value = 0;
        if (cond.type === "absolute" && !cond.strategy && metadata.strategies.length > 0) {
          cond.strategy = metadata.strategies[0].value;
        }
        if (cond._summary_or_value === "summary") {
          var summList = newOutput === "costs" ? metadata.costSummaries : metadata.outcomeSummaries;
          if (!cond.summary && summList.length > 0) cond.summary = summList[0].value;
        }
      } else if (newOutput === "trace") {
        cond.type = cond.type || "absolute";
        if (cond.target_value === undefined) cond.target_value = 0;
        if (!cond.state && metadata.states.length > 0) cond.state = metadata.states[0];
        if (cond.time === undefined) cond.time = 1;
        if (!cond.time_unit) cond.time_unit = "cycle";
        if (cond.type === "absolute" && !cond.strategy && metadata.strategies.length > 0) {
          cond.strategy = metadata.strategies[0].value;
        }
      }
      updateAll();
    }

    function onTypeChange(newType) {
      cond.type = newType;
      if (newType === "absolute") {
        if (!cond.strategy) {
          cond.strategy = cond.referent || (metadata.strategies.length > 0 ? metadata.strategies[0].value : "");
        }
      } else {
        if (!cond.referent) {
          cond.referent = cond.strategy || (metadata.strategies.length > 0 ? metadata.strategies[0].value : "");
        }
        if (!cond.comparator && metadata.strategies.length > 1) {
          var altStrat = metadata.strategies.find(function(s) { return s.value !== cond.referent; });
          if (altStrat) cond.comparator = altStrat.value;
        }
        if (!cond.comparator && metadata.strategies.length > 0) {
          cond.comparator = metadata.strategies[0].value;
        }
      }
      updateAll();
    }

    // Wire up change events
    for (var fid in fieldElements) {
      (function(fieldId) {
        var fe = fieldElements[fieldId];
        var eventName = fe.def.inputType === "select" ? "change" : "input";
        fe.input.addEventListener(eventName, function() {
          var rawVal = fe.input.value;
          var val = parseFieldValue(fe.def, rawVal);

          if (fieldId === "output") {
            onOutputChange(val);
            return;
          }
          if (fieldId === "type") {
            onTypeChange(val);
            return;
          }

          cond[fe.def.field] = val;

          if (fieldId === "referent" && cond.comparator === val && metadata.strategies.length > 1) {
            var alt = metadata.strategies.find(function(s) { return s.value !== val; });
            if (alt) cond.comparator = alt.value;
            updateAll();
          } else if (fieldId === "comparator" && cond.referent === val && metadata.strategies.length > 1) {
            var alt2 = metadata.strategies.find(function(s) { return s.value !== val; });
            if (alt2) cond.referent = alt2.value;
            updateAll();
          } else if (fieldId === "summary_or_value") {
            if (val === "summary") {
              cond.value = null;
              var sList = cond.output === "costs" ? metadata.costSummaries : metadata.outcomeSummaries;
              if (!cond.summary && sList.length > 0) cond.summary = sList[0].value;
            } else {
              cond.summary = null;
              var vList = cond.output === "costs" ? metadata.costValues : metadata.outcomeValues;
              if (!cond.value && vList.length > 0) cond.value = vList[0];
            }
            updateAll();
          }
        });
      })(fid);
    }

    // Initial render (call twice to ensure consistency after defaults are set)
    updateAll();
    updateAll();
  }

  // =========================================================================
  // Public API
  // =========================================================================

  // Factory returning a Tabulator cell editor function
  OQGrid.editors.condition = function(metadata) {
    return function(cell, onRendered, success, cancel) {
      var placeholder = document.createElement("div");

      onRendered(function() {
        openConditionEditor(cell.getElement(), cell.getRow().getData().condition || {}, metadata, function(newCondition) {
          success(newCondition);
        });
      });

      return placeholder;
    };
  };

  // Standalone open function (for cellDblClick usage)
  OQGrid.editors.openConditionEditor = openConditionEditor;

  // Display string formatter
  OQGrid.editors.getConditionDisplayString = getConditionDisplayString;

  // Expose FIELD_DEFS for potential extension
  OQGrid.editors.CONDITION_FIELD_DEFS = FIELD_DEFS;
})();
