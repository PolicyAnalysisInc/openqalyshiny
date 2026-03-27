/* Threshold Analysis Parameter Table — Tabulator */
(function() {
  "use strict";
  console.log("[Threshold Params] JS version 1.0.0 loaded");

  // =========================================================================
  // Tabulator CDN loader
  // =========================================================================
  var TABULATOR_CDN = "https://cdn.jsdelivr.net/npm/tabulator-tables@6.3.1/dist/js/tabulator.min.js";
  var TABULATOR_CSS = "https://cdn.jsdelivr.net/npm/tabulator-tables@6.3.1/dist/css/tabulator_bootstrap5.min.css";
  var _tabulatorCallbacks = [];
  var _tabulatorLoading = false;

  function ensureTabulator(callback) {
    if (typeof Tabulator !== "undefined") {
      callback();
      return;
    }
    _tabulatorCallbacks.push(callback);
    if (_tabulatorLoading) return;
    _tabulatorLoading = true;

    var cssLink = document.createElement("link");
    cssLink.rel = "stylesheet";
    cssLink.href = TABULATOR_CSS;
    document.head.appendChild(cssLink);

    var script = document.createElement("script");
    script.src = TABULATOR_CDN;
    script.onload = function() {
      console.log("[Threshold Params] Tabulator loaded");
      var cbs = _tabulatorCallbacks.slice();
      _tabulatorCallbacks = [];
      for (var i = 0; i < cbs.length; i++) cbs[i]();
    };
    script.onerror = function() {
      console.error("[Threshold Params] Failed to load Tabulator from CDN");
    };
    document.head.appendChild(script);
  }

  // =========================================================================
  // Helpers
  // =========================================================================

  function relayout(table) {
    var holder = table.element.querySelector(".tabulator-tableholder");
    var scrollLeft = holder ? holder.scrollLeft : 0;
    var scrollTop = holder ? holder.scrollTop : 0;

    table.redraw(true);

    requestAnimationFrame(function() {
      var containerWidth = table.element.clientWidth;
      var totalWidth = 0;
      var growCols = [];
      var totalGrow = 0;

      table.getColumns().forEach(function(col) {
        var def = col.getDefinition();
        var w = col.getWidth();
        totalWidth += w;
        if (def.widthGrow && def.widthGrow > 0) {
          growCols.push({ col: col, grow: def.widthGrow, width: w });
          totalGrow += def.widthGrow;
        }
      });

      if (totalWidth < containerWidth && growCols.length > 0) {
        var extra = containerWidth - totalWidth;
        growCols.forEach(function(c) {
          c.col.setWidth(c.width + Math.floor(extra * c.grow / totalGrow));
        });
      }

      holder = table.element.querySelector(".tabulator-tableholder");
      if (holder) {
        holder.scrollLeft = scrollLeft;
        holder.scrollTop = scrollTop;
      }
    });
  }

  function getTargeting(choices, name) {
    var t = choices.variableTargeting || {};
    var result = t[name] || { strategies: null, groups: null };
    if (typeof result.strategies === "string") result.strategies = [result.strategies];
    if (typeof result.groups === "string") result.groups = [result.groups];
    return result;
  }

  function applyTargetingDefaults(data, choices) {
    var targeting = getTargeting(choices, data.variable);
    if (targeting.strategies) {
      data.variable_strategy = targeting.strategies[0] || "";
    } else {
      data.variable_strategy = "";
    }
    if (targeting.groups) {
      data.variable_group = targeting.groups[0] || "";
    } else {
      data.variable_group = "";
    }
  }

  function getBaseCase(choices, data) {
    var key = (data.variable || "") + "|" + (data.variable_strategy || "") + "|" + (data.variable_group || "");
    return choices.variableFormulas[key] || "";
  }

  // =========================================================================
  // Condition display string
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
  // Condition Editor Overlay
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

  // Field definitions for the condition editor
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
  // Shiny sync
  // =========================================================================

  function syncToShiny(table, inputId) {
    var data = table.getData().map(function(d) {
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
    if (typeof Shiny !== "undefined") {
      Shiny.setInputValue(inputId, data, { priority: "event" });
    }
  }

  // =========================================================================
  // Column definitions
  // =========================================================================

  function buildColumnDefs(choices, inputId, metadata) {
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
          var data = cell.getRow().getData();
          var targeting = getTargeting(choices, data.variable);
          if (!targeting.strategies) return { values: [] };
          return {
            values: targeting.strategies.map(function(sVal) {
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
          var data = cell.getRow().getData();
          var targeting = getTargeting(choices, data.variable);
          return targeting.strategies !== null;
        },
        formatter: function(cell) {
          var data = cell.getRow().getData();
          var targeting = getTargeting(choices, data.variable);
          if (!targeting.strategies) return "\u2014";
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
          var data = cell.getRow().getData();
          var targeting = getTargeting(choices, data.variable);
          if (!targeting.groups) return { values: [] };
          return {
            values: targeting.groups.map(function(gVal) {
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
          var data = cell.getRow().getData();
          var targeting = getTargeting(choices, data.variable);
          return targeting.groups !== null;
        },
        formatter: function(cell) {
          var data = cell.getRow().getData();
          var targeting = getTargeting(choices, data.variable);
          if (!targeting.groups) return "\u2014";
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
          var data = cell.getRow().getData();
          return getBaseCase(choices, data);
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
          el.textContent = getConditionDisplayString(cell.getValue());
          return el;
        },
        cellDblClick: function(e, cell) {
          var tbl = cell.getTable();
          openConditionEditor(cell.getElement(), cell.getRow().getData().condition || {}, metadata, function(newCondition) {
            cell.getRow().update({ condition: newCondition });
            relayout(tbl);
            syncToShiny(tbl, inputId);
          });
        }
      },

      // Delete column
      {
        title: "",
        field: "_delete",
        width: 45,
        hozAlign: "center",
        headerSort: false,
        editor: false,
        clipboard: false,
        formatter: function(cell) {
          var btn = document.createElement("button");
          btn.type = "button";
          btn.className = "threshold-delete-btn";
          btn.textContent = "\u00d7";
          btn.addEventListener("click", function(ev) {
            ev.stopPropagation();
            var tbl = cell.getTable();
            cell.getRow().delete();
            relayout(tbl);
            syncToShiny(tbl, inputId);
          });
          return btn;
        }
      }
    ];
  }

  // =========================================================================
  // Grid initialization
  // =========================================================================

  var _activeTables = {};

  function initGrid(containerDiv) {
    var inputId = containerDiv.dataset.inputId;
    if (!inputId) return;

    if (_activeTables[inputId]) {
      try { _activeTables[inputId].destroy(); } catch (e) {}
      delete _activeTables[inputId];
    }

    var variableTargeting = {};
    try { variableTargeting = JSON.parse(containerDiv.dataset.variableTargeting || "{}"); } catch (e) {}

    var choices = {
      variables: JSON.parse(containerDiv.dataset.variables || "[]"),
      strategies: JSON.parse(containerDiv.dataset.strategies || "{}"),
      groups: JSON.parse(containerDiv.dataset.groups || "{}"),
      variableTargeting: variableTargeting,
      variableFormulas: JSON.parse(containerDiv.dataset.variableFormulas || "{}")
    };

    var outcomeSummariesRaw = JSON.parse(containerDiv.dataset.outcomeSummaries || "{}");
    var costSummariesRaw = JSON.parse(containerDiv.dataset.costSummaries || "{}");
    var metadata = {
      outcomeSummaries: Object.keys(outcomeSummariesRaw).map(function(k) {
        return { label: k, value: outcomeSummariesRaw[k] };
      }),
      costSummaries: Object.keys(costSummariesRaw).map(function(k) {
        return { label: k, value: costSummariesRaw[k] };
      }),
      strategies: Object.keys(choices.strategies).map(function(k) {
        return { label: k, value: choices.strategies[k] };
      }),
      groups: Object.keys(choices.groups).map(function(k) {
        return { label: k, value: choices.groups[k] };
      }),
      states: JSON.parse(containerDiv.dataset.states || "[]"),
      outcomeValues: JSON.parse(containerDiv.dataset.outcomeValues || "[]"),
      costValues: JSON.parse(containerDiv.dataset.costValues || "[]")
    };

    var initialData = [];
    try { initialData = JSON.parse(containerDiv.dataset.initial || "[]"); } catch (e) {}

    var columnDefs = buildColumnDefs(choices, inputId, metadata);

    var table = new Tabulator(containerDiv, {
      data: initialData,
      columns: columnDefs,
      layout: "fitData",
      layoutColumnsOnNewData: true,
      height: "100%",
      selectableRange: true,
      selectableRangeColumns: true,
      selectableRangeRows: true,
      selectableRangeClearCells: true,
      editTriggerEvent: "dblclick",
      clipboard: true,
      clipboardCopyRowRange: "range",
      clipboardPasteParser: "range",
      clipboardPasteAction: "range",
      clipboardCopyConfig: { rowHeaders: false, columnHeaders: false },
      clipboardCopyStyled: false,
      headerSortClickElement: "icon",

      rowFormatter: function(row) {
        var data = row.getData();
        var cells = row.getCells();
        for (var ci = 0; ci < cells.length; ci++) {
          var field = cells[ci].getColumn().getField();
          if (field === "variable_strategy" || field === "variable_group") {
            var el = cells[ci].getElement();
            var targeting = getTargeting(choices, data.variable);
            var isDisabled = (field === "variable_strategy" && !targeting.strategies) ||
                             (field === "variable_group" && !targeting.groups);
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
    });

    _activeTables[inputId] = table;

    table.on("cellEdited", function(cell) {
      var field = cell.getField();
      var data = cell.getRow().getData();

      if (field === "variable") {
        applyTargetingDefaults(data, choices);
        cell.getRow().update({
          variable_strategy: data.variable_strategy,
          variable_group: data.variable_group
        });
        relayout(table);
      }

      syncToShiny(table, inputId);
    });

    // Wire up Add button - search parent elements
    var addBtn = null;
    var runBtn = null;
    var parent = containerDiv.parentElement;
    while (parent && (!addBtn || !runBtn)) {
      if (!addBtn) addBtn = parent.querySelector(".threshold-add-btn");
      if (!runBtn) runBtn = parent.querySelector(".threshold-run-btn");
      parent = parent.parentElement;
    }

    if (addBtn) {
      var newAddBtn = addBtn.cloneNode(true);
      addBtn.parentNode.replaceChild(newAddBtn, addBtn);
      addBtn = newAddBtn;
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
        if (defaultVar) applyTargetingDefaults(newRow, choices);
        table.addRow(newRow);
        relayout(table);
        syncToShiny(table, inputId);
      });
    }

    if (runBtn) {
      var newRunBtn = runBtn.cloneNode(true);
      runBtn.parentNode.replaceChild(newRunBtn, runBtn);
      runBtn = newRunBtn;
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
        if (typeof Shiny !== "undefined") {
          Shiny.setInputValue("run_threshold_action", {
            nonce: Date.now(),
            analyses: analyses
          }, { priority: "event" });
        }
      });
    }

    syncToShiny(table, inputId);
    setTimeout(function() { relayout(table); }, 100);

    containerDiv.setAttribute("data-initialized", "true");
  }

  // =========================================================================
  // Lifecycle
  // =========================================================================

  function initAllGrids() {
    document.querySelectorAll(".threshold-params-container:not([data-initialized])").forEach(function(el) {
      ensureTabulator(function() { initGrid(el); });
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
