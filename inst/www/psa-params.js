/* PSA Parameter Table — Tabulator + Distribution Editor */
(function() {
  "use strict";
  console.log("[PSA Params] JS version 1.0.0 loaded");

  // =========================================================================
  // Tabulator CDN loader (idempotent — shared with DSA)
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
      var cbs = _tabulatorCallbacks.slice();
      _tabulatorCallbacks = [];
      for (var i = 0; i < cbs.length; i++) cbs[i]();
    };
    script.onerror = function() {
      console.error("[PSA Params] Failed to load Tabulator from CDN");
    };
    document.head.appendChild(script);
  }

  // =========================================================================
  // Handsontable CDN loader
  // =========================================================================
  var HOT_CDN_JS = "https://cdn.jsdelivr.net/npm/handsontable@14.6.1/dist/handsontable.full.min.js";
  var HOT_CDN_CSS = "https://cdn.jsdelivr.net/npm/handsontable@14.6.1/dist/handsontable.full.min.css";
  var _hotCallbacks = [];
  var _hotLoading = false;

  function ensureHandsontable(callback) {
    if (typeof Handsontable !== "undefined") {
      callback();
      return;
    }
    _hotCallbacks.push(callback);
    if (_hotLoading) return;
    _hotLoading = true;
    var cssLink = document.createElement("link");
    cssLink.rel = "stylesheet";
    cssLink.href = HOT_CDN_CSS;
    document.head.appendChild(cssLink);
    var script = document.createElement("script");
    script.src = HOT_CDN_JS;
    script.onload = function() {
      var cbs = _hotCallbacks.slice();
      _hotCallbacks = [];
      for (var i = 0; i < cbs.length; i++) cbs[i]();
    };
    script.onerror = function() {
      console.error("[PSA Params] Failed to load Handsontable from CDN");
    };
    document.head.appendChild(script);
  }

  // =========================================================================
  // Utility: clear all children from a DOM element (safe alternative to innerHTML = "")
  // =========================================================================
  function clearChildren(el) {
    while (el.firstChild) el.removeChild(el.firstChild);
  }

  // =========================================================================
  // Distribution registry
  // =========================================================================
  var DISTRIBUTIONS = {
    normal:      { params: ["mean", "sd"], labels: ["Mean", "SD"] },
    lognormal:   { parameterizations: {
      mean_sd:       { params: ["mean", "sd"], labels: ["Mean", "SD"] },
      meanlog_sdlog: { params: ["meanlog", "sdlog"], labels: ["Mean (log)", "SD (log)"] }
    }},
    gamma:       { parameterizations: {
      mean_sd:     { params: ["mean", "sd"], labels: ["Mean", "SD"] },
      shape_scale: { params: ["shape", "scale"], labels: ["Shape", "Scale"] }
    }},
    beta:        { parameterizations: {
      mean_sd:       { params: ["mean", "sd"], labels: ["Mean", "SD"] },
      shape1_shape2: { params: ["shape1", "shape2"], labels: ["Shape 1", "Shape 2"] }
    }},
    uniform:     { params: ["min", "max"], labels: ["Min", "Max"] },
    triangular:  { params: ["min", "mode", "max"], labels: ["Min", "Mode", "Max"] },
    bootstrap:   { params: ["x"], labels: ["Data"] }
  };

  var MV_DISTRIBUTIONS = {
    dirichlet:   { vectorParams: ["alpha"], vectorLabels: ["Alpha"] },
    mvnormal:    { parameterizations: {
      sd_cor: { vectorParams: ["mean", "sd"], scalarParams: ["cor"],
                vectorLabels: ["Mean", "SD"], scalarLabels: ["Correlation"] },
      cov:    { vectorParams: ["mean"], matrixParams: ["cov"],
                vectorLabels: ["Mean"], matrixLabels: ["Covariance"] }
    }},
    multinomial: { scalarParams: ["size"], vectorParams: ["prob"],
                   scalarLabels: ["Size"], vectorLabels: ["Probability"] }
  };

  // =========================================================================
  // Distribution string parser/builder
  // =========================================================================

  function parseDistributionString(str) {
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
  }

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
    return [];
  }

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

  function buildDistributionString(type, parameterization, params) {
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
    var parts = [];
    for (var i = 0; i < allNames.length; i++) {
      var name = allNames[i];
      var val = params[name];
      if (val !== undefined && val !== null && String(val).trim() !== "") {
        parts.push(name + " = " + val);
      }
    }
    return type + "(" + parts.join(", ") + ")";
  }

  function parseCVector(str) {
    if (!str) return [];
    str = str.trim();
    if (str.substring(0, 2) === "c(" && str.charAt(str.length - 1) === ")") {
      var inner = str.substring(2, str.length - 1);
      return splitTopLevel(inner).map(function(s) { return s.trim(); });
    }
    return [str];
  }

  function buildCVector(arr) {
    if (!arr || arr.length === 0) return "";
    return "c(" + arr.join(", ") + ")";
  }

  // =========================================================================
  // Helpers (shared patterns from DSA)
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

  function normalizeSegmentValue(value) {
    return value || "";
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

  // _rowCombos: our own source of truth for name/strategy/group per row.
  // Keyed by stable IDs stored on row DOM elements as data-combo-id.
  // NEVER reads from Tabulator row data (which may not reflect row.update() calls).

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

  function getBaseCase(choices, data) {
    var key = buildVariableKey(data.name, data.strategy, data.group);
    return choices.variableFormulas[key] || "";
  }

  function findFirstUnusedVariable(table, choices) {
    return findFirstUnusedInstance(choices, "", getUsedInstanceKeys(table));
  }

  // =========================================================================
  // Mini Ace Editor factory
  // =========================================================================

  function createMiniAceEditor(container, value, terms, suggestions, opts) {
    opts = opts || {};
    container.className = (container.className || "") + " psa-mini-ace";

    if (typeof ace === "undefined") {
      var inp = document.createElement("input");
      inp.type = "text";
      inp.className = "psa-input-editor";
      inp.value = value || "";
      container.appendChild(inp);
      return {
        getValue: function() { return inp.value; },
        setValue: function(v) { inp.value = v; },
        destroy: function() {},
        focus: function() { inp.focus(); },
        editor: null,
        element: inp
      };
    }

    var aceDiv = document.createElement("div");
    aceDiv.style.width = "100%";
    aceDiv.style.height = "100%";
    container.appendChild(aceDiv);

    ace.require("ace/ext/language_tools");
    var editor = ace.edit(aceDiv);
    editor.setTheme("ace/theme/chrome");
    editor.session.setMode("ace/mode/r");
    editor.setOptions({
      showGutter: false,
      showPrintMargin: false,
      highlightActiveLine: false,
      showFoldWidgets: false,
      displayIndentGuides: false,
      scrollPastEnd: 0,
      useSoftTabs: true,
      tabSize: 2,
      enableBasicAutocompletion: true,
      enableLiveAutocompletion: true,
      enableSnippets: false,
      maxLines: 1
    });
    editor.setValue(value || "", -1);

    try {
      if (terms && typeof FormulaInputMode !== "undefined") {
        FormulaInputMode.injectDefaultStyles();
        var hl = new FormulaInputMode.FormulaHighlighter(editor);
        hl.setTerms(terms);
      }
      if (suggestions && typeof FormulaInputAutocomplete !== "undefined") {
        var cmp = new FormulaInputAutocomplete.FormulaCompleter(editor, suggestions);
        editor.completers = [cmp];
      }
    } catch (e) {
      console.warn("[PSA Params] Highlighter/autocomplete init failed:", e.message);
    }

    editor.commands.addCommand({
      name: "acceptCompletion",
      bindKey: { win: "Tab", mac: "Tab" },
      exec: function(ed) {
        if (ed.completer && ed.completer.popup && ed.completer.popup.isOpen) {
          ed.completer.insertMatch();
          return true;
        }
        if (opts.onTab) { opts.onTab(); return true; }
        return false;
      }
    });

    aceDiv.addEventListener("paste", function(e) {
      e.preventDefault();
      e.stopPropagation();
      var text = (e.clipboardData || window.clipboardData).getData("text");
      editor.insert(text.replace(/[\r\n]+/g, " "));
    }, true);

    editor.on("focus", function() { container.classList.add("focused"); });
    editor.on("blur", function() { container.classList.remove("focused"); });

    return {
      getValue: function() { return editor.getValue(); },
      setValue: function(v) { editor.setValue(v || "", -1); },
      destroy: function() { try { editor.destroy(); } catch (e) {} },
      focus: function() { editor.focus(); },
      editor: editor,
      element: aceDiv
    };
  }

  // =========================================================================
  // Distribution overlay editor (univariate)
  // =========================================================================

  function distributionEditor(terms, suggestions) {
    return function(cell, onRendered, success, cancel) {
      var placeholder = document.createElement("div");
      placeholder.className = "psa-formula-placeholder";
      placeholder.addEventListener("focusout", function(e) { e.stopPropagation(); });

      var currentValue = cell.getValue() || "";
      var committed = false;
      var overlay = null;
      var miniEditors = [];
      var onDocMouseDown = null;
      var cellFocusRedirect = null;
      var editCellEl = null;

      var cellEl = cell.getElement();
      var cellRect = cellEl.getBoundingClientRect();

      function commit(val) {
        if (committed) return;
        committed = true;
        cleanup();
        try { success(val); } catch (e) {}
      }
      function doCancel() {
        if (committed) return;
        committed = true;
        cleanup();
        try { cancel(); } catch (e) {}
      }
      function cleanup() {
        if (onDocMouseDown) {
          document.removeEventListener("mousedown", onDocMouseDown, true);
          onDocMouseDown = null;
        }
        if (cellFocusRedirect && editCellEl) {
          editCellEl.removeEventListener("focus", cellFocusRedirect, true);
          cellFocusRedirect = null;
        }
        miniEditors.forEach(function(ed) { ed.destroy(); });
        miniEditors = [];
        if (overlay && overlay.parentNode) {
          overlay.parentNode.removeChild(overlay);
          overlay = null;
        }
      }

      function collectValue() {
        var select = overlay ? overlay.querySelector(".psa-dist-select") : null;
        if (!select) return currentValue;
        var type = select.value;
        if (!type) return "";
        var paramRadio = overlay.querySelector("input[name='psa-param-radio']:checked");
        var parameterization = paramRadio ? paramRadio.value : null;
        var params = {};
        miniEditors.forEach(function(ed) {
          if (ed._paramName) {
            params[ed._paramName] = ed.getValue();
          }
        });
        return buildDistributionString(type, parameterization, params);
      }

      onRendered(function() {
        overlay = document.createElement("div");
        overlay.className = "psa-distribution-overlay";
        overlay.style.position = "fixed";
        overlay.style.left = cellRect.left + "px";
        overlay.style.top = (cellRect.bottom + 2) + "px";
        overlay.style.minWidth = Math.max(320, cellRect.width) + "px";
        overlay.style.zIndex = "10000";
        document.body.appendChild(overlay);

        requestAnimationFrame(function() {
          if (!overlay) return;
          var overlayRect = overlay.getBoundingClientRect();
          if (overlayRect.bottom > window.innerHeight) {
            overlay.style.top = (cellRect.top - overlayRect.height - 2) + "px";
          }
        });

        ["mousedown", "pointerdown", "click", "mouseup", "pointerup", "focusin"].forEach(function(evt) {
          overlay.addEventListener(evt, function(e) { e.stopPropagation(); });
        });

        onDocMouseDown = function(e) {
          if (!overlay || committed) {
            document.removeEventListener("mousedown", onDocMouseDown, true);
            return;
          }
          var isOverlay = overlay.contains(e.target);
          var isAutocomplete = false;
          try { isAutocomplete = e.target.closest && e.target.closest(".ace_autocomplete"); } catch (x) {}
          if (!isOverlay && !isAutocomplete) {
            commit(collectValue());
          }
        };
        document.addEventListener("mousedown", onDocMouseDown, true);

        var parsed = parseDistributionString(currentValue);
        var currentType = parsed ? parsed.type : "";
        var currentParams = parsed ? parsed.params : {};
        var currentParam = parsed ? parsed.parameterization : null;

        buildOverlayContent(overlay, currentType, currentParam, currentParams);

        overlay.addEventListener("keydown", function(e) {
          if (e.key === "Escape") {
            e.preventDefault();
            e.stopPropagation();
            for (var i = 0; i < miniEditors.length; i++) {
              var ed = miniEditors[i].editor;
              if (ed && ed.completer && ed.completer.popup && ed.completer.popup.isOpen) {
                ed.completer.detach();
                return;
              }
            }
            doCancel();
          }
        }, true);

        editCellEl = cellEl;
        var savedTabindex = cellEl.getAttribute("tabindex");
        cellEl.setAttribute("tabindex", "-1");
        cellEl.style.pointerEvents = "none";
        cellFocusRedirect = function() {
          if (committed) return;
          var select = overlay ? overlay.querySelector(".psa-dist-select") : null;
          if (select) select.focus();
        };
        cellEl.addEventListener("focus", cellFocusRedirect, true);

        var origCleanup = cleanup;
        cleanup = function() {
          if (editCellEl) {
            editCellEl.removeEventListener("focus", cellFocusRedirect, true);
            if (savedTabindex !== null) {
              editCellEl.setAttribute("tabindex", savedTabindex);
            } else {
              editCellEl.removeAttribute("tabindex");
            }
            editCellEl.style.pointerEvents = "";
          }
          origCleanup();
        };

        var select = overlay.querySelector(".psa-dist-select");
        if (select) select.focus();
      });

      function buildOverlayContent(container, type, parameterization, params) {
        clearChildren(container);
        miniEditors.forEach(function(ed) { ed.destroy(); });
        miniEditors = [];

        var dropdownRow = document.createElement("div");
        dropdownRow.className = "psa-dist-dropdown-row";
        var label = document.createElement("label");
        label.textContent = "Distribution:";
        var select = document.createElement("select");
        select.className = "form-select form-select-sm psa-dist-select";
        var optEmpty = document.createElement("option");
        optEmpty.value = "";
        optEmpty.textContent = "\u2014 Select \u2014";
        select.appendChild(optEmpty);
        Object.keys(DISTRIBUTIONS).forEach(function(k) {
          var opt = document.createElement("option");
          opt.value = k;
          opt.textContent = k;
          if (k === type) opt.selected = true;
          select.appendChild(opt);
        });
        dropdownRow.appendChild(label);
        dropdownRow.appendChild(select);
        container.appendChild(dropdownRow);

        select.addEventListener("change", function() {
          buildParamsSection(container, select.value, null, {});
          if (miniEditors.length > 0) miniEditors[0].focus();
        });

        if (type && DISTRIBUTIONS[type]) {
          buildParamsSection(container, type, parameterization, params);
        }
      }

      function buildParamsSection(container, type, parameterization, params) {
        var existing = container.querySelector(".psa-params-section");
        if (existing) existing.parentNode.removeChild(existing);
        miniEditors.forEach(function(ed) { ed.destroy(); });
        miniEditors = [];

        if (!type || !DISTRIBUTIONS[type]) return;
        var config = DISTRIBUTIONS[type];
        var section = document.createElement("div");
        section.className = "psa-params-section";

        if (config.parameterizations) {
          var pKeys = Object.keys(config.parameterizations);
          if (!parameterization || !config.parameterizations[parameterization]) {
            parameterization = pKeys[0];
          }
          var toggleDiv = document.createElement("div");
          toggleDiv.className = "psa-parameterization-toggle";
          pKeys.forEach(function(pk) {
            var lbl = document.createElement("label");
            var radio = document.createElement("input");
            radio.type = "radio";
            radio.name = "psa-param-radio";
            radio.value = pk;
            if (pk === parameterization) radio.checked = true;
            radio.addEventListener("change", function() {
              var currentParams = {};
              miniEditors.forEach(function(ed) {
                if (ed._paramName) currentParams[ed._paramName] = ed.getValue();
              });
              buildParamsSection(container, type, pk, currentParams);
              if (miniEditors.length > 0) miniEditors[0].focus();
            });
            var pConfig = config.parameterizations[pk];
            var displayName = (pConfig.labels || pConfig.params || []).join("/");
            lbl.appendChild(radio);
            lbl.appendChild(document.createTextNode(" " + displayName));
            toggleDiv.appendChild(lbl);
          });
          section.appendChild(toggleDiv);
        }

        var activeConfig;
        if (config.parameterizations) {
          activeConfig = config.parameterizations[parameterization || Object.keys(config.parameterizations)[0]];
        } else {
          activeConfig = config;
        }

        var paramNames = activeConfig.params;
        var paramLabels = activeConfig.labels;

        for (var i = 0; i < paramNames.length; i++) {
          var row = document.createElement("div");
          row.className = "psa-param-row";
          var paramLabel = document.createElement("label");
          paramLabel.textContent = (paramLabels && paramLabels[i]) || paramNames[i];
          row.appendChild(paramLabel);

          var aceContainer = document.createElement("div");
          row.appendChild(aceContainer);

          var paramValue = params[paramNames[i]] || "";
          var isLast = (i === paramNames.length - 1);

          var ed = createMiniAceEditor(aceContainer, paramValue, terms, suggestions, {
            onTab: (function(idx) {
              return function() {
                if (idx + 1 < miniEditors.length) miniEditors[idx + 1].focus();
              };
            })(i)
          });
          ed._paramName = paramNames[i];
          miniEditors.push(ed);

          if (ed.editor) {
            (function(editor, last, idx) {
              editor.container.parentElement.addEventListener("keydown", function(e) {
                if (committed) return;
                if (e.key === "Enter") {
                  if (editor.completer && editor.completer.popup && editor.completer.popup.isOpen) {
                    editor.completer.detach();
                    e.preventDefault();
                    e.stopPropagation();
                    return;
                  }
                  e.preventDefault();
                  e.stopPropagation();
                  if (last) {
                    commit(collectValue());
                  } else if (idx + 1 < miniEditors.length) {
                    miniEditors[idx + 1].focus();
                  }
                }
              }, true);
            })(ed.editor, isLast, i);
          }

          section.appendChild(row);
        }

        container.appendChild(section);
      }

      return placeholder;
    };
  }

  // =========================================================================
  // Multivariate distribution overlay editor
  // =========================================================================

  function getModalMultiValue(el) {
    if (!el) return [];
    if (typeof $ !== "undefined") {
      var jqVal = $(el).val();
      if (Array.isArray(jqVal)) return jqVal;
      return jqVal ? [jqVal] : [];
    }
    if (el.multiple && el.selectedOptions) {
      return Array.prototype.map.call(el.selectedOptions, function(opt) { return opt.value; });
    }
    return el.value ? [el.value] : [];
  }

  function destroyMvBuilderState(state) {
    if (!state) return;
    if (state.miniEditors) {
      state.miniEditors.forEach(function(ed) {
        try { ed.destroy(); } catch (e) {}
      });
      state.miniEditors = [];
    }
    if (state.hotInstance) {
      try { state.hotInstance.destroy(); } catch (e) {}
      state.hotInstance = null;
    }
    if (state.container && state.container._mvBuilderSyncTimeout) {
      clearTimeout(state.container._mvBuilderSyncTimeout);
      state.container._mvBuilderSyncTimeout = null;
    }
  }

  function collectMvBuilderValue(state) {
    var container = state && state.container;
    var select = container ? container.querySelector(".psa-mv-dist-select") : null;
    if (!select) return "";
    var type = select.value;
    if (!type) return "";
    var paramRadio = container.querySelector("input[name='" + state.radioName + "']:checked");
    var parameterization = paramRadio ? paramRadio.value : null;
    var config = MV_DISTRIBUTIONS[type];
    if (!config) return "";
    var activeConfig = (config.parameterizations && parameterization)
      ? config.parameterizations[parameterization]
      : config;
    var params = {};

    state.miniEditors.forEach(function(ed) {
      if (ed._paramName && !ed._isVector) {
        params[ed._paramName] = ed.getValue();
      }
    });

    (activeConfig.vectorParams || []).forEach(function(vp) {
      var values = [];
      state.miniEditors.forEach(function(ed) {
        if (ed._paramName === vp && ed._isVector) {
          values[ed._vectorIdx] = ed.getValue();
        }
      });
      if (values.length > 0) params[vp] = buildCVector(values);
    });

    (activeConfig.matrixParams || []).forEach(function(mp) {
      if (state.hotInstance) {
        var data = state.hotInstance.getData();
        var n = data.length;
        var flat = [];
        for (var r = 0; r < n; r++) {
          for (var c = 0; c < n; c++) {
            flat.push(data[r][c] != null ? data[r][c] : 0);
          }
        }
        params[mp] = "matrix(c(" + flat.join(", ") + "), nrow = " + n + ")";
      }
    });

    return buildDistributionString(type, parameterization, params);
  }

  function scheduleMvBuilderSync(state) {
    if (!state || !state.onChange) return;
    if (state.container && state.container._mvBuilderSyncTimeout) {
      clearTimeout(state.container._mvBuilderSyncTimeout);
    }
    state.container._mvBuilderSyncTimeout = setTimeout(function() {
      if (!state || !state.onChange) return;
      state.onChange(collectMvBuilderValue(state));
    }, 0);
  }

  function buildMvParamsSectionShared(state, type, parameterization, params, vars) {
    var container = state.container;
    var existing = container.querySelector(".psa-mv-params-section");
    if (existing) existing.parentNode.removeChild(existing);
    if (state.emptyNote && state.emptyNote.parentNode) {
      state.emptyNote.parentNode.removeChild(state.emptyNote);
      state.emptyNote = null;
    }

    destroyMvBuilderState(state);

    if (!type || !MV_DISTRIBUTIONS[type]) return;

    if (!vars || vars.length === 0) {
      var emptyNote = document.createElement("div");
      emptyNote.className = "form-text";
      emptyNote.textContent = "Select variables to configure distribution parameters.";
      container.appendChild(emptyNote);
      state.emptyNote = emptyNote;
      scheduleMvBuilderSync(state);
      return;
    }

    var config = MV_DISTRIBUTIONS[type];
    var section = document.createElement("div");
    section.className = "psa-mv-params-section";

    if (config.parameterizations) {
      var pKeys = Object.keys(config.parameterizations);
      if (!parameterization || !config.parameterizations[parameterization]) {
        parameterization = pKeys[0];
      }
      var toggleDiv = document.createElement("div");
      toggleDiv.className = "psa-parameterization-toggle";
      pKeys.forEach(function(pk) {
        var lbl = document.createElement("label");
        var radio = document.createElement("input");
        radio.type = "radio";
        radio.name = state.radioName;
        radio.value = pk;
        if (pk === parameterization) radio.checked = true;
        radio.addEventListener("change", function() {
          var currentParams = {};
          state.miniEditors.forEach(function(ed) {
            if (ed._paramName && !ed._isVector) {
              currentParams[ed._paramName] = ed.getValue();
            }
          });
          (config.parameterizations[parameterization].vectorParams || []).forEach(function(vp) {
            var values = [];
            state.miniEditors.forEach(function(ed) {
              if (ed._paramName === vp && ed._isVector) values[ed._vectorIdx] = ed.getValue();
            });
            if (values.length > 0) currentParams[vp] = buildCVector(values);
          });
          buildMvParamsSectionShared(state, type, pk, currentParams, vars);
          scheduleMvBuilderSync(state);
        });
        var pConfig = config.parameterizations[pk];
        var allLabels = (pConfig.vectorLabels || []).concat(pConfig.scalarLabels || []).concat(pConfig.matrixLabels || []);
        lbl.appendChild(radio);
        lbl.appendChild(document.createTextNode(" " + allLabels.join("/")));
        toggleDiv.appendChild(lbl);
      });
      section.appendChild(toggleDiv);
    }

    var activeConfig = (config.parameterizations && parameterization)
      ? config.parameterizations[parameterization]
      : config;

    var scalarParams = activeConfig.scalarParams || [];
    var scalarLabels = activeConfig.scalarLabels || scalarParams;
    scalarParams.forEach(function(sp, idx) {
      var row = document.createElement("div");
      row.className = "psa-param-row";
      var lbl = document.createElement("label");
      lbl.textContent = scalarLabels[idx] || sp;
      row.appendChild(lbl);
      var aceContainer = document.createElement("div");
      row.appendChild(aceContainer);
      var val = params[sp] || "";
      var ed = createMiniAceEditor(aceContainer, val, state.terms, state.suggestions, {});
      ed._paramName = sp;
      ed._isVector = false;
      state.miniEditors.push(ed);
      if (ed.editor) ed.editor.session.on("change", function() { scheduleMvBuilderSync(state); });
      else if (ed.element) ed.element.addEventListener("input", function() { scheduleMvBuilderSync(state); });
      section.appendChild(row);
    });

    var vectorParams = activeConfig.vectorParams || [];
    var vectorLabels = activeConfig.vectorLabels || vectorParams;
    vectorParams.forEach(function(vp, vpIdx) {
      var vecDiv = document.createElement("div");
      vecDiv.className = "psa-vector-editor";
      var tbl = document.createElement("table");
      var thead = document.createElement("thead");
      var hrow = document.createElement("tr");
      var th1 = document.createElement("th");
      th1.textContent = "Variable";
      var th2 = document.createElement("th");
      th2.textContent = vectorLabels[vpIdx] || vp;
      hrow.appendChild(th1);
      hrow.appendChild(th2);
      thead.appendChild(hrow);
      tbl.appendChild(thead);

      var tbody = document.createElement("tbody");
      var parsedVec = parseCVector(params[vp] || "");

      for (var vi = 0; vi < vars.length; vi++) {
        var tr = document.createElement("tr");
        var td1 = document.createElement("td");
        td1.textContent = vars[vi];
        var td2 = document.createElement("td");
        var vectorContainer = document.createElement("div");
        td2.appendChild(vectorContainer);
        tr.appendChild(td1);
        tr.appendChild(td2);
        tbody.appendChild(tr);

        var vectorVal = (vi < parsedVec.length) ? parsedVec[vi] : "";
        var vectorEd = createMiniAceEditor(vectorContainer, vectorVal, state.terms, state.suggestions, {});
        vectorEd._paramName = vp;
        vectorEd._isVector = true;
        vectorEd._vectorIdx = vi;
        state.miniEditors.push(vectorEd);
        if (vectorEd.editor) vectorEd.editor.session.on("change", function() { scheduleMvBuilderSync(state); });
        else if (vectorEd.element) vectorEd.element.addEventListener("input", function() { scheduleMvBuilderSync(state); });
      }
      tbl.appendChild(tbody);
      vecDiv.appendChild(tbl);
      section.appendChild(vecDiv);
    });

    var matrixParams = activeConfig.matrixParams || [];
    var matrixLabels = activeConfig.matrixLabels || matrixParams;
    matrixParams.forEach(function(mp, mpIdx) {
      var matDiv = document.createElement("div");
      matDiv.className = "psa-matrix-editor";
      var matLabel = document.createElement("div");
      matLabel.className = "psa-matrix-label";
      matLabel.textContent = matrixLabels[mpIdx] || mp;
      matDiv.appendChild(matLabel);

      var hotContainer = document.createElement("div");
      matDiv.appendChild(hotContainer);
      section.appendChild(matDiv);

      var n = vars.length;
      var matrixData = [];
      for (var r = 0; r < n; r++) {
        var row = [];
        for (var c = 0; c < n; c++) row.push(0);
        matrixData.push(row);
      }

      var rawMatrix = params[mp] || "";
      if (rawMatrix) {
        var matMatch = rawMatrix.match(/matrix\s*\(\s*c\s*\(([^)]*)\)/);
        if (matMatch) {
          var vals = matMatch[1].split(",").map(function(s) { return parseFloat(s.trim()); });
          var idx = 0;
          for (var r2 = 0; r2 < n && idx < vals.length; r2++) {
            for (var c2 = 0; c2 < n && idx < vals.length; c2++) {
              if (!isNaN(vals[idx])) matrixData[r2][c2] = vals[idx];
              idx++;
            }
          }
        }
      }

      ensureHandsontable(function() {
        if (!document.body.contains(hotContainer)) return;
        state.hotInstance = new Handsontable(hotContainer, {
          data: matrixData,
          rowHeaders: vars,
          colHeaders: vars,
          width: "100%",
          height: "auto",
          type: "numeric",
          numericFormat: { pattern: "0.0000" },
          afterChange: function(changes, source) {
            if (source === "edit" && changes) {
              var self = this;
              changes.forEach(function(change) {
                var rowIdx = change[0], colIdx = change[1], newVal = change[3];
                if (rowIdx !== colIdx) self.setDataAtCell(colIdx, rowIdx, newVal, "symmetry");
              });
            }
            if (source !== "loadData") scheduleMvBuilderSync(state);
          },
          licenseKey: "non-commercial-and-evaluation"
        });
        scheduleMvBuilderSync(state);
      });
    });

    container.appendChild(section);
    scheduleMvBuilderSync(state);
  }

  function buildMvDistributionBuilder(state, type, parameterization, params) {
    var container = state.container;
    clearChildren(container);
    destroyMvBuilderState(state);

    var dropdownRow = document.createElement("div");
    dropdownRow.className = "psa-dist-dropdown-row";
    var label = document.createElement("label");
    label.textContent = "Distribution:";
    var select = document.createElement("select");
    select.className = "form-select form-select-sm psa-mv-dist-select";
    var optEmpty = document.createElement("option");
    optEmpty.value = "";
    optEmpty.textContent = "\u2014 Select \u2014";
    select.appendChild(optEmpty);
    Object.keys(MV_DISTRIBUTIONS).forEach(function(k) {
      var opt = document.createElement("option");
      opt.value = k;
      opt.textContent = k;
      if (k === type) opt.selected = true;
      select.appendChild(opt);
    });
    select.disabled = !state.variables || state.variables.length === 0;
    dropdownRow.appendChild(label);
    dropdownRow.appendChild(select);
    container.appendChild(dropdownRow);

    if (!state.variables || state.variables.length === 0) {
      var prompt = document.createElement("div");
      prompt.className = "form-text";
      prompt.textContent = "Select variables before configuring the distribution.";
      container.appendChild(prompt);
      state.emptyNote = prompt;
      scheduleMvBuilderSync(state);
      return;
    }

    select.addEventListener("change", function() {
      buildMvParamsSectionShared(state, select.value, null, {}, state.variables);
      scheduleMvBuilderSync(state);
    });

    if (type && MV_DISTRIBUTIONS[type]) {
      buildMvParamsSectionShared(state, type, parameterization, params, state.variables);
    } else {
      scheduleMvBuilderSync(state);
    }
  }

  function createMvDistributionBuilder(container, opts) {
    opts = opts || {};
    var initialValue = opts.value || "";
    var parsed = parseDistributionString(initialValue);
    var state = {
      container: container,
      variables: opts.variables || [],
      terms: opts.terms || null,
      suggestions: opts.suggestions || null,
      miniEditors: [],
      hotInstance: null,
      radioName: "psa-mv-param-radio-" + Date.now() + "-" + Math.round(Math.random() * 100000),
      onChange: opts.onChange || function() {},
      emptyNote: null
    };
    container._mvBuilderState = state;
    buildMvDistributionBuilder(
      state,
      parsed ? parsed.type : "",
      parsed ? parsed.parameterization : null,
      parsed ? parsed.params : {}
    );
    return state;
  }

  function setMvDistributionBuilderVariables(state, variables) {
    if (!state) return;
    state.variables = variables || [];
    var parsed = parseDistributionString(collectMvBuilderValue(state));
    buildMvDistributionBuilder(
      state,
      parsed ? parsed.type : "",
      parsed ? parsed.parameterization : null,
      parsed ? parsed.params : {}
    );
  }

  function initAddMvModalBuilder() {
    var container = document.getElementById("add_mv_distribution_builder");
    if (!container || container.hasAttribute("data-initialized")) return;

    var variablesInput = document.getElementById("add_mv_variables");
    var terms = null;
    var suggestions = null;
    try { terms = JSON.parse(container.dataset.terms || "null"); } catch (e) {}
    try { suggestions = JSON.parse(container.dataset.suggestions || "null"); } catch (e) {}

    function syncToShiny(value) {
      if (typeof Shiny !== "undefined") {
        Shiny.setInputValue("add_mv_distribution", value || "", { priority: "event" });
      }
    }

    var state = createMvDistributionBuilder(container, {
      variables: getModalMultiValue(variablesInput),
      terms: terms,
      suggestions: suggestions,
      onChange: syncToShiny
    });

    function handleVariableChange() {
      setMvDistributionBuilderVariables(state, getModalMultiValue(variablesInput));
    }

    if (variablesInput) {
      variablesInput.addEventListener("change", handleVariableChange);
    }

    container._mvModalCleanup = function() {
      if (variablesInput) variablesInput.removeEventListener("change", handleVariableChange);
      destroyMvBuilderState(state);
      syncToShiny("");
      if (container._mvBuilderState === state) delete container._mvBuilderState;
      container.removeAttribute("data-initialized");
    };

    syncToShiny(collectMvBuilderValue(state));
    container.setAttribute("data-initialized", "true");
  }

  function cleanupAddMvModalBuilder() {
    var container = document.getElementById("add_mv_distribution_builder");
    if (!container || !container._mvModalCleanup) return;
    container._mvModalCleanup();
    delete container._mvModalCleanup;
  }

  function mvDistributionEditor(choices, terms, suggestions) {
    return function(cell, onRendered, success, cancel) {
      var placeholder = document.createElement("div");
      placeholder.className = "psa-formula-placeholder";
      placeholder.addEventListener("focusout", function(e) { e.stopPropagation(); });

      var currentValue = cell.getValue() || "";
      var rowData = cell.getRow().getData();
      var variables = rowData.variables || [];
      var committed = false;
      var overlay = null;
      var miniEditors = [];
      var hotInstance = null;
      var onDocMouseDown = null;
      var cellFocusRedirect = null;
      var editCellEl = null;

      var cellEl = cell.getElement();
      var cellRect = cellEl.getBoundingClientRect();

      function commit(val) {
        if (committed) return;
        committed = true;
        cleanup();
        try { success(val); } catch (e) {}
      }
      function doCancel() {
        if (committed) return;
        committed = true;
        cleanup();
        try { cancel(); } catch (e) {}
      }
      function cleanup() {
        if (onDocMouseDown) {
          document.removeEventListener("mousedown", onDocMouseDown, true);
          onDocMouseDown = null;
        }
        if (cellFocusRedirect && editCellEl) {
          editCellEl.removeEventListener("focus", cellFocusRedirect, true);
          cellFocusRedirect = null;
        }
        if (overlay && overlay._mvBuilderState) {
          destroyMvBuilderState(overlay._mvBuilderState);
          delete overlay._mvBuilderState;
        }
        miniEditors = [];
        hotInstance = null;
        if (overlay && overlay.parentNode) {
          overlay.parentNode.removeChild(overlay);
          overlay = null;
        }
      }

      function collectValue() {
        var state = overlay ? overlay._mvBuilderState : null;
        return state ? collectMvBuilderValue(state) : currentValue;
      }

      onRendered(function() {
        overlay = document.createElement("div");
        overlay.className = "psa-distribution-overlay";
        overlay.style.position = "fixed";
        overlay.style.left = cellRect.left + "px";
        overlay.style.top = (cellRect.bottom + 2) + "px";
        overlay.style.minWidth = Math.max(400, cellRect.width) + "px";
        overlay.style.zIndex = "10000";
        document.body.appendChild(overlay);

        requestAnimationFrame(function() {
          if (!overlay) return;
          var overlayRect = overlay.getBoundingClientRect();
          if (overlayRect.bottom > window.innerHeight) {
            overlay.style.top = (cellRect.top - overlayRect.height - 2) + "px";
          }
        });

        ["mousedown", "pointerdown", "click", "mouseup", "pointerup", "focusin"].forEach(function(evt) {
          overlay.addEventListener(evt, function(e) { e.stopPropagation(); });
        });

        onDocMouseDown = function(e) {
          if (!overlay || committed) {
            document.removeEventListener("mousedown", onDocMouseDown, true);
            return;
          }
          var isOverlay = overlay.contains(e.target);
          var isAutocomplete = false;
          try { isAutocomplete = e.target.closest && e.target.closest(".ace_autocomplete"); } catch (x) {}
          if (!isOverlay && !isAutocomplete) {
            commit(collectValue());
          }
        };
        document.addEventListener("mousedown", onDocMouseDown, true);

        var parsed = parseDistributionString(currentValue);
        createMvDistributionBuilder(overlay, {
          value: parsed ? currentValue : "",
          variables: variables,
          terms: terms,
          suggestions: suggestions
        });

        overlay.addEventListener("keydown", function(e) {
          if (e.key === "Escape") {
            e.preventDefault();
            e.stopPropagation();
            var activeEditors = overlay && overlay._mvBuilderState ? overlay._mvBuilderState.miniEditors : [];
            for (var i = 0; i < activeEditors.length; i++) {
              var ed = activeEditors[i].editor;
              if (ed && ed.completer && ed.completer.popup && ed.completer.popup.isOpen) {
                ed.completer.detach();
                return;
              }
            }
            doCancel();
          }
        }, true);

        editCellEl = cellEl;
        var savedTabindex = cellEl.getAttribute("tabindex");
        cellEl.setAttribute("tabindex", "-1");
        cellEl.style.pointerEvents = "none";
        cellFocusRedirect = function() {
          if (committed) return;
          var select = overlay ? overlay.querySelector(".psa-mv-dist-select") : null;
          if (select) select.focus();
        };
        cellEl.addEventListener("focus", cellFocusRedirect, true);

        var origCleanup = cleanup;
        cleanup = function() {
          if (editCellEl) {
            editCellEl.removeEventListener("focus", cellFocusRedirect, true);
            if (savedTabindex !== null) {
              editCellEl.setAttribute("tabindex", savedTabindex);
            } else {
              editCellEl.removeAttribute("tabindex");
            }
            editCellEl.style.pointerEvents = "";
          }
          origCleanup();
        };

        var select = overlay.querySelector(".psa-mv-dist-select");
        if (select) select.focus();
      });

      return placeholder;
    };
  }

  // =========================================================================
  // Multi-select overlay editor (for variables column in MV grid)
  // =========================================================================

  function multiSelectEditor(allVariables) {
    return function(cell, onRendered, success, cancel) {
      var placeholder = document.createElement("div");
      placeholder.className = "psa-formula-placeholder";

      var currentValue = cell.getValue() || [];
      if (typeof currentValue === "string") {
        try { currentValue = JSON.parse(currentValue); } catch (e) { currentValue = []; }
      }
      var committed = false;
      var overlay = null;
      var onDocMouseDown = null;

      var cellEl = cell.getElement();
      var cellRect = cellEl.getBoundingClientRect();

      function commit(val) {
        if (committed) return;
        committed = true;
        if (onDocMouseDown) document.removeEventListener("mousedown", onDocMouseDown, true);
        if (overlay && overlay.parentNode) overlay.parentNode.removeChild(overlay);
        try { success(val); } catch (e) {}
      }
      function doCancel() {
        if (committed) return;
        committed = true;
        if (onDocMouseDown) document.removeEventListener("mousedown", onDocMouseDown, true);
        if (overlay && overlay.parentNode) overlay.parentNode.removeChild(overlay);
        try { cancel(); } catch (e) {}
      }

      onRendered(function() {
        overlay = document.createElement("div");
        overlay.className = "psa-multiselect-overlay";
        overlay.style.position = "fixed";
        overlay.style.left = cellRect.left + "px";
        overlay.style.top = (cellRect.bottom + 2) + "px";
        overlay.style.zIndex = "10001";
        document.body.appendChild(overlay);

        ["mousedown", "pointerdown", "click", "mouseup", "pointerup", "focusin"].forEach(function(evt) {
          overlay.addEventListener(evt, function(e) { e.stopPropagation(); });
        });

        var selected = new Set(currentValue);
        allVariables.forEach(function(v) {
          var lbl = document.createElement("label");
          var cb = document.createElement("input");
          cb.type = "checkbox";
          cb.value = v;
          cb.checked = selected.has(v);
          cb.addEventListener("change", function() {
            if (cb.checked) selected.add(v);
            else selected.delete(v);
          });
          lbl.appendChild(cb);
          lbl.appendChild(document.createTextNode(" " + v));
          overlay.appendChild(lbl);
        });

        onDocMouseDown = function(e) {
          if (!overlay || committed) return;
          if (!overlay.contains(e.target)) commit(Array.from(selected));
        };
        document.addEventListener("mousedown", onDocMouseDown, true);

        overlay.addEventListener("keydown", function(e) {
          if (e.key === "Escape") { e.preventDefault(); doCancel(); }
          else if (e.key === "Enter") { e.preventDefault(); commit(Array.from(selected)); }
        });
      });

      return placeholder;
    };
  }

  // =========================================================================
  // Shiny sync functions
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
    if (typeof Shiny !== "undefined") {
      Shiny.setInputValue(inputId, data, { priority: "event" });
    }
  }

  function syncMultivariateToShiny(table, inputId) {
    var data = table.getData().map(function(d) {
      return {
        name: d.name || "",
        distribution: d.distribution || "",
        variables: d.variables || [],
        description: d.description || ""
      };
    });
    if (typeof Shiny !== "undefined") {
      Shiny.setInputValue(inputId, data, { priority: "event" });
    }
  }

  // =========================================================================
  // Univariate column definitions
  // =========================================================================

  function buildUnivariateColumns(choices, inputId, terms, suggestions) {
    var strategyKeys = Object.keys(choices.strategies);
    var groupKeys = Object.keys(choices.groups);

    return [
      {
        title: "Name", field: "name", widthGrow: 1, minWidth: 120,
        editor: "list",
        editorParams: function(cell) {
          var currentData = cell.getRow().getData();
          var usedKeys = getUsedInstanceKeys(cell.getTable(), getComboId(cell));
          return { values: getSelectableNames(choices, currentData.name, usedKeys)
            .map(function(name) { return { label: name, value: name }; }) };
        }
      },
      {
        title: "Strategy", field: "strategy", widthGrow: 1, minWidth: 100,
        editor: "list",
        editorParams: function(cell) {
          var data = cell.getRow().getData();
          var targeting = getTargeting(choices, data.name);
          if (!targeting.strategies) return { values: [] };
          var currentGroup = data.group || "";
          var usedKeys = getUsedInstanceKeys(cell.getTable(), getComboId(cell));
          var strategyValues = [];
          getInstancesForName(choices, data.name).forEach(function(instance) {
            if (normalizeSegmentValue(instance.group) !== currentGroup) return;
            if (usedKeys.has(getInstanceKey(instance))) return;
            var strategyVal = normalizeSegmentValue(instance.strategy);
            if (strategyValues.indexOf(strategyVal) >= 0) return;
            strategyValues.push(strategyVal);
          });
          return { values: strategyValues.map(function(strategyVal) {
              var sLabel = strategyVal;
              for (var sk = 0; sk < strategyKeys.length; sk++) {
                if (choices.strategies[strategyKeys[sk]] === strategyVal) { sLabel = strategyKeys[sk]; break; }
              }
              return { label: sLabel, value: strategyVal };
            }) };
        },
        editable: function(cell) { return getTargeting(choices, cell.getRow().getData().name).strategies !== null; },
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
      {
        title: "Group", field: "group", widthGrow: 1, minWidth: 100,
        editor: "list",
        editorParams: function(cell) {
          var data = cell.getRow().getData();
          var targeting = getTargeting(choices, data.name);
          if (!targeting.groups) return { values: [] };
          var currentStrategy = data.strategy || "";
          var usedKeys = getUsedInstanceKeys(cell.getTable(), getComboId(cell));
          var groupValues = [];
          getInstancesForName(choices, data.name).forEach(function(instance) {
            if (normalizeSegmentValue(instance.strategy) !== currentStrategy) return;
            if (usedKeys.has(getInstanceKey(instance))) return;
            var groupVal = normalizeSegmentValue(instance.group);
            if (groupValues.indexOf(groupVal) >= 0) return;
            groupValues.push(groupVal);
          });
          return { values: groupValues.map(function(groupVal) {
              var gLabel = groupVal;
              for (var gk = 0; gk < groupKeys.length; gk++) {
                if (choices.groups[groupKeys[gk]] === groupVal) { gLabel = groupKeys[gk]; break; }
              }
              return { label: gLabel, value: groupVal };
            }) };
        },
        editable: function(cell) { return getTargeting(choices, cell.getRow().getData().name).groups !== null; },
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
      {
        title: "Base Case", field: "_baseCase", widthGrow: 1, minWidth: 120,
        editor: false, headerSort: false, clipboard: false,
        formatter: function(cell) { return getBaseCase(choices, cell.getRow().getData()); }
      },
      {
        title: "Sampling", field: "sampling", widthGrow: 2, minWidth: 250,
        editor: distributionEditor(terms, suggestions),
        formatter: function(cell) {
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
      },
      {
        title: "", field: "_delete", width: 60, widthGrow: 0, hozAlign: "center",
        headerSort: false, editor: false, clipboard: false,
        formatter: function(cell) {
          var btn = document.createElement("button");
          btn.type = "button";
          btn.className = "psa-delete-btn";
          btn.textContent = "\u00d7";
          btn.addEventListener("click", function(e) {
            e.stopPropagation();
            var rowData = cell.getRow().getData();
            var tbl = cell.getTable();
            var delComboId = getComboId(cell);
            setInstanceSampling(choices, rowData.name, rowData.strategy, rowData.group, "");
            cell.getRow().delete();
            removeRowCombo(tbl, delComboId);
            relayout(tbl);
            syncUnivariateToShiny(tbl, inputId);
            if (typeof Shiny !== "undefined") {
              Shiny.setInputValue("model_action", {
                type: "remove_variable_sampling",
                variable: rowData.name || "",
                strategy: rowData.strategy || "",
                group: rowData.group || ""
              }, { priority: "event" });
            }
          });
          return btn;
        }
      }
    ];
  }

  // =========================================================================
  // Multivariate column definitions
  // =========================================================================

  function buildMultivariateColumns(choices, inputId, terms, suggestions) {
    return [
      { title: "Name", field: "name", widthGrow: 1, minWidth: 120, editor: "input" },
      {
        title: "Distribution", field: "distribution", widthGrow: 2, minWidth: 250,
        editor: mvDistributionEditor(choices, terms, suggestions),
        formatter: function(cell) {
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
      },
      {
        title: "Variables", field: "variables", widthGrow: 1, minWidth: 150,
        editor: multiSelectEditor(choices.variables),
        formatter: function(cell) {
          var val = cell.getValue();
          if (!val || (Array.isArray(val) && val.length === 0)) return "";
          if (Array.isArray(val)) return val.join(", ");
          return String(val);
        }
      },
      { title: "Description", field: "description", widthGrow: 1, minWidth: 120, editor: "input" },
      {
        title: "", field: "_delete", width: 60, widthGrow: 0, hozAlign: "center",
        headerSort: false, editor: false, clipboard: false,
        formatter: function(cell) {
          var btn = document.createElement("button");
          btn.type = "button";
          btn.className = "psa-delete-btn";
          btn.textContent = "\u00d7";
          btn.addEventListener("click", function(e) {
            e.stopPropagation();
            var rowData = cell.getRow().getData();
            var tbl = cell.getTable();
            cell.getRow().delete();
            relayout(tbl);
            syncMultivariateToShiny(tbl, inputId);
            if (typeof Shiny !== "undefined") {
              Shiny.setInputValue("model_action", {
                type: "remove_multivariate_sampling",
                name: rowData.name || ""
              }, { priority: "event" });
            }
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
      variableInstances: JSON.parse(containerDiv.dataset.variableInstances || "[]"),
      variableFormulas: JSON.parse(containerDiv.dataset.variableFormulas || "{}")
    };

    var terms = null;
    var suggestions = null;
    try { terms = JSON.parse(containerDiv.dataset.terms || "null"); } catch (e) {}
    try { suggestions = JSON.parse(containerDiv.dataset.suggestions || "null"); } catch (e) {}

    var initialData = [];
    try { initialData = JSON.parse(containerDiv.dataset.initial || "[]"); } catch (e) {}

    var isMultivariate = containerDiv.classList.contains("psa-multivariate-container");
    var columnDefs = isMultivariate
      ? buildMultivariateColumns(choices, inputId, terms, suggestions)
      : buildUnivariateColumns(choices, inputId, terms, suggestions);
    var syncFn = isMultivariate ? syncMultivariateToShiny : syncUnivariateToShiny;

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
        if (isMultivariate) return;
        var data = row.getData();
        row.getCells().forEach(function(c) {
          var field = c.getColumn().getField();
          if (field === "strategy" || field === "group") {
            var el = c.getElement();
            var targeting = getTargeting(choices, data.name);
            var isDisabled = (field === "strategy" && !targeting.strategies) ||
                             (field === "group" && !targeting.groups);
            el.style.color = isDisabled ? "var(--bs-secondary, #6c757d)" : "";
            el.style.fontStyle = isDisabled ? "italic" : "";
          }
        });
      }
    });

    // Fix: redraw when tab becomes visible
    if (typeof ResizeObserver !== "undefined") {
      var ro = new ResizeObserver(function(entries) {
        for (var i = 0; i < entries.length; i++) {
          if (entries[i].contentRect.width > 0) {
            relayout(table);
            ro.disconnect();
            break;
          }
        }
      });
      ro.observe(containerDiv);
    }

    _activeTables[inputId] = table;
    initRowCombos(table, initialData);

    table.on("cellEdited", function(cell) {
      var field = cell.getField();
      var data = cell.getRow().getData();
      var oldValue = cell.getOldValue();

      if (isMultivariate) {
        if (typeof Shiny !== "undefined") {
          if (field === "name" && oldValue) {
            Shiny.setInputValue("model_action", {
              type: "edit_multivariate_sampling", name: oldValue, new_name: data.name
            }, { priority: "event" });
          } else if (field === "distribution") {
            Shiny.setInputValue("model_action", {
              type: "edit_multivariate_sampling", name: data.name, distribution: data.distribution || ""
            }, { priority: "event" });
          } else if (field === "variables") {
            Shiny.setInputValue("model_action", {
              type: "edit_multivariate_sampling", name: data.name, variables: data.variables || []
            }, { priority: "event" });
          } else if (field === "description") {
            Shiny.setInputValue("model_action", {
              type: "edit_multivariate_sampling", name: data.name, description: data.description || ""
            }, { priority: "event" });
          }
        }
        syncMultivariateToShiny(table, inputId);
      } else {
        if (typeof Shiny !== "undefined") {
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
                Shiny.setInputValue("model_action", {
                  type: "rename_variable_sampling",
                  variable: oldName,
                  strategy: oldStrategy,
                  group: oldGroup,
                  new_variable: data.name,
                  new_strategy: data.strategy || "",
                  new_group: data.group || "",
                  sampling: data.sampling || ""
                }, { priority: "event" });
              } else {
                data.name = oldName;
                cell.getRow().update({ name: oldName });
                setRowCombo(cell.getTable(), cid, oldName, oldStrategy, oldGroup);
              }
            }
          } else if (field === "sampling") {
            setInstanceSampling(choices, data.name, data.strategy, data.group, data.sampling || "");
            Shiny.setInputValue("model_action", {
              type: "edit_variable_sampling",
              variable: data.name, strategy: data.strategy || "",
              group: data.group || "", sampling: data.sampling || ""
            }, { priority: "event" });
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
              Shiny.setInputValue("model_action", {
                type: "rename_variable_sampling",
                variable: data.name,
                strategy: priorStrategy,
                group: priorGroup,
                new_variable: data.name,
                new_strategy: data.strategy || "",
                new_group: data.group || "",
                sampling: data.sampling || ""
              }, { priority: "event" });
            }
          }
        }
        syncUnivariateToShiny(table, inputId);
      }
      relayout(table);
    });

    // Wire add button
    var parent = containerDiv.parentElement;
    var addBtnClass = isMultivariate ? ".psa-add-mv-btn" : ".psa-add-variable-btn";
    var addBtn = parent ? parent.querySelector(addBtnClass) : null;
    if (addBtn) {
      var newBtn = addBtn.cloneNode(true);
      addBtn.parentNode.replaceChild(newBtn, addBtn);
      addBtn = newBtn;
      addBtn.addEventListener("click", function() {
        if (isMultivariate) {
          if (typeof Shiny !== "undefined") {
            Shiny.setInputValue("show_add_multivariate_modal", {
              nonce: Date.now()
            }, { priority: "event" });
          }
        } else {
          var unused = findFirstUnusedVariable(table, choices);
          if (!unused) return;
          var newRow2 = {
            name: unused.name,
            display_name: unused.name,
            strategy: normalizeSegmentValue(unused.strategy),
            group: normalizeSegmentValue(unused.group),
            sampling: unused.sampling || ""
          };
          table.addRow(newRow2, false).then(function(row) {
            relayout(table);
            row.getElement().scrollIntoView({ behavior: "smooth", block: "nearest" });
            addRowCombo(table, row, newRow2.name, newRow2.strategy, newRow2.group);
            syncUnivariateToShiny(table, inputId);
          });
        }
      });
    }

    syncFn(table, inputId);
    containerDiv.setAttribute("data-initialized", "true");
  }

  // =========================================================================
  // PSA Settings (n_sim / seed)
  // =========================================================================

  function initSettings(settingsDiv) {
    if (!settingsDiv || settingsDiv.hasAttribute("data-initialized")) return;
    var nSimInput = settingsDiv.querySelector(".psa-nsim-input");
    var seedInput = settingsDiv.querySelector(".psa-seed-input");
    function sendSettings() {
      if (typeof Shiny !== "undefined") {
        Shiny.setInputValue("model_action", {
          type: "set_psa_settings",
          n_sim: nSimInput ? parseInt(nSimInput.value, 10) || 1000 : 1000,
          seed: seedInput ? seedInput.value : ""
        }, { priority: "event" });
      }
    }
    if (nSimInput) nSimInput.addEventListener("change", sendSettings);
    if (seedInput) seedInput.addEventListener("change", sendSettings);
    settingsDiv.setAttribute("data-initialized", "true");
  }

  // =========================================================================
  // Run button
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
        return { name: d.name || "", distribution: d.distribution || "", variables: d.variables || [], description: d.description || "" };
      }) : [];

      if (typeof Shiny !== "undefined") {
        Shiny.setInputValue("run_psa_action", {
          nonce: Date.now(),
          n_sim: nSimInput ? parseInt(nSimInput.value, 10) || 1000 : 1000,
          seed: seedInput ? seedInput.value : "",
          params: univData,
          multivariate: mvData
        }, { priority: "event" });
      }
    });
  }

  // =========================================================================
  // Lifecycle
  // =========================================================================

  function initAllPsa() {
    document.querySelectorAll(".psa-settings-container:not([data-initialized])").forEach(initSettings);

    var containers = document.querySelectorAll(
      ".psa-params-container:not([data-initialized]), .psa-multivariate-container:not([data-initialized])"
    );
    if (containers.length > 0) {
      ensureTabulator(function() { containers.forEach(initGrid); });
    }

    document.querySelectorAll(".psa-inputs-wrapper:not([data-run-wired])").forEach(function(w) {
      wireRunButton(w);
      w.setAttribute("data-run-wired", "true");
    });

    initAddMvModalBuilder();
  }

  if (typeof Shiny !== "undefined") {
    $(document).on("shiny:connected", function() { setTimeout(initAllPsa, 100); });
    $(document).on("shiny:value", function() { setTimeout(initAllPsa, 100); });
    $(document).on("shown.bs.modal", ".modal", function() { setTimeout(initAllPsa, 50); });
    $(document).on("hidden.bs.modal", ".modal", function() { cleanupAddMvModalBuilder(); });
    setTimeout(initAllPsa, 100);
  }
})();
