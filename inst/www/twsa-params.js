/* TWSA Parameter Table — Tabulator with multi-analysis state */
(function() {
  "use strict";
  console.log("[TWSA Params] JS version 1.0.0 loaded");

  // =========================================================================
  // Tabulator CDN loader (shared — idempotent)
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
      console.log("[TWSA Params] Tabulator loaded");
      var cbs = _tabulatorCallbacks.slice();
      _tabulatorCallbacks = [];
      for (var i = 0; i < cbs.length; i++) cbs[i]();
    };
    script.onerror = function() {
      console.error("[TWSA Params] Failed to load Tabulator from CDN");
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

  function buildSettingsDisplayMap(settingsObj) {
    var map = {};
    var keys = Object.keys(settingsObj);
    for (var i = 0; i < keys.length; i++) {
      map[settingsObj[keys[i]]] = keys[i];
    }
    return map;
  }

  var SETTING_EXCLUSIONS = {
    "discount_rate": ["discount_cost", "discount_outcomes"],
    "discount_cost": ["discount_rate"],
    "discount_outcomes": ["discount_rate"]
  };

  function expandWithExclusions(usedNames) {
    usedNames.forEach(function(used) {
      var excl = SETTING_EXCLUSIONS[used];
      if (excl) excl.forEach(function(e) { usedNames.add(e); });
    });
  }

  function getBaseCase(choices, data) {
    if (data.param_type === "setting") {
      var val = choices.settingValues[data.name];
      return val !== undefined ? String(val) : "";
    }
    var key = (data.name || "") + "|" + (data.strategy || "") + "|" + (data.group || "");
    return choices.variableFormulas[key] || "";
  }

  function getTargeting(choices, name) {
    var t = choices.variableTargeting || {};
    var result = t[name] || { strategies: null, groups: null };
    if (typeof result.strategies === "string") result.strategies = [result.strategies];
    if (typeof result.groups === "string") result.groups = [result.groups];
    return result;
  }

  function applyTargetingDefaults(data, choices, usedCombos) {
    var targeting = getTargeting(choices, data.name);
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
        var key = (strats[si] || "") + "|" + (grps[gi] || "");
        if (!usedCombos.has(key)) {
          data.strategy = strats[si] || "";
          data.group = grps[gi] || "";
          found = true;
        }
      }
    }
    if (!found) {
      data.strategy = strats[0] || "";
      data.group = grps[0] || "";
    }
  }

  function initRowCombos(table, initialData) {
    table._rowCombos = {};
    table._nextComboId = 1;
    var rows = table.getRows();
    for (var i = 0; i < rows.length; i++) {
      var id = table._nextComboId++;
      rows[i].getElement().setAttribute("data-combo-id", id);
      var r = (initialData || [])[i] || {};
      table._rowCombos[id] = {
        param_type: r.param_type || "variable", name: r.name || "",
        strategy: r.strategy || "", group: r.group || ""
      };
    }
  }

  function getComboId(cell) {
    return cell.getRow().getElement().getAttribute("data-combo-id");
  }

  function setRowCombo(table, comboId, paramType, name, strategy, group) {
    table._rowCombos[comboId] = {
      param_type: paramType || "variable", name: name || "",
      strategy: strategy || "", group: group || ""
    };
  }

  function removeRowCombo(table, comboId) {
    delete table._rowCombos[comboId];
  }

  function getUsedCombosForVariable(table, variableName, excludeId) {
    var usedCombos = new Set();
    var combos = table._rowCombos || {};
    Object.keys(combos).forEach(function(id) {
      if (id !== excludeId && combos[id].name === variableName) {
        usedCombos.add((combos[id].strategy || "") + "|" + (combos[id].group || ""));
      }
    });
    return usedCombos;
  }

  function getUsedCombosForDropdown(table, excludeId) {
    var usedCombos = new Set();
    var combos = table._rowCombos || {};
    Object.keys(combos).forEach(function(id) {
      if (id !== excludeId) {
        var c = combos[id];
        if (c.param_type === "variable") {
          usedCombos.add((c.name || "") + "|" + (c.strategy || "") + "|" + (c.group || ""));
        }
      }
    });
    return usedCombos;
  }

  function allVariableCombos(choices) {
    return choices.variables.flatMap(function(v) {
      var t = getTargeting(choices, v);
      var strats = Array.isArray(t.strategies) ? t.strategies : (t.strategies ? [t.strategies] : [""]);
      var grps = Array.isArray(t.groups) ? t.groups : (t.groups ? [t.groups] : [""]);
      return strats.flatMap(function(s) {
        return grps.map(function(g) { return { name: v, strategy: s, group: g }; });
      });
    });
  }

  function findFirstUnusedVariable(usedKeys, choices) {
    return allVariableCombos(choices)
      .find(function(c) { return !usedKeys.has(c.name + "|" + c.strategy + "|" + c.group); }) || null;
  }

  function findFirstUnusedSetting(usedNames, choices) {
    var expanded = new Set(usedNames);
    expandWithExclusions(expanded);
    var settingKeys = Object.keys(choices.settings);
    var key = settingKeys.find(function(k) { return !expanded.has(choices.settings[k]); });
    return key ? { name: choices.settings[key], display: key } : null;
  }

  // =========================================================================
  // Formula editor (with twsa- CSS classes)
  // =========================================================================

  function formulaEditor(terms, suggestions) {
    return function(cell, onRendered, success, cancel) {
      var placeholder = document.createElement("div");
      placeholder.className = "twsa-formula-placeholder";
      placeholder.addEventListener("focusout", function(e) { e.stopPropagation(); });

      var currentValue = cell.getValue() || "";
      var committed = false;
      var overlay = null;
      var aceEditor = null;
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
        if (aceEditor) {
          try { aceEditor.destroy(); } catch (e) {}
          aceEditor = null;
        }
        if (overlay && overlay.parentNode) {
          overlay.parentNode.removeChild(overlay);
          overlay = null;
        }
      }

      onRendered(function() {
        if (typeof ace === "undefined") {
          var input = document.createElement("input");
          input.type = "text";
          input.className = "twsa-input-editor";
          input.value = currentValue;
          input.addEventListener("keydown", function(e) {
            if (e.key === "Enter") { commit(input.value); e.preventDefault(); }
            if (e.key === "Escape") { doCancel(); e.preventDefault(); }
          });
          input.addEventListener("blur", function() { commit(input.value); });
          placeholder.appendChild(input);
          input.focus();
          input.select();
          return;
        }

        var lineH = 18;
        var innerH = cellRect.height - 4;
        var vPad = Math.max(0, Math.round((innerH - lineH) / 2));

        overlay = document.createElement("div");
        overlay.className = "twsa-formula-overlay";
        overlay.style.position = "fixed";
        overlay.style.left = cellRect.left + "px";
        overlay.style.top = cellRect.top + "px";
        overlay.style.width = cellRect.width + "px";
        overlay.style.height = cellRect.height + "px";
        overlay.style.zIndex = "10000";

        var aceContainer = document.createElement("div");
        aceContainer.className = "twsa-ace-container";
        aceContainer.style.position = "absolute";
        aceContainer.style.left = "0";
        aceContainer.style.right = "0";
        aceContainer.style.top = vPad + "px";
        aceContainer.style.bottom = "0";
        overlay.appendChild(aceContainer);
        document.body.appendChild(overlay);

        onDocMouseDown = function(e) {
          if (!overlay || committed) {
            document.removeEventListener("mousedown", onDocMouseDown, true);
            return;
          }
          var isAce = overlay.contains(e.target);
          var isAutocomplete = false;
          try { isAutocomplete = e.target.closest && e.target.closest(".ace_autocomplete"); } catch (x) {}
          if (!isAce && !isAutocomplete) {
            var val = currentValue;
            try { if (aceEditor) val = aceEditor.getValue(); } catch (x) {}
            commit(val);
          }
        };
        document.addEventListener("mousedown", onDocMouseDown, true);

        ["mousedown", "pointerdown", "click", "mouseup", "pointerup", "focusin"].forEach(function(evt) {
          overlay.addEventListener(evt, function(e) { e.stopPropagation(); });
        });

        ace.require("ace/ext/language_tools");
        aceEditor = ace.edit(aceContainer);
        aceEditor.setTheme("ace/theme/chrome");
        aceEditor.session.setMode("ace/mode/r");
        aceEditor.setOptions({
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
          enableSnippets: false
        });
        aceEditor.setValue(currentValue, -1);

        try {
          if (terms && typeof FormulaInputMode !== "undefined") {
            FormulaInputMode.injectDefaultStyles();
            var hl = new FormulaInputMode.FormulaHighlighter(aceEditor);
            hl.setTerms(terms);
          }
          if (suggestions && typeof FormulaInputAutocomplete !== "undefined") {
            var cmp = new FormulaInputAutocomplete.FormulaCompleter(aceEditor, suggestions);
            aceEditor.completers = [cmp];
          }
        } catch (e) {
          console.warn("[TWSA Params] Term highlighting/autocomplete init failed:", e.message);
        }

        overlay.addEventListener("keydown", function(e) {
          if (committed || !aceEditor) return;
          if (e.key === "Enter") {
            e.preventDefault();
            e.stopPropagation();
            if (aceEditor.completer && aceEditor.completer.popup &&
                aceEditor.completer.popup.isOpen) {
              aceEditor.completer.detach();
            }
            commit(aceEditor.getValue());
          } else if (e.key === "Escape") {
            e.preventDefault();
            e.stopPropagation();
            if (aceEditor.completer && aceEditor.completer.popup &&
                aceEditor.completer.popup.isOpen) {
              aceEditor.completer.detach();
              return;
            }
            doCancel();
          }
        }, true);

        aceEditor.commands.addCommand({
          name: "acceptCompletion",
          bindKey: { win: "Tab", mac: "Tab" },
          exec: function(ed) {
            if (ed.completer && ed.completer.popup &&
                ed.completer.popup.isOpen) {
              ed.completer.insertMatch();
              return true;
            }
            return false;
          }
        });

        aceEditor.container.addEventListener("paste", function(e) {
          e.preventDefault();
          e.stopPropagation();
          var text = (e.clipboardData || window.clipboardData).getData("text");
          aceEditor.insert(text.replace(/[\r\n]+/g, " "));
        }, true);

        aceEditor.on("blur", function() {
          setTimeout(function() {
            if (!committed && aceEditor && !aceEditor.isFocused()) {
              commit(aceEditor.getValue());
            }
          }, 300);
        });

        aceEditor.resize();
        editCellEl = cellEl;
        var savedTabindex = cellEl.getAttribute("tabindex");
        cellEl.setAttribute("tabindex", "-1");
        cellEl.style.pointerEvents = "none";
        cellFocusRedirect = function() {
          if (committed || !aceEditor) return;
          aceEditor.focus();
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

        cellEl.blur();
        aceEditor.focus();
        var focusTimer = setInterval(function() {
          if (committed || !aceEditor) { clearInterval(focusTimer); return; }
          aceEditor.focus();
        }, 30);
        setTimeout(function() { clearInterval(focusTimer); }, 300);
      });

      return placeholder;
    };
  }

  // =========================================================================
  // Values popup editor
  // =========================================================================

  function createValuesPopup(cell, terms, suggestions, onSave, onCancel) {
    var data = cell.getRow().getData();
    var method = data.type || "radius";
    var currentData = data.data || {};

    var cellEl = cell.getElement();
    var cellRect = cellEl.getBoundingClientRect();

    // Overlay backdrop to catch outside clicks
    var backdrop = document.createElement("div");
    backdrop.className = "twsa-popup-backdrop";
    backdrop.style.position = "fixed";
    backdrop.style.top = "0";
    backdrop.style.left = "0";
    backdrop.style.width = "100%";
    backdrop.style.height = "100%";
    backdrop.style.zIndex = "10001";
    backdrop.style.background = "transparent";

    // Popup container
    var popup = document.createElement("div");
    popup.className = "twsa-values-popup";
    popup.style.position = "fixed";
    popup.style.zIndex = "10002";
    popup.style.background = "#fff";
    popup.style.border = "2px solid #0d6efd";
    popup.style.borderRadius = "6px";
    popup.style.boxShadow = "0 4px 16px rgba(0,0,0,0.2)";
    popup.style.padding = "12px";
    popup.style.minWidth = "320px";
    popup.style.maxWidth = "400px";

    // Position below the cell, or above if not enough space
    var popupTop = cellRect.bottom + 4;
    var spaceBelow = window.innerHeight - cellRect.bottom;
    if (spaceBelow < 250) {
      popupTop = cellRect.top - 260;
      if (popupTop < 10) popupTop = 10;
    }
    popup.style.left = Math.max(10, cellRect.left) + "px";
    popup.style.top = popupTop + "px";

    var aceEditors = [];
    var destroyed = false;

    function cleanupPopup() {
      if (destroyed) return;
      destroyed = true;
      aceEditors.forEach(function(ed) {
        try { ed.destroy(); } catch (e) {}
      });
      aceEditors = [];
      if (backdrop.parentNode) backdrop.parentNode.removeChild(backdrop);
      if (popup.parentNode) popup.parentNode.removeChild(popup);
    }

    // Build an Ace formula field inside the popup
    function makeFormulaField(label, initialValue) {
      var row = document.createElement("div");
      row.className = "twsa-popup-field";
      row.style.marginBottom = "8px";

      var lbl = document.createElement("label");
      lbl.className = "twsa-popup-label";
      lbl.textContent = label;
      lbl.style.display = "block";
      lbl.style.fontSize = "0.8rem";
      lbl.style.fontWeight = "500";
      lbl.style.marginBottom = "2px";
      row.appendChild(lbl);

      var editorDiv = document.createElement("div");
      editorDiv.className = "twsa-popup-ace";
      editorDiv.style.width = "100%";
      editorDiv.style.height = "28px";
      editorDiv.style.border = "1px solid #ced4da";
      editorDiv.style.borderRadius = "3px";
      editorDiv.style.overflow = "hidden";
      row.appendChild(editorDiv);

      var aceRef = { editor: null, getValue: function() { return initialValue || ""; } };

      setTimeout(function() {
        if (destroyed) return;
        if (typeof ace === "undefined") {
          var input = document.createElement("input");
          input.type = "text";
          input.value = initialValue || "";
          input.style.width = "100%";
          input.style.border = "none";
          input.style.outline = "none";
          input.style.fontSize = "0.85rem";
          input.style.padding = "2px 4px";
          while (editorDiv.firstChild) editorDiv.removeChild(editorDiv.firstChild);
          editorDiv.appendChild(input);
          aceRef.getValue = function() { return input.value; };
          return;
        }

        ace.require("ace/ext/language_tools");
        var ed = ace.edit(editorDiv);
        ed.setTheme("ace/theme/chrome");
        ed.session.setMode("ace/mode/r");
        ed.setOptions({
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
          maxLines: 1,
          minLines: 1
        });
        ed.setValue(initialValue || "", -1);

        try {
          if (terms && typeof FormulaInputMode !== "undefined") {
            FormulaInputMode.injectDefaultStyles();
            var hl = new FormulaInputMode.FormulaHighlighter(ed);
            hl.setTerms(terms);
          }
          if (suggestions && typeof FormulaInputAutocomplete !== "undefined") {
            var cmp = new FormulaInputAutocomplete.FormulaCompleter(ed, suggestions);
            ed.completers = [cmp];
          }
        } catch (e) {
          console.warn("[TWSA Params] Popup ace init warning:", e.message);
        }

        ed.commands.addCommand({
          name: "acceptCompletion",
          bindKey: { win: "Tab", mac: "Tab" },
          exec: function(editor) {
            if (editor.completer && editor.completer.popup && editor.completer.popup.isOpen) {
              editor.completer.insertMatch();
              return true;
            }
            return false;
          }
        });

        ed.container.addEventListener("paste", function(e) {
          e.preventDefault();
          e.stopPropagation();
          var text = (e.clipboardData || window.clipboardData).getData("text");
          ed.insert(text.replace(/[\r\n]+/g, " "));
        }, true);

        aceEditors.push(ed);
        aceRef.editor = ed;
        aceRef.getValue = function() { return ed.getValue(); };
      }, 0);

      return { element: row, ref: aceRef };
    }

    // Build a numeric input field
    function makeNumericField(label, initialValue, min, max) {
      var row = document.createElement("div");
      row.className = "twsa-popup-field";
      row.style.marginBottom = "8px";

      var lbl = document.createElement("label");
      lbl.className = "twsa-popup-label";
      lbl.textContent = label;
      lbl.style.display = "block";
      lbl.style.fontSize = "0.8rem";
      lbl.style.fontWeight = "500";
      lbl.style.marginBottom = "2px";
      row.appendChild(lbl);

      var input = document.createElement("input");
      input.type = "number";
      input.className = "twsa-popup-numeric";
      input.value = initialValue != null ? initialValue : "";
      input.min = min;
      input.max = max;
      input.style.width = "80px";
      input.style.fontSize = "0.85rem";
      input.style.padding = "2px 6px";
      input.style.border = "1px solid #ced4da";
      input.style.borderRadius = "3px";
      row.appendChild(input);

      return { element: row, input: input };
    }

    // Build a checkbox field
    function makeCheckboxField(label, initialValue) {
      var row = document.createElement("div");
      row.className = "twsa-popup-field";
      row.style.marginBottom = "8px";

      var checkLabel = document.createElement("label");
      checkLabel.style.fontSize = "0.8rem";
      checkLabel.style.cursor = "pointer";
      checkLabel.style.display = "flex";
      checkLabel.style.alignItems = "center";
      checkLabel.style.gap = "6px";

      var checkbox = document.createElement("input");
      checkbox.type = "checkbox";
      checkbox.checked = initialValue !== false;
      checkLabel.appendChild(checkbox);

      var span = document.createElement("span");
      span.textContent = label;
      checkLabel.appendChild(span);

      row.appendChild(checkLabel);
      return { element: row, checkbox: checkbox };
    }

    // Title
    var title = document.createElement("div");
    title.style.fontWeight = "600";
    title.style.fontSize = "0.85rem";
    title.style.marginBottom = "10px";
    title.textContent = method === "range" ? "Range Parameters" :
                        method === "radius" ? "Radius Parameters" : "Custom Values";
    popup.appendChild(title);

    // Build fields based on method
    var fieldRefs = {};

    if (method === "range") {
      var minField = makeFormulaField("Min", currentData.min || "");
      popup.appendChild(minField.element);
      fieldRefs.min = minField.ref;

      var maxField = makeFormulaField("Max", currentData.max || "");
      popup.appendChild(maxField.element);
      fieldRefs.max = maxField.ref;

      var stepsField = makeNumericField("Steps", currentData.steps || 5, 1, 10);
      popup.appendChild(stepsField.element);
      fieldRefs.stepsInput = stepsField.input;
    } else if (method === "radius") {
      var radiusField = makeFormulaField("Radius", currentData.radius || "");
      popup.appendChild(radiusField.element);
      fieldRefs.radius = radiusField.ref;

      var stepsField2 = makeNumericField("Steps", currentData.steps || 3, 1, 5);
      popup.appendChild(stepsField2.element);
      fieldRefs.stepsInput = stepsField2.input;
    } else {
      // custom
      var valuesField = makeFormulaField("Values", currentData.values || "");
      popup.appendChild(valuesField.element);
      fieldRefs.values = valuesField.ref;
    }

    // Include Base Case checkbox
    var cbField = makeCheckboxField("Include Base Case", currentData.include_bc);
    popup.appendChild(cbField.element);
    fieldRefs.includeBcCheckbox = cbField.checkbox;

    // Buttons
    var btnRow = document.createElement("div");
    btnRow.style.display = "flex";
    btnRow.style.gap = "6px";
    btnRow.style.justifyContent = "flex-end";
    btnRow.style.marginTop = "10px";

    var cancelBtn = document.createElement("button");
    cancelBtn.type = "button";
    cancelBtn.className = "btn btn-sm btn-outline-secondary";
    cancelBtn.textContent = "Cancel";
    cancelBtn.addEventListener("click", function(e) {
      e.stopPropagation();
      cleanupPopup();
      onCancel();
    });
    btnRow.appendChild(cancelBtn);

    var saveBtn = document.createElement("button");
    saveBtn.type = "button";
    saveBtn.className = "btn btn-sm btn-primary";
    saveBtn.textContent = "Save";
    saveBtn.addEventListener("click", function(e) {
      e.stopPropagation();
      var result = { include_bc: fieldRefs.includeBcCheckbox.checked };
      if (method === "range") {
        result.min = fieldRefs.min.getValue();
        result.max = fieldRefs.max.getValue();
        result.steps = parseInt(fieldRefs.stepsInput.value, 10) || 5;
      } else if (method === "radius") {
        result.radius = fieldRefs.radius.getValue();
        result.steps = parseInt(fieldRefs.stepsInput.value, 10) || 3;
      } else {
        result.values = fieldRefs.values.getValue();
      }
      cleanupPopup();
      onSave(result);
    });
    btnRow.appendChild(saveBtn);

    popup.appendChild(btnRow);

    // Block events from reaching Tabulator
    ["mousedown", "pointerdown", "click", "mouseup", "pointerup", "focusin"].forEach(function(evt) {
      popup.addEventListener(evt, function(e) { e.stopPropagation(); });
    });

    // Backdrop click cancels
    backdrop.addEventListener("mousedown", function(e) {
      e.stopPropagation();
      cleanupPopup();
      onCancel();
    });

    document.body.appendChild(backdrop);
    document.body.appendChild(popup);

    return { cleanup: cleanupPopup };
  }

  // =========================================================================
  // Format values data for display
  // =========================================================================

  function formatValuesDisplay(method, data) {
    if (!data || typeof data !== "object") return "";
    if (method === "range") {
      var min = data.min || "?";
      var max = data.max || "?";
      var steps = data.steps || "?";
      return min + " to " + max + " with " + steps + " steps";
    } else if (method === "radius") {
      var radius = data.radius || "?";
      var rsteps = data.steps || "?";
      return "\u00b1" + radius + " with " + rsteps + " steps";
    } else if (method === "custom") {
      return data.values || "";
    }
    return "";
  }

  // =========================================================================
  // Multi-analysis state management
  // =========================================================================

  var _state = {
    analyses: [],
    selectedId: null,
    nextId: 1
  };

  function getSelectedAnalysis() {
    for (var i = 0; i < _state.analyses.length; i++) {
      if (_state.analyses[i].id === _state.selectedId) return _state.analyses[i];
    }
    return null;
  }

  function getAnalysisById(id) {
    for (var i = 0; i < _state.analyses.length; i++) {
      if (_state.analyses[i].id === id) return _state.analyses[i];
    }
    return null;
  }

  function addAnalysis(name) {
    var a = {
      id: _state.nextId++,
      name: name || "TWSA " + _state.nextId,
      description: "",
      parameters: []
    };
    _state.analyses.push(a);
    _state.selectedId = a.id;
    return a;
  }

  function removeAnalysis(id) {
    var idx = -1;
    for (var i = 0; i < _state.analyses.length; i++) {
      if (_state.analyses[i].id === id) { idx = i; break; }
    }
    if (idx === -1) return;
    _state.analyses.splice(idx, 1);
    if (_state.selectedId === id) {
      if (_state.analyses.length > 0) {
        var newIdx = Math.min(idx, _state.analyses.length - 1);
        _state.selectedId = _state.analyses[newIdx].id;
      } else {
        _state.selectedId = null;
      }
    }
  }

  function saveGridToState(table) {
    var analysis = getSelectedAnalysis();
    if (!analysis || !table) return;
    analysis.parameters = table.getData().map(function(d) {
      return {
        axis: d.axis || "x",
        param_type: d.param_type || "variable",
        name: d.name || "",
        display_name: d.display_name || d.name || "",
        strategy: d.strategy || "",
        group: d.group || "",
        type: d.type || "radius",
        data: d.data || {}
      };
    });
  }

  function loadStateToGrid(table, id) {
    var analysis = getAnalysisById(id);
    _state.selectedId = id;
    if (!analysis || !table) return;
    table.setData(analysis.parameters || []);
    relayout(table);
  }

  // =========================================================================
  // Grid visibility
  // =========================================================================

  function updateGridVisibility(wrapperDiv) {
    var gridPanel = wrapperDiv ? wrapperDiv.querySelector(".twsa-grid-panel") : null;
    if (!gridPanel) return;
    if (_state.analyses.length === 0 || _state.selectedId === null) {
      gridPanel.style.display = "none";
    } else {
      gridPanel.style.display = "";
    }
  }

  // =========================================================================
  // Shiny sync
  // =========================================================================

  function syncAllToShiny(table, inputId) {
    saveGridToState(table);
    var payload = _state.analyses.map(function(a) {
      var variableParams = [];
      var settingParams = [];
      (a.parameters || []).forEach(function(p) {
        if (p.param_type === "setting") {
          settingParams.push({
            axis: p.axis,
            name: p.name,
            type: p.type || "radius",
            data: p.data || {}
          });
        } else {
          variableParams.push({
            axis: p.axis,
            name: p.name,
            strategy: p.strategy || "",
            group: p.group || "",
            type: p.type || "radius",
            data: p.data || {}
          });
        }
      });
      return {
        name: a.name,
        description: a.description || "",
        variable_params: variableParams,
        setting_params: settingParams
      };
    });
    if (typeof Shiny !== "undefined") {
      Shiny.setInputValue(inputId, payload, { priority: "event" });
    }
  }

  // =========================================================================
  // Analysis list rendering
  // =========================================================================

  function renderAnalysisList(listContainer, table, inputId) {
    while (listContainer.firstChild) {
      listContainer.removeChild(listContainer.firstChild);
    }

    _state.analyses.forEach(function(a) {
      var item = document.createElement("div");
      item.className = "twsa-list-item" + (a.id === _state.selectedId ? " twsa-list-item--active" : "");
      item.setAttribute("data-id", a.id);

      var nameSpan = document.createElement("div");
      nameSpan.className = "twsa-list-item-name";
      nameSpan.textContent = a.name;
      item.appendChild(nameSpan);

      if (a.description) {
        var descSpan = document.createElement("div");
        descSpan.className = "twsa-list-item-desc";
        descSpan.textContent = a.description;
        item.appendChild(descSpan);
      }

      item.addEventListener("click", function() {
        if (a.id === _state.selectedId) return;
        saveGridToState(table);
        loadStateToGrid(table, a.id);
        renderAnalysisList(listContainer, table, inputId);
        syncAllToShiny(table, inputId);
      });

      item.addEventListener("dblclick", function(e) {
        e.stopPropagation();
        saveGridToState(table);
        Shiny.setInputValue("show_edit_twsa_modal", {
          nonce: Date.now(),
          id: a.id,
          name: a.name,
          description: a.description || ""
        }, { priority: "event" });
      });

      listContainer.appendChild(item);
    });
  }

  // =========================================================================
  // Column definitions
  // =========================================================================

  function buildColumnDefs(choices, inputId, terms, suggestions) {
    var settingsDisplay = buildSettingsDisplayMap(choices.settings);
    var strategyKeys = Object.keys(choices.strategies);
    var groupKeys = Object.keys(choices.groups);

    // Active popup reference for cleanup
    var activePopup = null;

    return [
      // Axis column (read-only)
      {
        title: "Axis",
        field: "axis",
        width: 60,
        hozAlign: "center",
        headerSort: false,
        formatter: function(cell) {
          var val = cell.getValue();
          return val === "x" ? "X" : val === "y" ? "Y" : (val || "").toUpperCase();
        }
      },

      // Type column (read-only)
      {
        title: "Type",
        field: "param_type",
        width: 80,
        headerSort: false,
        formatter: function(cell) {
          var val = cell.getValue();
          if (val === "variable") return "Variable";
          if (val === "setting") return "Setting";
          return val || "";
        }
      },

      // Name column
      {
        title: "Name",
        field: "name",
        widthGrow: 2,
        minWidth: 120,
        editor: "list",
        editorParams: function(cell) {
          var data = cell.getRow().getData();
          if (data.param_type === "variable") {
            var currentData = data;
            var cid = getComboId(cell);
            var usedCombos = getUsedCombosForDropdown(cell.getTable(), cid);
            return { values: choices.variables
              .filter(function(v) {
                if (v === currentData.name) return true;
                var combos = allVariableCombos(choices).filter(function(c) { return c.name === v; });
                return combos.some(function(c) {
                  return !usedCombos.has(c.name + "|" + c.strategy + "|" + c.group);
                });
              })
              .map(function(v) { return { label: v, value: v }; }) };
          } else {
            var currentName = cell.getValue();
            var usedSettingNames = new Set();
            cell.getTable().getData().forEach(function(r) {
              if (r.param_type === "setting" && r.name !== currentName) {
                usedSettingNames.add(r.name);
              }
            });
            expandWithExclusions(usedSettingNames);
            var sKeys = Object.keys(choices.settings);
            return { values: sKeys
              .filter(function(k) { return !usedSettingNames.has(choices.settings[k]); })
              .map(function(k) { return { label: k, value: choices.settings[k] }; }) };
          }
        },
        formatter: function(cell) {
          var data = cell.getRow().getData();
          if (data.param_type === "setting") {
            return settingsDisplay[cell.getValue()] || cell.getValue() || "";
          }
          return cell.getValue() || "";
        }
      },

      // Strategy column
      {
        title: "Strategy",
        field: "strategy",
        width: 100,
        editor: "list",
        editorParams: function(cell) {
          var data = cell.getRow().getData();
          var targeting = getTargeting(choices, data.name);
          if (!targeting.strategies) return { values: [] };
          var currentStrategy = cell.getValue();
          var currentGroup = data.group || "";
          var usedStrategies = new Set(
            cell.getTable().getData()
              .filter(function(r) {
                return r.param_type === "variable" &&
                  r.name === data.name &&
                  (r.group || "") === currentGroup &&
                  (r.strategy || "") !== currentStrategy;
              })
              .map(function(r) { return r.strategy || ""; })
          );
          return {
            values: targeting.strategies
              .filter(function(sVal) { return !usedStrategies.has(sVal); })
              .map(function(sVal) {
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
          if (data.param_type === "setting") return false;
          var targeting = getTargeting(choices, data.name);
          return targeting.strategies !== null;
        },
        formatter: function(cell) {
          var data = cell.getRow().getData();
          if (data.param_type === "setting") return "\u2014";
          var targeting = getTargeting(choices, data.name);
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

      // Group column
      {
        title: "Group",
        field: "group",
        width: 100,
        editor: "list",
        editorParams: function(cell) {
          var data = cell.getRow().getData();
          var targeting = getTargeting(choices, data.name);
          if (!targeting.groups) return { values: [] };
          var currentGroup = cell.getValue();
          var currentStrategy = data.strategy || "";
          var usedGroups = new Set(
            cell.getTable().getData()
              .filter(function(r) {
                return r.param_type === "variable" &&
                  r.name === data.name &&
                  (r.strategy || "") === currentStrategy &&
                  (r.group || "") !== currentGroup;
              })
              .map(function(r) { return r.group || ""; })
          );
          return {
            values: targeting.groups
              .filter(function(gVal) { return !usedGroups.has(gVal); })
              .map(function(gVal) {
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
          if (data.param_type === "setting") return false;
          var targeting = getTargeting(choices, data.name);
          return targeting.groups !== null;
        },
        formatter: function(cell) {
          var data = cell.getRow().getData();
          if (data.param_type === "setting") return "\u2014";
          var targeting = getTargeting(choices, data.name);
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

      // Method column
      {
        title: "Method",
        field: "type",
        width: 100,
        editor: "list",
        editorParams: {
          values: [
            { label: "Range", value: "range" },
            { label: "Radius", value: "radius" },
            { label: "Custom", value: "custom" }
          ]
        },
        formatter: function(cell) {
          var val = cell.getValue();
          if (val === "range") return "Range";
          if (val === "radius") return "Radius";
          if (val === "custom") return "Custom";
          return val || "";
        }
      },

      // Values column (popup editor)
      {
        title: "Values",
        field: "data",
        minWidth: 265,
        widthGrow: 2,
        editor: false,
        headerSort: false,
        formatter: function(cell) {
          var data = cell.getRow().getData();
          var text = formatValuesDisplay(data.type, data.data);
          var container = document.createElement("div");
          container.style.display = "flex";
          container.style.alignItems = "center";
          container.style.gap = "4px";
          container.style.cursor = "pointer";
          container.style.width = "100%";

          var span = document.createElement("span");
          span.style.flex = "1";
          span.style.overflow = "hidden";
          span.style.textOverflow = "ellipsis";
          span.style.whiteSpace = "nowrap";
          span.textContent = text || "(click to edit)";
          if (!text) span.style.color = "#999";
          container.appendChild(span);

          var editIcon = document.createElement("span");
          editIcon.textContent = "\u270e";
          editIcon.style.fontSize = "0.75rem";
          editIcon.style.color = "#6c757d";
          container.appendChild(editIcon);

          return container;
        },
        cellClick: function(e, cell) {
          e.stopPropagation();
          if (activePopup) {
            activePopup.cleanup();
            activePopup = null;
          }
          var rowData = cell.getRow().getData();
          var analysis = getSelectedAnalysis();
          var twsaName = analysis ? analysis.name : "";
          var tbl = cell.getTable();

          activePopup = createValuesPopup(cell, terms, suggestions,
            function onSave(result) {
              activePopup = null;
              cell.getRow().update({ data: result });
              relayout(tbl);
              saveGridToState(tbl);

              // Dispatch action with flat fields
              var d = cell.getRow().getData();
              if (d.param_type === "variable") {
                Shiny.setInputValue("model_action", {
                  type: "edit_twsa_variable",
                  twsa_name: twsaName,
                  variable: d.name,
                  strategy: d.strategy || "",
                  group: d.group || "",
                  method_type: d.type || "radius",
                  min: result.min || null,
                  max: result.max || null,
                  radius: result.radius || null,
                  steps: result.steps || null,
                  values: result.values || null,
                  include_base_case: result.include_bc
                }, { priority: "event" });
              } else {
                Shiny.setInputValue("model_action", {
                  type: "edit_twsa_setting",
                  twsa_name: twsaName,
                  setting: d.name,
                  method_type: d.type || "radius",
                  min: result.min || null,
                  max: result.max || null,
                  radius: result.radius || null,
                  steps: result.steps || null,
                  values: result.values || null,
                  include_base_case: result.include_bc
                }, { priority: "event" });
              }
              syncAllToShiny(tbl, inputId);
            },
            function onCancel() {
              activePopup = null;
            }
          );
        }
      }
    ];
  }

  // =========================================================================
  // Grid initialization
  // =========================================================================

  var _activeTables = {};

  function initGrid(wrapperDiv) {
    var containerDiv = wrapperDiv.querySelector(".twsa-params-container");
    if (!containerDiv) return;
    var inputId = containerDiv.dataset.inputId;
    if (!inputId) return;

    // Destroy previous table
    if (_activeTables[inputId]) {
      try { _activeTables[inputId].destroy(); } catch (e) {}
      delete _activeTables[inputId];
    }

    // Parse data attributes
    var variableTargeting = {};
    try {
      variableTargeting = JSON.parse(containerDiv.dataset.variableTargeting || "{}");
    } catch (e) {}

    var choices = {
      variables: JSON.parse(containerDiv.dataset.variables || "[]"),
      settings: JSON.parse(containerDiv.dataset.settings || "{}"),
      strategies: JSON.parse(containerDiv.dataset.strategies || "{}"),
      groups: JSON.parse(containerDiv.dataset.groups || "{}"),
      variableTargeting: variableTargeting,
      variableFormulas: JSON.parse(containerDiv.dataset.variableFormulas || "{}"),
      settingValues: JSON.parse(containerDiv.dataset.settingValues || "{}")
    };

    var terms = null;
    var suggestions = null;
    try { terms = JSON.parse(containerDiv.dataset.terms || "null"); } catch (e) {}
    try { suggestions = JSON.parse(containerDiv.dataset.suggestions || "null"); } catch (e) {}

    // Parse initial TWSA data
    var initialTwsa = [];
    try {
      initialTwsa = JSON.parse(containerDiv.dataset.initialTwsa || "[]");
    } catch (e) {}

    // Build state from initial data
    var settingsDisplay = buildSettingsDisplayMap(choices.settings);
    _state.analyses = [];
    _state.nextId = 1;

    for (var ai = 0; ai < initialTwsa.length; ai++) {
      var tw = initialTwsa[ai];
      var parameters = [];

      // Variable params
      var varParams = tw.variable_params || [];
      for (var vi = 0; vi < varParams.length; vi++) {
        var v = varParams[vi];
        parameters.push({
          axis: v.axis || "x",
          param_type: "variable",
          name: v.name || "",
          display_name: v.name || "",
          strategy: v.strategy || "",
          group: v.group || "",
          type: v.type || "radius",
          data: v.data || {}
        });
      }

      // Setting params
      var setParams = tw.setting_params || [];
      for (var si = 0; si < setParams.length; si++) {
        var s = setParams[si];
        parameters.push({
          axis: s.axis || "x",
          param_type: "setting",
          name: s.name || "",
          display_name: settingsDisplay[s.name] || s.name || "",
          strategy: "",
          group: "",
          type: s.type || "radius",
          data: s.data || {}
        });
      }

      _state.analyses.push({
        id: _state.nextId++,
        name: tw.name || "TWSA " + _state.nextId,
        description: tw.description || "",
        parameters: parameters
      });
    }

    // Select first analysis if any
    if (_state.analyses.length > 0) {
      _state.selectedId = _state.analyses[0].id;
    } else {
      _state.selectedId = null;
    }

    var listContainer = wrapperDiv.querySelector(".twsa-list");
    var table = null;

    var columnDefs = buildColumnDefs(choices, inputId, terms, suggestions);

    var initialParams = getSelectedAnalysis() ? getSelectedAnalysis().parameters : [];

    table = new Tabulator(containerDiv, {
      data: initialParams,
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
          if (field === "strategy" || field === "group") {
            var el = cells[ci].getElement();
            if (data.param_type === "setting") {
              el.style.color = "var(--bs-secondary, #6c757d)";
              el.style.fontStyle = "italic";
            } else {
              var targeting = getTargeting(choices, data.name);
              var isDisabled = (field === "strategy" && !targeting.strategies) ||
                               (field === "group" && !targeting.groups);
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
    });

    _activeTables[inputId] = table;
    initRowCombos(table, initialParams);

    // Cell edited — cascading + dispatch actions
    table.on("cellEdited", function(cell) {
      var field = cell.getField();
      var data = cell.getRow().getData();
      var oldValue = cell.getOldValue();
      var analysis = getSelectedAnalysis();
      var twsaName = analysis ? analysis.name : "";

      if (field === "name") {
        if (data.param_type === "setting") {
          cell.getRow().update({ display_name: settingsDisplay[data.name] || data.name, data: {} });
          var d3 = cell.getRow().getData();
          Shiny.setInputValue("model_action", {
            type: "edit_twsa_setting",
            twsa_name: twsaName,
            setting: oldValue || "",
            new_setting: d3.name
          }, { priority: "event" });
        } else {
          var oldStrategy = data.strategy || "";
          var oldGroup = data.group || "";
          var cid = getComboId(cell);
          var usedCombos = getUsedCombosForVariable(cell.getTable(), data.name, cid);
          cell.getRow().update({ display_name: data.name });
          applyTargetingDefaults(data, choices, usedCombos);
          cell.getRow().update({ strategy: data.strategy, group: data.group });
          setRowCombo(cell.getTable(), cid, "variable", data.name, data.strategy, data.group);
          var d4 = cell.getRow().getData();
          Shiny.setInputValue("model_action", {
            type: "edit_twsa_variable",
            twsa_name: twsaName,
            variable: oldValue || "",
            new_variable: d4.name,
            strategy: oldStrategy,
            group: oldGroup,
            new_strategy: d4.strategy || "",
            new_group: d4.group || ""
          }, { priority: "event" });
        }
      } else if (field === "type") {
        // Method changed — clear data
        cell.getRow().update({ data: {} });
        var d5 = cell.getRow().getData();
        if (d5.param_type === "variable") {
          Shiny.setInputValue("model_action", {
            type: "edit_twsa_variable",
            twsa_name: twsaName,
            variable: d5.name,
            strategy: d5.strategy || "",
            group: d5.group || "",
            method_type: d5.type || "radius"
          }, { priority: "event" });
        } else {
          Shiny.setInputValue("model_action", {
            type: "edit_twsa_setting",
            twsa_name: twsaName,
            setting: d5.name,
            method_type: d5.type || "radius"
          }, { priority: "event" });
        }
      } else if (field === "strategy" || field === "group") {
        var payload = {
          type: "edit_twsa_variable",
          twsa_name: twsaName,
          variable: data.name,
          strategy: field === "strategy" ? (oldValue || "") : (data.strategy || ""),
          group: field === "group" ? (oldValue || "") : (data.group || ""),
          axis: data.axis
        };
        if (field === "strategy") payload.new_strategy = data.strategy || "";
        if (field === "group") payload.new_group = data.group || "";
        Shiny.setInputValue("model_action", payload, { priority: "event" });
      }

      saveGridToState(table);
      relayout(table);
      syncAllToShiny(table, inputId);
    });

    // Clipboard paste sync
    table.on("clipboardPasted", function(clipboard, rowData, rows) {
      var pasteSettingsDisplay = buildSettingsDisplayMap(choices.settings);
      rows.forEach(function(row) {
        var d = row.getData();
        if (!d.display_name) {
          if (d.param_type === "setting") {
            row.update({ display_name: pasteSettingsDisplay[d.name] || d.name });
          } else {
            row.update({ display_name: d.name || "" });
          }
        }
      });
      syncAllToShiny(table, inputId);
    });

    // Render analysis list
    renderAnalysisList(listContainer, table, inputId);

    // Wire add-twsa button — opens modal via Shiny
    var addTwsaBtn = wrapperDiv.querySelector(".twsa-add-btn");
    if (addTwsaBtn) {
      var newAddBtn = addTwsaBtn.cloneNode(true);
      addTwsaBtn.parentNode.replaceChild(newAddBtn, addTwsaBtn);
      newAddBtn.addEventListener("click", function() {
        saveGridToState(table);
        Shiny.setInputValue("show_add_twsa_modal", {
          nonce: Date.now()
        }, { priority: "event" });
      });
    }

    // Initial visibility check
    updateGridVisibility(wrapperDiv);

    // Wire remove-twsa button
    var removeTwsaBtn = wrapperDiv.querySelector(".twsa-remove-btn");
    if (removeTwsaBtn) {
      var newRemBtn = removeTwsaBtn.cloneNode(true);
      removeTwsaBtn.parentNode.replaceChild(newRemBtn, removeTwsaBtn);
      newRemBtn.addEventListener("click", function() {
        var selected = getSelectedAnalysis();
        if (!selected) return;
        Shiny.setInputValue("remove_twsa_action", {
          nonce: Date.now(),
          name: selected.name
        }, { priority: "event" });
      });
    }

    // Store references
    table._choices = choices;

    // Initial sync
    syncAllToShiny(table, inputId);

    // Run button
    var runBtn = wrapperDiv.querySelector(".twsa-run-btn");
    if (runBtn) {
      var newRunBtn = runBtn.cloneNode(true);
      runBtn.parentNode.replaceChild(newRunBtn, runBtn);
      newRunBtn.addEventListener("click", function() {
        saveGridToState(table);
        Shiny.setInputValue("run_twsa_action", {
          nonce: Date.now()
        }, { priority: "event" });
      });
    }

    wrapperDiv.setAttribute("data-initialized", "true");
  }

  // =========================================================================
  // Lifecycle
  // =========================================================================

  function initAllGrids() {
    var wrappers = document.querySelectorAll(".twsa-params-wrapper:not([data-initialized])");
    if (wrappers.length === 0) return;
    ensureTabulator(function() {
      wrappers.forEach(initGrid);
    });
  }

  if (typeof Shiny !== "undefined") {
    $(document).on("shiny:connected", function() {
      setTimeout(initAllGrids, 100);
    });

    $(document).on("shiny:value", function() {
      setTimeout(initAllGrids, 100);
    });

    setTimeout(initAllGrids, 100);
  }
})();
