/* Scenario Parameter Table — Tabulator with multi-scenario state */
(function() {
  "use strict";
  console.log("[Scenario Params] JS version 1.0.0 loaded");

  // =========================================================================
  // Tabulator CDN loader (shared with DSA — idempotent)
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
      console.log("[Scenario Params] Tabulator loaded");
      var cbs = _tabulatorCallbacks.slice();
      _tabulatorCallbacks = [];
      for (var i = 0; i < cbs.length; i++) cbs[i]();
    };
    script.onerror = function() {
      console.error("[Scenario Params] Failed to load Tabulator from CDN");
    };
    document.head.appendChild(script);
  }

  // =========================================================================
  // Helpers (same as dsa-params.js)
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

  // Mutual exclusion map for settings
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
    if (data.type === "setting") {
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
        type: r.type || "variable", name: r.name || "",
        strategy: r.strategy || "", group: r.group || ""
      };
    }
  }

  function getComboId(cell) {
    return cell.getRow().getElement().getAttribute("data-combo-id");
  }

  function setRowCombo(table, comboId, type, name, strategy, group) {
    table._rowCombos[comboId] = {
      type: type || "variable", name: name || "",
      strategy: strategy || "", group: group || ""
    };
  }

  function addRowCombo(table, row, type, name, strategy, group) {
    var id = table._nextComboId++;
    row.getElement().setAttribute("data-combo-id", id);
    table._rowCombos[id] = {
      type: type || "variable", name: name || "",
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
        if (c.type === "variable") {
          usedCombos.add((c.name || "") + "|" + (c.strategy || "") + "|" + (c.group || ""));
        }
      }
    });
    return usedCombos;
  }

  // Build all possible (name, strategy, group) combos for variables
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

  function findFirstUnusedVariable(table, choices) {
    var usedKeys = new Set();
    var combos = table._rowCombos || {};
    Object.keys(combos).forEach(function(id) {
      var c = combos[id];
      if (c.type === "variable") {
        usedKeys.add((c.name || "") + "|" + (c.strategy || "") + "|" + (c.group || ""));
      }
    });
    return allVariableCombos(choices)
      .find(function(c) { return !usedKeys.has(c.name + "|" + c.strategy + "|" + c.group); }) || null;
  }

  function findFirstUnusedSetting(table, choices) {
    var usedNames = new Set(
      table.getData()
        .filter(function(r) { return r.type === "setting"; })
        .map(function(r) { return r.name; })
    );
    expandWithExclusions(usedNames);
    var settingKeys = Object.keys(choices.settings);
    var key = settingKeys.find(function(k) { return !usedNames.has(choices.settings[k]); });
    return key ? { name: choices.settings[key], display: key } : null;
  }

  function updateButtonStates(table, choices, addVarBtn, addSettingBtn) {
    if (addVarBtn) addVarBtn.disabled = !findFirstUnusedVariable(table, choices);
    if (addSettingBtn) addSettingBtn.disabled = !findFirstUnusedSetting(table, choices);
  }

  // =========================================================================
  // Formula editor (duplicated from dsa-params.js, with scenario- class names)
  // =========================================================================

  function formulaEditor(terms, suggestions) {
    return function(cell, onRendered, success, cancel) {
      var placeholder = document.createElement("div");
      placeholder.className = "scenario-formula-placeholder";
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
          input.className = "scenario-input-editor";
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
        overlay.className = "scenario-formula-overlay";
        overlay.style.position = "fixed";
        overlay.style.left = cellRect.left + "px";
        overlay.style.top = cellRect.top + "px";
        overlay.style.width = cellRect.width + "px";
        overlay.style.height = cellRect.height + "px";
        overlay.style.zIndex = "10000";

        var aceContainer = document.createElement("div");
        aceContainer.className = "scenario-ace-container";
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
          console.warn("[Scenario Params] Term highlighting/autocomplete init failed:", e.message);
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
  // Multi-scenario state management
  // =========================================================================

  var _state = {
    scenarios: [],
    selectedId: null,
    nextId: 1
  };

  function getSelectedScenario() {
    for (var i = 0; i < _state.scenarios.length; i++) {
      if (_state.scenarios[i].id === _state.selectedId) return _state.scenarios[i];
    }
    return null;
  }

  function getScenarioById(id) {
    for (var i = 0; i < _state.scenarios.length; i++) {
      if (_state.scenarios[i].id === id) return _state.scenarios[i];
    }
    return null;
  }

  function addScenario(name) {
    var s = {
      id: _state.nextId++,
      name: name || "Scenario " + _state.nextId,
      description: "",
      overrides: []
    };
    _state.scenarios.push(s);
    _state.selectedId = s.id;
    return s;
  }

  function removeScenario(id) {
    var idx = -1;
    for (var i = 0; i < _state.scenarios.length; i++) {
      if (_state.scenarios[i].id === id) { idx = i; break; }
    }
    if (idx === -1) return;
    _state.scenarios.splice(idx, 1);
    if (_state.selectedId === id) {
      if (_state.scenarios.length > 0) {
        var newIdx = Math.min(idx, _state.scenarios.length - 1);
        _state.selectedId = _state.scenarios[newIdx].id;
      } else {
        _state.selectedId = null;
      }
    }
  }

  function saveGridToState(table) {
    var scenario = getSelectedScenario();
    if (!scenario || !table) return;
    scenario.overrides = table.getData().map(function(d) {
      return {
        type: d.type || "variable",
        name: d.name || "",
        display_name: d.display_name || d.name || "",
        strategy: d.strategy || "",
        group: d.group || "",
        value: d.value || ""
      };
    });
  }

  function loadStateToGrid(table, id) {
    var scenario = getScenarioById(id);
    _state.selectedId = id;
    if (!scenario || !table) return;
    table.setData(scenario.overrides || []);
    relayout(table);
  }

  // =========================================================================
  // Grid visibility — hide grid when no scenarios exist
  // =========================================================================

  function updateGridVisibility(wrapperDiv) {
    var gridPanel = wrapperDiv ? wrapperDiv.querySelector(".scenario-grid-panel") : null;
    if (!gridPanel) return;
    if (_state.scenarios.length === 0 || _state.selectedId === null) {
      gridPanel.style.display = "none";
    } else {
      gridPanel.style.display = "";
    }
  }

  // =========================================================================
  // Shiny sync — sends all scenarios
  // =========================================================================

  function syncAllToShiny(table, inputId) {
    saveGridToState(table);
    var payload = _state.scenarios.map(function(s) {
      var variableOverrides = [];
      var settingOverrides = [];
      (s.overrides || []).forEach(function(o) {
        if (o.type === "setting") {
          settingOverrides.push({ name: o.name, value: o.value });
        } else {
          variableOverrides.push({
            name: o.name,
            value: o.value,
            strategy: o.strategy || "",
            group: o.group || ""
          });
        }
      });
      return {
        name: s.name,
        description: s.description || "",
        variable_overrides: variableOverrides,
        setting_overrides: settingOverrides
      };
    });
    if (typeof Shiny !== "undefined") {
      Shiny.setInputValue(inputId, payload, { priority: "event" });
    }
  }

  // =========================================================================
  // Scenario list rendering
  // =========================================================================

  function renderScenarioList(listContainer, table, inputId) {
    // Clear children safely
    while (listContainer.firstChild) {
      listContainer.removeChild(listContainer.firstChild);
    }

    _state.scenarios.forEach(function(s) {
      var item = document.createElement("div");
      item.className = "scenario-list-item" + (s.id === _state.selectedId ? " scenario-list-item--active" : "");
      item.setAttribute("data-id", s.id);

      var nameSpan = document.createElement("div");
      nameSpan.className = "scenario-list-item-name";
      nameSpan.textContent = s.name;
      item.appendChild(nameSpan);

      if (s.description) {
        var descSpan = document.createElement("div");
        descSpan.className = "scenario-list-item-desc";
        descSpan.textContent = s.description;
        item.appendChild(descSpan);
      }

      // Click to select
      item.addEventListener("click", function() {
        if (s.id === _state.selectedId) return;
        saveGridToState(table);
        loadStateToGrid(table, s.id);
        renderScenarioList(listContainer, table, inputId);
        syncAllToShiny(table, inputId);
      });

      // Double-click to edit via modal
      item.addEventListener("dblclick", function(e) {
        e.stopPropagation();
        saveGridToState(table);
        Shiny.setInputValue("show_edit_scenario_modal", {
          nonce: Date.now(),
          id: s.id,
          name: s.name,
          description: s.description || ""
        }, { priority: "event" });
      });

      listContainer.appendChild(item);
    });
  }

  // =========================================================================
  // Column definitions (adapted from DSA: Value instead of Low/High)
  // =========================================================================

  function buildColumnDefs(choices, inputId, terms, suggestions) {
    var settingsDisplay = buildSettingsDisplayMap(choices.settings);
    var strategyKeys = Object.keys(choices.strategies);
    var groupKeys = Object.keys(choices.groups);

    return [
      // Type column (read-only — type is set by which add button was clicked)
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

      // Name column
      {
        title: "Name",
        field: "name",
        widthGrow: 1,
        minWidth: 120,
        editor: "list",
        editorParams: function(cell) {
          var data = cell.getRow().getData();
          var currentName = cell.getValue();
          if (data.type === "variable") {
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
            var usedSettingNames = new Set(
              cell.getTable().getData()
                .filter(function(r) { return r.type === "setting" && r.name !== currentName; })
                .map(function(r) { return r.name; })
            );
            expandWithExclusions(usedSettingNames);
            var sKeys = Object.keys(choices.settings);
            return { values: sKeys
              .filter(function(k) { return !usedSettingNames.has(choices.settings[k]); })
              .map(function(k) { return { label: k, value: choices.settings[k] }; }) };
          }
        },
        formatter: function(cell) {
          var data = cell.getRow().getData();
          if (data.type === "setting") {
            return settingsDisplay[cell.getValue()] || cell.getValue() || "";
          }
          return cell.getValue() || "";
        }
      },

      // Strategy column
      {
        title: "Strategy",
        field: "strategy",
        widthGrow: 1,
        minWidth: 120,
        editor: "list",
        editorParams: function(cell) {
          var data = cell.getRow().getData();
          var targeting = getTargeting(choices, data.name);
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
          if (data.type === "setting") return false;
          var targeting = getTargeting(choices, data.name);
          return targeting.strategies !== null;
        },
        formatter: function(cell) {
          var data = cell.getRow().getData();
          if (data.type === "setting") return "\u2014";
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
        widthGrow: 1,
        minWidth: 120,
        editor: "list",
        editorParams: function(cell) {
          var data = cell.getRow().getData();
          var targeting = getTargeting(choices, data.name);
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
          if (data.type === "setting") return false;
          var targeting = getTargeting(choices, data.name);
          return targeting.groups !== null;
        },
        formatter: function(cell) {
          var data = cell.getRow().getData();
          if (data.type === "setting") return "\u2014";
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
        minWidth: 120,
        editor: false,
        headerSort: false,
        clipboard: false,
        formatter: function(cell) {
          var data = cell.getRow().getData();
          return getBaseCase(choices, data);
        }
      },

      // Value column (single, replaces Low/High)
      {
        title: "Value",
        field: "value",
        widthGrow: 2,
        minWidth: 450,
        editor: formulaEditor(terms, suggestions)
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
          btn.className = "scenario-delete-btn";
          btn.textContent = "\u00d7";
          btn.addEventListener("click", function(e) {
            e.stopPropagation();
            var rowData = cell.getRow().getData();
            var scenario = getSelectedScenario();
            var tbl = cell.getTable();
            var delId = getComboId(cell);
            cell.getRow().delete();
            removeRowCombo(tbl, delId);
            relayout(tbl);
            saveGridToState(tbl);
            if (scenario) {
              if (rowData.type === "variable") {
                Shiny.setInputValue("model_action", {
                  type: "remove_scenario_variable",
                  scenario: scenario.name,
                  variable: rowData.name || "",
                  strategy: rowData.strategy || "",
                  group: rowData.group || ""
                }, { priority: "event" });
              } else if (rowData.type === "setting") {
                Shiny.setInputValue("model_action", {
                  type: "remove_scenario_setting",
                  scenario: scenario.name,
                  setting: rowData.name || ""
                }, { priority: "event" });
              }
            }
            updateButtonStates(tbl, tbl._choices, tbl._addVarBtn, tbl._addSettingBtn);
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

  function initGrid(wrapperDiv) {
    var containerDiv = wrapperDiv.querySelector(".scenario-params-container");
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

    // Parse initial scenarios
    var initialScenarios = [];
    try {
      initialScenarios = JSON.parse(containerDiv.dataset.initialScenarios || "[]");
    } catch (e) {}

    // Build state from initial data
    var settingsDisplay = buildSettingsDisplayMap(choices.settings);
    _state.scenarios = [];
    _state.nextId = 1;

    for (var si = 0; si < initialScenarios.length; si++) {
      var sc = initialScenarios[si];
      var overrides = [];

      // Variable overrides
      var varOvs = sc.variable_overrides || [];
      for (var vi = 0; vi < varOvs.length; vi++) {
        var v = varOvs[vi];
        overrides.push({
          type: "variable",
          name: v.name || "",
          display_name: v.name || "",
          strategy: v.strategy || "",
          group: v.group || "",
          value: String(v.value || "")
        });
      }

      // Setting overrides
      var setOvs = sc.setting_overrides || [];
      for (var sti = 0; sti < setOvs.length; sti++) {
        var st = setOvs[sti];
        overrides.push({
          type: "setting",
          name: st.name || "",
          display_name: settingsDisplay[st.name] || st.name || "",
          strategy: "",
          group: "",
          value: String(st.value || "")
        });
      }

      _state.scenarios.push({
        id: _state.nextId++,
        name: sc.name || "Scenario " + _state.nextId,
        description: sc.description || "",
        overrides: overrides
      });
    }

    // Select first scenario if any exist
    if (_state.scenarios.length > 0) {
      _state.selectedId = _state.scenarios[0].id;
    } else {
      _state.selectedId = null;
    }

    var listContainer = wrapperDiv.querySelector(".scenario-list");
    var table = null;

    var columnDefs = buildColumnDefs(choices, inputId, terms, suggestions);

    var initialOverrides = getSelectedScenario() ? getSelectedScenario().overrides : [];

    table = new Tabulator(containerDiv, {
      data: initialOverrides,
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
            if (data.type === "setting") {
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
    initRowCombos(table, initialOverrides);

    // Cell edited — cascading + dispatch individual actions
    table.on("cellEdited", function(cell) {
      var field = cell.getField();
      var data = cell.getRow().getData();
      var oldValue = cell.getOldValue();
      var scenario = getSelectedScenario();
      var scenarioName = scenario ? scenario.name : "";

      if (field === "name") {
        // Use edit actions (not remove+add) so undo/redo captures the change atomically
        if (data.type === "setting") {
          var newBcVal = getBaseCase(choices, { type: "setting", name: data.name });
          cell.getRow().update({ display_name: settingsDisplay[data.name] || data.name, value: newBcVal });
          var d3 = cell.getRow().getData();
          Shiny.setInputValue("model_action", {
            type: "edit_scenario_setting", scenario: scenarioName,
            setting: oldValue || "", new_setting: d3.name, value: d3.value || ""
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
            type: "edit_scenario_variable", scenario: scenarioName,
            variable: oldValue || "", strategy: oldStrategy, group: oldGroup,
            new_variable: d4.name, new_strategy: d4.strategy || "", new_group: d4.group || "",
            value: d4.value || ""
          }, { priority: "event" });
        }
      } else if (field === "value") {
        if (data.type === "variable") {
          Shiny.setInputValue("model_action", {
            type: "edit_scenario_variable", scenario: scenarioName,
            variable: data.name, strategy: data.strategy || "", group: data.group || "",
            value: data.value || ""
          }, { priority: "event" });
        } else {
          Shiny.setInputValue("model_action", {
            type: "edit_scenario_setting", scenario: scenarioName,
            setting: data.name, value: data.value || ""
          }, { priority: "event" });
        }
      } else if (field === "strategy" || field === "group") {
        var payload = {
          type: "edit_scenario_variable", scenario: scenarioName,
          variable: data.name,
          strategy: field === "strategy" ? (oldValue || "") : (data.strategy || ""),
          group: field === "group" ? (oldValue || "") : (data.group || "")
        };
        if (field === "strategy") payload.new_strategy = data.strategy || "";
        if (field === "group") payload.new_group = data.group || "";
        Shiny.setInputValue("model_action", payload, { priority: "event" });
      }

      saveGridToState(table);
      relayout(table);
    });

    // Clipboard paste sync
    table.on("clipboardPasted", function(clipboard, rowData, rows) {
      var pasteSettingsDisplay = buildSettingsDisplayMap(choices.settings);
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
      syncAllToShiny(table, inputId);
    });

    // Render scenario list
    renderScenarioList(listContainer, table, inputId);

    // Wire add-scenario button
    // Wire add-scenario button — opens modal via Shiny
    var addScenarioBtn = wrapperDiv.querySelector(".scenario-add-btn");
    if (addScenarioBtn) {
      var newAddBtn = addScenarioBtn.cloneNode(true);
      addScenarioBtn.parentNode.replaceChild(newAddBtn, addScenarioBtn);
      newAddBtn.addEventListener("click", function() {
        saveGridToState(table);
        Shiny.setInputValue("show_add_scenario_modal", {
          nonce: Date.now()
        }, { priority: "event" });
      });
    }

    // Store references for message handlers
    // Initial visibility check
    updateGridVisibility(wrapperDiv);

    // Wire remove-scenario button
    var removeScenarioBtn = wrapperDiv.querySelector(".scenario-remove-btn");
    if (removeScenarioBtn) {
      var newRemBtn = removeScenarioBtn.cloneNode(true);
      removeScenarioBtn.parentNode.replaceChild(newRemBtn, removeScenarioBtn);
      newRemBtn.addEventListener("click", function() {
        var selected = getSelectedScenario();
        if (!selected) return;
        Shiny.setInputValue("remove_scenario_action", {
          nonce: Date.now(),
          name: selected.name
        }, { priority: "event" });
      });
    }

    // Wire add-variable button
    var addVarBtn = wrapperDiv.querySelector(".scenario-add-variable-btn");
    if (addVarBtn) {
      var newVarBtn = addVarBtn.cloneNode(true);
      addVarBtn.parentNode.replaceChild(newVarBtn, addVarBtn);
      addVarBtn = newVarBtn;
      addVarBtn.addEventListener("click", function() {
        var scenario = getSelectedScenario();
        if (!scenario) return;
        var unused = findFirstUnusedVariable(table, choices);
        if (!unused) return;
        var newRow = {
          type: "variable",
          name: unused.name,
          display_name: unused.name,
          strategy: unused.strategy,
          group: unused.group,
          value: "bc"
        };
        Shiny.setInputValue("model_action", {
          type: "add_scenario_variable",
          scenario: scenario.name,
          variable: newRow.name,
          value: "bc",
          strategy: newRow.strategy || "",
          group: newRow.group || ""
        }, { priority: "event" });
        table.addRow(newRow).then(function(row) {
          relayout(table);
          addRowCombo(table, row, "variable", newRow.name, newRow.strategy, newRow.group);
          saveGridToState(table);
          updateButtonStates(table, choices, addVarBtn, addSettingBtn);
        });
      });
    }

    // Wire add-setting button
    var addSettingBtn = wrapperDiv.querySelector(".scenario-add-setting-btn");
    if (addSettingBtn) {
      var newSettingBtn = addSettingBtn.cloneNode(true);
      addSettingBtn.parentNode.replaceChild(newSettingBtn, addSettingBtn);
      addSettingBtn = newSettingBtn;
      addSettingBtn.addEventListener("click", function() {
        var scenario = getSelectedScenario();
        if (!scenario) return;
        var unused = findFirstUnusedSetting(table, choices);
        if (!unused) return;
        var bcVal = getBaseCase(choices, { type: "setting", name: unused.name });
        var newRow = {
          type: "setting",
          name: unused.name,
          display_name: unused.display,
          strategy: "",
          group: "",
          value: bcVal
        };
        Shiny.setInputValue("model_action", {
          type: "add_scenario_setting",
          scenario: scenario.name,
          setting: newRow.name,
          value: bcVal
        }, { priority: "event" });
        table.addRow(newRow).then(function() {
          relayout(table);
          saveGridToState(table);
          updateButtonStates(table, choices, addVarBtn, addSettingBtn);
        });
      });
    }

    // Initial button state
    updateButtonStates(table, choices, addVarBtn, addSettingBtn);

    // Store button refs on table for delete handler access
    table._addVarBtn = addVarBtn;
    table._addSettingBtn = addSettingBtn;
    table._choices = choices;

    // Initial sync
    syncAllToShiny(table, inputId);

    // Run button — bundles all scenarios with nonce
    var runBtn = wrapperDiv.querySelector(".scenario-run-btn");
    if (runBtn) {
      var newRunBtn = runBtn.cloneNode(true);
      runBtn.parentNode.replaceChild(newRunBtn, runBtn);
      newRunBtn.addEventListener("click", function() {
        saveGridToState(table);
        var scenarios = _state.scenarios.map(function(s) {
          var variableOverrides = [];
          var settingOverrides = [];
          (s.overrides || []).forEach(function(o) {
            if (o.type === "setting") {
              settingOverrides.push({ name: o.name, value: o.value });
            } else {
              variableOverrides.push({
                name: o.name,
                value: o.value,
                strategy: o.strategy || "",
                group: o.group || ""
              });
            }
          });
          return {
            name: s.name,
            description: s.description || "",
            variable_overrides: variableOverrides,
            setting_overrides: settingOverrides
          };
        });
        Shiny.setInputValue("run_scenario_action", {
          nonce: Date.now(),
          scenarios: scenarios
        }, { priority: "event" });
      });
    }

    wrapperDiv.setAttribute("data-initialized", "true");
  }

  // =========================================================================
  // Lifecycle
  // =========================================================================

  function initAllGrids() {
    var wrappers = document.querySelectorAll(".scenario-params-wrapper:not([data-initialized])");
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
