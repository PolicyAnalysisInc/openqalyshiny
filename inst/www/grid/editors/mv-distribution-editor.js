/* OQGrid — Multivariate Distribution Overlay Editor (PSA) */
(function() {
  "use strict";

  var OQGrid = window.OQGrid = window.OQGrid || {};
  OQGrid.editors = OQGrid.editors || {};

  var MV_DISTRIBUTIONS = OQGrid.helpers.distributions.MV_DISTRIBUTIONS;
  var parseDistributionString = OQGrid.helpers.distributions.parseDistributionString;
  var buildDistributionString = OQGrid.helpers.distributions.buildDistributionString;
  var parseCVector = OQGrid.helpers.distributions.parseCVector;
  var buildCVector = OQGrid.helpers.distributions.buildCVector;

  // =========================================================================
  // Utility: clear all children from a DOM element
  // =========================================================================
  function clearChildren(el) {
    while (el.firstChild) el.removeChild(el.firstChild);
  }

  // =========================================================================
  // Utility: get multi-select value from a <select> element
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

  // =========================================================================
  // Builder state management
  // =========================================================================
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
    if (!select) return null;
    var type = select.value;
    if (!type) return null;
    var paramRadio = container.querySelector("input[name='" + state.radioName + "']:checked");
    var parameterization = paramRadio ? paramRadio.value : null;
    var config = MV_DISTRIBUTIONS[type];
    if (!config) return null;
    var activeConfig = (config.parameterizations && parameterization)
      ? config.parameterizations[parameterization]
      : config;

    var result = { type: type };

    // Scalar params (e.g., n for dirichlet, size for multinomial)
    (activeConfig.scalarParams || []).forEach(function(sp) {
      state.miniEditors.forEach(function(ed) {
        if (ed._paramName === sp && !ed._isVector) {
          var val = ed.getValue();
          if (val !== undefined && val !== null && String(val).trim() !== "") {
            result[sp] = parseFloat(val) || val;
          }
        }
      });
    });

    // Vector params (e.g., alpha for dirichlet)
    (activeConfig.vectorParams || []).forEach(function(vp) {
      var values = [];
      state.miniEditors.forEach(function(ed) {
        if (ed._paramName === vp && ed._isVector) {
          var val = ed.getValue();
          values[ed._vectorIdx] = (val !== undefined && val !== null && String(val).trim() !== "")
            ? (parseFloat(val) || 0) : 0;
        }
      });
      if (values.length > 0) result[vp] = values;
    });

    return result;
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

  // =========================================================================
  // Build the params section for a multivariate distribution
  // (scalar params, vector params, matrix params with Handsontable)
  // =========================================================================
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

    // Scalar params
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
      var ed = OQGrid.editors._createMiniAceEditor(aceContainer, val, state.terms, state.suggestions, {});
      ed._paramName = sp;
      ed._isVector = false;
      state.miniEditors.push(ed);
      if (ed.editor) ed.editor.session.on("change", function() { scheduleMvBuilderSync(state); });
      else if (ed.element) ed.element.addEventListener("input", function() { scheduleMvBuilderSync(state); });
      section.appendChild(row);
    });

    // Vector params
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
        var vectorEd = OQGrid.editors._createMiniAceEditor(vectorContainer, vectorVal, state.terms, state.suggestions, {});
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

    // Matrix params (Handsontable)
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

      OQGrid.ensureHandsontable(function() {
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

  // =========================================================================
  // Build the full MV distribution builder UI
  // =========================================================================
  function buildMvDistributionBuilder(state, type, parameterization, params) {
    var container = state.container;
    clearChildren(container);
    destroyMvBuilderState(state);

    // When a fixed type is set, skip the dropdown
    if (state.fixedType) {
      type = state.fixedType;

      // Show type label (read-only) instead of dropdown
      var typeLabel = document.createElement("div");
      typeLabel.className = "psa-dist-dropdown-row";
      var lbl = document.createElement("label");
      lbl.textContent = "Type: " + type;
      typeLabel.appendChild(lbl);
      // Add hidden select for collectMvBuilderValue
      var hiddenSelect2 = document.createElement("select");
      hiddenSelect2.className = "psa-mv-dist-select";
      hiddenSelect2.style.display = "none";
      var opt2 = document.createElement("option");
      opt2.value = type;
      opt2.selected = true;
      hiddenSelect2.appendChild(opt2);
      container.appendChild(hiddenSelect2);
      container.appendChild(typeLabel);

      if (!state.variables || state.variables.length === 0) {
        var prompt = document.createElement("div");
        prompt.className = "form-text";
        prompt.textContent = "Select variables to configure distribution parameters.";
        container.appendChild(prompt);
        state.emptyNote = prompt;
        scheduleMvBuilderSync(state);
        return;
      }

      buildMvParamsSectionShared(state, type, parameterization, params, state.variables);
      return;
    }

    // Fallback: show dropdown (used in modal)
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

  // =========================================================================
  // Create a new MV distribution builder instance
  // =========================================================================
  function createMvDistributionBuilder(container, opts) {
    opts = opts || {};
    var initialValue = opts.value || "";
    var parsed = parseDistributionString(initialValue);
    var state = {
      container: container,
      variables: opts.variables || [],
      fixedType: opts.fixedType || null,
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
      parsed ? parsed.type : (opts.fixedType || ""),
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
      parsed ? parsed.type : (state.fixedType || ""),
      parsed ? parsed.parameterization : null,
      parsed ? parsed.params : {}
    );
  }

  function setMvDistributionBuilderType(state, type) {
    if (!state) return;
    state.fixedType = type || null;
    var parsed = parseDistributionString(collectMvBuilderValue(state));
    buildMvDistributionBuilder(
      state,
      type || (parsed ? parsed.type : ""),
      parsed ? parsed.parameterization : null,
      parsed ? parsed.params : {}
    );
  }

  // =========================================================================
  // Modal builder init/cleanup (for "Add Multivariate" modal)
  // =========================================================================
  function initAddMvModalBuilder() {
    var container = document.getElementById("add_mv_distribution_builder");
    if (!container || container.hasAttribute("data-initialized")) return;

    var variablesInput = document.getElementById("add_mv_variables");
    var typeInput = document.getElementById("add_mv_type");
    var terms = null;
    var suggestions = null;
    try { terms = JSON.parse(container.dataset.terms || "null"); } catch (e) {}
    try { suggestions = JSON.parse(container.dataset.suggestions || "null"); } catch (e) {}

    var initialType = typeInput ? typeInput.value : null;

    function syncToShiny(value) {
      if (typeof Shiny !== "undefined") {
        Shiny.setInputValue("add_mv_params", value || null, { priority: "event" });
      }
    }

    var state = createMvDistributionBuilder(container, {
      fixedType: initialType,
      variables: getModalMultiValue(variablesInput),
      terms: terms,
      suggestions: suggestions,
      onChange: syncToShiny
    });

    function handleVariableChange() {
      setMvDistributionBuilderVariables(state, getModalMultiValue(variablesInput));
    }

    function handleTypeChange() {
      setMvDistributionBuilderType(state, typeInput ? typeInput.value : null);
    }

    if (variablesInput) {
      variablesInput.addEventListener("change", handleVariableChange);
    }
    if (typeInput) {
      typeInput.addEventListener("change", handleTypeChange);
    }

    container._mvModalCleanup = function() {
      if (variablesInput) variablesInput.removeEventListener("change", handleVariableChange);
      if (typeInput) typeInput.removeEventListener("change", handleTypeChange);
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

  // =========================================================================
  // Expose modal builder functions for lifecycle wiring
  // =========================================================================
  OQGrid.editors._initAddMvModalBuilder = initAddMvModalBuilder;
  OQGrid.editors._cleanupAddMvModalBuilder = cleanupAddMvModalBuilder;
  OQGrid.editors._destroyMvBuilderState = destroyMvBuilderState;
  OQGrid.editors._collectMvBuilderValue = collectMvBuilderValue;

  // =========================================================================
  // Typeahead tag input editor (for variables column in MV grid)
  // =========================================================================
  OQGrid.editors.tagInput = function(allVariables) {
    return function(cell, onRendered, success, cancel) {
      var placeholder = document.createElement("div");
      placeholder.className = "psa-formula-placeholder";

      var currentValue = cell.getValue() || [];
      if (typeof currentValue === "string") {
        try { currentValue = JSON.parse(currentValue); } catch (e) { currentValue = []; }
      }
      var selected = currentValue.slice();
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

      function renderTags(tagsContainer, input) {
        var existing = tagsContainer.querySelectorAll(".psa-tag");
        for (var i = existing.length - 1; i >= 0; i--) existing[i].parentNode.removeChild(existing[i]);
        selected.forEach(function(v) {
          var tag = document.createElement("span");
          tag.className = "psa-tag";
          tag.textContent = v;
          var removeBtn = document.createElement("span");
          removeBtn.className = "psa-tag-remove";
          removeBtn.textContent = "\u00D7";
          removeBtn.addEventListener("mousedown", function(e) {
            e.preventDefault();
            e.stopPropagation();
            selected = selected.filter(function(s) { return s !== v; });
            renderTags(tagsContainer, input);
          });
          tag.appendChild(removeBtn);
          tagsContainer.insertBefore(tag, input);
        });
      }

      function clearSuggestions(suggestionsEl) {
        while (suggestionsEl.firstChild) suggestionsEl.removeChild(suggestionsEl.firstChild);
      }

      function renderSuggestions(suggestionsEl, query) {
        clearSuggestions(suggestionsEl);
        var q = (query || "").toLowerCase();
        var filtered = allVariables.filter(function(v) {
          if (selected.indexOf(v) >= 0) return false;
          return !q || v.toLowerCase().indexOf(q) >= 0;
        });
        if (filtered.length === 0) {
          suggestionsEl.style.display = "none";
          return;
        }
        suggestionsEl.style.display = "";
        filtered.forEach(function(v) {
          var item = document.createElement("div");
          item.className = "psa-tag-suggestion";
          item.textContent = v;
          item.addEventListener("mousedown", function(e) {
            e.preventDefault();
            e.stopPropagation();
            selected.push(v);
            var input = overlay.querySelector(".psa-tag-input");
            if (input) input.value = "";
            renderTags(overlay.querySelector(".psa-tag-container"), input);
            renderSuggestions(suggestionsEl, "");
            if (input) input.focus();
          });
          suggestionsEl.appendChild(item);
        });
      }

      onRendered(function() {
        overlay = document.createElement("div");
        overlay.className = "psa-tag-overlay";
        overlay.style.position = "fixed";
        overlay.style.left = cellRect.left + "px";
        overlay.style.top = (cellRect.bottom + 2) + "px";
        overlay.style.minWidth = Math.max(300, cellRect.width) + "px";
        overlay.style.zIndex = "10001";
        document.body.appendChild(overlay);

        ["mousedown", "pointerdown", "click", "mouseup", "pointerup", "focusin"].forEach(function(evt) {
          overlay.addEventListener(evt, function(e) { e.stopPropagation(); });
        });

        var tagsContainer = document.createElement("div");
        tagsContainer.className = "psa-tag-container";

        var input = document.createElement("input");
        input.type = "text";
        input.className = "psa-tag-input";
        input.placeholder = "Type to search...";
        tagsContainer.appendChild(input);
        overlay.appendChild(tagsContainer);

        var suggestionsEl = document.createElement("div");
        suggestionsEl.className = "psa-tag-suggestions";
        overlay.appendChild(suggestionsEl);

        renderTags(tagsContainer, input);
        renderSuggestions(suggestionsEl, "");

        input.addEventListener("input", function() {
          renderSuggestions(suggestionsEl, input.value);
        });

        input.addEventListener("keydown", function(e) {
          if (e.key === "Escape") { e.preventDefault(); doCancel(); }
          else if (e.key === "Backspace" && !input.value && selected.length > 0) {
            selected.pop();
            renderTags(tagsContainer, input);
            renderSuggestions(suggestionsEl, "");
          }
        });

        onDocMouseDown = function(e) {
          if (!overlay || committed) return;
          if (!overlay.contains(e.target)) commit(selected.slice());
        };
        document.addEventListener("mousedown", onDocMouseDown, true);

        input.focus();
      });

      return placeholder;
    };
  };

  // =========================================================================
  // MV distribution editor factory
  // Returns a Tabulator cell editor for multivariate distributions.
  // Creates a fixed-position overlay with the full MV builder.
  // =========================================================================
  // Helper: extract structured params from row data for initializing the editor
  function rowDataToBuilderParams(rowData) {
    var params = {};
    if (rowData.n != null) params.n = String(rowData.n);
    if (rowData.covariance) params.covariance = rowData.covariance;
    return params;
  }

  OQGrid.editors.mvDistribution = function(choices, terms, suggestions) {
    return function(cell, onRendered, success, cancel) {
      var placeholder = document.createElement("div");
      placeholder.className = "psa-formula-placeholder";
      placeholder.addEventListener("focusout", function(e) { e.stopPropagation(); });

      var rowData = cell.getRow().getData();
      var variables = rowData.variables || [];
      var committed = false;
      var overlay = null;
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
        if (overlay && overlay.parentNode) {
          overlay.parentNode.removeChild(overlay);
          overlay = null;
        }
      }

      function collectValue() {
        var state = overlay ? overlay._mvBuilderState : null;
        return state ? collectMvBuilderValue(state) : null;
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

        // Initialize builder from row data's structured fields
        var initType = rowData.type || "";
        var initParams = rowDataToBuilderParams(rowData);

        createMvDistributionBuilder(overlay, {
          value: "",
          fixedType: null,
          variables: variables,
          terms: terms,
          suggestions: suggestions
        });

        // After builder is created, set the type and params
        var state = overlay._mvBuilderState;
        if (state && initType) {
          buildMvDistributionBuilder(state, initType, null, initParams);
        }

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
  };
})();
