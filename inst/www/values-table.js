/* Values Table — Tabulator (individual edit/remove actions) */
(function() {
  "use strict";
  console.log("[Values Table] JS version 1.0.0 loaded");

  // Prevent Tabulator's internal focus() calls from triggering browser auto-scroll
  (function() {
    var origFocus = HTMLElement.prototype.focus;
    HTMLElement.prototype.focus = function(opts) {
      if (this.closest && this.closest('.tabulator')) {
        return origFocus.call(this, Object.assign({}, opts, { preventScroll: true }));
      }
      return origFocus.call(this, opts);
    };
  })();

  // =========================================================================
  // Tabulator CDN loader (shared — guards double-load)
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
      console.log("[Values Table] Tabulator loaded");
      var cbs = _tabulatorCallbacks.slice();
      _tabulatorCallbacks = [];
      for (var i = 0; i < cbs.length; i++) cbs[i]();
    };
    script.onerror = function() {
      console.error("[Values Table] Failed to load Tabulator from CDN");
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

  function emdashIfEmpty(cell) {
    var val = cell.getValue();
    if (val === null || val === undefined || val === "") return "\u2014";
    return val;
  }

  // =========================================================================
  // Typeahead text editor (free-text input with suggestions dropdown)
  // =========================================================================

  function typeaheadEditor(getValueNames) {
    return function(cell, onRendered, success, cancel) {
      var valueNames = getValueNames();
      var currentValue = cell.getValue() || "";
      var committed = false;
      var onDocMouseDown = null;

      var wrapper = document.createElement("div");
      wrapper.className = "val-typeahead-wrapper";
      wrapper.style.position = "relative";
      wrapper.style.width = "100%";
      wrapper.style.height = "100%";

      var input = document.createElement("input");
      input.type = "text";
      input.className = "val-input-editor";
      input.value = currentValue;
      wrapper.appendChild(input);

      // Dropdown appended to document.body with position:fixed
      var dropdown = document.createElement("div");
      dropdown.className = "val-typeahead-dropdown";
      document.body.appendChild(dropdown);

      var selectedIdx = -1;

      function positionDropdown() {
        var rect = input.getBoundingClientRect();
        dropdown.style.left = rect.left + "px";
        dropdown.style.top = rect.bottom + "px";
        dropdown.style.width = rect.width + "px";
      }

      function cleanup() {
        if (onDocMouseDown) {
          document.removeEventListener("mousedown", onDocMouseDown, true);
          onDocMouseDown = null;
        }
        if (dropdown && dropdown.parentNode) {
          dropdown.parentNode.removeChild(dropdown);
        }
      }

      function commit(val) {
        if (committed) return;
        committed = true;
        hideDropdown();
        cleanup();
        try { success(val); } catch (e) {}
      }
      function doCancel() {
        if (committed) return;
        committed = true;
        hideDropdown();
        cleanup();
        try { cancel(); } catch (e) {}
      }

      function clearDropdown() {
        while (dropdown.firstChild) {
          dropdown.removeChild(dropdown.firstChild);
        }
      }

      function hideDropdown() {
        dropdown.style.display = "none";
        clearDropdown();
        selectedIdx = -1;
      }

      function showSuggestions(filter) {
        clearDropdown();
        selectedIdx = -1;
        var query = (filter || "").toLowerCase();
        var seen = {};
        var matches = [];
        for (var i = 0; i < valueNames.length; i++) {
          var n = valueNames[i];
          if (!seen[n] && n.toLowerCase().indexOf(query) !== -1) {
            seen[n] = true;
            matches.push(n);
          }
        }
        if (matches.length === 0 || (matches.length === 1 && matches[0] === filter)) {
          hideDropdown();
          return;
        }
        matches.forEach(function(name) {
          var item = document.createElement("div");
          item.className = "val-typeahead-item";
          item.textContent = name;
          item.addEventListener("mousedown", function(e) {
            e.preventDefault();
            e.stopPropagation();
            commit(name);
          });
          dropdown.appendChild(item);
        });
        positionDropdown();
        dropdown.style.display = "block";
      }

      function highlightItem(idx) {
        var items = dropdown.querySelectorAll(".val-typeahead-item");
        items.forEach(function(el, i) {
          el.classList.toggle("val-typeahead-item-active", i === idx);
        });
        if (items[idx]) {
          items[idx].scrollIntoView({ block: "nearest" });
        }
      }

      input.addEventListener("input", function() {
        showSuggestions(input.value);
      });

      input.addEventListener("keydown", function(e) {
        var items = dropdown.querySelectorAll(".val-typeahead-item");
        if (e.key === "ArrowDown") {
          e.preventDefault();
          if (items.length > 0) {
            selectedIdx = Math.min(selectedIdx + 1, items.length - 1);
            highlightItem(selectedIdx);
          }
        } else if (e.key === "ArrowUp") {
          e.preventDefault();
          if (items.length > 0) {
            selectedIdx = Math.max(selectedIdx - 1, 0);
            highlightItem(selectedIdx);
          }
        } else if (e.key === "Enter") {
          e.preventDefault();
          if (selectedIdx >= 0 && items[selectedIdx]) {
            commit(items[selectedIdx].textContent);
          } else {
            commit(input.value);
          }
        } else if (e.key === "Escape") {
          e.preventDefault();
          if (dropdown.style.display === "block") {
            hideDropdown();
          } else {
            doCancel();
          }
        } else if (e.key === "Tab") {
          e.preventDefault();
          if (selectedIdx >= 0 && items[selectedIdx]) {
            commit(items[selectedIdx].textContent);
          } else {
            commit(input.value);
          }
        }
      });

      // Outside-click handler (same pattern as formula editor)
      onDocMouseDown = function(e) {
        if (committed) {
          document.removeEventListener("mousedown", onDocMouseDown, true);
          return;
        }
        if (wrapper.contains(e.target) || dropdown.contains(e.target)) return;
        commit(input.value);
      };
      document.addEventListener("mousedown", onDocMouseDown, true);

      onRendered(function() {
        input.focus();
        input.select();
        showSuggestions(currentValue);
      });

      return wrapper;
    };
  }

  // =========================================================================
  // Custom formula editor
  // =========================================================================

  function formulaEditor(terms, suggestions) {
    return function(cell, onRendered, success, cancel) {
      var placeholder = document.createElement("div");
      placeholder.className = "val-formula-placeholder";
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
          input.className = "val-input-editor";
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
        overlay.className = "val-formula-overlay";
        overlay.style.position = "fixed";
        overlay.style.left = cellRect.left + "px";
        overlay.style.top = cellRect.top + "px";
        overlay.style.width = cellRect.width + "px";
        overlay.style.height = cellRect.height + "px";
        overlay.style.zIndex = "10000";

        var aceContainer = document.createElement("div");
        aceContainer.className = "val-ace-container";
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
          console.warn("[Values Table] Term highlighting/autocomplete init failed:", e.message);
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
          name: "acceptCompletionAndCommit",
          bindKey: { win: "Tab", mac: "Tab" },
          exec: function(ed) {
            if (ed.completer && ed.completer.popup &&
                ed.completer.popup.isOpen) {
              ed.completer.insertMatch();
            }
            commit(ed.getValue());
            setTimeout(function() { cell.navigateNext(); }, 0);
            return true;
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
  // Column definitions by model type
  // =========================================================================

  function buildColumnDefs(modelType, inputId, stateNames, valueNames, terms, suggestions) {
    var cols = [];
    var fEditor = formulaEditor(terms, suggestions);
    var nameEditor = typeaheadEditor(function() {
      var data = table.getData();
      var seen = {}, names = [];
      for (var i = 0; i < data.length; i++) {
        var n = data[i].name;
        if (n && !seen[n]) { seen[n] = true; names.push(n); }
      }
      return names;
    });

    // Build state dropdown values
    var stateDropdownValues = [
      { label: "All", value: "All" },
      { label: "All Other", value: "All Other" }
    ].concat(stateNames.map(function(s) {
      return { label: s, value: s };
    }));

    // Build destination dropdown values (markov only)
    var destDropdownValues = [
      { label: "\u2014 (None)", value: "" }
    ].concat(stateNames.map(function(s) {
      return { label: s, value: s };
    }));

    var typeValues = [
      { label: "Outcome", value: "outcome" },
      { label: "Cost", value: "cost" }
    ];

    // Name column (all model types) — typeahead with existing value names
    cols.push({
      title: "Name",
      field: "name",
      widthGrow: 1,
      minWidth: 120,
      editor: nameEditor,
      formatter: emdashIfEmpty
    });

    // Display Name column
    cols.push({
      title: "Display Name",
      field: "display_name",
      widthGrow: 1,
      minWidth: 120,
      editor: "input",
      formatter: emdashIfEmpty
    });

    // Description column
    cols.push({
      title: "Description",
      field: "description",
      widthGrow: 1,
      minWidth: 120,
      editor: "input",
      formatter: emdashIfEmpty
    });

    // Type column
    cols.push({
      title: "Type",
      field: "type",
      minWidth: 100,
      editor: "list",
      editorParams: { values: typeValues },
      formatter: function(cell) {
        var val = cell.getValue();
        if (!val) return "\u2014";
        return val.charAt(0).toUpperCase() + val.slice(1);
      }
    });

    // State column (psm, custom_psm, markov — not decision_tree)
    if (modelType !== "decision_tree") {
      cols.push({
        title: "State",
        field: "state",
        minWidth: 140,
        editor: "list",
        editorParams: { values: stateDropdownValues },
        formatter: emdashIfEmpty
      });
    }

    // Destination column (markov only)
    if (modelType === "markov") {
      cols.push({
        title: "Destination",
        field: "destination",
        minWidth: 140,
        editor: "list",
        editorParams: { values: destDropdownValues },
        formatter: emdashIfEmpty
      });
    }

    // Formula column
    cols.push({
      title: "Formula",
      field: "formula",
      widthGrow: 2,
      minWidth: 450,
      editor: fEditor,
      formatter: emdashIfEmpty
    });

    // Discounting Override column
    cols.push({
      title: "Discounting Override",
      field: "discounting_override",
      widthGrow: 1,
      minWidth: 120,
      editor: "input",
      formatter: emdashIfEmpty
    });

    // Delete column
    cols.push({
      title: "",
      field: "_delete",
      width: 60,
      widthGrow: 0,
      hozAlign: "center",
      headerSort: false,
      editor: false,
      clipboard: false,
      formatter: function(cell) {
        var data = cell.getRow().getData();

        if (data._isNew) {
          var wrapper = document.createElement("span");

          var confirmBtn = document.createElement("button");
          confirmBtn.type = "button";
          confirmBtn.className = "val-confirm-btn";
          confirmBtn.textContent = "\u2713";
          confirmBtn.addEventListener("click", function(e) {
            e.stopPropagation();
            var rowData = cell.getRow().getData();
            var name = (rowData.name || "").trim();
            var formula = (rowData.formula || "").trim();
            if (!name || !formula) {
              if (typeof Shiny !== "undefined") {
                Shiny.notifications.show({ html: "Name and formula are required.", type: "warning", duration: 3000 });
              }
              return;
            }
            var payload = {
              type: "add_value",
              name: name,
              formula: formula,
              state: (rowData.state || "").trim(),
              destination: (rowData.destination || "").trim(),
              value_type: (rowData.type || "cost").trim(),
              display_name: (rowData.display_name || "").trim(),
              description: (rowData.description || "").trim(),
              discounting_override: (rowData.discounting_override || "").trim()
            };
            cell.getRow().delete();
            relayout(table);
            if (typeof Shiny !== "undefined") {
              Shiny.setInputValue(inputId, payload, { priority: "event" });
            }
          });

          var cancelBtn = document.createElement("button");
          cancelBtn.type = "button";
          cancelBtn.className = "val-cancel-btn";
          cancelBtn.textContent = "\u00d7";
          cancelBtn.addEventListener("click", function(e) {
            e.stopPropagation();
            cell.getRow().delete();
            relayout(table);
          });

          wrapper.appendChild(confirmBtn);
          wrapper.appendChild(cancelBtn);
          return wrapper;
        }

        var btn = document.createElement("button");
        btn.type = "button";
        btn.className = "val-delete-btn";
        btn.textContent = "\u00d7";
        btn.addEventListener("click", function(e) {
          e.stopPropagation();
          var data = cell.getRow().getData();
          if (typeof Shiny !== "undefined") {
            Shiny.setInputValue(inputId, {
              type: "remove_value",
              name: data.name,
              state: data.state || "",
              destination: data.destination || ""
            }, { priority: "event" });
          }
        });
        return btn;
      }
    });

    return cols;
  }

  // =========================================================================
  // Grid initialization
  // =========================================================================

  var _activeTables = {};
  var table; // module-level reference for confirm button closure

  function initGrid(containerDiv) {
    var inputId = containerDiv.dataset.inputId;
    if (!inputId) return;

    if (_activeTables[inputId + "_val"]) {
      try { _activeTables[inputId + "_val"].destroy(); } catch (e) {}
      delete _activeTables[inputId + "_val"];
    }

    var modelType = containerDiv.dataset.modelType || "markov";

    var stateNames = [];
    try { stateNames = JSON.parse(containerDiv.dataset.stateNames || "[]"); } catch (e) {}
    var terms = null;
    try { terms = JSON.parse(containerDiv.dataset.terms || "null"); } catch (e) {}
    var suggestions = null;
    try { suggestions = JSON.parse(containerDiv.dataset.suggestions || "null"); } catch (e) {}

    var initialData = [];
    try {
      initialData = JSON.parse(containerDiv.dataset.initial || "[]");
    } catch (e) {
      initialData = [];
    }

    // Extract unique value names from initial data for typeahead
    var valueNamesSeen = {};
    var valueNames = [];
    for (var i = 0; i < initialData.length; i++) {
      var n = initialData[i].name;
      if (n && !valueNamesSeen[n]) {
        valueNamesSeen[n] = true;
        valueNames.push(n);
      }
    }

    var columnDefs = buildColumnDefs(modelType, inputId, stateNames, valueNames, terms, suggestions);

    table = new Tabulator(containerDiv, {
      index: "_id",
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
      clipboardCopyStyled: false,
      headerSortClickElement: "icon"
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

    _activeTables[inputId + "_val"] = table;

    // "Add Value" button above the table
    var addBtn = document.createElement("button");
    addBtn.type = "button";
    addBtn.className = "val-add-btn";
    addBtn.textContent = "+ Add Value";
    containerDiv.parentNode.insertBefore(addBtn, containerDiv);
    addBtn.addEventListener("click", function() {
      var emptyRow = { _isNew: true, name: "", formula: "", type: "cost",
                       display_name: "", description: "", discounting_override: "" };
      if (modelType !== "decision_tree") {
        emptyRow.state = "";
      }
      if (modelType === "markov") {
        emptyRow.destination = "";
      }
      table.addRow(emptyRow, false).then(function(row) {
        relayout(table);
        row.getElement().scrollIntoView({ behavior: "smooth", block: "nearest" });
        var nameCell = row.getCell("name");
        if (nameCell) {
          setTimeout(function() { nameCell.edit(); }, 50);
        }
      });
    });

    // Cell edited — fire edit_value action
    table.on("cellEdited", function(cell) {
      var field = cell.getField();
      var data = cell.getRow().getData();
      if (data._isNew) {
        relayout(table);
        return;
      }
      if (cell.getOldValue() === cell.getValue()) {
        relayout(table);
        return;
      }

      // Composite key: name + state + destination
      // Use old values for key fields since data already reflects the new value
      var name = data.name;
      var state = data.state || "";
      var destination = data.destination || "";
      if (field === "name") name = cell.getOldValue() || "";
      if (field === "state") state = cell.getOldValue() || "";
      if (field === "destination") destination = cell.getOldValue() || "";

      var payload = {
        type: "edit_value",
        name: name,
        state: state,
        destination: destination,
        field: field,
        value: cell.getValue()
      };

      // Add error flags for name edits
      if (field === "name") {
        payload.error_on_name_sharing = true;
        payload.error_on_field_changes = true;
      }

      if (typeof Shiny !== "undefined") {
        Shiny.setInputValue(inputId, payload, { priority: "event" });
      }

      relayout(table);
    });

    containerDiv.setAttribute("data-initialized", "true");
  }

  // =========================================================================
  // Lifecycle
  // =========================================================================

  function initAllGrids() {
    var containers = document.querySelectorAll(".values-table-container:not([data-initialized])");
    if (containers.length === 0) return;
    ensureTabulator(function() {
      containers.forEach(initGrid);
    });
  }

  if (typeof Shiny !== "undefined") {
    $(document).on("shiny:connected", function() {
      setTimeout(initAllGrids, 100);
    });

    $(document).on("shiny:value", function() {
      setTimeout(initAllGrids, 100);
    });

    Shiny.addCustomMessageHandler("values-table-update", function(msg) {
      var key = msg.inputId + "_val";
      var tbl = _activeTables[key];
      if (!tbl) return;

      // Save scroll position
      var holder = tbl.element.querySelector(".tabulator-tableholder");
      var scrollLeft = holder ? holder.scrollLeft : 0;
      var scrollTop = holder ? holder.scrollTop : 0;

      // Replace all data in server-defined order (no reorder, no append)
      tbl.replaceData(msg.data);

      // Restore scroll position
      if (holder) {
        requestAnimationFrame(function() {
          holder.scrollLeft = scrollLeft;
          holder.scrollTop = scrollTop;
        });
      }
    });

    setTimeout(initAllGrids, 100);
  }
})();
