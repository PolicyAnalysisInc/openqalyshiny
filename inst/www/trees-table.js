/* Trees Table — Tabulator (individual edit/remove actions) */
(function() {
  "use strict";
  console.log("[Trees Table] JS version 1.0.0 loaded");

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
      console.log("[Trees Table] Tabulator loaded");
      var cbs = _tabulatorCallbacks.slice();
      _tabulatorCallbacks = [];
      for (var i = 0; i < cbs.length; i++) cbs[i]();
    };
    script.onerror = function() {
      console.error("[Trees Table] Failed to load Tabulator from CDN");
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
  // Custom formula editor for formula column
  // =========================================================================

  function formulaEditor(terms, suggestions) {
    return function(cell, onRendered, success, cancel) {
      var placeholder = document.createElement("div");
      placeholder.className = "tt-formula-placeholder";
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
          input.className = "tt-input-editor";
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
        overlay.className = "tt-formula-overlay";
        overlay.style.position = "fixed";
        overlay.style.left = cellRect.left + "px";
        overlay.style.top = cellRect.top + "px";
        overlay.style.width = cellRect.width + "px";
        overlay.style.height = cellRect.height + "px";
        overlay.style.zIndex = "10000";

        var aceContainer = document.createElement("div");
        aceContainer.className = "tt-ace-container";
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
          console.warn("[Trees Table] Term highlighting/autocomplete init failed:", e.message);
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
  // Typeahead text editor (free-text input with suggestions dropdown)
  // =========================================================================

  function typeaheadEditor(getNames) {
    return function(cell, onRendered, success, cancel) {
      var names = getNames();
      var currentValue = cell.getValue() || "";
      var committed = false;
      var onDocMouseDown = null;

      var wrapper = document.createElement("div");
      wrapper.className = "tt-typeahead-wrapper";
      wrapper.style.position = "relative";
      wrapper.style.width = "100%";
      wrapper.style.height = "100%";

      var input = document.createElement("input");
      input.type = "text";
      input.className = "tt-input-editor";
      input.value = currentValue;
      wrapper.appendChild(input);

      var dropdown = document.createElement("div");
      dropdown.className = "tt-typeahead-dropdown";
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
        for (var i = 0; i < names.length; i++) {
          var n = names[i];
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
          item.className = "tt-typeahead-item";
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
        var items = dropdown.querySelectorAll(".tt-typeahead-item");
        items.forEach(function(el, i) {
          el.classList.toggle("tt-typeahead-item-active", i === idx);
        });
        if (items[idx]) {
          items[idx].scrollIntoView({ block: "nearest" });
        }
      }

      input.addEventListener("input", function() {
        showSuggestions(input.value);
      });

      input.addEventListener("keydown", function(e) {
        var items = dropdown.querySelectorAll(".tt-typeahead-item");
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
  // Multi-tag editor for tags column
  // =========================================================================

  function multiTagEditor(getAvailableTags) {
    return function(cell, onRendered, success, cancel) {
      var available = getAvailableTags();
      var currentValue = cell.getValue() || "";
      var selectedTags = currentValue ? currentValue.split(",").map(function(v) { return v.trim(); }).filter(Boolean) : [];
      var committed = false;
      var onDocMouseDown = null;
      var dropdownVisible = false;
      var selectedIdx = -1;

      var placeholder = document.createElement("div");
      placeholder.className = "tt-multitag-placeholder";
      placeholder.textContent = "\u00a0";

      var overlay = document.createElement("div");
      overlay.className = "tt-multitag-overlay";
      document.body.appendChild(overlay);

      var dropdown = document.createElement("div");
      dropdown.className = "tt-multitag-dropdown";
      document.body.appendChild(dropdown);

      function positionOverlay() {
        var cellEl = cell.getElement();
        var rect = cellEl.getBoundingClientRect();
        overlay.style.left = rect.left + "px";
        overlay.style.top = rect.top + "px";
        overlay.style.minWidth = rect.width + "px";
        overlay.style.minHeight = rect.height + "px";
      }

      function positionDropdown() {
        var rect = overlay.getBoundingClientRect();
        dropdown.style.left = rect.left + "px";
        dropdown.style.top = rect.bottom + "px";
        dropdown.style.width = rect.width + "px";
      }

      function cleanup() {
        if (onDocMouseDown) {
          document.removeEventListener("mousedown", onDocMouseDown, true);
          onDocMouseDown = null;
        }
        if (overlay && overlay.parentNode) overlay.parentNode.removeChild(overlay);
        if (dropdown && dropdown.parentNode) dropdown.parentNode.removeChild(dropdown);
      }

      function commit() {
        if (committed) return;
        committed = true;
        hideDropdown();
        cleanup();
        try { success(selectedTags.join(",")); } catch (e) {}
      }

      function doCancel() {
        if (committed) return;
        committed = true;
        hideDropdown();
        cleanup();
        try { cancel(); } catch (e) {}
      }

      function clearDropdown() {
        while (dropdown.firstChild) dropdown.removeChild(dropdown.firstChild);
      }

      function hideDropdown() {
        dropdown.style.display = "none";
        clearDropdown();
        selectedIdx = -1;
        dropdownVisible = false;
      }

      function renderTags() {
        while (overlay.firstChild) overlay.removeChild(overlay.firstChild);

        selectedTags.forEach(function(tag, idx) {
          var tagEl = document.createElement("span");
          tagEl.className = "tt-tag-editable";
          tagEl.textContent = tag;

          var removeBtn = document.createElement("span");
          removeBtn.className = "tt-tag-remove";
          removeBtn.textContent = "\u00d7";
          removeBtn.addEventListener("mousedown", function(e) {
            e.preventDefault();
            e.stopPropagation();
            selectedTags.splice(idx, 1);
            renderTags();
            showSuggestions(input.value);
            input.focus();
          });
          tagEl.appendChild(removeBtn);
          overlay.appendChild(tagEl);
        });

        overlay.appendChild(input);
      }

      var input = document.createElement("input");
      input.type = "text";
      input.className = "tt-multitag-input";
      input.value = "";

      function getFilteredItems(filter) {
        var query = (filter || "").toLowerCase();
        var selectedSet = {};
        selectedTags.forEach(function(t) { selectedSet[t] = true; });
        var matches = [];
        for (var i = 0; i < available.length; i++) {
          var n = available[i];
          if (!selectedSet[n] && n.toLowerCase().indexOf(query) !== -1) {
            matches.push(n);
          }
        }
        return matches;
      }

      function showSuggestions(filter) {
        clearDropdown();
        selectedIdx = -1;
        var matches = getFilteredItems(filter);
        if (matches.length === 0) {
          hideDropdown();
          return;
        }
        matches.forEach(function(name) {
          var item = document.createElement("div");
          item.className = "tt-multitag-item";
          item.textContent = name;
          item.addEventListener("mousedown", function(e) {
            e.preventDefault();
            e.stopPropagation();
            selectedTags.push(name);
            input.value = "";
            renderTags();
            showSuggestions("");
            input.focus();
          });
          dropdown.appendChild(item);
        });
        positionDropdown();
        dropdown.style.display = "block";
        dropdownVisible = true;
      }

      function highlightItem(idx) {
        var items = dropdown.querySelectorAll(".tt-multitag-item");
        items.forEach(function(el, i) {
          el.classList.toggle("tt-multitag-item-active", i === idx);
        });
        if (items[idx]) items[idx].scrollIntoView({ block: "nearest" });
      }

      input.addEventListener("input", function() {
        showSuggestions(input.value);
      });

      input.addEventListener("keydown", function(e) {
        var items = dropdown.querySelectorAll(".tt-multitag-item");
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
          if (dropdownVisible && selectedIdx >= 0 && items[selectedIdx]) {
            selectedTags.push(items[selectedIdx].textContent);
            input.value = "";
            renderTags();
            showSuggestions("");
            input.focus();
          } else if (input.value.trim()) {
            selectedTags.push(input.value.trim());
            input.value = "";
            renderTags();
            showSuggestions("");
            input.focus();
          } else if (!dropdownVisible) {
            commit();
          }
        } else if (e.key === "Escape") {
          e.preventDefault();
          if (dropdownVisible) {
            hideDropdown();
          } else {
            doCancel();
          }
        } else if (e.key === "Tab") {
          e.preventDefault();
          if (dropdownVisible && selectedIdx >= 0 && items[selectedIdx]) {
            selectedTags.push(items[selectedIdx].textContent);
            input.value = "";
            renderTags();
            showSuggestions("");
            input.focus();
          } else if (input.value.trim()) {
            selectedTags.push(input.value.trim());
            input.value = "";
            renderTags();
            showSuggestions("");
            input.focus();
          } else {
            commit();
          }
        } else if (e.key === "Backspace" && input.value === "" && selectedTags.length > 0) {
          selectedTags.pop();
          renderTags();
          showSuggestions("");
          input.focus();
        }
      });

      overlay.addEventListener("mousedown", function(e) {
        e.stopPropagation();
      });
      overlay.addEventListener("click", function(e) {
        e.stopPropagation();
        input.focus();
      });

      onDocMouseDown = function(e) {
        if (committed) {
          document.removeEventListener("mousedown", onDocMouseDown, true);
          return;
        }
        if (overlay.contains(e.target) || dropdown.contains(e.target)) return;
        commit();
      };
      document.addEventListener("mousedown", onDocMouseDown, true);

      onRendered(function() {
        positionOverlay();
        renderTags();
        input.focus();
        showSuggestions("");
      });

      return placeholder;
    };
  }

  // =========================================================================
  // Tag formatter (display pills)
  // =========================================================================

  function tagFormatter(cell) {
    var val = cell.getValue();
    if (!val) return "\u2014";
    var container = document.createElement("div");
    container.className = "tt-tags-display";
    val.split(",").forEach(function(v) {
      v = v.trim();
      if (!v) return;
      var tag = document.createElement("span");
      tag.className = "tt-tag";
      tag.textContent = v;
      container.appendChild(tag);
    });
    return container;
  }

  // =========================================================================
  // Column definitions
  // =========================================================================

  function buildColumnDefs(inputId, treeName, terms, suggestions, getTableData) {
    var getAvailableTags = function() {
      var data = getTableData();
      var seen = {}, tags = [];
      for (var i = 0; i < data.length; i++) {
        var t = data[i].tags;
        if (!t) continue;
        t.split(",").forEach(function(v) {
          v = v.trim();
          if (v && !seen[v]) { seen[v] = true; tags.push(v); }
        });
      }
      return tags;
    };

    var cols = [
      {
        title: "Node",
        field: "node",
        widthGrow: 1,
        minWidth: 120,
        editor: "input",
        formatter: emdashIfEmpty
      },
      {
        title: "Parent",
        field: "parent",
        widthGrow: 1,
        minWidth: 120,
        editor: "list",
        editorParams: {
          valuesLookup: function() {
            var data = getTableData();
            var seen = {}, nodes = [];
            nodes.push({ label: "\u2014 (None)", value: "" });
            for (var i = 0; i < data.length; i++) {
              var n = data[i].node;
              if (n && !seen[n]) { seen[n] = true; nodes.push({ label: n, value: n }); }
            }
            return nodes;
          }
        },
        formatter: emdashIfEmpty
      },
      {
        title: "Tags",
        field: "tags",
        widthGrow: 1,
        minWidth: 120,
        editor: multiTagEditor(getAvailableTags),
        formatter: tagFormatter
      },
      {
        title: "Formula",
        field: "formula",
        widthGrow: 2,
        minWidth: 200,
        editor: formulaEditor(terms, suggestions),
        formatter: emdashIfEmpty
      }
    ];

    // Delete / confirm column
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
          confirmBtn.className = "tt-confirm-btn";
          confirmBtn.textContent = "\u2713";
          confirmBtn.addEventListener("click", function(e) {
            e.stopPropagation();
            var rowData = cell.getRow().getData();
            var node = (rowData.node || "").trim();
            if (!node) {
              alert("Node is required.");
              return;
            }
            cell.getRow().delete();
            relayout(table);
            if (typeof Shiny !== "undefined") {
              var payload = {
                type: "add_tree_node",
                tree_name: treeName,
                node: node,
                parent: (rowData.parent || "").trim(),
                formula: (rowData.formula || "").trim(),
                tags: (rowData.tags || "").trim()
              };
              Shiny.setInputValue(inputId, payload, { priority: "event" });
            }
          });

          var cancelBtn = document.createElement("button");
          cancelBtn.type = "button";
          cancelBtn.className = "tt-cancel-btn";
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
        btn.className = "tt-delete-btn";
        btn.textContent = "\u00d7";
        btn.addEventListener("click", function(e) {
          e.stopPropagation();
          var data = cell.getRow().getData();
          if (typeof Shiny !== "undefined") {
            Shiny.setInputValue(inputId, {
              type: "remove_tree_node",
              tree_name: treeName,
              node: data.node
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

    var treeName = containerDiv.dataset.treeName || "";

    if (_activeTables[inputId + "_trees"]) {
      try { _activeTables[inputId + "_trees"].destroy(); } catch (e) {}
      delete _activeTables[inputId + "_trees"];
    }

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

    var columnDefs = buildColumnDefs(inputId, treeName, terms, suggestions, function() {
      return table ? table.getData() : initialData;
    });

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

    // Fix: redraw when tab becomes visible (container goes from 0 to non-zero width)
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

    _activeTables[inputId + "_trees"] = table;

    // "Add Node" button
    var addBtn = document.createElement("button");
    addBtn.type = "button";
    addBtn.className = "tt-add-btn";
    addBtn.textContent = "+ Add Node";
    var toolbarLeft = containerDiv.closest('.trees-editor');
    toolbarLeft = toolbarLeft ? toolbarLeft.querySelector('.trees-toolbar-left') : null;
    if (toolbarLeft) {
      toolbarLeft.appendChild(addBtn);
    } else {
      containerDiv.parentNode.insertBefore(addBtn, containerDiv);
    }

    addBtn.addEventListener("click", function() {
      table.addRow(
        {
          _isNew: true, node: "", parent: "",
          formula: "", tags: ""
        },
        false
      ).then(function(row) {
        relayout(table);
        row.getElement().scrollIntoView({ behavior: "smooth", block: "nearest" });
        var nodeCell = row.getCell("node");
        if (nodeCell) {
          setTimeout(function() { nodeCell.edit(); }, 50);
        }
      });
    });

    // Cell edited — fire edit_tree_node action
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

      var node = data.node;

      if (typeof Shiny !== "undefined") {
        if (field === "node") {
          Shiny.setInputValue(inputId, {
            type: "edit_tree_node",
            tree_name: treeName,
            node: cell.getOldValue() || "",
            field: "node",
            value: cell.getValue()
          }, { priority: "event" });
        } else {
          Shiny.setInputValue(inputId, {
            type: "edit_tree_node",
            tree_name: treeName,
            node: node,
            field: field,
            value: cell.getValue()
          }, { priority: "event" });
        }
      }

      relayout(table);
    });

    containerDiv.setAttribute("data-initialized", "true");
  }

  // =========================================================================
  // Lifecycle
  // =========================================================================

  function initAllGrids() {
    var containers = document.querySelectorAll(".trees-table-container:not([data-initialized])");
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

    setTimeout(initAllGrids, 100);
  }
})();
