/* Summaries Table — Tabulator (individual edit/remove actions) */
(function() {
  "use strict";
  console.log("[Summaries Table] JS version 1.0.0 loaded");

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
      console.log("[Summaries Table] Tabulator loaded");
      var cbs = _tabulatorCallbacks.slice();
      _tabulatorCallbacks = [];
      for (var i = 0; i < cbs.length; i++) cbs[i]();
    };
    script.onerror = function() {
      console.error("[Summaries Table] Failed to load Tabulator from CDN");
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
  // Multi-tag editor for values column
  // =========================================================================

  function multiTagEditor(outcomeValues, costValues) {
    return function(cell, onRendered, success, cancel) {
      var rowType = cell.getRow().getData().type || "outcome";
      var available = rowType === "cost" ? costValues : outcomeValues;
      var currentValue = cell.getValue() || "";
      var selectedTags = currentValue ? currentValue.split(",").map(function(v) { return v.trim(); }).filter(Boolean) : [];
      var committed = false;
      var onDocMouseDown = null;
      var dropdownVisible = false;
      var selectedIdx = -1;

      // Invisible placeholder keeps the cell sized
      var placeholder = document.createElement("div");
      placeholder.className = "sum-multitag-placeholder";
      placeholder.textContent = "\u00a0";

      // Fixed overlay over the cell
      var overlay = document.createElement("div");
      overlay.className = "sum-multitag-overlay";
      document.body.appendChild(overlay);

      // Dropdown below the overlay
      var dropdown = document.createElement("div");
      dropdown.className = "sum-multitag-dropdown";
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
        // Remove existing tags and input from overlay
        while (overlay.firstChild) overlay.removeChild(overlay.firstChild);

        selectedTags.forEach(function(tag, idx) {
          var tagEl = document.createElement("span");
          tagEl.className = "sum-tag-editable";
          tagEl.textContent = tag;

          var removeBtn = document.createElement("span");
          removeBtn.className = "sum-tag-remove";
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
      input.className = "sum-multitag-input";
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
          item.className = "sum-multitag-item";
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
        var items = dropdown.querySelectorAll(".sum-multitag-item");
        items.forEach(function(el, i) {
          el.classList.toggle("sum-multitag-item-active", i === idx);
        });
        if (items[idx]) items[idx].scrollIntoView({ block: "nearest" });
      }

      input.addEventListener("input", function() {
        showSuggestions(input.value);
      });

      input.addEventListener("keydown", function(e) {
        var items = dropdown.querySelectorAll(".sum-multitag-item");
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

      // Stop event propagation on overlay
      overlay.addEventListener("mousedown", function(e) {
        e.stopPropagation();
      });
      overlay.addEventListener("click", function(e) {
        e.stopPropagation();
        input.focus();
      });

      // Outside-click handler
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
  // Column definitions
  // =========================================================================

  function buildColumnDefs(inputId, outcomeValues, costValues) {
    return [
      // Name
      {
        title: "Name",
        field: "name",
        widthGrow: 1,
        minWidth: 120,
        editor: "input",
        formatter: emdashIfEmpty
      },

      // Display Name
      {
        title: "Display Name",
        field: "display_name",
        widthGrow: 1,
        minWidth: 120,
        editor: "input",
        formatter: emdashIfEmpty
      },

      // Description
      {
        title: "Description",
        field: "description",
        widthGrow: 1,
        minWidth: 120,
        editor: "input",
        formatter: emdashIfEmpty
      },

      // Type
      {
        title: "Type",
        field: "type",
        minWidth: 100,
        editor: "list",
        editorParams: {
          values: [
            { label: "Outcome", value: "outcome" },
            { label: "Cost", value: "cost" }
          ]
        },
        formatter: function(cell) {
          var val = cell.getValue();
          if (!val) return "\u2014";
          return val.charAt(0).toUpperCase() + val.slice(1);
        }
      },

      // WTP
      {
        title: "WTP",
        field: "wtp",
        minWidth: 80,
        editor: "input",
        formatter: emdashIfEmpty
      },

      // Values (multi-tag)
      {
        title: "Values",
        field: "values",
        widthGrow: 2,
        minWidth: 300,
        editor: multiTagEditor(outcomeValues, costValues),
        formatter: function(cell) {
          var val = cell.getValue();
          if (!val) return "\u2014";
          var container = document.createElement("div");
          container.className = "sum-tags-display";
          val.split(",").forEach(function(v) {
            v = v.trim();
            if (!v) return;
            var tag = document.createElement("span");
            tag.className = "sum-tag";
            tag.textContent = v;
            container.appendChild(tag);
          });
          return container;
        }
      },

      // Delete column
      {
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
            confirmBtn.className = "sum-confirm-btn";
            confirmBtn.textContent = "\u2713";
            confirmBtn.addEventListener("click", function(e) {
              e.stopPropagation();
              var rowData = cell.getRow().getData();
              var name = (rowData.name || "").trim();
              var values = (rowData.values || "").trim();
              if (!name || !values) {
                alert("Name and values are required.");
                return;
              }
              cell.getRow().delete();
              relayout(table);
              if (typeof Shiny !== "undefined") {
                Shiny.setInputValue(inputId, {
                  type: "add_summary",
                  name: name,
                  values: values,
                  display_name: (rowData.display_name || "").trim(),
                  description: (rowData.description || "").trim(),
                  summary_type: rowData.type || "outcome",
                  wtp: (rowData.wtp || "").trim()
                }, { priority: "event" });
              }
            });

            var cancelBtn = document.createElement("button");
            cancelBtn.type = "button";
            cancelBtn.className = "sum-cancel-btn";
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
          btn.className = "sum-delete-btn";
          btn.textContent = "\u00d7";
          btn.addEventListener("click", function(e) {
            e.stopPropagation();
            var data = cell.getRow().getData();
            cell.getRow().delete();
            relayout(table);
            if (typeof Shiny !== "undefined") {
              Shiny.setInputValue(inputId, {
                type: "remove_summary",
                name: data.name
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

    // Destroy previous table for this inputId if it exists
    if (_activeTables[inputId]) {
      try { _activeTables[inputId].destroy(); } catch (e) {}
      delete _activeTables[inputId];
    }

    var initialData = [];
    try {
      initialData = JSON.parse(containerDiv.dataset.initial || "[]");
    } catch (e) {
      initialData = [];
    }

    var outcomeValues = [];
    try { outcomeValues = JSON.parse(containerDiv.dataset.outcomeValues || "[]"); } catch (e) {}
    var costValues = [];
    try { costValues = JSON.parse(containerDiv.dataset.costValues || "[]"); } catch (e) {}

    var columnDefs = buildColumnDefs(inputId, outcomeValues, costValues);

    var table = new Tabulator(containerDiv, {
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

    _activeTables[inputId] = table;

    // "Add Summary" button above the table
    var addBtn = document.createElement("button");
    addBtn.type = "button";
    addBtn.className = "sum-add-btn";
    addBtn.textContent = "+ Add Summary";
    containerDiv.parentNode.insertBefore(addBtn, containerDiv);
    addBtn.addEventListener("click", function() {
      table.addRow(
        { _isNew: true, name: "", values: "", type: "outcome", display_name: "", description: "", wtp: "" },
        false
      ).then(function(row) {
        relayout(table);
        row.getElement().scrollIntoView({ behavior: "smooth", block: "nearest" });
        var nameCell = row.getCell("name");
        if (nameCell) {
          setTimeout(function() { nameCell.edit(); }, 50);
        }
      });
    });

    // Cell edited — fire individual edit_summary action
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

      var name = data.name;
      if (field === "name") name = cell.getOldValue() || "";

      if (typeof Shiny !== "undefined") {
        Shiny.setInputValue(inputId, {
          type: "edit_summary",
          name: name,
          field: field,
          value: cell.getValue()
        }, { priority: "event" });
      }

      relayout(table);
    });

    containerDiv.setAttribute("data-initialized", "true");
  }

  // =========================================================================
  // Lifecycle — listen for renderUI re-renders
  // =========================================================================

  function initAllGrids() {
    var containers = document.querySelectorAll(".summaries-table-container:not([data-initialized])");
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
