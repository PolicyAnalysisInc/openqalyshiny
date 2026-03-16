/* Add Strategy Modal — Tabulator table for strategy-specific variables */
(function() {
  "use strict";
  console.log("[Add Strategy Modal] JS version 1.0.0 loaded");

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
      console.log("[Add Strategy Modal] Tabulator loaded");
      var cbs = _tabulatorCallbacks.slice();
      _tabulatorCallbacks = [];
      for (var i = 0; i < cbs.length; i++) cbs[i]();
    };
    script.onerror = function() {
      console.error("[Add Strategy Modal] Failed to load Tabulator from CDN");
    };
    document.head.appendChild(script);
  }

  // =========================================================================
  // Helpers
  // =========================================================================

  function emdashIfEmpty(cell) {
    var val = cell.getValue();
    if (val === null || val === undefined || val === "") return "\u2014";
    return val;
  }

  // =========================================================================
  // Custom formula editor (duplicated from variables-table.js)
  // =========================================================================

  function formulaEditor(terms, suggestions) {
    return function(cell, onRendered, success, cancel) {
      var placeholder = document.createElement("div");
      placeholder.className = "var-formula-placeholder";
      placeholder.addEventListener("focusout", function(e) { e.stopPropagation(); });

      var currentValue = cell.getValue() || "";
      var committed = false;
      var overlay = null;
      var aceEditor = null;
      var onDocMouseDown = null;
      var acFocusTrap = null;
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
        if (acFocusTrap) {
          document.removeEventListener("focusin", acFocusTrap, true);
          document.removeEventListener("focus", acFocusTrap, true);
          acFocusTrap = null;
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
          input.className = "var-input-editor";
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
        var minOverlayH = 36;
        var overlayH = Math.max(cellRect.height, minOverlayH);
        var topOffset = (overlayH - cellRect.height) / 2;
        var innerH = overlayH - 4;
        var vPad = Math.max(0, Math.round((innerH - lineH) / 2));

        overlay = document.createElement("div");
        overlay.className = "var-formula-overlay add-strategy-formula-overlay";
        overlay.style.position = "fixed";
        overlay.style.left = cellRect.left + "px";
        overlay.style.top = (cellRect.top - topOffset) + "px";
        overlay.style.width = cellRect.width + "px";
        overlay.style.height = overlayH + "px";
        overlay.style.zIndex = "10600";

        var aceContainer = document.createElement("div");
        aceContainer.className = "var-ace-container";
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

        // Prevent Bootstrap modal focus trap from closing autocomplete popup.
        // Ace appends .ace_autocomplete to document.body (outside the modal),
        // so focusin events propagate and trigger Bootstrap's focus enforcement.
        acFocusTrap = function(e) {
          if (e.target.closest && e.target.closest(".ace_autocomplete")) {
            e.stopPropagation();
          }
        };
        document.addEventListener("focusin", acFocusTrap, true);
        document.addEventListener("focus", acFocusTrap, true);

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
          console.warn("[Add Strategy Modal] Term highlighting/autocomplete init failed:", e.message);
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
              // Don't commit if autocomplete popup is open
              if (aceEditor.completer && aceEditor.completer.popup &&
                  aceEditor.completer.popup.isOpen) {
                return;
              }
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
  // Modal table instance
  // =========================================================================

  var _modalTable = null;

  // =========================================================================
  // Shiny message handlers
  // =========================================================================

  if (typeof Shiny !== "undefined") {

    // Initialize the variables table inside the modal
    Shiny.addCustomMessageHandler("init_add_strategy_vars_table", function(msg) {
      var variables = msg.variables || [];

      // terms/suggestions are pre-serialized JSON strings (auto_unbox=FALSE)
      // to preserve array structure for FormulaHighlighter/FormulaCompleter
      var terms = null;
      var suggestions = null;
      try { terms = typeof msg.terms_json === "string" ? JSON.parse(msg.terms_json) : (msg.terms_json || null); } catch (e) {}
      try { suggestions = typeof msg.suggestions_json === "string" ? JSON.parse(msg.suggestions_json) : (msg.suggestions_json || null); } catch (e) {}

      function tryInit(attempt) {
        var container = document.getElementById("add-strategy-vars-container");
        if (!container) {
          if (attempt < 20) {
            setTimeout(function() { tryInit(attempt + 1); }, 100);
          } else {
            console.error("[Add Strategy Modal] Could not find #add-strategy-vars-container");
          }
          return;
        }

        ensureTabulator(function() {
          if (_modalTable) {
            try { _modalTable.destroy(); } catch (e) {}
            _modalTable = null;
          }

          var columns = [
            {
              title: "Name",
              field: "name",
              widthGrow: 1,
              minWidth: 120,
              editor: false,
              formatter: emdashIfEmpty
            },
            {
              title: "Display Name",
              field: "display_name",
              widthGrow: 1,
              minWidth: 120,
              editor: "input",
              formatter: emdashIfEmpty
            },
            {
              title: "Description",
              field: "description",
              widthGrow: 1,
              minWidth: 120,
              editor: "input",
              formatter: emdashIfEmpty
            },
            {
              title: "Formula",
              field: "formula",
              widthGrow: 2,
              minWidth: 150,
              editor: formulaEditor(terms, suggestions),
              formatter: emdashIfEmpty
            }
          ];

          _modalTable = new Tabulator(container, {
            data: variables,
            columns: columns,
            layout: "fitColumns",
            height: "auto",
            editTriggerEvent: "dblclick",
            headerSortClickElement: "icon"
          });
        });
      }

      tryInit(0);
    });

    // Collect data from the modal table and send back to Shiny
    Shiny.addCustomMessageHandler("collect_add_strategy_vars", function(msg) {
      var data = [];
      if (_modalTable) {
        data = _modalTable.getData();
      }
      Shiny.setInputValue("add_strategy_modal_vars", data, { priority: "event" });
    });
  }
})();
