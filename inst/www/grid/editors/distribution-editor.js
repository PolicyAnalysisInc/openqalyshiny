/* OQGrid — Distribution Overlay Editor (univariate PSA) */
(function() {
  "use strict";

  var OQGrid = window.OQGrid = window.OQGrid || {};
  OQGrid.editors = OQGrid.editors || {};

  var DISTRIBUTIONS = OQGrid.helpers.distributions.DISTRIBUTIONS;
  var parseDistributionString = OQGrid.helpers.distributions.parseDistributionString;
  var buildDistributionString = OQGrid.helpers.distributions.buildDistributionString;

  // =========================================================================
  // Mini Ace Editor factory (shared by distribution and mv-distribution editors)
  // Creates a single-line Ace editor in a container with term highlighting
  // and autocomplete support.
  // =========================================================================
  OQGrid.editors._createMiniAceEditor = function(container, value, terms, suggestions, opts) {
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
      console.warn("[OQGrid] Mini Ace highlighter/autocomplete init failed:", e.message);
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
  };

  // =========================================================================
  // Utility: clear all children from a DOM element
  // =========================================================================
  function clearChildren(el) {
    while (el.firstChild) el.removeChild(el.firstChild);
  }

  // =========================================================================
  // Distribution editor factory
  // Returns a Tabulator cell editor function for univariate distributions.
  // Creates a fixed-position overlay with distribution type dropdown,
  // parameterization radio buttons, and mini Ace editors for each param.
  // =========================================================================
  OQGrid.editors.distribution = function(terms, suggestions) {
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

          var ed = OQGrid.editors._createMiniAceEditor(aceContainer, paramValue, terms, suggestions, {
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
  };
})();
