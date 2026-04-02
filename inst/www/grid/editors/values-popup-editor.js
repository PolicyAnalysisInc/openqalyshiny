/* OQGrid — TWSA Values Popup Editor */
(function() {
  "use strict";

  var OQGrid = window.OQGrid = window.OQGrid || {};
  OQGrid.editors = OQGrid.editors || {};

  // =========================================================================
  // Format values data for display in the cell
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
  // Create a values popup
  // =========================================================================
  function createValuesPopup(cell, terms, suggestions, onSave, onCancel) {
    var data = cell.getRow().getData();
    var method = data.type || "radius";
    var currentData = data.data || {};

    var cellEl = cell.getElement();
    var cellRect = cellEl.getBoundingClientRect();

    // Overlay backdrop to catch outside clicks
    var backdrop = document.createElement("div");
    backdrop.className = "oq-grid-popup-backdrop";
    backdrop.style.position = "fixed";
    backdrop.style.top = "0";
    backdrop.style.left = "0";
    backdrop.style.width = "100%";
    backdrop.style.height = "100%";
    backdrop.style.zIndex = "10001";
    backdrop.style.background = "transparent";

    // Popup container
    var popup = document.createElement("div");
    popup.className = "oq-grid-values-popup";
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

    // =====================================================================
    // Field builders
    // =====================================================================

    // Build an Ace formula field inside the popup
    function makeFormulaField(label, initialValue) {
      var row = document.createElement("div");
      row.className = "oq-grid-popup-field";
      row.style.marginBottom = "8px";

      var lbl = document.createElement("label");
      lbl.className = "oq-grid-popup-label";
      lbl.textContent = label;
      lbl.style.display = "block";
      lbl.style.fontSize = "0.8rem";
      lbl.style.fontWeight = "500";
      lbl.style.marginBottom = "2px";
      row.appendChild(lbl);

      var editorDiv = document.createElement("div");
      editorDiv.className = "oq-grid-popup-ace";
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
          console.warn("[OQGrid Values Popup] Ace init warning:", e.message);
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
      row.className = "oq-grid-popup-field";
      row.style.marginBottom = "8px";

      var lbl = document.createElement("label");
      lbl.className = "oq-grid-popup-label";
      lbl.textContent = label;
      lbl.style.display = "block";
      lbl.style.fontSize = "0.8rem";
      lbl.style.fontWeight = "500";
      lbl.style.marginBottom = "2px";
      row.appendChild(lbl);

      var input = document.createElement("input");
      input.type = "number";
      input.className = "oq-grid-popup-numeric";
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
      row.className = "oq-grid-popup-field";
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

    // =====================================================================
    // Build popup content
    // =====================================================================

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
  // Public API
  // =========================================================================

  // Factory returning a Tabulator cell editor function
  OQGrid.editors.valuesPopup = function(terms, suggestions) {
    return function(cell, onRendered, success, cancel) {
      var placeholder = document.createElement("div");

      onRendered(function() {
        createValuesPopup(cell, terms, suggestions,
          function onSaveHandler(result) {
            success(result);
          },
          function onCancelHandler() {
            try { cancel(); } catch (e) {}
          }
        );
      });

      return placeholder;
    };
  };

  // Standalone create function for cellClick usage
  OQGrid.editors.createValuesPopup = createValuesPopup;

  // Display formatter
  OQGrid.editors.formatValuesDisplay = formatValuesDisplay;
})();
