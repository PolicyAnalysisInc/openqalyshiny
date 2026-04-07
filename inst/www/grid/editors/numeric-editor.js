/* OQGrid — Numeric Editor (number input with blank support) */
(function() {
  "use strict";

  var OQGrid = window.OQGrid = window.OQGrid || {};
  OQGrid.editors = OQGrid.editors || {};

  OQGrid.editors.numeric = function() {
    return function(cell, onRendered, success, cancel) {
      var currentValue = cell.getValue();
      var committed = false;
      var onDocMouseDown = null;

      // If current value is non-numeric (e.g. "Inf"), start blank
      var isNumeric = currentValue !== null && currentValue !== undefined &&
        currentValue !== "" && isFinite(Number(currentValue));
      var startValue = isNumeric ? String(currentValue) : "";

      var input = document.createElement("input");
      input.type = "number";
      input.className = "oq-grid-input-editor";
      input.value = startValue;
      input.style.width = "100%";
      input.style.height = "100%";
      input.min = "0";
      input.step = "any";

      function cleanup() {
        if (onDocMouseDown) {
          document.removeEventListener("mousedown", onDocMouseDown, true);
          onDocMouseDown = null;
        }
      }

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

      function commitFromInput() {
        var raw = input.value.trim();
        if (raw === "") {
          commit("");
        } else {
          var num = Number(raw);
          if (!isNaN(num) && num >= 0) {
            commit(String(num));
          } else {
            doCancel();
          }
        }
      }

      input.addEventListener("keydown", function(e) {
        if (e.key === "Enter" || e.key === "Tab") {
          e.preventDefault();
          commitFromInput();
        } else if (e.key === "Escape") {
          e.preventDefault();
          doCancel();
        }
      });

      // Outside-click handler
      onDocMouseDown = function(e) {
        if (committed) {
          document.removeEventListener("mousedown", onDocMouseDown, true);
          return;
        }
        if (input.contains(e.target)) return;
        commitFromInput();
      };
      document.addEventListener("mousedown", onDocMouseDown, true);

      onRendered(function() {
        input.focus();
        input.select();
      });

      return input;
    };
  };
})();
