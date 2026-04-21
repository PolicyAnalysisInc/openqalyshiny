/* OQGrid — Typeahead Editor (free-text input with suggestions dropdown) */
(function() {
  "use strict";

  var OQGrid = window.OQGrid = window.OQGrid || {};
  OQGrid.editors = OQGrid.editors || {};

  // =========================================================================
  // Typeahead editor factory
  // getValues: function() returning array of suggestion strings
  // =========================================================================
  OQGrid.editors.typeahead = function(getValues) {
    return function(cell, onRendered, success, cancel) {
      var valueNames = typeof getValues === "function" ? getValues() : (getValues || []);
      var currentValue = cell.getValue() || "";
      var committed = false;
      var onDocMouseDown = null;

      var wrapper = document.createElement("div");
      wrapper.className = "oq-grid-typeahead-wrapper";
      wrapper.style.position = "relative";
      wrapper.style.width = "100%";
      wrapper.style.height = "100%";

      var input = document.createElement("input");
      input.type = "text";
      input.className = "oq-grid-input-editor";
      input.value = currentValue;
      wrapper.appendChild(input);

      // Dropdown appended to document.body with position:fixed
      var dropdown = document.createElement("div");
      dropdown.className = "oq-grid-typeahead-dropdown";
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
        while (dropdown.firstChild) dropdown.removeChild(dropdown.firstChild);
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
          item.className = "oq-grid-typeahead-item";
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
        var items = dropdown.querySelectorAll(".oq-grid-typeahead-item");
        items.forEach(function(el, i) {
          el.classList.toggle("oq-grid-typeahead-item-active", i === idx);
        });
        if (items[idx]) items[idx].scrollIntoView({ block: "nearest" });
      }

      input.addEventListener("input", function() {
        showSuggestions(input.value);
      });

      input.addEventListener("keydown", function(e) {
        var items = dropdown.querySelectorAll(".oq-grid-typeahead-item");
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

      // Outside-click handler
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
  };
})();
