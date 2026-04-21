/* OQGrid — Multi-Tag Editor (multi-select tag pills with dropdown) */
(function() {
  "use strict";

  var OQGrid = window.OQGrid = window.OQGrid || {};
  OQGrid.editors = OQGrid.editors || {};

  // =========================================================================
  // Multi-tag editor factory
  // getAvailable: function(rowData) returning array of available tag strings
  //               OR just an array of strings
  // options: { allowFreeText: false } — if true, Enter/Tab on free text adds it as a tag
  // =========================================================================
  OQGrid.editors.multiTag = function(getAvailable, options) {
    options = options || {};

    return function(cell, onRendered, success, cancel) {
      var rowData = cell.getRow().getData();
      var available = typeof getAvailable === "function" ? getAvailable(rowData) : (getAvailable || []);
      var currentValue = cell.getValue() || "";
      var selectedTags = currentValue ? currentValue.split(",").map(function(v) { return v.trim(); }).filter(Boolean) : [];
      var committed = false;
      var onDocMouseDown = null;
      var dropdownVisible = false;
      var selectedIdx = -1;

      // Invisible placeholder keeps the cell sized
      var placeholder = document.createElement("div");
      placeholder.className = "oq-grid-multitag-placeholder";
      placeholder.textContent = "\u00a0";

      // Fixed overlay over the cell
      var overlay = document.createElement("div");
      overlay.className = "oq-grid-multitag-overlay";
      document.body.appendChild(overlay);

      // Dropdown below the overlay
      var dropdown = document.createElement("div");
      dropdown.className = "oq-grid-multitag-dropdown";
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

      var input = document.createElement("input");
      input.type = "text";
      input.className = "oq-grid-multitag-input";
      input.value = "";

      function renderTags() {
        while (overlay.firstChild) overlay.removeChild(overlay.firstChild);

        selectedTags.forEach(function(tag, idx) {
          var tagEl = document.createElement("span");
          tagEl.className = "oq-grid-tag-editable";
          tagEl.textContent = tag;

          var removeBtn = document.createElement("span");
          removeBtn.className = "oq-grid-tag-remove";
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
          item.className = "oq-grid-multitag-item";
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
        var items = dropdown.querySelectorAll(".oq-grid-multitag-item");
        items.forEach(function(el, i) {
          el.classList.toggle("oq-grid-multitag-item-active", i === idx);
        });
        if (items[idx]) items[idx].scrollIntoView({ block: "nearest" });
      }

      input.addEventListener("input", function() {
        showSuggestions(input.value);
      });

      input.addEventListener("keydown", function(e) {
        var items = dropdown.querySelectorAll(".oq-grid-multitag-item");
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
          } else if (options.allowFreeText && input.value.trim()) {
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
          } else if (options.allowFreeText && input.value.trim()) {
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

      // Stop event propagation on overlay
      overlay.addEventListener("mousedown", function(e) { e.stopPropagation(); });
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
  };
})();
