/* OQGrid — Shared Utilities */
(function() {
  "use strict";

  var OQGrid = window.OQGrid = window.OQGrid || {};
  OQGrid.utils = OQGrid.utils || {};

  // =========================================================================
  // Focus scroll prevention — applied ONCE globally
  // Prevents Tabulator's internal focus() calls from scrolling the page
  // =========================================================================
  (function() {
    var origFocus = HTMLElement.prototype.focus;
    HTMLElement.prototype.focus = function(opts) {
      if (this.closest && this.closest(".tabulator")) {
        return origFocus.call(this, Object.assign({}, opts, { preventScroll: true }));
      }
      return origFocus.call(this, opts);
    };
  })();

  // =========================================================================
  // Reverse-lookup map: value -> display name
  // e.g. {"Drug A": "drug_a"} → {"drug_a": "Drug A"}
  // =========================================================================
  OQGrid.utils.buildDisplayMap = function(obj) {
    var map = {};
    var keys = Object.keys(obj || {});
    for (var i = 0; i < keys.length; i++) {
      map[obj[keys[i]]] = keys[i];
    }
    return map;
  };

  // =========================================================================
  // Convert { displayName: value } object to Tabulator list editor values
  // Options: { emptyLabel: "— (None)" } prepends an empty option
  // =========================================================================
  OQGrid.utils.toListValues = function(obj, opts) {
    opts = opts || {};
    var values = [];
    if (opts.emptyLabel) {
      values.push({ label: opts.emptyLabel, value: "" });
    }
    var keys = Object.keys(obj || {});
    for (var i = 0; i < keys.length; i++) {
      values.push({ label: keys[i], value: obj[keys[i]] });
    }
    return values;
  };

  // =========================================================================
  // Safely parse a data-* attribute with JSON fallback
  // =========================================================================
  OQGrid.utils.parseDataAttr = function(el, attr, fallback) {
    try {
      return JSON.parse(el.dataset[attr] || JSON.stringify(fallback));
    } catch (e) {
      return fallback;
    }
  };

  // =========================================================================
  // Info title — titleFormatter that adds an info-trigger popover to a header
  // =========================================================================
  OQGrid.utils.infoTitle = function(description) {
    return function(cell) {
      var title = cell.getValue();
      var wrapper = document.createElement("span");
      wrapper.style.display = "inline-flex";
      wrapper.style.alignItems = "center";
      wrapper.style.gap = "4px";

      var text = document.createElement("span");
      text.textContent = title;
      wrapper.appendChild(text);

      var btn = document.createElement("button");
      btn.className = "info-trigger";
      var icon = document.createElement("i");
      icon.className = "fa-solid fa-circle-info";
      btn.appendChild(icon);
      var popover = document.createElement("span");
      popover.className = "info-popover";
      popover.textContent = description;
      btn.appendChild(popover);
      wrapper.appendChild(btn);

      return wrapper;
    };
  };

  // =========================================================================
  // Relayout — redraw with column width distribution and scroll preservation
  // =========================================================================
  OQGrid.relayout = function(table) {
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
  };
})();
