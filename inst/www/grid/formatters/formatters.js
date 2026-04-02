/* OQGrid — Shared Formatters */
(function() {
  "use strict";

  var OQGrid = window.OQGrid = window.OQGrid || {};
  OQGrid.fmt = OQGrid.fmt || {};

  // Em-dash for empty/null/undefined values
  OQGrid.fmt.emdash = function(cell) {
    var val = cell.getValue();
    if (val === null || val === undefined || val === "") return "\u2014";
    return val;
  };

  // Display map reverse-lookup formatter factory
  // Usage: formatter: OQGrid.fmt.displayMap({"drug_a": "Drug A"})
  OQGrid.fmt.displayMap = function(map) {
    return function(cell) {
      var val = cell.getValue();
      if (!val || val === "") return "\u2014";
      return map[val] || val;
    };
  };

  // Boolean → "Yes"/"No"
  OQGrid.fmt.yesNo = function(cell) {
    var val = cell.getValue();
    return (val === 1 || val === "1" || val === true || val === "Yes" || val === "TRUE") ? "Yes" : "No";
  };

  // Capitalize first letter
  OQGrid.fmt.capitalize = function(cell) {
    var val = cell.getValue();
    if (!val) return "\u2014";
    return val.charAt(0).toUpperCase() + val.slice(1);
  };

  // Comma-separated tags rendered as pill badges
  OQGrid.fmt.tags = function(cell) {
    var val = cell.getValue();
    if (!val) return "\u2014";
    var container = document.createElement("div");
    container.className = "oq-grid-tags-display";
    val.split(",").forEach(function(v) {
      v = v.trim();
      if (!v) return;
      var tag = document.createElement("span");
      tag.className = "oq-grid-tag";
      tag.textContent = v;
      container.appendChild(tag);
    });
    return container;
  };
})();
