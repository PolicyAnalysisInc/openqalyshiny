/* OQGrid — Tabulator CDN Loader (single copy) */
(function() {
  "use strict";

  // Establish global namespace
  window.OQGrid = window.OQGrid || {};

  var TABULATOR_CDN = "https://cdn.jsdelivr.net/npm/tabulator-tables@6.3.1/dist/js/tabulator.min.js";
  var TABULATOR_CSS = "https://cdn.jsdelivr.net/npm/tabulator-tables@6.3.1/dist/css/tabulator_bootstrap5.min.css";
  var _callbacks = [];
  var _loading = false;

  OQGrid.ensureTabulator = function(callback) {
    if (typeof Tabulator !== "undefined") {
      callback();
      return;
    }
    _callbacks.push(callback);
    if (_loading) return;
    _loading = true;

    var cssLink = document.createElement("link");
    cssLink.rel = "stylesheet";
    cssLink.href = TABULATOR_CSS;
    document.head.appendChild(cssLink);

    var script = document.createElement("script");
    script.src = TABULATOR_CDN;
    script.onload = function() {
      console.log("[OQGrid] Tabulator loaded");
      var cbs = _callbacks.slice();
      _callbacks = [];
      for (var i = 0; i < cbs.length; i++) cbs[i]();
    };
    script.onerror = function() {
      console.error("[OQGrid] Failed to load Tabulator from CDN");
    };
    document.head.appendChild(script);
  };
})();
