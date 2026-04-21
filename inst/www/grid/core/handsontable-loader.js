/* OQGrid — Handsontable CDN Loader (single copy) */
(function() {
  "use strict";

  // Establish global namespace
  window.OQGrid = window.OQGrid || {};

  var HANDSONTABLE_CDN = "https://cdn.jsdelivr.net/npm/handsontable@14.1.0/dist/handsontable.full.min.js";
  var HANDSONTABLE_CSS = "https://cdn.jsdelivr.net/npm/handsontable@14.1.0/dist/handsontable.full.min.css";
  var _callbacks = [];
  var _loading = false;

  OQGrid.ensureHandsontable = function(callback) {
    if (typeof Handsontable !== "undefined") {
      callback();
      return;
    }
    _callbacks.push(callback);
    if (_loading) return;
    _loading = true;

    var cssLink = document.createElement("link");
    cssLink.rel = "stylesheet";
    cssLink.href = HANDSONTABLE_CSS;
    document.head.appendChild(cssLink);

    var script = document.createElement("script");
    script.src = HANDSONTABLE_CDN;
    script.onload = function() {
      console.log("[OQGrid] Handsontable loaded");
      var cbs = _callbacks.slice();
      _callbacks = [];
      for (var i = 0; i < cbs.length; i++) cbs[i]();
    };
    script.onerror = function() {
      console.error("[OQGrid] Failed to load Handsontable from CDN");
    };
    document.head.appendChild(script);
  };
})();
