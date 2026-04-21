/* OQGrid — Shiny Communication Bridge */
(function() {
  "use strict";

  var OQGrid = window.OQGrid = window.OQGrid || {};
  OQGrid.shiny = OQGrid.shiny || {};

  // Dispatch an event payload to a specific Shiny input
  OQGrid.shiny.dispatch = function(inputId, payload) {
    if (typeof Shiny !== "undefined") {
      Shiny.setInputValue(inputId, payload, { priority: "event" });
    }
  };

  // Dispatch a model_action event (used by analysis param grids for side-effects)
  OQGrid.shiny.dispatchModelAction = function(payload) {
    if (typeof Shiny !== "undefined") {
      Shiny.setInputValue("model_action", payload, { priority: "event" });
    }
  };

  // Register a custom Shiny message handler (used by values-table for server-push)
  OQGrid.shiny.registerMessageHandler = function(handlerName, callback) {
    if (typeof Shiny !== "undefined") {
      Shiny.addCustomMessageHandler(handlerName, callback);
    }
  };
})();
