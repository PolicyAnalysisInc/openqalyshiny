/* OQGrid — Grid Factory (spec registration + Shiny lifecycle) */
(function() {
  "use strict";

  var OQGrid = window.OQGrid = window.OQGrid || {};

  // Registry of spec factories
  OQGrid._specs = OQGrid._specs || {};

  // Active controller instances keyed by "specName_inputId"
  OQGrid._active = OQGrid._active || {};

  // =========================================================================
  // Register a spec factory
  // =========================================================================
  OQGrid.registerSpec = function(name, specFactory) {
    OQGrid._specs[name] = specFactory;
  };

  // =========================================================================
  // Initialize all grids matching a spec's container selector
  // =========================================================================
  OQGrid.initSpec = function(name) {
    var specFactory = OQGrid._specs[name];
    if (!specFactory) return;

    // Create a fresh spec instance
    var spec = specFactory();

    var containers = document.querySelectorAll(
      spec.containerSelector + ":not([data-initialized])"
    );
    if (containers.length === 0) return;

    containers.forEach(function(containerDiv) {
      var inputId = containerDiv.dataset.inputId;
      var key = name + "_" + inputId;

      // Destroy previous instance
      if (OQGrid._active[key]) {
        try {
          OQGrid.keyboard.destroy(OQGrid._active[key]);
          if (OQGrid._active[key].table) {
            OQGrid._active[key].table.destroy();
          }
        } catch (e) {}
        delete OQGrid._active[key];
      }

      var controller = new OQGrid.Controller(containerDiv, spec);
      OQGrid._active[key] = controller;
    });
  };

  // =========================================================================
  // Initialize ALL registered specs
  // =========================================================================
  OQGrid.initAll = function() {
    var names = Object.keys(OQGrid._specs);
    for (var i = 0; i < names.length; i++) {
      OQGrid.initSpec(names[i]);
    }
  };

  // =========================================================================
  // Shiny lifecycle wiring
  // =========================================================================
  if (typeof Shiny !== "undefined") {
    $(document).on("shiny:connected", function() {
      OQGrid.ensureTabulator(function() {
        setTimeout(OQGrid.initAll, 100);
      });
    });

    $(document).on("shiny:value", function() {
      OQGrid.ensureTabulator(function() {
        setTimeout(OQGrid.initAll, 100);
      });
    });

    OQGrid.ensureTabulator(function() {
      setTimeout(OQGrid.initAll, 100);
    });
  }
})();
