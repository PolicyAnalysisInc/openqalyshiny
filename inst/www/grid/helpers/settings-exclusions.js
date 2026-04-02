/* OQGrid — Analysis Param Helpers: Settings Mutual Exclusions */
(function() {
  "use strict";

  var OQGrid = window.OQGrid = window.OQGrid || {};
  OQGrid.helpers = OQGrid.helpers || {};
  OQGrid.helpers.exclusions = {};

  // =========================================================================
  // Mutual exclusion map for settings.
  // When one setting is used, its exclusions become unavailable.
  // e.g. selecting "discount_rate" excludes "discount_cost" and "discount_outcomes".
  // =========================================================================
  OQGrid.helpers.exclusions.SETTING_EXCLUSIONS = {
    "discount_rate": ["discount_cost", "discount_outcomes"],
    "discount_cost": ["discount_rate"],
    "discount_outcomes": ["discount_rate"]
  };

  // =========================================================================
  // Expand a Set of used setting names to include their mutual exclusions.
  // Mutates the set in-place.
  // =========================================================================
  OQGrid.helpers.exclusions.expandWithExclusions = function(usedNames) {
    var EXCL = OQGrid.helpers.exclusions.SETTING_EXCLUSIONS;
    usedNames.forEach(function(used) {
      var excl = EXCL[used];
      if (excl) excl.forEach(function(e) { usedNames.add(e); });
    });
  };
})();
