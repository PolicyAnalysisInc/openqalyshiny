/* OQGrid — Distribution Registry (univariate + multivariate) */
(function() {
  "use strict";

  var OQGrid = window.OQGrid = window.OQGrid || {};
  OQGrid.helpers = OQGrid.helpers || {};
  OQGrid.helpers.distributions = {};

  // =========================================================================
  // Univariate distribution definitions
  // Each entry has either:
  //   { params, labels }  for single parameterization, or
  //   { parameterizations: { name: { params, labels }, ... } }
  // =========================================================================
  OQGrid.helpers.distributions.DISTRIBUTIONS = {
    normal:      { params: ["mean", "sd"], labels: ["Mean", "SD"] },
    lognormal:   { parameterizations: {
      mean_sd:       { params: ["mean", "sd"], labels: ["Mean", "SD"] },
      meanlog_sdlog: { params: ["meanlog", "sdlog"], labels: ["Mean (log)", "SD (log)"] }
    }},
    gamma:       { parameterizations: {
      mean_sd:     { params: ["mean", "sd"], labels: ["Mean", "SD"] },
      shape_scale: { params: ["shape", "scale"], labels: ["Shape", "Scale"] }
    }},
    beta:        { parameterizations: {
      mean_sd:       { params: ["mean", "sd"], labels: ["Mean", "SD"] },
      shape1_shape2: { params: ["shape1", "shape2"], labels: ["Shape 1", "Shape 2"] }
    }},
    uniform:     { params: ["min", "max"], labels: ["Min", "Max"] },
    triangular:  { params: ["min", "mode", "max"], labels: ["Min", "Mode", "Max"] },
    bootstrap:   { params: ["x"], labels: ["Data"] }
  };

  // =========================================================================
  // Multivariate distribution definitions
  // Each entry may have vectorParams, scalarParams, matrixParams
  // and corresponding label arrays.
  // =========================================================================
  OQGrid.helpers.distributions.MV_DISTRIBUTIONS = {
    dirichlet:   { scalarParams: ["n"], scalarLabels: ["Effective Sample Size (n)"] },
    mvnormal:    { scalarParams: ["covariance"], scalarLabels: ["Covariance"] },
    multinomial: {}
  };
})();
