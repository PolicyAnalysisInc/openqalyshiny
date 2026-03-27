# =============================================================================
# Model Diff Utilities
# =============================================================================

#' Convert Model to YAML Lines
#'
#' Serializes an openqaly model to YAML using \code{openqaly::write_model_yaml}
#' and returns the lines as a character vector.
#'
#' @param model An openqaly model object.
#' @return Character vector of YAML lines.
#' @keywords internal
model_to_yaml_lines <- function(model) {
  tmp <- tempfile(fileext = ".yaml")
  on.exit(unlink(tmp), add = TRUE)
  openqaly::write_model_yaml(model, tmp)
  readLines(tmp)
}
