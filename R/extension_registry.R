# Internal environment for registered model editor extensions.
# Extensions are registered by external packages via register_editor_extension().
.editor_extensions <- new.env(parent = emptyenv())

#' Register a tab extension for the model editor
#'
#' Called from external packages in their .onLoad() to add a new tab to the
#' model editor. Extensions are identified by tab_id and must not conflict
#' with existing tab values ("Tables", "Scripts", "Variables", etc.).
#'
#' @param tab_id Unique string id for the tab (lowercase with underscores).
#'   This string is used as the bslib nav_panel value and is readable via
#'   input$model_inputs_tabs in the extension's server function.
#' @param label Display name shown in the tab.
#' @param ui_fn function(tab_id) returning Shiny UI for the tab content.
#' @param server_fn function(model, apply_action, input, output, session)
#'   called during server initialization. model is the reactiveVal;
#'   apply_action dispatches model actions; input/output/session are the
#'   outer server objects.
#' @export
register_editor_extension <- function(tab_id, label, ui_fn, server_fn) {
  if (!is.character(tab_id) || length(tab_id) != 1L || !nzchar(tab_id)) {
    stop("tab_id must be a non-empty string")
  }
  .editor_extensions[[tab_id]] <- list(
    tab_id    = tab_id,
    label     = label,
    ui_fn     = ui_fn,
    server_fn = server_fn
  )
  invisible(NULL)
}

# Internal: return all registered extensions as a list.
.get_editor_extensions <- function() as.list(.editor_extensions)
