# Internal environment for registered model editor extensions.
# Extensions are registered by external packages via register_editor_extension().
.editor_extensions <- new.env(parent = emptyenv())

# tab_ids that collide with built-in editor pages and must not be reused.
.RESERVED_TAB_IDS <- c(
  "documentation", "editor", "results",
  "vbp", "dsa", "scenario", "twsa", "threshold", "psa"
)

#' Register an extension for the model editor
#'
#' Called from external packages in their .onLoad() to add either a new
#' top-level page or a floating overlay (e.g. a support-chat-style widget)
#' to the model editor.
#'
#' @param tab_id Unique string id (lowercase with underscores). For page
#'   extensions this is used as the bslib nav_panel value, the dropdown
#'   data-page attribute, and the `page_<tab_id>` DOM id. For overlay
#'   extensions it is used only as the `overlay_<tab_id>` DOM id and as
#'   the documented `<tab_id>_overlay_open` Shiny input convention.
#'   Must not collide with built-in page ids: documentation, editor,
#'   results, vbp, dsa, scenario, twsa, threshold, psa.
#' @param label Display name. For page extensions, shown in the dropdown
#'   menu. For overlay extensions, used as `aria-label`/`title` on the
#'   overlay container.
#' @param ui_fn function(tab_id) returning Shiny UI. For page extensions
#'   this is wrapped in a `.app-page` div. For overlay extensions it is
#'   wrapped in a `.editor-overlay` div positioned fixed at the viewport
#'   root.
#' @param server_fn function(model, apply_action, input, output, session)
#'   called once during server initialization regardless of extension type.
#' @param type One of "page" (default) or "overlay".
#' @param icon Optional Shiny icon prepended to the dropdown label for
#'   page extensions. Currently ignored for overlays.
#' @export
register_editor_extension <- function(
  tab_id,
  label,
  ui_fn,
  server_fn,
  type = c("page", "overlay"),
  icon = NULL
) {
  if (!is.character(tab_id) || length(tab_id) != 1L || !nzchar(tab_id)) {
    stop("tab_id must be a non-empty string")
  }
  if (tab_id %in% .RESERVED_TAB_IDS) {
    stop(sprintf(
      "tab_id '%s' is reserved by openqalyshiny. Reserved ids: %s",
      tab_id, paste(.RESERVED_TAB_IDS, collapse = ", ")
    ))
  }
  type <- match.arg(type)

  existing <- .editor_extensions[[tab_id]]
  if (!is.null(existing) && !identical(existing$type, type)) {
    stop(sprintf(
      "tab_id '%s' is already registered with type '%s'; cannot re-register as type '%s'",
      tab_id, existing$type, type
    ))
  }

  .editor_extensions[[tab_id]] <- list(
    tab_id    = tab_id,
    label     = label,
    ui_fn     = ui_fn,
    server_fn = server_fn,
    type      = type,
    icon      = icon
  )
  invisible(NULL)
}

# Internal: return all registered extensions as a list.
.get_editor_extensions <- function() as.list(.editor_extensions)

# Internal: return only extensions of the given type.
.get_editor_extensions_by_type <- function(type) {
  Filter(function(x) identical(x$type, type), as.list(.editor_extensions))
}
