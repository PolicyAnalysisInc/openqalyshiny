# =============================================================================
# Model Diff Sub-Module
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

#' Diff Results UI
#' @param id Module namespace ID.
#' @keywords internal
diffResultsUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::selectInput(ns("diff_mode"), "Diff Mode",
      choices = c("Side by Side" = "sidebyside", "Unified" = "unified"),
      selected = "sidebyside"
    ),
    shiny::uiOutput(ns("diff_output"))
  )
}

#' Diff Results Server
#' @param id Module namespace ID.
#' @param model Reactive containing the original model.
#' @param current_model Reactive containing the current model with all UI edits applied.
#' @keywords internal
diffResultsServer <- function(id, model, current_model) {
  shiny::moduleServer(id, function(input, output, session) {

    output$diff_output <- shiny::renderUI({
      m <- model()
      cm <- current_model()
      shiny::req(m, cm)

      tryCatch({
        original_lines <- model_to_yaml_lines(m)
        current_lines <- model_to_yaml_lines(cm)

        if (identical(original_lines, current_lines)) {
          return(shiny::tags$div(
            class = "text-muted p-3",
            "No changes from original model."
          ))
        }

        mode <- input$diff_mode %||% "sidebyside"

        diff_result <- diffobj::diffChr(
          original_lines,
          current_lines,
          format = "html",
          context = 7L,
          style = list(html.output = "diff.w.style"),
          mode = mode,
          tar.banner = "Original Model",
          cur.banner = "Current"
        )

        shiny::tags$div(
          style = "max-height: 80vh; overflow: auto;",
          shiny::HTML(as.character(diff_result))
        )
      }, error = function(e) {
        shiny::tags$div(
          class = "alert alert-danger",
          paste("Failed to generate diff:", conditionMessage(e))
        )
      })
    })
  })
}
