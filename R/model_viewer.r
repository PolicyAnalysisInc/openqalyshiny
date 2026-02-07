#' Model Viewer UI
#'
#' Creates a Shiny module UI for viewing and interacting with openqaly models.
#' The left panel displays model overrides and the right panel shows results
#' across multiple categories (Trace, Outcomes, NMB, Pairwise CE, Incremental CE).
#'
#' @param id The module namespace ID.
#'
#' @return A Shiny UI element.
#'
#' @export
modelViewerUI <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_columns(
    col_widths = c(4, 8),
    shiny::uiOutput(ns("override_panel")),
    shiny::tagList(
      shiny::conditionalPanel(
        condition = sprintf("!output['%s']", ns("has_results")),
        shiny::tags$div(
          class = "text-muted p-3",
          "No model loaded."
        )
      ),
      shiny::conditionalPanel(
        condition = sprintf("output['%s']", ns("has_results")),
        bslib::navset_card_tab(
          bslib::nav_panel("Trace", traceResultsUI(ns("trace"))),
          bslib::nav_panel("Outcomes", outcomesResultsUI(ns("outcomes"))),
          bslib::nav_panel("NMB", nmbResultsUI(ns("nmb"))),
          bslib::nav_panel("Pairwise CE",
            pairwiseCeResultsUI(ns("pairwise_ce"))),
          bslib::nav_panel("Incremental CE",
            incrementalCeResultsUI(ns("incremental_ce")))
        )
      )
    )
  )
}

#' Model Viewer Server
#'
#' Server logic for the model viewer module. Manages model loading, override
#' collection, debounced model execution, and result display.
#'
#' @param id The module namespace ID.
#' @param model A reactive expression returning an openqaly model object,
#'   or \code{reactive(NULL)} if the model will be loaded via the file browser.
#'
#' @return Invisible NULL. Called for side effects.
#'
#' @export
modelViewerServer <- function(id, model = shiny::reactive(NULL)) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rv <- shiny::reactiveValues(
      model = NULL,
      results = NULL,
      metadata = NULL
    )

    # Load model when reactive changes and run immediately
    shiny::observeEvent(model(), {
      rv$model <- model()
      rv$results <- NULL
      rv$metadata <- NULL

      # Run model immediately so results are available before any override change
      tryCatch({
        res <- openqaly::run_model(rv$model)
        rv$results <- res
        rv$metadata <- res$metadata
      }, error = function(e) {
        shiny::showNotification(
          paste("Initial model run failed:", conditionMessage(e)),
          type = "error",
          duration = 10
        )
      })
    })

    # Render override panel
    output$override_panel <- shiny::renderUI({
      m <- rv$model
      if (is.null(m)) {
        return(shiny::tags$div(
          class = "text-muted p-3",
          "No model loaded."
        ))
      }
      if (is.null(m$override_categories) || length(m$override_categories) == 0) {
        return(shiny::tags$div(
          class = "text-muted p-3",
          "This model has no overrides."
        ))
      }
      overrideInput(ns("overrides"), m)
    })

    # Collect all override values into a reactive list
    override_values <- shiny::reactive({
      m <- rv$model
      if (is.null(m) || is.null(m$override_categories)) return(NULL)

      values <- list()
      for (cat in m$override_categories) {
        for (override in cat$overrides) {
          input_id <- paste0("overrides_", gsub("[^a-zA-Z0-9_]", "_", override$name))
          val <- input[[input_id]]
          if (!is.null(val)) {
            values[[override$name]] <- val
          }
        }
      }
      values
    })

    # Debounce override values (1000ms delay)
    override_values_debounced <- shiny::debounce(override_values, 1000)

    # Run model when debounced override values change
    shiny::observeEvent(override_values_debounced(), {
      m <- rv$model
      vals <- override_values_debounced()
      if (is.null(m) || is.null(vals)) return()

      tryCatch({
        # Deep copy the model
        updated_model <- m

        # Update overridden_expression for each override
        for (i in seq_along(updated_model$override_categories)) {
          cat <- updated_model$override_categories[[i]]
          for (j in seq_along(cat$overrides)) {
            override <- cat$overrides[[j]]
            if (override$name %in% names(vals)) {
              updated_model$override_categories[[i]]$overrides[[j]]$overridden_expression <-
                as.character(vals[[override$name]])
            }
          }
        }

        # Run the model
        res <- openqaly::run_model(updated_model)
        rv$results <- res
      }, error = function(e) {
        rv$results <- NULL
        shiny::showNotification(
          paste("Model run failed:", conditionMessage(e)),
          type = "error",
          duration = 10
        )
      })

    }, ignoreNULL = TRUE)

    # Flag for conditional panel visibility
    output$has_results <- shiny::reactive({
      !is.null(rv$results)
    })
    shiny::outputOptions(output, "has_results", suspendWhenHidden = FALSE)

    # Result sub-module servers
    results_reactive <- shiny::reactive(rv$results)
    metadata_reactive <- shiny::reactive(rv$metadata)

    traceResultsServer("trace", results_reactive, metadata_reactive)
    outcomesResultsServer("outcomes", results_reactive, metadata_reactive)
    nmbResultsServer("nmb", results_reactive, metadata_reactive)
    pairwiseCeResultsServer("pairwise_ce", results_reactive, metadata_reactive)
    incrementalCeResultsServer("incremental_ce", results_reactive, metadata_reactive)
  })
}
