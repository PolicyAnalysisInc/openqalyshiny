# =============================================================================
# VBP Results Sub-Module
# =============================================================================

#' VBP Results UI
#' @param id Module namespace ID.
#' @keywords internal
vbpResultsSidebarUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::selectInput(ns("viz_type"), "Visualization",
      choices = c("Line Chart" = "line", "Table" = "table")
    ),
    shiny::uiOutput(ns("controls"))
  )
}

#' VBP Results UI
#' @param id Module namespace ID.
#' @keywords internal
vbpResultsUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::conditionalPanel(
      condition = sprintf("input['%s'] != 'table'", ns("viz_type")),
      shiny::plotOutput(ns("result_plot"))
    ),
    shiny::conditionalPanel(
      condition = sprintf("input['%s'] == 'table'", ns("viz_type")),
      shiny::uiOutput(ns("result_table"))
    ),
    shiny::uiOutput(ns("error_display"))
  )
}

#' VBP Results Server
#' @param id Module namespace ID.
#' @param vbp_results Reactive containing VBP results.
#' @param metadata Reactive containing model metadata.
#' @keywords internal
vbpResultsServer <- function(id, vbp_results, metadata) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    error_msg <- shiny::reactiveVal(NULL)

    prev_groups <- shiny::reactiveVal(NULL)

    shiny::observeEvent(input$groups, {
      new_val <- input$groups
      corrected <- enforce_exclusive_groups(new_val, prev_groups())
      prev_groups(corrected)
      if (!identical(corrected, new_val)) {
        shiny::freezeReactiveValue(input, "groups")
        shiny::updateSelectInput(
          session, "groups", selected = corrected
        )
      }
    }, ignoreNULL = FALSE, ignoreInit = TRUE)

    output$controls <- shiny::renderUI({
      meta <- metadata()
      res <- vbp_results()
      if (is.null(meta) || is.null(res)) return(NULL)
      viz <- input$viz_type
      if (is.null(viz)) return(NULL)
      groups <- get_group_choices(meta)

      # Build VBP-specific comparator choices:
      # Exclude the intervention strategy, add aggregate options
      all_strategies <- get_strategy_choices(meta)
      intervention <- res$spec$intervention_strategy
      individual_comparators <- all_strategies[all_strategies != intervention]

      comparator_choices <- c(
        "Overall (Aggregate)" = "overall",
        "All (Individual + Aggregate)" = "all",
        "All (Individual Only)" = "all_comparators",
        individual_comparators
      )

      comparators_input <- shiny::selectInput(
        ns("comparators"), "Comparators",
        choices = comparator_choices,
        selected = "overall",
        multiple = FALSE
      )

      groups_input <- if (length(groups) > 1) {
        shiny::selectInput(
          ns("groups"), "Groups",
          choices = groups, selected = "overall", multiple = TRUE
        )
      }

      inputs <- list(
        comparators_input,
        groups_input
      )

      if (viz != "table") {
        inputs <- c(inputs, list(
          shiny::numericInput(ns("wtp_min"), "WTP Min", value = 0, min = 0, step = 10000),
          shiny::numericInput(ns("wtp_max"), "WTP Max", value = 200000, min = 0, step = 10000),
          shiny::numericInput(ns("wtp_step"), "WTP Step", value = 1000, min = 100, step = 500),
          shiny::checkboxInput(ns("show_default_wtp"), "Show Default WTP", value = TRUE),
          shiny::checkboxInput(ns("show_vbp_at_wtp"), "Show VBP at WTP", value = TRUE)
        ))
      } else {
        inputs <- c(inputs, list(
          shiny::textInput(ns("wtp_thresholds"), "WTP Thresholds (comma-separated)",
            value = "50000, 100000, 150000")
        ))
      }

      inputs <- Filter(Negate(is.null), inputs)

      build_results_sidebar_controls(inputs)
    })

    output$result_plot <- shiny::renderPlot({
      res <- vbp_results()
      shiny::req(res, input$viz_type, input$viz_type != "table")
      args <- list(res)
      if (!is.null(input$comparators)) args$comparators <- input$comparators
      if (!is.null(input$groups)) args$groups <- input$groups
      if (!is.null(input$wtp_min) && !is.null(input$wtp_max)) {
        args$wtp_range <- c(input$wtp_min, input$wtp_max)
      }
      if (!is.null(input$wtp_step)) args$wtp_step <- input$wtp_step
      if (!is.null(input$show_default_wtp)) args$show_default_wtp <- input$show_default_wtp
      if (!is.null(input$show_vbp_at_wtp)) args$show_vbp_at_wtp <- input$show_vbp_at_wtp
      error_msg(NULL)
      tryCatch({
        do.call(openqaly::vbp_plot, args)
      }, error = function(e) {
        error_msg(conditionMessage(e))
        NULL
      })
    })

    output$result_table <- shiny::renderUI({
      res <- vbp_results()
      shiny::req(res, input$viz_type == "table")
      args <- list(res)
      if (!is.null(input$comparators)) args$comparators <- input$comparators
      if (!is.null(input$groups)) args$groups <- input$groups
      if (!is.null(input$wtp_thresholds) && nchar(trimws(input$wtp_thresholds)) > 0) {
        thresholds <- as.numeric(trimws(strsplit(input$wtp_thresholds, ",")[[1]]))
        thresholds <- thresholds[!is.na(thresholds)]
        if (length(thresholds) > 0) args$wtp_thresholds <- thresholds
      }
      error_msg(NULL)
      tryCatch({
        ft <- do.call(openqaly::vbp_table, args)
        render_flextable_html(ft)
      }, error = function(e) {
        error_msg(conditionMessage(e))
        NULL
      })
    })

    output$error_display <- shiny::renderUI({
      msg <- error_msg()
      if (!is.null(msg)) {
        shiny::tags$div(class = "alert alert-danger", msg)
      }
    })
  })
}
