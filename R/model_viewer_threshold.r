# =============================================================================
# Threshold Analysis Result Modules
# =============================================================================

#' Threshold Summary UI
#' @param id Module namespace ID.
#' @export
thresholdSummaryUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("summary_table")),
    shiny::uiOutput(ns("error_display"))
  )
}

#' Threshold Summary Server
#' @param id Module namespace ID.
#' @param threshold_results Reactive containing threshold results.
#' @export
thresholdSummaryServer <- function(id, threshold_results) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    error_msg <- shiny::reactiveVal(NULL)

    output$summary_table <- shiny::renderUI({
      res <- threshold_results()
      shiny::req(res)
      error_msg(NULL)
      tryCatch({
        tv <- res$threshold_values
        ft <- flextable::flextable(tv)
        ft <- flextable::autofit(ft)
        ft <- flextable::set_header_labels(ft,
          name = "Analysis",
          variable = "Variable",
          value = "Threshold Value",
          converged = "Converged"
        )
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

#' Threshold Result Tab UI
#' @param id Module namespace ID.
#' @export
thresholdResultTabUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("controls")),
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

#' Threshold Result Tab Server
#' @param id Module namespace ID.
#' @param tab_type Fixed string: "detail" or "convergence".
#' @param threshold_results Reactive containing threshold results.
#' @export
thresholdResultTabServer <- function(id, tab_type, threshold_results) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    error_msg <- shiny::reactiveVal(NULL)

    output$controls <- shiny::renderUI({
      res <- threshold_results()
      if (is.null(res)) return(NULL)

      analysis_names <- unique(res$threshold_values$name)
      analysis_choices <- c("All" = "__all__", setNames(analysis_names, analysis_names))

      inputs <- list(
        shiny::selectInput(ns("viz_type"), "Output Format",
          choices = c("Plot" = "plot", "Table" = "table")
        ),
        shiny::selectInput(ns("analysis"), "Analysis",
          choices = analysis_choices,
          selected = "__all__"
        )
      )

      do.call(bslib::layout_columns, c(
        list(col_widths = bslib::breakpoints(sm = 12, md = 6)),
        inputs
      ))
    })

    selected_analysis <- shiny::reactive({
      sel <- input$analysis
      if (is.null(sel) || sel == "__all__") NULL else sel
    })

    output$result_plot <- shiny::renderPlot({
      res <- threshold_results()
      shiny::req(res, input$viz_type, input$viz_type != "table")
      error_msg(NULL)
      tryCatch({
        args <- list(res)
        if (!is.null(selected_analysis())) {
          args$analyses <- selected_analysis()
        }
        if (tab_type == "detail") {
          do.call(openqaly::threshold_plot, args)
        } else {
          do.call(openqaly::threshold_convergence_plot, args)
        }
      }, error = function(e) {
        error_msg(conditionMessage(e))
        NULL
      })
    })

    output$result_table <- shiny::renderUI({
      res <- threshold_results()
      shiny::req(res, input$viz_type == "table")
      error_msg(NULL)
      tryCatch({
        args <- list(res)
        if (!is.null(selected_analysis())) {
          args$analyses <- selected_analysis()
        }
        ft <- if (tab_type == "detail") {
          do.call(openqaly::threshold_table, args)
        } else {
          do.call(openqaly::threshold_convergence_table, args)
        }
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
