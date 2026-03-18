# =============================================================================
# Variable Diagnostics Sub-Module
# =============================================================================

#' Variable Diagnostics UI
#' @param id Module namespace ID.
#' @keywords internal
variableDiagnosticsUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("controls")),
    shiny::uiOutput(ns("output_selector")),
    shiny::uiOutput(ns("header_info")),
    shiny::uiOutput(ns("selected_text")),
    shiny::uiOutput(ns("selected_table")),
    shiny::plotOutput(ns("selected_plot"), height = "800px"),
    shiny::uiOutput(ns("error_display"))
  )
}

#' Variable Diagnostics Server
#' @param id Module namespace ID.
#' @param results Reactive containing model results.
#' @param metadata Reactive containing model metadata.
#' @keywords internal
variableDiagnosticsServer <- function(id, results, metadata) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    error_msg <- shiny::reactiveVal(NULL)
    diag_result <- shiny::reactiveVal(NULL)
    all_outputs <- shiny::reactiveVal(list())

    shiny::observeEvent(input$variable_name, {
      res <- results()
      shiny::req(res, input$variable_name)

      error_msg(NULL)
      diag_result(NULL)
      all_outputs(list())

      tryCatch({
        diag <- openqaly::diagnose_variable(res, input$variable_name)
        diag_result(diag)

        outputs <- list()
        if (!is.null(diag$text) && nzchar(diag$text)) {
          outputs[["Text"]] <- list(type = "text", content = diag$text)
        }
        if (!is.null(diag$table)) {
          outputs[["Table"]] <- list(type = "table", content = diag$table)
        }
        if (!is.null(diag$plot)) {
          outputs[["Plot"]] <- list(type = "plot", content = diag$plot)
        }
        all_outputs(outputs)
      }, error = function(e) {
        error_msg(conditionMessage(e))
      })
    })

    output$controls <- shiny::renderUI({
      meta <- metadata()
      if (is.null(meta)) return(NULL)

      vars <- meta$variables
      if (is.null(vars) || nrow(vars) == 0) {
        return(shiny::tags$div(
          class = "text-muted p-3",
          "No variables found in this model."
        ))
      }

      var_choices <- unique(vars$name)
      shiny::selectInput(ns("variable_name"), "Variable",
        choices = var_choices,
        selected = var_choices[1]
      )
    })

    output$output_selector <- shiny::renderUI({
      output_choices <- names(all_outputs())
      if (length(output_choices) > 1) {
        shiny::selectInput(ns("output_name"), "Output",
          choices = output_choices,
          selected = output_choices[1]
        )
      }
    })

    selected_output <- shiny::reactive({
      outputs <- all_outputs()
      if (length(outputs) == 0) return(NULL)

      if (length(outputs) == 1) {
        return(outputs[[1]])
      }

      shiny::req(input$output_name)
      outputs[[input$output_name]]
    })

    output$header_info <- shiny::renderUI({
      diag <- diag_result()
      if (is.null(diag)) return(NULL)

      info_items <- list()
      if (!is.null(diag$display_name) && nzchar(diag$display_name)) {
        info_items <- c(info_items, list(
          shiny::tags$strong("Display Name: "),
          diag$display_name, shiny::tags$br()
        ))
      }
      if (!is.null(diag$type) && nzchar(diag$type)) {
        info_items <- c(info_items, list(
          shiny::tags$strong("Type: "),
          diag$type, shiny::tags$br()
        ))
      }
      if (!is.null(diag$formula_text) && nzchar(diag$formula_text)) {
        info_items <- c(info_items, list(
          shiny::tags$strong("Formula: "),
          shiny::tags$code(diag$formula_text), shiny::tags$br()
        ))
      }

      if (length(info_items) == 0) return(NULL)
      shiny::tags$div(class = "mb-3 p-2 bg-light rounded", info_items)
    })

    output$selected_text <- shiny::renderUI({
      sel <- selected_output()
      if (is.null(sel) || sel$type != "text") return(NULL)
      shiny::tags$pre(style = "white-space: pre-wrap;", sel$content)
    })

    output$selected_table <- shiny::renderUI({
      sel <- selected_output()
      if (is.null(sel) || sel$type != "table") return(NULL)
      render_flextable_html(sel$content)
    })

    output$selected_plot <- shiny::renderPlot({
      sel <- selected_output()
      shiny::req(sel, sel$type == "plot")
      sel$content
    })

    output$error_display <- shiny::renderUI({
      msg <- error_msg()
      if (is.null(msg)) return(NULL)
      shiny::tags$div(
        class = "alert alert-danger mt-2",
        shiny::tags$strong("Error: "), msg
      )
    })
  })
}
