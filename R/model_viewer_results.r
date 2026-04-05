# =============================================================================
# Trace Results Sub-Module
# =============================================================================

#' Trace Results UI
#' @param id Module namespace ID.
#' @keywords internal
traceResultsSidebarUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::selectInput(ns("viz_type"), "Visualization",
      choices = c("Area Chart" = "area", "Line Chart" = "line", "Table" = "table")
    ),
    shiny::uiOutput(ns("controls"))
  )
}

#' Trace Results UI
#' @param id Module namespace ID.
#' @keywords internal
traceResultsUI <- function(id) {
  ns <- shiny::NS(id)
  results_fill_panel(
    shiny::conditionalPanel(
      condition = sprintf("input['%s'] != 'table'", ns("viz_type")),
      results_fill_plot_output(ns("result_plot"))
    ),
    shiny::conditionalPanel(
      condition = sprintf("input['%s'] == 'table'", ns("viz_type")),
      shiny::uiOutput(ns("result_table"))
    ),
    shiny::uiOutput(ns("error_display"))
  )
}

#' Trace Results Server
#' @param id Module namespace ID.
#' @param results Reactive containing model results.
#' @param metadata Reactive containing model metadata.
#' @keywords internal
traceResultsServer <- function(id, results, metadata) {
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
      if (is.null(meta)) return(NULL)
      viz <- input$viz_type
      if (is.null(viz)) return(NULL)
      groups <- get_group_choices(meta)
      strategies <- get_strategy_choices(meta)

      extra <- switch(viz,
        "line" = shiny::checkboxInput(ns("by_state"), "By State", value = FALSE),
        NULL
      )

      build_results_sidebar_controls(list(
        if (length(groups) > 1) shiny::selectInput(ns("groups"), "Groups", choices = groups, selected = "overall", multiple = TRUE),
        if (length(strategies) > 1) shiny::selectInput(ns("strategies"), "Strategies", choices = strategies, selected = strategies, multiple = TRUE),
        shiny::selectInput(ns("time_unit"), "Time Unit",
          choices = c("cycle", "day", "week", "month", "year"),
          selected = "cycle"
        ),
        extra
      ))
    })

    # Build args list from current inputs
    build_args <- function(res) {
      args <- list(res)
      if (!is.null(input$groups)) args$groups <- input$groups
      if (!is.null(input$strategies)) args$strategies <- input$strategies
      if (!is.null(input$time_unit)) args$time_unit <- input$time_unit
      args
    }

    output$result_plot <- shiny::renderPlot({
      res <- results()
      shiny::req(res, input$viz_type, input$viz_type != "table")
      args <- build_args(res)
      error_msg(NULL)
      tryCatch({
        if (input$viz_type == "area") {
          do.call(openqaly::trace_plot_area, args)
        } else {
          if (!is.null(input$by_state)) args$by_state <- input$by_state
          do.call(openqaly::trace_plot_line, args)
        }
      }, error = function(e) {
        error_msg(conditionMessage(e))
        NULL
      })
    })

    output$result_table <- shiny::renderUI({
      res <- results()
      shiny::req(res, input$viz_type == "table")
      args <- build_args(res)
      error_msg(NULL)
      tryCatch({
        ft <- do.call(openqaly::trace_table, args)
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

# =============================================================================
# Outcomes Results Sub-Module
# =============================================================================

#' Outcomes Results UI
#' @param id Module namespace ID.
#' @keywords internal
outcomesResultsSidebarUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::selectInput(ns("viz_type"), "Visualization",
      choices = c("Bar Chart" = "bar", "Line Chart" = "line", "Table" = "table")
    ),
    shiny::selectInput(ns("analysis_type"), "Type",
      choices = c("Absolute" = "absolute", "Differences" = "differences")
    ),
    shiny::uiOutput(ns("controls"))
  )
}

#' Outcomes Results UI
#' @param id Module namespace ID.
#' @keywords internal
outcomesResultsUI <- function(id) {
  ns <- shiny::NS(id)
  results_fill_panel(
    shiny::conditionalPanel(
      condition = sprintf("input['%s'] != 'table'", ns("viz_type")),
      results_fill_plot_output(ns("result_plot"))
    ),
    shiny::conditionalPanel(
      condition = sprintf("input['%s'] == 'table'", ns("viz_type")),
      shiny::uiOutput(ns("result_table"))
    ),
    shiny::uiOutput(ns("error_display"))
  )
}

#' Outcomes Results Server
#' @param id Module namespace ID.
#' @param results Reactive containing model results.
#' @param metadata Reactive containing model metadata.
#' @keywords internal
outcomesResultsServer <- function(id, results, metadata) {
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

    prev_interventions <- shiny::reactiveVal(NULL)
    prev_comparators <- shiny::reactiveVal(NULL)

    shiny::observeEvent(input$interventions, {
      if (is.null(prev_interventions()) ||
          is.null(prev_comparators())) {
        prev_interventions(input$interventions)
        return()
      }
      result <- enforce_exclusive_strategies(
        input$interventions,
        prev_comparators(),
        prev_interventions()
      )
      prev_interventions(result$changed)
      prev_comparators(result$other)
      if (!identical(input$interventions, result$changed)) {
        shiny::freezeReactiveValue(input, "interventions")
        shiny::updateSelectInput(
          session, "interventions",
          selected = result$changed
        )
      }
      if (!identical(input$comparators, result$other)) {
        shiny::freezeReactiveValue(input, "comparators")
        shiny::updateSelectInput(
          session, "comparators",
          selected = result$other
        )
      }
    }, ignoreNULL = FALSE, ignoreInit = TRUE)

    shiny::observeEvent(input$comparators, {
      if (is.null(prev_comparators()) ||
          is.null(prev_interventions())) {
        prev_comparators(input$comparators)
        return()
      }
      result <- enforce_exclusive_strategies(
        input$comparators,
        prev_interventions(),
        prev_comparators()
      )
      prev_comparators(result$changed)
      prev_interventions(result$other)
      if (!identical(input$comparators, result$changed)) {
        shiny::freezeReactiveValue(input, "comparators")
        shiny::updateSelectInput(
          session, "comparators",
          selected = result$changed
        )
      }
      if (!identical(input$interventions, result$other)) {
        shiny::freezeReactiveValue(input, "interventions")
        shiny::updateSelectInput(
          session, "interventions",
          selected = result$other
        )
      }
    }, ignoreNULL = FALSE, ignoreInit = TRUE)

    shiny::observeEvent(input$analysis_type, {
      prev_interventions(NULL)
      prev_comparators(NULL)
    }, ignoreInit = TRUE)

    output$controls <- shiny::renderUI({
      meta <- metadata()
      if (is.null(meta)) return(NULL)
      atype <- input$analysis_type
      if (is.null(atype)) return(NULL)
      viz <- input$viz_type
      if (is.null(viz)) return(NULL)
      groups <- get_group_choices(meta)
      strategies <- get_strategy_choices(meta)
      outcome_choices <- get_outcome_summary_choices(meta)

      strategy_controls <- if (atype == "absolute") {
        list(
          if (length(strategies) > 1) shiny::selectInput(ns("strategies"), "Strategies", choices = strategies, selected = strategies, multiple = TRUE)
        )
      } else {
        list(
          if (length(strategies) > 1) shiny::selectInput(ns("interventions"), "Interventions",
            choices = strategies,
            selected = if (length(strategies) > 1) strategies[2] else strategies[1],
            multiple = TRUE
          ),
          if (length(strategies) > 1) shiny::selectInput(ns("comparators"), "Comparators",
            choices = strategies, selected = strategies[-2], multiple = TRUE
          )
        )
      }

      extra <- if (viz == "line") {
        list(
          shiny::selectInput(ns("time_unit"), "Time Unit",
            choices = c("cycle", "day", "week", "month", "year"),
            selected = "cycle"
          ),
          shiny::checkboxInput(ns("cumulative"), "Cumulative", value = TRUE),
          shiny::checkboxInput(ns("discounted"), "Discounted", value = TRUE)
        )
      } else {
        list()
      }

      build_results_sidebar_controls(c(
        list(
        shiny::selectInput(ns("outcome"), "Outcome Summary",
          choices = outcome_choices,
          selected = if (length(outcome_choices) > 0) outcome_choices[1] else NULL
        ),
        if (length(groups) > 1) shiny::selectInput(ns("groups"), "Groups", choices = groups, selected = "overall", multiple = TRUE)
        ),
        strategy_controls,
        extra
      ))
    })

    build_args <- function(res) {
      args <- list(res, outcome = input$outcome)
      if (!is.null(input$groups)) args$groups <- input$groups
      if (input$analysis_type == "absolute") {
        if (!is.null(input$strategies))
          args$strategies <- input$strategies
      } else {
        if (!is.null(input$comparators))
          args$comparators <- input$comparators
        if (!is.null(input$interventions))
          args$interventions <- input$interventions
      }
      args
    }

    output$result_plot <- shiny::renderPlot({
      res <- results()
      shiny::req(res, input$viz_type, input$outcome, input$analysis_type, input$viz_type != "table")
      if (input$analysis_type == "differences") {
        shiny::req(input$comparators, input$interventions)
      }
      args <- build_args(res)
      error_msg(NULL)
      tryCatch({
        if (input$viz_type == "bar") {
          do.call(openqaly::outcomes_plot_bar, args)
        } else {
          if (!is.null(input$time_unit)) args$time_unit <- input$time_unit
          if (!is.null(input$cumulative)) args$cumulative <- input$cumulative
          if (!is.null(input$discounted)) args$discounted <- input$discounted
          do.call(openqaly::outcomes_plot_line, args)
        }
      }, error = function(e) {
        error_msg(conditionMessage(e))
        NULL
      })
    })

    output$result_table <- shiny::renderUI({
      res <- results()
      shiny::req(res, input$viz_type == "table", input$outcome, input$analysis_type)
      if (input$analysis_type == "differences") {
        shiny::req(input$comparators, input$interventions)
      }
      args <- build_args(res)
      error_msg(NULL)
      tryCatch({
        ft <- do.call(openqaly::outcomes_table, args)
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

# =============================================================================
# Costs Results Sub-Module
# =============================================================================

#' Costs Results UI
#' @param id Module namespace ID.
#' @keywords internal
costsResultsSidebarUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::selectInput(ns("viz_type"), "Visualization",
      choices = c("Bar Chart" = "bar", "Line Chart" = "line", "Table" = "table")
    ),
    shiny::selectInput(ns("analysis_type"), "Type",
      choices = c("Absolute" = "absolute", "Differences" = "differences")
    ),
    shiny::uiOutput(ns("controls"))
  )
}

#' Costs Results UI
#' @param id Module namespace ID.
#' @keywords internal
costsResultsUI <- function(id) {
  ns <- shiny::NS(id)
  results_fill_panel(
    shiny::conditionalPanel(
      condition = sprintf("input['%s'] != 'table'", ns("viz_type")),
      results_fill_plot_output(ns("result_plot"))
    ),
    shiny::conditionalPanel(
      condition = sprintf("input['%s'] == 'table'", ns("viz_type")),
      shiny::uiOutput(ns("result_table"))
    ),
    shiny::uiOutput(ns("error_display"))
  )
}

#' Costs Results Server
#' @param id Module namespace ID.
#' @param results Reactive containing model results.
#' @param metadata Reactive containing model metadata.
#' @keywords internal
costsResultsServer <- function(id, results, metadata) {
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

    prev_interventions <- shiny::reactiveVal(NULL)
    prev_comparators <- shiny::reactiveVal(NULL)

    shiny::observeEvent(input$interventions, {
      if (is.null(prev_interventions()) ||
          is.null(prev_comparators())) {
        prev_interventions(input$interventions)
        return()
      }
      result <- enforce_exclusive_strategies(
        input$interventions,
        prev_comparators(),
        prev_interventions()
      )
      prev_interventions(result$changed)
      prev_comparators(result$other)
      if (!identical(input$interventions, result$changed)) {
        shiny::freezeReactiveValue(input, "interventions")
        shiny::updateSelectInput(
          session, "interventions",
          selected = result$changed
        )
      }
      if (!identical(input$comparators, result$other)) {
        shiny::freezeReactiveValue(input, "comparators")
        shiny::updateSelectInput(
          session, "comparators",
          selected = result$other
        )
      }
    }, ignoreNULL = FALSE, ignoreInit = TRUE)

    shiny::observeEvent(input$comparators, {
      if (is.null(prev_comparators()) ||
          is.null(prev_interventions())) {
        prev_comparators(input$comparators)
        return()
      }
      result <- enforce_exclusive_strategies(
        input$comparators,
        prev_interventions(),
        prev_comparators()
      )
      prev_comparators(result$changed)
      prev_interventions(result$other)
      if (!identical(input$comparators, result$changed)) {
        shiny::freezeReactiveValue(input, "comparators")
        shiny::updateSelectInput(
          session, "comparators",
          selected = result$changed
        )
      }
      if (!identical(input$interventions, result$other)) {
        shiny::freezeReactiveValue(input, "interventions")
        shiny::updateSelectInput(
          session, "interventions",
          selected = result$other
        )
      }
    }, ignoreNULL = FALSE, ignoreInit = TRUE)

    shiny::observeEvent(input$analysis_type, {
      prev_interventions(NULL)
      prev_comparators(NULL)
    }, ignoreInit = TRUE)

    output$controls <- shiny::renderUI({
      meta <- metadata()
      if (is.null(meta)) return(NULL)
      atype <- input$analysis_type
      if (is.null(atype)) return(NULL)
      viz <- input$viz_type
      if (is.null(viz)) return(NULL)
      groups <- get_group_choices(meta)
      strategies <- get_strategy_choices(meta)
      cost_choices <- get_cost_summary_choices(meta)

      strategy_controls <- if (atype == "absolute") {
        list(
          if (length(strategies) > 1) shiny::selectInput(ns("strategies"), "Strategies", choices = strategies, selected = strategies, multiple = TRUE)
        )
      } else {
        list(
          if (length(strategies) > 1) shiny::selectInput(ns("interventions"), "Interventions",
            choices = strategies,
            selected = if (length(strategies) > 1) strategies[2] else strategies[1],
            multiple = TRUE
          ),
          if (length(strategies) > 1) shiny::selectInput(ns("comparators"), "Comparators",
            choices = strategies, selected = strategies[-2], multiple = TRUE
          )
        )
      }

      extra <- if (viz == "line") {
        list(
          shiny::selectInput(ns("time_unit"), "Time Unit",
            choices = c("cycle", "day", "week", "month", "year"),
            selected = "cycle"
          ),
          shiny::checkboxInput(ns("cumulative"), "Cumulative", value = TRUE),
          shiny::checkboxInput(ns("discounted"), "Discounted", value = TRUE)
        )
      } else {
        list()
      }

      build_results_sidebar_controls(c(
        list(
        shiny::selectInput(ns("outcome"), "Cost Summary",
          choices = cost_choices,
          selected = if (length(cost_choices) > 0) cost_choices[1] else NULL
        ),
        if (length(groups) > 1) shiny::selectInput(ns("groups"), "Groups", choices = groups, selected = "overall", multiple = TRUE)
        ),
        strategy_controls,
        extra
      ))
    })

    build_args <- function(res) {
      args <- list(res, outcome = input$outcome)
      if (!is.null(input$groups)) args$groups <- input$groups
      if (input$analysis_type == "absolute") {
        if (!is.null(input$strategies))
          args$strategies <- input$strategies
      } else {
        if (!is.null(input$comparators))
          args$comparators <- input$comparators
        if (!is.null(input$interventions))
          args$interventions <- input$interventions
      }
      args
    }

    output$result_plot <- shiny::renderPlot({
      res <- results()
      shiny::req(res, input$viz_type, input$outcome, input$analysis_type, input$viz_type != "table")
      if (input$analysis_type == "differences") {
        shiny::req(input$comparators, input$interventions)
      }
      args <- build_args(res)
      error_msg(NULL)
      tryCatch({
        if (input$viz_type == "bar") {
          do.call(openqaly::costs_plot_bar, args)
        } else {
          if (!is.null(input$time_unit)) args$time_unit <- input$time_unit
          if (!is.null(input$cumulative)) args$cumulative <- input$cumulative
          if (!is.null(input$discounted)) args$discounted <- input$discounted
          do.call(openqaly::costs_plot_line, args)
        }
      }, error = function(e) {
        error_msg(conditionMessage(e))
        NULL
      })
    })

    output$result_table <- shiny::renderUI({
      res <- results()
      shiny::req(res, input$viz_type == "table", input$outcome, input$analysis_type)
      if (input$analysis_type == "differences") {
        shiny::req(input$comparators, input$interventions)
      }
      args <- build_args(res)
      error_msg(NULL)
      tryCatch({
        ft <- do.call(openqaly::costs_table, args)
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

# =============================================================================
# NMB Results Sub-Module
# =============================================================================

#' NMB Results UI
#' @param id Module namespace ID.
#' @keywords internal
nmbResultsSidebarUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::selectInput(ns("viz_type"), "Visualization",
      choices = c("Bar Chart" = "bar", "Line Chart" = "line", "Table" = "table")
    ),
    shiny::uiOutput(ns("controls"))
  )
}

#' NMB Results UI
#' @param id Module namespace ID.
#' @keywords internal
nmbResultsUI <- function(id) {
  ns <- shiny::NS(id)
  results_fill_panel(
    shiny::conditionalPanel(
      condition = sprintf("input['%s'] != 'table'", ns("viz_type")),
      results_fill_plot_output(ns("result_plot"))
    ),
    shiny::conditionalPanel(
      condition = sprintf("input['%s'] == 'table'", ns("viz_type")),
      shiny::uiOutput(ns("result_table"))
    ),
    shiny::uiOutput(ns("error_display"))
  )
}

#' NMB Results Server
#' @param id Module namespace ID.
#' @param results Reactive containing model results.
#' @param metadata Reactive containing model metadata.
#' @keywords internal
nmbResultsServer <- function(id, results, metadata) {
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

    prev_interventions <- shiny::reactiveVal(NULL)
    prev_comparators <- shiny::reactiveVal(NULL)

    shiny::observeEvent(input$interventions, {
      if (is.null(prev_interventions()) ||
          is.null(prev_comparators())) {
        prev_interventions(input$interventions)
        return()
      }
      result <- enforce_exclusive_strategies(
        input$interventions,
        prev_comparators(),
        prev_interventions()
      )
      prev_interventions(result$changed)
      prev_comparators(result$other)
      if (!identical(input$interventions, result$changed)) {
        shiny::freezeReactiveValue(input, "interventions")
        shiny::updateSelectInput(
          session, "interventions",
          selected = result$changed
        )
      }
      if (!identical(input$comparators, result$other)) {
        shiny::freezeReactiveValue(input, "comparators")
        shiny::updateSelectInput(
          session, "comparators",
          selected = result$other
        )
      }
    }, ignoreNULL = FALSE, ignoreInit = TRUE)

    shiny::observeEvent(input$comparators, {
      if (is.null(prev_comparators()) ||
          is.null(prev_interventions())) {
        prev_comparators(input$comparators)
        return()
      }
      result <- enforce_exclusive_strategies(
        input$comparators,
        prev_interventions(),
        prev_comparators()
      )
      prev_comparators(result$changed)
      prev_interventions(result$other)
      if (!identical(input$comparators, result$changed)) {
        shiny::freezeReactiveValue(input, "comparators")
        shiny::updateSelectInput(
          session, "comparators",
          selected = result$changed
        )
      }
      if (!identical(input$interventions, result$other)) {
        shiny::freezeReactiveValue(input, "interventions")
        shiny::updateSelectInput(
          session, "interventions",
          selected = result$other
        )
      }
    }, ignoreNULL = FALSE, ignoreInit = TRUE)

    output$controls <- shiny::renderUI({
      meta <- metadata()
      if (is.null(meta)) return(NULL)
      viz <- input$viz_type
      if (is.null(viz)) return(NULL)
      groups <- get_group_choices(meta)
      strategies <- get_strategy_choices(meta)
      outcome_choices <- get_outcome_summary_choices(meta)
      cost_choices <- get_cost_summary_choices(meta)

      extra <- if (viz == "line") {
        list(
          shiny::selectInput(ns("time_unit"), "Time Unit",
            choices = c("cycle", "day", "week", "month", "year"),
            selected = "cycle"
          ),
          shiny::checkboxInput(ns("cumulative"), "Cumulative", value = TRUE)
        )
      } else {
        list()
      }

      build_results_sidebar_controls(c(
        list(
        shiny::selectInput(ns("health_outcome"), "Health Outcome",
          choices = outcome_choices,
          selected = if (length(outcome_choices) > 0) outcome_choices[1] else NULL
        ),
        shiny::selectInput(ns("cost_outcome"), "Cost Outcome",
          choices = cost_choices,
          selected = if (length(cost_choices) > 0) cost_choices[1] else NULL
        ),
        shiny::numericInput(ns("wtp"), "Willingness to Pay", value = 100000, min = 0, step = 10000),
        if (length(groups) > 1) shiny::selectInput(ns("groups"), "Groups", choices = groups, selected = "overall", multiple = TRUE),
        if (length(strategies) > 1) shiny::selectInput(ns("interventions"), "Interventions",
          choices = strategies,
          selected = if (length(strategies) > 1) strategies[2] else strategies[1],
          multiple = TRUE
        ),
        if (length(strategies) > 1) shiny::selectInput(ns("comparators"), "Comparators",
          choices = strategies, selected = strategies[-2], multiple = TRUE
        )
        ),
        extra
      ))
    })

    build_args <- function(res) {
      args <- list(
        res,
        health_outcome = input$health_outcome,
        cost_outcome = input$cost_outcome,
        wtp = if (!is.null(input$wtp)) input$wtp else 100000
      )
      if (!is.null(input$groups)) args$groups <- input$groups
      if (!is.null(input$comparators)) args$comparators <- input$comparators
      if (!is.null(input$interventions)) args$interventions <- input$interventions
      args
    }

    output$result_plot <- shiny::renderPlot({
      res <- results()
      shiny::req(
        res, input$viz_type, input$health_outcome,
        input$cost_outcome, input$viz_type != "table",
        input$comparators, input$interventions
      )
      args <- build_args(res)
      error_msg(NULL)
      tryCatch({
        if (input$viz_type == "bar") {
          do.call(openqaly::nmb_plot_bar, args)
        } else {
          if (!is.null(input$time_unit)) args$time_unit <- input$time_unit
          if (!is.null(input$cumulative)) args$cumulative <- input$cumulative
          do.call(openqaly::nmb_plot_line, args)
        }
      }, error = function(e) {
        error_msg(conditionMessage(e))
        NULL
      })
    })

    output$result_table <- shiny::renderUI({
      res <- results()
      shiny::req(
        res, input$viz_type == "table",
        input$health_outcome, input$cost_outcome,
        input$comparators, input$interventions
      )
      args <- build_args(res)
      error_msg(NULL)
      tryCatch({
        ft <- do.call(openqaly::nmb_table, args)
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

# =============================================================================
# Pairwise CE Results Sub-Module
# =============================================================================

#' Pairwise CE Results UI
#' @param id Module namespace ID.
#' @keywords internal
pairwiseCeResultsSidebarUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::selectInput(ns("viz_type"), "Visualization",
      choices = c("CE Plane Plot" = "plot", "Table" = "table")
    ),
    shiny::uiOutput(ns("controls"))
  )
}

#' Pairwise CE Results UI
#' @param id Module namespace ID.
#' @keywords internal
pairwiseCeResultsUI <- function(id) {
  ns <- shiny::NS(id)
  results_fill_panel(
    shiny::conditionalPanel(
      condition = sprintf("input['%s'] != 'table'", ns("viz_type")),
      results_fill_plot_output(ns("result_plot"))
    ),
    shiny::conditionalPanel(
      condition = sprintf("input['%s'] == 'table'", ns("viz_type")),
      shiny::uiOutput(ns("result_table"))
    ),
    shiny::uiOutput(ns("error_display"))
  )
}

#' Pairwise CE Results Server
#' @param id Module namespace ID.
#' @param results Reactive containing model results.
#' @param metadata Reactive containing model metadata.
#' @keywords internal
pairwiseCeResultsServer <- function(id, results, metadata) {
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

    prev_interventions <- shiny::reactiveVal(NULL)
    prev_comparators <- shiny::reactiveVal(NULL)

    shiny::observeEvent(input$interventions, {
      if (is.null(prev_interventions()) ||
          is.null(prev_comparators())) {
        prev_interventions(input$interventions)
        return()
      }
      result <- enforce_exclusive_strategies(
        input$interventions,
        prev_comparators(),
        prev_interventions()
      )
      prev_interventions(result$changed)
      prev_comparators(result$other)
      if (!identical(input$interventions, result$changed)) {
        shiny::freezeReactiveValue(input, "interventions")
        shiny::updateSelectInput(
          session, "interventions",
          selected = result$changed
        )
      }
      if (!identical(input$comparators, result$other)) {
        shiny::freezeReactiveValue(input, "comparators")
        shiny::updateSelectInput(
          session, "comparators",
          selected = result$other
        )
      }
    }, ignoreNULL = FALSE, ignoreInit = TRUE)

    shiny::observeEvent(input$comparators, {
      if (is.null(prev_comparators()) ||
          is.null(prev_interventions())) {
        prev_comparators(input$comparators)
        return()
      }
      result <- enforce_exclusive_strategies(
        input$comparators,
        prev_interventions(),
        prev_comparators()
      )
      prev_comparators(result$changed)
      prev_interventions(result$other)
      if (!identical(input$comparators, result$changed)) {
        shiny::freezeReactiveValue(input, "comparators")
        shiny::updateSelectInput(
          session, "comparators",
          selected = result$changed
        )
      }
      if (!identical(input$interventions, result$other)) {
        shiny::freezeReactiveValue(input, "interventions")
        shiny::updateSelectInput(
          session, "interventions",
          selected = result$other
        )
      }
    }, ignoreNULL = FALSE, ignoreInit = TRUE)

    output$controls <- shiny::renderUI({
      meta <- metadata()
      if (is.null(meta)) return(NULL)
      groups <- get_group_choices(meta)
      strategies <- get_strategy_choices(meta)
      outcome_choices <- get_outcome_summary_choices(meta)
      cost_choices <- get_cost_summary_choices(meta)

      build_results_sidebar_controls(list(
        shiny::selectInput(ns("health_outcome"), "Health Outcome",
          choices = outcome_choices,
          selected = if (length(outcome_choices) > 0) outcome_choices[1] else NULL
        ),
        shiny::selectInput(ns("cost_outcome"), "Cost Outcome",
          choices = cost_choices,
          selected = if (length(cost_choices) > 0) cost_choices[1] else NULL
        ),
        shiny::numericInput(ns("wtp"), "Willingness to Pay", value = 100000, min = 0, step = 10000),
        if (length(groups) > 1) shiny::selectInput(ns("groups"), "Groups", choices = groups, selected = "overall", multiple = TRUE),
        if (length(strategies) > 1) shiny::selectInput(ns("interventions"), "Interventions",
          choices = strategies,
          selected = if (length(strategies) > 1) strategies[2] else strategies[1],
          multiple = TRUE
        ),
        if (length(strategies) > 1) shiny::selectInput(ns("comparators"), "Comparators",
          choices = strategies, selected = strategies[-2], multiple = TRUE
        )
      ))
    })

    build_args <- function(res, include_wtp = FALSE) {
      args <- list(
        res,
        health_outcome = input$health_outcome,
        cost_outcome = input$cost_outcome
      )
      if (include_wtp && !is.null(input$wtp)) args$wtp <- input$wtp
      if (!is.null(input$groups)) args$groups <- input$groups
      if (!is.null(input$comparators)) args$comparators <- input$comparators
      if (!is.null(input$interventions)) args$interventions <- input$interventions
      args
    }

    output$result_plot <- shiny::renderPlot({
      res <- results()
      shiny::req(
        res, input$viz_type, input$health_outcome,
        input$cost_outcome, input$viz_type != "table",
        input$comparators, input$interventions
      )
      args <- build_args(res, include_wtp = TRUE)
      error_msg(NULL)
      tryCatch({
        do.call(openqaly::pairwise_ce_plot, args)
      }, error = function(e) {
        error_msg(conditionMessage(e))
        NULL
      })
    })

    output$result_table <- shiny::renderUI({
      res <- results()
      shiny::req(
        res, input$viz_type == "table",
        input$health_outcome, input$cost_outcome,
        input$comparators, input$interventions
      )
      args <- build_args(res, include_wtp = FALSE)
      error_msg(NULL)
      tryCatch({
        ft <- do.call(openqaly::pairwise_ce_table, args)
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

# =============================================================================
# Incremental CE Results Sub-Module
# =============================================================================

#' Incremental CE Results UI
#' @param id Module namespace ID.
#' @keywords internal
incrementalCeResultsSidebarUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::selectInput(ns("viz_type"), "Visualization",
      choices = c("Frontier Plot" = "plot", "Table" = "table")
    ),
    shiny::uiOutput(ns("controls"))
  )
}

#' Incremental CE Results UI
#' @param id Module namespace ID.
#' @keywords internal
incrementalCeResultsUI <- function(id) {
  ns <- shiny::NS(id)
  results_fill_panel(
    shiny::conditionalPanel(
      condition = sprintf("input['%s'] != 'table'", ns("viz_type")),
      results_fill_plot_output(ns("result_plot"))
    ),
    shiny::conditionalPanel(
      condition = sprintf("input['%s'] == 'table'", ns("viz_type")),
      shiny::uiOutput(ns("result_table"))
    ),
    shiny::uiOutput(ns("error_display"))
  )
}

#' Incremental CE Results Server
#' @param id Module namespace ID.
#' @param results Reactive containing model results.
#' @param metadata Reactive containing model metadata.
#' @keywords internal
incrementalCeResultsServer <- function(id, results, metadata) {
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
      if (is.null(meta)) return(NULL)
      groups <- get_group_choices(meta)
      strategies <- get_strategy_choices(meta)
      outcome_choices <- get_outcome_summary_choices(meta)
      cost_choices <- get_cost_summary_choices(meta)

      build_results_sidebar_controls(list(
        shiny::selectInput(ns("health_outcome"), "Health Outcome",
          choices = outcome_choices,
          selected = if (length(outcome_choices) > 0) outcome_choices[1] else NULL
        ),
        shiny::selectInput(ns("cost_outcome"), "Cost Outcome",
          choices = cost_choices,
          selected = if (length(cost_choices) > 0) cost_choices[1] else NULL
        ),
        if (length(groups) > 1) shiny::selectInput(ns("groups"), "Groups", choices = groups, selected = "overall", multiple = TRUE),
        if (length(strategies) > 1) shiny::selectInput(ns("strategies"), "Strategies", choices = strategies, selected = strategies, multiple = TRUE)
      ))
    })

    build_args <- function(res) {
      args <- list(
        res,
        health_outcome = input$health_outcome,
        cost_outcome = input$cost_outcome
      )
      if (!is.null(input$groups)) args$groups <- input$groups
      if (!is.null(input$strategies)) args$strategies <- input$strategies
      args
    }

    output$result_plot <- shiny::renderPlot({
      res <- results()
      shiny::req(res, input$viz_type, input$health_outcome, input$cost_outcome, input$viz_type != "table")
      args <- build_args(res)
      error_msg(NULL)
      tryCatch({
        do.call(openqaly::incremental_ce_plot, args)
      }, error = function(e) {
        error_msg(conditionMessage(e))
        NULL
      })
    })

    output$result_table <- shiny::renderUI({
      res <- results()
      shiny::req(res, input$viz_type == "table", input$health_outcome, input$cost_outcome)
      args <- build_args(res)
      error_msg(NULL)
      tryCatch({
        ft <- do.call(openqaly::incremental_ce_table, args)
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
