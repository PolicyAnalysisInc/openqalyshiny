# =============================================================================
# Scenario Result Tab Sub-Module (one instance per analysis type)
# =============================================================================

#' Scenario Result Tab UI
#' @param id Module namespace ID.
#' @keywords internal
scenarioResultTabUI <- function(id) {
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

#' Scenario Result Tab Server
#' @param id Module namespace ID.
#' @param analysis_type Fixed string: "outcomes", "costs", "nmb", "ce", or "vbp".
#' @param scenario_results Reactive containing scenario results.
#' @param metadata Reactive containing model metadata.
#' @keywords internal
scenarioResultTabServer <- function(id, analysis_type, scenario_results, metadata) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    error_msg <- shiny::reactiveVal(NULL)

    # ---- Group enforcement ----
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

    # ---- Strategy enforcement (interventions/comparators) ----
    if (analysis_type %in% c("outcomes", "costs", "nmb", "ce")) {
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
    }

    # ---- Dynamic controls ----
    output$controls <- shiny::renderUI({
      meta <- metadata()
      res <- scenario_results()
      if (is.null(meta) || is.null(res)) return(NULL)

      groups <- get_group_choices(meta)
      strategies <- get_strategy_choices(meta)
      outcome_choices <- get_outcome_summary_choices(meta)
      cost_choices <- get_cost_summary_choices(meta)

      inputs <- list(
        shiny::selectInput(ns("viz_type"), "Output Format",
          choices = c("Plot" = "plot", "Table" = "table")
        )
      )

      if (analysis_type == "outcomes") {
        inputs <- c(inputs, list(
          shiny::selectInput(ns("analysis_type"), "Type",
            choices = c("Absolute" = "absolute", "Differences" = "differences"),
            selected = input$analysis_type
          ),
          shiny::selectInput(ns("outcome"), "Outcome Summary",
            choices = outcome_choices,
            selected = if (length(outcome_choices) > 0) outcome_choices[1] else NULL
          ),
          if (!is.null(input$analysis_type) && input$analysis_type == "differences") {
            if (length(strategies) > 1) shiny::selectInput(ns("interventions"), "Interventions",
              choices = strategies,
              selected = if (length(strategies) > 1) strategies[2] else strategies[1],
              multiple = TRUE
            )
          } else {
            shiny::selectInput(ns("strategies"), "Strategies",
              choices = strategies, selected = strategies, multiple = TRUE
            )
          },
          if (!is.null(input$analysis_type) && input$analysis_type == "differences") {
            if (length(strategies) > 1) shiny::selectInput(ns("comparators"), "Comparators",
              choices = strategies, selected = strategies[-2], multiple = TRUE
            )
          },
          shiny::checkboxInput(ns("discounted"), "Discounted", value = TRUE)
        ))
      } else if (analysis_type == "costs") {
        inputs <- c(inputs, list(
          shiny::selectInput(ns("analysis_type"), "Type",
            choices = c("Absolute" = "absolute", "Differences" = "differences"),
            selected = input$analysis_type
          ),
          shiny::selectInput(ns("outcome"), "Cost Summary",
            choices = cost_choices,
            selected = if (length(cost_choices) > 0) cost_choices[1] else NULL
          ),
          if (!is.null(input$analysis_type) && input$analysis_type == "differences") {
            if (length(strategies) > 1) shiny::selectInput(ns("interventions"), "Interventions",
              choices = strategies,
              selected = if (length(strategies) > 1) strategies[2] else strategies[1],
              multiple = TRUE
            )
          } else {
            shiny::selectInput(ns("strategies"), "Strategies",
              choices = strategies, selected = strategies, multiple = TRUE
            )
          },
          if (!is.null(input$analysis_type) && input$analysis_type == "differences") {
            if (length(strategies) > 1) shiny::selectInput(ns("comparators"), "Comparators",
              choices = strategies, selected = strategies[-2], multiple = TRUE
            )
          },
          shiny::checkboxInput(ns("discounted"), "Discounted", value = TRUE)
        ))
      } else if (analysis_type == "nmb") {
        inputs <- c(inputs, list(
          shiny::selectInput(ns("health_outcome"), "Health Outcome",
            choices = outcome_choices,
            selected = if (length(outcome_choices) > 0) outcome_choices[1] else NULL
          ),
          shiny::selectInput(ns("cost_outcome"), "Cost Outcome",
            choices = cost_choices,
            selected = if (length(cost_choices) > 0) cost_choices[1] else NULL
          ),
          shiny::numericInput(ns("wtp"), "Willingness to Pay",
            value = 100000, min = 0, step = 10000
          ),
          if (length(strategies) > 1) shiny::selectInput(ns("interventions"), "Interventions",
            choices = strategies,
            selected = if (length(strategies) > 1) strategies[2] else strategies[1],
            multiple = TRUE
          ),
          if (length(strategies) > 1) shiny::selectInput(ns("comparators"), "Comparators",
            choices = strategies, selected = strategies[-2], multiple = TRUE
          )
        ))
      } else if (analysis_type == "ce") {
        inputs <- c(inputs, list(
          shiny::selectInput(ns("health_outcome"), "Health Outcome",
            choices = outcome_choices,
            selected = if (length(outcome_choices) > 0) outcome_choices[1] else NULL
          ),
          shiny::selectInput(ns("cost_outcome"), "Cost Outcome",
            choices = cost_choices,
            selected = if (length(cost_choices) > 0) cost_choices[1] else NULL
          ),
          if (length(strategies) > 1) shiny::selectInput(ns("interventions"), "Interventions",
            choices = strategies,
            selected = if (length(strategies) > 1) strategies[2] else strategies[1],
            multiple = TRUE
          ),
          if (length(strategies) > 1) shiny::selectInput(ns("comparators"), "Comparators",
            choices = strategies, selected = strategies[-2], multiple = TRUE
          )
        ))
      } else if (analysis_type == "vbp") {
        all_strategies <- get_strategy_choices(meta)
        vbp_spec <- res$vbp_spec
        if (!is.null(vbp_spec)) {
          intervention <- vbp_spec$intervention_strategy
          individual_comparators <- all_strategies[all_strategies != intervention]
        } else {
          individual_comparators <- all_strategies
        }
        comparator_choices <- c(
          "Overall (Aggregate)" = "overall",
          "All (Individual + Aggregate)" = "all",
          "All (Individual Only)" = "all_comparators",
          individual_comparators
        )

        inputs <- c(inputs, list(
          shiny::numericInput(ns("wtp"), "Willingness to Pay",
            value = 100000, min = 0, step = 10000
          ),
          shiny::selectInput(ns("vbp_comparators"), "Comparators",
            choices = comparator_choices,
            selected = "all",
            multiple = FALSE
          )
        ))
      }

      # Add groups input for all types when model has multiple groups
      if (length(groups) > 1) {
        inputs <- c(inputs, list(
          shiny::selectInput(ns("groups"), "Groups",
            choices = groups, selected = "overall", multiple = TRUE
          )
        ))
      }

      inputs <- Filter(Negate(is.null), inputs)

      do.call(bslib::layout_columns, c(
        list(col_widths = bslib::breakpoints(sm = 12, md = 6)),
        inputs
      ))
    })

    # ---- Plot rendering ----
    output$result_plot <- shiny::renderPlot({
      res <- scenario_results()
      shiny::req(res, input$viz_type, input$viz_type != "table")
      error_msg(NULL)

      tryCatch({
        if (analysis_type == "outcomes") {
          shiny::req(input$outcome)
          args <- list(res, summary_name = input$outcome)
          if (!is.null(input$groups)) args$groups <- input$groups
          if (!is.null(input$discounted)) args$discounted <- input$discounted
          if (!is.null(input$analysis_type) && input$analysis_type == "absolute") {
            if (!is.null(input$strategies)) args$strategies <- input$strategies
          } else {
            shiny::req(input$interventions, input$comparators)
            args$interventions <- input$interventions
            args$comparators <- input$comparators
          }
          do.call(openqaly::scenario_outcomes_plot, args)

        } else if (analysis_type == "costs") {
          shiny::req(input$outcome)
          args <- list(res, summary_name = input$outcome)
          if (!is.null(input$groups)) args$groups <- input$groups
          if (!is.null(input$discounted)) args$discounted <- input$discounted
          if (!is.null(input$analysis_type) && input$analysis_type == "absolute") {
            if (!is.null(input$strategies)) args$strategies <- input$strategies
          } else {
            shiny::req(input$interventions, input$comparators)
            args$interventions <- input$interventions
            args$comparators <- input$comparators
          }
          do.call(openqaly::scenario_costs_plot, args)

        } else if (analysis_type == "nmb") {
          shiny::req(input$health_outcome, input$cost_outcome,
                     input$interventions, input$comparators)
          args <- list(res,
            health_outcome = input$health_outcome,
            cost_outcome = input$cost_outcome
          )
          if (!is.null(input$wtp)) args$wtp <- input$wtp
          if (!is.null(input$groups)) args$groups <- input$groups
          args$interventions <- input$interventions
          args$comparators <- input$comparators
          do.call(openqaly::scenario_nmb_plot, args)

        } else if (analysis_type == "ce") {
          shiny::req(input$health_outcome, input$cost_outcome,
                     input$interventions, input$comparators)
          args <- list(res,
            health_outcome = input$health_outcome,
            cost_outcome = input$cost_outcome
          )
          if (!is.null(input$groups)) args$groups <- input$groups
          args$interventions <- input$interventions
          args$comparators <- input$comparators
          do.call(openqaly::scenario_ce_plot, args)

        } else if (analysis_type == "vbp") {
          args <- list(res)
          if (!is.null(input$wtp)) args$wtp <- input$wtp
          if (!is.null(input$vbp_comparators)) args$comparators <- input$vbp_comparators
          if (!is.null(input$groups)) args$groups <- input$groups
          do.call(openqaly::scenario_vbp_plot, args)
        }
      }, error = function(e) {
        error_msg(conditionMessage(e))
        NULL
      })
    })

    # ---- Table rendering ----
    output$result_table <- shiny::renderUI({
      res <- scenario_results()
      shiny::req(res, input$viz_type == "table")
      error_msg(NULL)

      tryCatch({
        ft <- if (analysis_type == "outcomes") {
          shiny::req(input$outcome)
          args <- list(res, outcome = input$outcome)
          if (!is.null(input$groups)) args$groups <- input$groups
          if (!is.null(input$discounted)) args$discounted <- input$discounted
          if (!is.null(input$analysis_type) && input$analysis_type == "absolute") {
            if (!is.null(input$strategies)) args$strategies <- input$strategies
          } else {
            shiny::req(input$interventions, input$comparators)
            args$interventions <- input$interventions
            args$comparators <- input$comparators
          }
          do.call(openqaly::scenario_outcomes_table, args)

        } else if (analysis_type == "costs") {
          shiny::req(input$outcome)
          args <- list(res, outcome = input$outcome)
          if (!is.null(input$groups)) args$groups <- input$groups
          if (!is.null(input$discounted)) args$discounted <- input$discounted
          if (!is.null(input$analysis_type) && input$analysis_type == "absolute") {
            if (!is.null(input$strategies)) args$strategies <- input$strategies
          } else {
            shiny::req(input$interventions, input$comparators)
            args$interventions <- input$interventions
            args$comparators <- input$comparators
          }
          do.call(openqaly::scenario_costs_table, args)

        } else if (analysis_type == "nmb") {
          shiny::req(input$health_outcome, input$cost_outcome,
                     input$interventions, input$comparators)
          args <- list(res,
            health_outcome = input$health_outcome,
            cost_outcome = input$cost_outcome
          )
          if (!is.null(input$wtp)) args$wtp <- input$wtp
          if (!is.null(input$groups)) args$groups <- input$groups
          args$interventions <- input$interventions
          args$comparators <- input$comparators
          do.call(openqaly::scenario_nmb_table, args)

        } else if (analysis_type == "ce") {
          shiny::req(input$health_outcome, input$cost_outcome,
                     input$interventions, input$comparators)
          args <- list(res,
            health_outcome = input$health_outcome,
            cost_outcome = input$cost_outcome
          )
          if (!is.null(input$groups)) args$groups <- input$groups
          args$interventions <- input$interventions
          args$comparators <- input$comparators
          do.call(openqaly::scenario_ce_table, args)

        } else if (analysis_type == "vbp") {
          args <- list(res)
          if (!is.null(input$wtp)) args$wtp <- input$wtp
          if (!is.null(input$vbp_comparators)) args$comparators <- input$vbp_comparators
          if (!is.null(input$groups)) args$groups <- input$groups
          do.call(openqaly::scenario_vbp_table, args)
        }

        render_flextable_html(ft)
      }, error = function(e) {
        error_msg(conditionMessage(e))
        NULL
      })
    })

    # ---- Error display ----
    output$error_display <- shiny::renderUI({
      msg <- error_msg()
      if (!is.null(msg)) {
        shiny::tags$div(class = "alert alert-danger", msg)
      }
    })
  })
}
