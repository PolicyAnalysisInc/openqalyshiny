# =============================================================================
# PSA Result Tab Sub-Module (one instance per analysis type)
# =============================================================================

#' PSA Result Tab UI
#' @param id Module namespace ID.
#' @keywords internal
psaResultTabSidebarUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("controls"))
  )
}

#' PSA Result Tab UI
#' @param id Module namespace ID.
#' @keywords internal
psaResultTabUI <- function(id) {
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

#' Normalize PSA parameter group filters for tuple choices
#' @param groups_input Selected group filter values from the UI.
#' @param metadata Optional PSA metadata.
#' @keywords internal
#' Encode a PSA parameter tuple as a select input value
#' @param variable Variable name.
#' @param strategy Strategy name.
#' @param group Group name.
#' @keywords internal
encode_psa_parameter_choice <- function(variable, strategy = NULL, group = NULL) {
  paste(variable %||% "", strategy %||% "", group %||% "", sep = "|||")
}

#' Decode PSA parameter tuple selections from the UI
#' @param selections Character vector of encoded tuple selections.
#' @keywords internal
decode_psa_parameter_choices <- function(selections) {
  if (is.null(selections) || length(selections) == 0) {
    return(data.frame(
      variable = character(0),
      strategy = character(0),
      group = character(0),
      stringsAsFactors = FALSE
    ))
  }

  parts <- strsplit(selections, "|||", fixed = TRUE)
  padded_parts <- lapply(parts, function(part) {
    c(part, rep("", max(0, 3 - length(part))))[seq_len(3)]
  })

  tuples <- as.data.frame(do.call(rbind, padded_parts), stringsAsFactors = FALSE)
  names(tuples) <- c("variable", "strategy", "group")
  tuples$strategy[!nzchar(tuples$strategy)] <- NA_character_
  tuples$group[!nzchar(tuples$group)] <- NA_character_
  tuples
}

#' Build selectable PSA parameter choices from metadata variables
#'
#' Returns one entry per row in `metadata$variables`, using display names
#' as labels and encoded variable/strategy/group tuples as values.
#' @param metadata PSA metadata containing a `variables` data frame.
#' @keywords internal
get_psa_parameter_choices <- function(metadata) {
  if (is.null(metadata$variables) ||
      !is.data.frame(metadata$variables) ||
      nrow(metadata$variables) == 0) {
    return(character(0))
  }

  vars <- metadata$variables

  choice_labels <- vapply(seq_len(nrow(vars)), function(i) {
    dn <- if ("display_name" %in% names(vars)) vars$display_name[i] else NA_character_
    if (is.na(dn) || !nzchar(dn)) vars$name[i] else dn
  }, character(1))

  # Disambiguate duplicate labels with [Strategy / Group] qualifiers
  duplicate_labels <- unique(choice_labels[duplicated(choice_labels)])
  if (length(duplicate_labels) > 0) {
    for (dup in duplicate_labels) {
      idx <- which(choice_labels == dup)
      qualifiers <- vapply(idx, function(i) {
        parts <- character(0)
        s <- if ("strategy" %in% names(vars)) vars$strategy[i] else ""
        g <- if ("group" %in% names(vars)) vars$group[i] else ""
        if (!is.na(s) && nzchar(s)) {
          parts <- c(parts, map_psa_strategy_name(s, metadata))
        }
        if (!is.na(g) && nzchar(g)) {
          parts <- c(parts, map_psa_group_name(g, metadata))
        }
        paste(parts, collapse = " / ")
      }, character(1))
      choice_labels[idx] <- ifelse(
        nzchar(qualifiers),
        sprintf("%s [%s]", dup, qualifiers),
        choice_labels[idx]
      )
    }
  }

  encoded_values <- vapply(seq_len(nrow(vars)), function(i) {
    encode_psa_parameter_choice(
      vars$name[i],
      if ("strategy" %in% names(vars)) vars$strategy[i] %||% "" else "",
      if ("group" %in% names(vars)) vars$group[i] %||% "" else ""
    )
  }, character(1))

  stats::setNames(encoded_values, choice_labels)
}

#' Map a strategy name to its display name
#' @keywords internal
map_psa_strategy_name <- function(strategy_name, metadata) {
  if (!nzchar(strategy_name) ||
      is.null(metadata$strategies) ||
      !is.data.frame(metadata$strategies) ||
      nrow(metadata$strategies) == 0) {
    return(strategy_name)
  }
  strategy_meta <- metadata$strategies[metadata$strategies$name == strategy_name, , drop = FALSE]
  if (nrow(strategy_meta) == 0 ||
      !"display_name" %in% names(strategy_meta) ||
      is.na(strategy_meta$display_name[1]) ||
      !nzchar(strategy_meta$display_name[1])) {
    return(strategy_name)
  }
  strategy_meta$display_name[1]
}

#' Map a group name to its display name
#' @keywords internal
map_psa_group_name <- function(group_name, metadata) {
  if (!nzchar(group_name) ||
      is.null(metadata$groups) ||
      !is.data.frame(metadata$groups) ||
      nrow(metadata$groups) == 0) {
    return(group_name)
  }
  group_meta <- metadata$groups[metadata$groups$name == group_name, , drop = FALSE]
  if (nrow(group_meta) == 0 ||
      !"display_name" %in% names(group_meta) ||
      is.na(group_meta$display_name[1]) ||
      !nzchar(group_meta$display_name[1])) {
    return(group_name)
  }
  group_meta$display_name[1]
}

#' PSA Result Tab Server
#' @param id Module namespace ID.
#' @param analysis_type Fixed string: "outcomes", "costs", "nmb",
#'   "incremental_ce", "pairwise_ce", "evpi", or "parameters".
#' @param psa_results Reactive containing PSA results.
#' @param metadata Reactive containing model metadata.
#' @keywords internal
psaResultTabServer <- function(id, analysis_type, psa_results, metadata) {
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
    if (analysis_type %in% c("nmb", "pairwise_ce")) {
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
      res <- psa_results()
      if (is.null(meta) || is.null(res)) return(NULL)

      groups <- get_group_choices(meta)
      strategies <- get_strategy_choices(meta)
      outcome_choices <- get_outcome_summary_choices(meta)
      cost_choices <- get_cost_summary_choices(meta)

      # Build viz_type choices based on analysis type
      viz_choices <- if (analysis_type %in% c("outcomes", "costs", "nmb")) {
        c("Density Plot" = "density", "Ridge Plot" = "ridge", "Table" = "table")
      } else if (analysis_type %in% c("incremental_ce", "pairwise_ce")) {
        c("Scatter" = "scatter", "CEAC" = "ceac", "Table" = "table")
      } else {
        c("Plot" = "plot", "Table" = "table")
      }

      inputs <- list(
        shiny::selectInput(ns("viz_type"), "Output Format",
          choices = viz_choices
        )
      )

      if (analysis_type == "outcomes") {
        inputs <- c(inputs, list(
          shiny::selectInput(ns("outcome"), "Outcome Summary",
            choices = outcome_choices,
            selected = if (length(outcome_choices) > 0) outcome_choices[1] else NULL
          ),
          shiny::selectInput(ns("strategies"), "Strategies",
            choices = strategies, selected = strategies, multiple = TRUE
          ),
          shiny::checkboxInput(ns("discounted"), "Discounted", value = TRUE)
        ))
      } else if (analysis_type == "costs") {
        inputs <- c(inputs, list(
          shiny::selectInput(ns("outcome"), "Cost Summary",
            choices = cost_choices,
            selected = if (length(cost_choices) > 0) cost_choices[1] else NULL
          ),
          shiny::selectInput(ns("strategies"), "Strategies",
            choices = strategies, selected = strategies, multiple = TRUE
          ),
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
      } else if (analysis_type == "incremental_ce") {
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
          )
        ))
      } else if (analysis_type == "pairwise_ce") {
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
      } else if (analysis_type == "evpi") {
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
          )
        ))
      } else if (analysis_type == "parameters") {
        var_choices <- get_psa_parameter_choices(meta)
        var_values <- unname(var_choices)
        selected_values <- intersect(input$variables %||% character(0), var_values)
        inputs <- c(inputs, list(
          shiny::selectInput(ns("variables"), "Variables",
            choices = var_choices,
            selected = selected_values,
            multiple = TRUE
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

      build_results_sidebar_controls(c(inputs, list(plot_scale_input(ns))))
    })

    # ---- Helper: translate group UI keywords for parameter functions ----
    resolve_param_group <- function(groups_input) {
      if (is.null(groups_input)) return(NULL)
      # "all" and "all_groups" are UI-only keywords meaning all groups
      if (any(groups_input %in% c("all", "all_groups"))) return(NULL)
      # Single value (including "overall") is valid
      if (length(groups_input) == 1) return(groups_input)
      # Multiple specific groups: use first (function only accepts single)
      groups_input[1]
    }

    build_psa_parameter_args <- function(results, variable_selections, groups_input = NULL) {
      args <- list(results)
      tuples <- decode_psa_parameter_choices(variable_selections)
      if (nrow(tuples) == 0) {
        group_val <- resolve_param_group(groups_input)
        if (!is.null(group_val)) args$group <- group_val
        return(args)
      }

      args$variables <- tuples$variable

      tuple_strategies <- tuples$strategy
      tuple_groups <- tuples$group
      has_tuple_strategy <- any(!is.na(tuple_strategies) & nzchar(tuple_strategies))
      has_tuple_group <- any(!is.na(tuple_groups) & nzchar(tuple_groups))

      if (nrow(tuples) > 1) {
        if (has_tuple_strategy) args$strategies <- tuple_strategies
        if (has_tuple_group) {
          args$group <- tuple_groups
        } else {
          group_val <- resolve_param_group(groups_input)
          if (!is.null(group_val)) args$group <- group_val
        }
      } else {
        if (has_tuple_strategy) args$strategies <- tuple_strategies[1]
        if (has_tuple_group) {
          args$group <- tuple_groups[1]
        } else {
          group_val <- resolve_param_group(groups_input)
          if (!is.null(group_val)) args$group <- group_val
        }
      }

      args
    }

    # ---- Plot rendering ----
    shiny::observe({
      scale <- input$plot_scale %||% 1
      output$result_plot <- shiny::renderPlot({
      res <- psa_results()
      shiny::req(res, input$viz_type, input$viz_type != "table")
      error_msg(NULL)

      tryCatch({
        if (analysis_type == "outcomes") {
          shiny::req(input$outcome)
          args <- list(res, outcome = input$outcome)
          if (!is.null(input$groups)) args$groups <- input$groups
          if (!is.null(input$strategies)) args$strategies <- input$strategies
          if (!is.null(input$discounted)) args$discounted <- input$discounted
          if (input$viz_type == "ridge") {
            do.call(openqaly::outcome_ridgeline_plot, args)
          } else {
            do.call(openqaly::outcome_density_plot, args)
          }

        } else if (analysis_type == "costs") {
          shiny::req(input$outcome)
          args <- list(res, outcome = input$outcome)
          if (!is.null(input$groups)) args$groups <- input$groups
          if (!is.null(input$strategies)) args$strategies <- input$strategies
          if (!is.null(input$discounted)) args$discounted <- input$discounted
          if (input$viz_type == "ridge") {
            do.call(openqaly::cost_ridgeline_plot, args)
          } else {
            do.call(openqaly::cost_density_plot, args)
          }

        } else if (analysis_type == "nmb") {
          shiny::req(input$health_outcome, input$cost_outcome, input$wtp,
                     input$interventions, input$comparators)
          args <- list(res,
            health_outcome = input$health_outcome,
            cost_outcome = input$cost_outcome,
            wtp = input$wtp
          )
          if (!is.null(input$groups)) args$groups <- input$groups
          args$interventions <- input$interventions
          args$comparators <- input$comparators
          if (input$viz_type == "ridge") {
            do.call(openqaly::nmb_ridgeline_plot, args)
          } else {
            do.call(openqaly::nmb_density_plot, args)
          }

        } else if (analysis_type == "incremental_ce") {
          shiny::req(input$health_outcome, input$cost_outcome)
          args <- list(res,
            health_outcome = input$health_outcome,
            cost_outcome = input$cost_outcome
          )
          if (!is.null(input$groups)) args$groups <- input$groups
          if (input$viz_type == "scatter") {
            do.call(openqaly::psa_scatter_plot, args)
          } else {
            do.call(openqaly::incremental_ceac_plot, args)
          }

        } else if (analysis_type == "pairwise_ce") {
          shiny::req(input$health_outcome, input$cost_outcome,
                     input$interventions, input$comparators)
          args <- list(res,
            health_outcome = input$health_outcome,
            cost_outcome = input$cost_outcome,
            interventions = input$interventions,
            comparators = input$comparators
          )
          if (!is.null(input$groups)) args$groups <- input$groups
          if (input$viz_type == "scatter") {
            if (!is.null(input$wtp)) args$wtp <- input$wtp
            do.call(openqaly::pairwise_psa_scatter_plot, args)
          } else {
            do.call(openqaly::pairwise_ceac_plot, args)
          }

        } else if (analysis_type == "evpi") {
          shiny::req(input$health_outcome, input$cost_outcome)
          args <- list(res,
            health_outcome = input$health_outcome,
            cost_outcome = input$cost_outcome
          )
          if (!is.null(input$groups)) args$groups <- input$groups
          do.call(openqaly::evpi_plot, args)

        } else if (analysis_type == "parameters") {
          shiny::req(input$variables)
          args <- build_psa_parameter_args(res, input$variables, input$groups)
          do.call(openqaly::psa_parameter_scatter_matrix, args)
        }
      }, error = function(e) {
        error_msg(conditionMessage(e))
        NULL
      })
    }, res = 72 * scale)
    })

    # ---- Table rendering ----
    output$result_table <- shiny::renderUI({
      res <- psa_results()
      shiny::req(res, input$viz_type == "table")
      error_msg(NULL)

      tryCatch({
        ft <- if (analysis_type == "outcomes") {
          shiny::req(input$outcome)
          args <- list(res, outcome = input$outcome)
          if (!is.null(input$groups)) args$groups <- input$groups
          if (!is.null(input$strategies)) args$strategies <- input$strategies
          if (!is.null(input$discounted)) args$discounted <- input$discounted
          do.call(openqaly::psa_outcomes_table, args)

        } else if (analysis_type == "costs") {
          shiny::req(input$outcome)
          args <- list(res, outcome = input$outcome)
          if (!is.null(input$groups)) args$groups <- input$groups
          if (!is.null(input$strategies)) args$strategies <- input$strategies
          if (!is.null(input$discounted)) args$discounted <- input$discounted
          do.call(openqaly::psa_costs_table, args)

        } else if (analysis_type == "nmb") {
          shiny::req(input$health_outcome, input$cost_outcome, input$wtp,
                     input$interventions, input$comparators)
          args <- list(res,
            health_outcome = input$health_outcome,
            cost_outcome = input$cost_outcome,
            wtp = input$wtp
          )
          if (!is.null(input$groups)) args$groups <- input$groups
          args$interventions <- input$interventions
          args$comparators <- input$comparators
          do.call(openqaly::psa_nmb_table, args)

        } else if (analysis_type == "incremental_ce") {
          shiny::req(input$health_outcome, input$cost_outcome)
          args <- list(res,
            health_outcome = input$health_outcome,
            cost_outcome = input$cost_outcome
          )
          if (!is.null(input$groups)) args$groups <- input$groups
          do.call(openqaly::incremental_ceac_table, args)

        } else if (analysis_type == "pairwise_ce") {
          shiny::req(input$health_outcome, input$cost_outcome,
                     input$interventions, input$comparators)
          args <- list(res,
            health_outcome = input$health_outcome,
            cost_outcome = input$cost_outcome,
            interventions = input$interventions,
            comparators = input$comparators
          )
          if (!is.null(input$groups)) args$groups <- input$groups
          do.call(openqaly::pairwise_ceac_table, args)

        } else if (analysis_type == "evpi") {
          shiny::req(input$health_outcome, input$cost_outcome)
          args <- list(res,
            health_outcome = input$health_outcome,
            cost_outcome = input$cost_outcome
          )
          if (!is.null(input$groups)) args$groups <- input$groups
          do.call(openqaly::evpi_table, args)

        } else if (analysis_type == "parameters") {
          shiny::req(input$variables)
          args <- build_psa_parameter_args(res, input$variables, input$groups)
          do.call(openqaly::psa_parameters_table, args)
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
