# =============================================================================
# PSA Result Tab Sub-Module (one instance per analysis type)
# =============================================================================

#' PSA Result Tab UI
#' @param id Module namespace ID.
#' @keywords internal
psaResultTabUI <- function(id) {
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

#' Normalize PSA parameter group filters for tuple choices
#' @param groups_input Selected group filter values from the UI.
#' @param metadata Optional PSA metadata.
#' @keywords internal
normalize_psa_parameter_groups <- function(groups_input, metadata = NULL) {
  if (is.null(groups_input) || length(groups_input) == 0) {
    return(NULL)
  }

  if ("all" %in% groups_input || "overall" %in% groups_input) {
    return(NULL)
  }

  specific_groups <- setdiff(groups_input, "all_groups")
  if ("all_groups" %in% groups_input &&
      !is.null(metadata$groups) &&
      is.data.frame(metadata$groups) &&
      nrow(metadata$groups) > 0) {
    specific_groups <- c(specific_groups, metadata$groups$name)
  }

  specific_groups <- unique(specific_groups[!is.na(specific_groups) & nzchar(specific_groups)])
  if (length(specific_groups) == 0) {
    return(NULL)
  }

  specific_groups
}

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

#' Build selectable PSA parameter choices from PSA results
#' @param results PSA results list returned by `openqaly::run_psa()`.
#' @param metadata Optional PSA metadata, used to map display names.
#' @param groups_input Selected group filter values from the UI.
#' @keywords internal
get_psa_parameter_choices <- function(results, metadata = NULL, groups_input = NULL) {
  if (is.null(results$segments) ||
      !"parameter_overrides" %in% names(results$segments)) {
    return(character(0))
  }

  segments <- results$segments
  allowed_groups <- normalize_psa_parameter_groups(groups_input, metadata)
  if (!is.null(allowed_groups) && "group" %in% names(segments)) {
    segments <- segments[segments$group %in% allowed_groups, , drop = FALSE]
  }

  tuple_rows <- lapply(seq_len(nrow(segments)), function(i) {
    overrides <- segments$parameter_overrides[[i]]
    if (is.null(overrides) || length(overrides) == 0) {
      return(NULL)
    }

    data.frame(
      variable = names(overrides),
      strategy = rep_len(segments$strategy[i] %||% "", length(overrides)),
      group = rep_len(segments$group[i] %||% "", length(overrides)),
      stringsAsFactors = FALSE
    )
  })

  tuple_rows <- Filter(Negate(is.null), tuple_rows)
  if (length(tuple_rows) == 0) {
    return(character(0))
  }

  tuples <- unique(do.call(rbind, tuple_rows))
  tuples <- tuples[!is.na(tuples$variable) & nzchar(tuples$variable), , drop = FALSE]
  if (nrow(tuples) == 0) {
    return(character(0))
  }

  map_strategy_name <- function(strategy_name) {
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

  map_group_name <- function(group_name) {
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

  map_variable_label <- function(variable_name, strategy_name, group_name) {
    if (is.null(metadata$variables) ||
        !is.data.frame(metadata$variables) ||
        nrow(metadata$variables) == 0 ||
        !"display_name" %in% names(metadata$variables)) {
      return(variable_name)
    }

    variable_meta <- metadata$variables
    meta_strategy <- if ("strategy" %in% names(variable_meta)) variable_meta$strategy else ""
    meta_group <- if ("group" %in% names(variable_meta)) variable_meta$group else ""
    meta_strategy[is.na(meta_strategy)] <- ""
    meta_group[is.na(meta_group)] <- ""

    exact_match <- variable_meta$name == variable_name &
      meta_strategy == strategy_name &
      meta_group == group_name

    if (!any(exact_match)) {
      exact_match <- variable_meta$name == variable_name &
        meta_strategy == strategy_name &
        !nzchar(meta_group)
    }
    if (!any(exact_match)) {
      exact_match <- variable_meta$name == variable_name &
        !nzchar(meta_strategy) &
        meta_group == group_name
    }
    if (!any(exact_match)) {
      exact_match <- variable_meta$name == variable_name &
        !nzchar(meta_strategy) &
        !nzchar(meta_group)
    }
    if (!any(exact_match)) {
      exact_match <- variable_meta$name == variable_name
    }

    matched <- variable_meta[exact_match, , drop = FALSE]
    if (nrow(matched) == 0 ||
        is.na(matched$display_name[1]) ||
        !nzchar(matched$display_name[1])) {
      return(variable_name)
    }

    matched$display_name[1]
  }

  choice_labels <- vapply(seq_len(nrow(tuples)), function(i) {
    map_variable_label(tuples$variable[i], tuples$strategy[i], tuples$group[i])
  }, character(1))

  duplicate_labels <- unique(choice_labels[duplicated(choice_labels)])
  if (length(duplicate_labels) > 0) {
    for (duplicate_label in duplicate_labels) {
      idx <- which(choice_labels == duplicate_label)
      qualifiers <- vapply(idx, function(i) {
        qualifier_parts <- character(0)
        if (nzchar(tuples$strategy[i])) {
          qualifier_parts <- c(qualifier_parts, map_strategy_name(tuples$strategy[i]))
        }
        if (nzchar(tuples$group[i])) {
          qualifier_parts <- c(qualifier_parts, map_group_name(tuples$group[i]))
        }
        qualifier <- paste(qualifier_parts, collapse = " / ")
        if (!nzchar(qualifier)) {
          qualifier <- paste(tuples$variable[i], tuples$strategy[i], tuples$group[i], sep = " / ")
        }
        qualifier
      }, character(1))
      choice_labels[idx] <- sprintf("%s [%s]", duplicate_label, qualifiers)
    }
  }

  encoded_values <- vapply(seq_len(nrow(tuples)), function(i) {
    encode_psa_parameter_choice(tuples$variable[i], tuples$strategy[i], tuples$group[i])
  }, character(1))

  stats::setNames(encoded_values, choice_labels)
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
            choices = strategies, selected = strategies[1], multiple = TRUE
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
            choices = strategies, selected = strategies[1], multiple = TRUE
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
        var_choices <- get_psa_parameter_choices(res, meta, input$groups)
        var_values <- unname(var_choices)
        selected_values <- intersect(input$variables %||% character(0), var_values)
        if (length(selected_values) == 0 && length(var_values) > 0) {
          selected_values <- var_values[seq_len(min(5, length(var_values)))]
        }

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

      do.call(bslib::layout_columns, c(
        list(col_widths = bslib::breakpoints(sm = 12, md = 6)),
        inputs
      ))
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
    output$result_plot <- shiny::renderPlot({
      res <- psa_results()
      shiny::req(res, input$viz_type, input$viz_type != "table")
      error_msg(NULL)

      tryCatch({
        if (analysis_type == "outcomes") {
          shiny::req(input$outcome)
          args <- list(res, outcome_summary = input$outcome)
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
          args <- list(res, outcome_summary = input$outcome)
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
            outcome_summary = input$health_outcome,
            cost_summary = input$cost_outcome,
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
            outcome_summary = input$health_outcome,
            cost_summary = input$cost_outcome
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
            outcome_summary = input$health_outcome,
            cost_summary = input$cost_outcome,
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
            outcome_summary = input$health_outcome,
            cost_summary = input$cost_outcome
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
    })

    # ---- Table rendering ----
    output$result_table <- shiny::renderUI({
      res <- psa_results()
      shiny::req(res, input$viz_type == "table")
      error_msg(NULL)

      tryCatch({
        ft <- if (analysis_type == "outcomes") {
          shiny::req(input$outcome)
          args <- list(res, outcome_summary = input$outcome)
          if (!is.null(input$groups)) args$groups <- input$groups
          if (!is.null(input$strategies)) args$strategies <- input$strategies
          if (!is.null(input$discounted)) args$discounted <- input$discounted
          do.call(openqaly::psa_outcomes_table, args)

        } else if (analysis_type == "costs") {
          shiny::req(input$outcome)
          args <- list(res, outcome_summary = input$outcome)
          if (!is.null(input$groups)) args$groups <- input$groups
          if (!is.null(input$strategies)) args$strategies <- input$strategies
          if (!is.null(input$discounted)) args$discounted <- input$discounted
          do.call(openqaly::psa_costs_table, args)

        } else if (analysis_type == "nmb") {
          shiny::req(input$health_outcome, input$cost_outcome, input$wtp,
                     input$interventions, input$comparators)
          args <- list(res,
            outcome_summary = input$health_outcome,
            cost_summary = input$cost_outcome,
            wtp = input$wtp
          )
          if (!is.null(input$groups)) args$groups <- input$groups
          args$interventions <- input$interventions
          args$comparators <- input$comparators
          do.call(openqaly::psa_nmb_table, args)

        } else if (analysis_type == "incremental_ce") {
          shiny::req(input$health_outcome, input$cost_outcome)
          args <- list(res,
            outcome_summary = input$health_outcome,
            cost_summary = input$cost_outcome
          )
          if (!is.null(input$groups)) args$groups <- input$groups
          do.call(openqaly::incremental_ceac_table, args)

        } else if (analysis_type == "pairwise_ce") {
          shiny::req(input$health_outcome, input$cost_outcome,
                     input$interventions, input$comparators)
          args <- list(res,
            outcome_summary = input$health_outcome,
            cost_summary = input$cost_outcome,
            interventions = input$interventions,
            comparators = input$comparators
          )
          if (!is.null(input$groups)) args$groups <- input$groups
          do.call(openqaly::pairwise_ceac_table, args)

        } else if (analysis_type == "evpi") {
          shiny::req(input$health_outcome, input$cost_outcome)
          args <- list(res,
            outcome_summary = input$health_outcome,
            cost_summary = input$cost_outcome
          )
          if (!is.null(input$groups)) args$groups <- input$groups
          do.call(openqaly::evpi_table, args)

        } else if (analysis_type == "parameters") {
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
