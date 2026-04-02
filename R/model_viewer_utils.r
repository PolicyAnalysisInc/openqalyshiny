#' Get Outcome Summary Choices
#'
#' Extract available outcome summaries from model results metadata.
#'
#' @param metadata A metadata list from openqaly model results.
#'
#' @return A named character vector of outcome summary choices.
#' @keywords internal
get_outcome_summary_choices <- function(metadata) {
  summaries <- metadata$summaries
  if (is.null(summaries) || nrow(summaries) == 0) return(character(0))
  df <- summaries[summaries$type == "outcome", , drop = FALSE]
  if (nrow(df) == 0) return(character(0))
  setNames(df$name, df$display_name)
}

#' Get Cost Summary Choices
#'
#' Extract available cost summaries from model results metadata.
#'
#' @param metadata A metadata list from openqaly model results.
#'
#' @return A named character vector of cost summary choices.
#' @keywords internal
get_cost_summary_choices <- function(metadata) {
  summaries <- metadata$summaries
  if (is.null(summaries) || nrow(summaries) == 0) return(character(0))
  df <- summaries[summaries$type == "cost", , drop = FALSE]
  if (nrow(df) == 0) return(character(0))
  setNames(df$name, df$display_name)
}

#' Get Strategy Choices
#'
#' Extract available strategy names from model results metadata.
#'
#' @param metadata A metadata list from openqaly model results.
#'
#' @return A character vector of strategy names.
#' @keywords internal
get_strategy_choices <- function(metadata) {
  df <- metadata$strategies
  if (is.null(df) || nrow(df) == 0) return(character(0))
  setNames(df$name, df$display_name)
}

#' Get Group Choices
#'
#' Extract available group names from model results metadata.
#'
#' @param metadata A metadata list from openqaly model results.
#'
#' @return A character vector of group names.
#' @keywords internal
get_group_choices <- function(metadata) {
  df <- metadata$groups
  if (is.null(df) || nrow(df) == 0) return(character(0))
  group_choices <- setNames(df$name, df$display_name)
  c(
    "Overall" = "overall",
    "All (Overall + Groups)" = "all",
    "All Groups" = "all_groups",
    group_choices
  )
}

#' Enforce Exclusive Group Selection
#'
#' Given the new and previous group selections, enforce that exclusive keywords
#' ("all", "all_groups") are the only selection when chosen, and that selecting
#' a non-exclusive option removes any exclusive option.
#'
#' @param new_val Character vector of newly selected group values.
#' @param prev_val Character vector of previously selected group values.
#'
#' @return A corrected character vector of group selections.
#' @keywords internal
enforce_exclusive_groups <- function(new_val, prev_val) {
  exclusive <- c("all", "all_groups")
  added <- setdiff(new_val, prev_val)

  # If an exclusive option was just added, select only it
  exclusive_added <- intersect(added, exclusive)
  if (length(exclusive_added) > 0) {
    return(exclusive_added[1])
  }

  # If a non-exclusive option was added while exclusive is selected,
  # remove the exclusive option
  if (any(exclusive %in% new_val) && length(added) > 0) {
    return(setdiff(new_val, exclusive))
  }

  new_val
}

#' Enforce Exclusive Strategy Selection
#'
#' When the user changes one of two mutually exclusive strategy selects
#' (interventions / comparators), remove conflicts from the other select.
#' Prevents either select from becoming empty.
#'
#' @param changed_new Character vector of newly selected values for the changed input.
#' @param other_current Character vector of current values for the other input.
#' @param changed_prev Character vector of previous values for the changed input.
#'
#' @return A list with elements `changed` and `other`.
#' @keywords internal
enforce_exclusive_strategies <- function(changed_new, other_current, changed_prev) {
  # If user cleared all selections, revert
  if (length(changed_new) == 0) {
    return(list(changed = changed_prev, other = other_current))
  }
  # Find conflicts and remove from other
  conflict <- intersect(changed_new, other_current)
  other_updated <- setdiff(other_current, conflict)
  # If other still has items, simple case
  if (length(other_updated) > 0) {
    return(list(changed = changed_new, other = other_updated))
  }
  # Auto-swap: other would be empty, move a strategy from changed to other
  # Prefer moving a pre-existing strategy (not one the user just added)
  newly_added <- setdiff(changed_new, changed_prev)
  candidates <- setdiff(changed_new, newly_added)
  if (length(candidates) > 0) {
    to_move <- candidates[1]
    return(list(
      changed = setdiff(changed_new, to_move),
      other = to_move
    ))
  }
  # Fallback: use a strategy freed from changed (user replaced all selections)
  freed <- setdiff(changed_prev, changed_new)
  if (length(freed) > 0) {
    return(list(changed = changed_new, other = freed[1]))
  }
  # No resolution possible, revert
  list(changed = changed_prev, other = other_current)
}

lookup_model_display_name <- function(df, item_name) {
  if (is.null(item_name) || length(item_name) != 1 || is.na(item_name) || !nzchar(item_name) ||
      is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
    return(item_name)
  }

  match_df <- df[df$name == item_name, , drop = FALSE]
  if (nrow(match_df) == 0 ||
      !"display_name" %in% names(match_df) ||
      is.na(match_df$display_name[1]) ||
      !nzchar(match_df$display_name[1])) {
    return(item_name)
  }

  match_df$display_name[1]
}

encode_vbp_variable_choice <- function(variable, strategy = NULL, group = NULL, row_index = NULL) {
  paste(
    row_index %||% "",
    variable %||% "",
    strategy %||% "",
    group %||% "",
    sep = "|||"
  )
}

decode_vbp_variable_choice <- function(selection) {
  if (is.null(selection) || length(selection) == 0) {
    return(data.frame(
      row_index = integer(0),
      variable = character(0),
      strategy = character(0),
      group = character(0),
      stringsAsFactors = FALSE
    ))
  }

  parsed <- lapply(selection, function(item) {
    if (is.null(item) || is.na(item) || !grepl("|||", item, fixed = TRUE)) {
      return(c(NA_character_, item %||% "", "", ""))
    }

    parts <- strsplit(item, "|||", fixed = TRUE)[[1]]
    c(parts, rep("", max(0, 4 - length(parts))))[seq_len(4)]
  })

  out <- as.data.frame(do.call(rbind, parsed), stringsAsFactors = FALSE)
  names(out) <- c("row_index", "variable", "strategy", "group")
  out$row_index <- suppressWarnings(as.integer(out$row_index))
  out$strategy[!nzchar(out$strategy)] <- NA_character_
  out$group[!nzchar(out$group)] <- NA_character_
  out
}

build_vbp_variable_choice_data <- function(model) {
  vars <- openqaly::get_variables(model)
  if (nrow(vars) == 0) {
    return(data.frame(
      value = character(0),
      variable = character(0),
      strategy = character(0),
      group = character(0),
      label = character(0),
      stringsAsFactors = FALSE
    ))
  }

  choice_labels <- vapply(seq_len(nrow(vars)), function(i) {
    dn <- if ("display_name" %in% names(vars)) vars$display_name[i] else NA_character_
    if (is.na(dn) || !nzchar(dn)) vars$name[i] else dn
  }, character(1))

  duplicate_labels <- unique(choice_labels[duplicated(choice_labels)])
  if (length(duplicate_labels) > 0) {
    strategies_df <- openqaly::get_strategies(model)
    groups_df <- openqaly::get_groups(model)

    for (dup in duplicate_labels) {
      idx <- which(choice_labels == dup)
      qualifiers <- vapply(idx, function(i) {
        parts <- character(0)
        s <- if ("strategy" %in% names(vars)) vars$strategy[i] else ""
        g <- if ("group" %in% names(vars)) vars$group[i] else ""

        if (!is.na(s) && nzchar(s)) {
          parts <- c(parts, lookup_model_display_name(strategies_df, s))
        }
        if (!is.na(g) && nzchar(g)) {
          parts <- c(parts, lookup_model_display_name(groups_df, g))
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

  strategy_vals <- if ("strategy" %in% names(vars)) vars$strategy else rep("", nrow(vars))
  group_vals <- if ("group" %in% names(vars)) vars$group else rep("", nrow(vars))
  strategy_vals[is.na(strategy_vals)] <- ""
  group_vals[is.na(group_vals)] <- ""
  values <- vapply(seq_len(nrow(vars)), function(i) {
    encode_vbp_variable_choice(
      vars$name[i],
      strategy_vals[i],
      group_vals[i],
      i
    )
  }, character(1))

  data.frame(
    value = values,
    variable = vars$name,
    strategy = strategy_vals,
    group = group_vals,
    label = choice_labels,
    stringsAsFactors = FALSE
  )
}

normalize_vbp_variable_choice <- function(selection) {
  decoded <- decode_vbp_variable_choice(selection)
  if (nrow(decoded) == 0) {
    return(NULL)
  }
  decoded$variable[1]
}

resolve_vbp_variable_choice <- function(model,
                                        variable_name,
                                        intervention_strategy = NULL,
                                        preferred_selection = NULL) {
  choices_df <- build_vbp_variable_choice_data(model)
  if (nrow(choices_df) == 0) {
    return(NULL)
  }

  if (!is.null(preferred_selection) &&
      length(preferred_selection) == 1 &&
      preferred_selection %in% choices_df$value) {
    return(preferred_selection)
  }

  if (is.null(variable_name) || length(variable_name) != 1 || is.na(variable_name) || !nzchar(variable_name)) {
    return(choices_df$value[1])
  }

  matches <- choices_df[choices_df$variable == variable_name, , drop = FALSE]
  if (nrow(matches) == 0) {
    return(choices_df$value[1])
  }

  if (!is.null(intervention_strategy) && length(intervention_strategy) == 1 &&
      !is.na(intervention_strategy) && nzchar(intervention_strategy)) {
    strategy_matches <- matches[!is.na(matches$strategy) &
      nzchar(matches$strategy) &
      matches$strategy == intervention_strategy, , drop = FALSE]
    if (nrow(strategy_matches) > 0) {
      return(strategy_matches$value[1])
    }
  }

  global_matches <- matches[
    (is.na(matches$strategy) | !nzchar(matches$strategy)) &
      (is.na(matches$group) | !nzchar(matches$group)),
    ,
    drop = FALSE
  ]
  if (nrow(global_matches) > 0) {
    return(global_matches$value[1])
  }

  matches$value[1]
}

#' Get Variable Choices
#'
#' Build selectable VBP price-variable choices from model variables.
#'
#' Returns one entry per row in `model$variables`, using display names as
#' labels and unique encoded row selections as values. Duplicate labels are
#' disambiguated with strategy/group qualifiers when available.
#'
#' @param model An openqaly model object.
#'
#' @return A named character vector of variable choices.
#' @keywords internal
get_variable_choices <- function(model) {
  choice_df <- build_vbp_variable_choice_data(model)
  if (nrow(choice_df) == 0) return(character(0))
  stats::setNames(choice_df$value, choice_df$label)
}

get_vbp_group_specific_variable_groups <- function(model, variable_selection) {
  variable_name <- normalize_vbp_variable_choice(variable_selection)
  if (is.null(variable_name) || length(variable_name) != 1 || is.na(variable_name) || !nzchar(variable_name)) {
    return(character(0))
  }

  vars <- openqaly::get_variables(model)
  if (nrow(vars) == 0 || !"group" %in% names(vars)) {
    return(character(0))
  }

  matching_vars <- vars[vars$name == variable_name, , drop = FALSE]
  if (nrow(matching_vars) == 0) {
    return(character(0))
  }

  groups <- unique(matching_vars$group[!is.na(matching_vars$group) & nzchar(matching_vars$group)])
  if (length(groups) == 0) {
    return(character(0))
  }

  groups_df <- openqaly::get_groups(model)
  vapply(groups, function(group_name) {
    lookup_model_display_name(groups_df, group_name)
  }, character(1), USE.NAMES = FALSE)
}

#' Render Flextable as HTML
#'
#' Converts a flextable object to an HTML widget suitable for Shiny renderUI.
#'
#' @param ft A flextable object.
#'
#' @return An htmltools tag object.
#' @keywords internal
render_flextable_html <- function(ft) {
  html_str <- flextable::htmltools_value(ft)
  html_str
}

#' Get DSA Setting Choices
#'
#' Returns a named character vector of valid model settings for DSA analysis.
#'
#' @return A named character vector where names are display labels and values
#'   are setting identifiers.
#' @keywords internal
get_dsa_setting_choices <- function() {
  c(
    "Discount Rate" = "discount_rate",
    "Discount Rate (Cost)" = "discount_cost",
    "Discount Rate (Outcomes)" = "discount_outcomes",
    "Timeframe" = "timeframe",
    "Cycle Length" = "cycle_length",
    "Timeframe Unit" = "timeframe_unit",
    "Cycle Length Unit" = "cycle_length_unit",
    "Half-Cycle Method" = "half_cycle_method",
    "Reduce State Cycle" = "reduce_state_cycle",
    "Days Per Year" = "days_per_year"
  )
}

#' DSA Params Table Dependency
#'
#' Returns the HTML dependency for the DSA parameter table JS and CSS assets.
#'
#' @return An htmltools htmlDependency object.
#' @keywords internal
#' OQGrid Core Dependency
#'
#' Returns the HTML dependency for the shared OQGrid infrastructure.
#' Includes all core files, editors, formatters, actions, helpers, and columns.
#' Individual grid specs are loaded separately via their own dependency functions.
#'
#' @return An htmltools htmlDependency object.
#' @keywords internal
grid_core_dependency <- function() {
  htmltools::htmlDependency(
    name = "oq-grid-core",
    version = "1.0.0",
    src = c(file = system.file("www/grid", package = "openqalyshiny")),
    script = c(
      # Core infrastructure (order matters)
      "core/tabulator-loader.js",
      "core/utils.js",
      "core/shiny-bridge.js",
      "core/handsontable-loader.js",
      # Formatters
      "formatters/formatters.js",
      # Helpers (must load before editors that reference them)
      "helpers/targeting.js",
      "helpers/combo-tracking.js",
      "helpers/settings-exclusions.js",
      "helpers/distribution-registry.js",
      "helpers/distribution-parser.js",
      # Editors
      "editors/formula-editor.js",
      "editors/typeahead-editor.js",
      "editors/multi-tag-editor.js",
      "editors/distribution-editor.js",
      "editors/mv-distribution-editor.js",
      "editors/condition-editor.js",
      "editors/values-popup-editor.js",
      # Actions & columns
      "actions/crud-action-handler.js",
      "actions/sync-action-handler.js",
      "columns/delete-column.js",
      # Controller & factory (must be after editors/helpers)
      "core/grid-controller.js",
      "core/grid-factory.js"
    ),
    stylesheet = "grid.css",
    all_files = FALSE
  )
}

#' Grid Spec Dependency
#'
#' Returns an HTML dependency for a specific grid spec file.
#'
#' @param spec_name The spec filename without extension (e.g., "variables-spec").
#' @return An htmltools htmlDependency object.
#' @keywords internal
grid_spec_dependency <- function(spec_name) {
  htmltools::htmlDependency(
    name = paste0("oq-grid-", spec_name),
    version = "1.0.0",
    src = c(file = system.file("www/grid/specs", package = "openqalyshiny")),
    script = paste0(spec_name, ".js"),
    all_files = FALSE
  )
}

dsa_params_dependency <- function() {
  list(grid_core_dependency(), grid_spec_dependency("dsa-spec"))
}

#' Scenario Parameters Dependency
#'
#' Returns the HTML dependency for the scenario parameters JS and CSS assets.
#'
#' @return An htmltools htmlDependency object.
#' @keywords internal
scenario_params_dependency <- function() {
  list(grid_core_dependency(), grid_spec_dependency("scenario-spec"))
}

#' TWSA Parameters Dependency
#'
#' Returns the HTML dependency for the TWSA parameter table JS and CSS assets.
#'
#' @return An htmltools htmlDependency object.
#' @keywords internal
twsa_params_dependency <- function() {
  list(grid_core_dependency(), grid_spec_dependency("twsa-spec"))
}

#' Apply Scenario Parameters to Model
#'
#' Takes a list of scenario objects from the JS grid and applies them to the model,
#' replacing any existing scenarios.
#'
#' @param model An openqaly model object.
#' @param scenarios A list of scenario objects with name, description,
#'   variable_overrides, and setting_overrides.
#' @return The modified model object.
#' @keywords internal
apply_scenario_params <- function(model, scenarios) {
  # Remove existing scenarios
  if (!is.null(model$scenarios)) {
    for (s in rev(model$scenarios)) {
      model <- openqaly::remove_scenario(model, s$name)
    }
  }
  # Add new scenarios from UI
  for (s in scenarios) {
    model <- openqaly::add_scenario(model, s$name, s$description %||% "")
    var_ovs <- s$variable_overrides
    if (!is.null(var_ovs)) {
      for (v in var_ovs) {
        expr <- rlang::parse_expr(v$value)
        model <- rlang::inject(openqaly::add_scenario_variable(
          model,
          scenario = s$name,
          variable = v$name,
          value = !!expr,
          strategy = v$strategy %||% "",
          group = v$group %||% ""
        ))
      }
    }
    set_ovs <- s$setting_overrides
    if (!is.null(set_ovs)) {
      for (st in set_ovs) {
        val <- st$value
        if (is.character(val)) {
          numeric_val <- suppressWarnings(as.numeric(val))
          if (!is.na(numeric_val)) val <- numeric_val
        }
        model <- openqaly::add_scenario_setting(
          model,
          scenario = s$name,
          setting = st$name,
          value = val
        )
      }
    }
  }
  model
}

#' Variables Table Dependency
#'
#' Returns the HTML dependency for the variables table JS and CSS assets.
#'
#' @return An htmltools htmlDependency object.
#' @keywords internal
variables_table_dependency <- function() {
  list(grid_core_dependency(), grid_spec_dependency("variables-spec"))
}

#' States Table Dependency
#'
#' Returns the HTML dependency for the states table JS and CSS assets.
#'
#' @return An htmltools htmlDependency object.
#' @keywords internal
states_table_dependency <- function() {
  list(grid_core_dependency(), grid_spec_dependency("states-spec"))
}

#' Transitions Table Dependency
#'
#' Returns the HTML dependency for the transitions table JS and CSS assets.
#'
#' @return An htmltools htmlDependency object.
#' @keywords internal
transitions_table_dependency <- function() {
  list(grid_core_dependency(), grid_spec_dependency("transitions-spec"))
}

#' Values Table Dependency
#'
#' Returns the HTML dependency for the values table JS and CSS assets.
#'
#' @return An htmltools htmlDependency object.
#' @keywords internal
values_table_dependency <- function() {
  list(grid_core_dependency(), grid_spec_dependency("values-spec"))
}

#' Summaries Table Dependency
#'
#' Returns the HTML dependency for the summaries table JS and CSS assets.
#'
#' @return An htmltools htmlDependency object.
#' @keywords internal
summaries_table_dependency <- function() {
  list(grid_core_dependency(), grid_spec_dependency("summaries-spec"))
}

#' Groups Table Dependency
#'
#' Returns the HTML dependency for the groups table JS and CSS assets.
#'
#' @return An htmltools htmlDependency object.
#' @keywords internal
groups_table_dependency <- function() {
  list(grid_core_dependency(), grid_spec_dependency("groups-spec"))
}

#' Add Strategy Modal Dependency
#'
#' Returns the HTML dependency for the add strategy modal JS and CSS assets.
#'
#' @return An htmltools htmlDependency object.
#' @keywords internal
add_strategy_modal_dependency <- function() {
  htmltools::htmlDependency(
    name = "add-strategy-modal",
    version = "1.0.0",
    src = c(file = system.file("www", package = "openqalyshiny")),
    script = "add-strategy-modal.js",
    stylesheet = "add-strategy-modal.css",
    all_files = FALSE
  )
}

#' Add Group Modal Dependency
#'
#' Returns the HTML dependency for the add group modal JS and CSS assets.
#'
#' @return An htmltools htmlDependency object.
#' @keywords internal
add_group_modal_dependency <- function() {
  htmltools::htmlDependency(
    name = "add-group-modal",
    version = "1.0.0",
    src = c(file = system.file("www", package = "openqalyshiny")),
    script = "add-group-modal.js",
    stylesheet = "add-group-modal.css",
    all_files = FALSE
  )
}

#' Trees Table Dependency
#'
#' Returns the HTML dependency for the trees table JS and CSS assets.
#'
#' @return An htmltools htmlDependency object.
#' @keywords internal
trees_table_dependency <- function() {
  list(grid_core_dependency(), grid_spec_dependency("trees-spec"))
}

#' Documentation Editor Dependency
#'
#' Returns the HTML dependency for the documentation editor JS and CSS assets.
#'
#' @return An htmltools htmlDependency object.
#' @keywords internal
documentation_editor_dependency <- function() {
  htmltools::htmlDependency(
    name = "documentation-editor",
    version = "1.0.0",
    src = c(file = system.file("www", package = "openqalyshiny")),
    script = c("milkdown-crepe.bundle.js", "documentation-editor.js"),
    stylesheet = c("milkdown-crepe.bundle.css", "documentation-editor.css"),
    all_files = FALSE
  )
}

#' Strategies Table Dependency
#'
#' Returns the HTML dependency for the strategies table JS and CSS assets.
#'
#' @return An htmltools htmlDependency object.
#' @export
strategies_table_dependency <- function() {
  list(grid_core_dependency(), grid_spec_dependency("strategies-spec"))
}

#' Normalize DSA Parameters from Shiny Input
#'
#' Shiny/jsonlite can deserialize a JS array of objects as a data frame,
#' a flat named list (single-element array), or a list of named character
#' vectors. This function normalizes all cases to a list of proper R lists.
#'
#' @param params Raw DSA parameter input from Shiny.
#'
#' @return A list of lists, each representing one DSA parameter.
#' @keywords internal
normalize_dsa_params <- function(params) {
  if (is.null(params) || length(params) == 0) return(list())
  # Data frame -> split into one list per row
  if (is.data.frame(params)) {
    return(lapply(seq_len(nrow(params)), function(i) as.list(params[i, , drop = FALSE])))
  }
  # Named list with "type" key - could be a single param (scalars) or

  # multiple params flattened column-wise (vectors of length > 1)
  if (is.list(params) && !is.null(names(params)) && "type" %in% names(params)) {
    if (length(params$type) > 1) {
      # Column-wise list of vectors -> convert to row-wise list of lists
      n <- length(params$type)
      return(lapply(seq_len(n), function(i) {
        lapply(params, function(col) col[[i]])
      }))
    }
    return(list(as.list(params)))
  }
  # List of params - ensure each element is a proper list (not atomic vector)
  if (is.list(params)) {
    return(lapply(params, as.list))
  }
  # Named atomic vector (Shiny deserializes JS array of objects as a flat
  # character vector with repeated names: type,name,...,type,name,...)
  if (is.atomic(params) && !is.null(names(params)) && "type" %in% names(params)) {
    type_idx <- which(names(params) == "type")
    boundaries <- c(type_idx, length(params) + 1L)
    return(lapply(seq_along(type_idx), function(i) {
      as.list(params[boundaries[i]:(boundaries[i + 1L] - 1L)])
    }))
  }
  # Other unexpected type - cannot normalize
  list()
}

#' Apply DSA Parameters to a Model
#'
#' Takes a list of DSA parameter specifications and adds them to a model
#' using \code{openqaly::add_dsa_variable} or \code{openqaly::add_dsa_setting}.
#'
#' @param model An openqaly model object.
#' @param params A list of DSA parameter lists (as returned by
#'   \code{normalize_dsa_params}).
#'
#' @return The model with DSA parameters added.
#' @keywords internal
apply_dsa_params <- function(model, params) {
  params <- normalize_dsa_params(params)
  for (p in params) {
    if (is.null(p$low) || is.null(p$high) ||
        nchar(trimws(p$low)) == 0 || nchar(trimws(p$high)) == 0) {
      next
    }
    if (p$type == "variable") {
      low_expr <- rlang::parse_expr(p$low)
      high_expr <- rlang::parse_expr(p$high)
      model <- rlang::inject(openqaly::add_dsa_variable(
        model,
        variable = p$name,
        low = !!low_expr,
        high = !!high_expr,
        strategy = p$strategy %||% "",
        group = p$group %||% "",
        display_name = p$display_name
      ))
    } else {
      model <- openqaly::add_dsa_setting(
        model,
        setting = p$name,
        low = as.numeric(p$low),
        high = as.numeric(p$high),
        display_name = p$display_name
      )
    }
  }
  model
}

#' PSA Parameters Dependency
#'
#' Returns the HTML dependency for the PSA parameter table JS and CSS assets.
#'
#' @return An htmltools htmlDependency object.
#' @keywords internal
psa_params_dependency <- function() {
  list(grid_core_dependency(), grid_spec_dependency("psa-spec"))
}

#' Normalize PSA Parameters from Shiny Input
#'
#' Shiny/jsonlite can deserialize a JS array of objects in various formats.
#' This function normalizes all cases to a list of proper R lists, each
#' containing name, strategy, group, and sampling fields.
#'
#' @param params Raw PSA parameter input from Shiny.
#'
#' @return A list of lists, each representing one PSA parameter.
#' @keywords internal
normalize_psa_params <- function(params) {
  if (is.null(params) || length(params) == 0) return(list())
  if (is.data.frame(params)) {
    return(lapply(seq_len(nrow(params)), function(i) as.list(params[i, , drop = FALSE])))
  }
  if (is.list(params) && !is.null(names(params)) && "name" %in% names(params)) {
    if (length(params$name) > 1) {
      n <- length(params$name)
      return(lapply(seq_len(n), function(i) {
        lapply(params, function(col) col[[i]])
      }))
    }
    return(list(as.list(params)))
  }
  if (is.list(params)) {
    return(lapply(params, as.list))
  }
  list()
}

#' Apply PSA Parameters to a Model
#'
#' Takes univariate sampling params and multivariate sampling specs from the
#' UI and applies them to a model. Univariate params update the sampling
#' distribution on each variable. Multivariate specs are removed and re-added.
#'
#' @param model An openqaly model object.
#' @param params A list of univariate PSA parameter lists with name, strategy,
#'   group, and sampling fields.
#' @param multivariate A list of multivariate sampling specs with name,
#'   distribution, variables, and description fields.
#'
#' @return The modified model object.
#' @keywords internal
apply_psa_params <- function(model, params, multivariate = NULL) {
  params <- normalize_psa_params(params)
  for (p in params) {
    sampling_str <- p$sampling %||% ""
    if (!nzchar(trimws(sampling_str))) next
    sampling_expr <- rlang::parse_expr(sampling_str)
    model <- rlang::inject(openqaly::edit_variable(
      model,
      name = p$name,
      strategy = if (nzchar(p$strategy %||% "")) p$strategy else NA_character_,
      group = if (nzchar(p$group %||% "")) p$group else NA_character_,
      sampling = !!sampling_expr
    ))
  }
  # Remove existing multivariate specs and re-add from UI

  if (!is.null(multivariate)) {
    existing <- model$multivariate_sampling
    if (!is.null(existing)) {
      for (spec in rev(existing)) {
        model <- openqaly::remove_multivariate_sampling(model, spec$name)
      }
    }
    for (mv in multivariate) {
      mv_type <- mv$type %||% ""
      if (!nzchar(mv_type)) next
      add_args <- list(
        model,
        name = mv$name,
        type = mv_type,
        variables = as.character(unlist(mv$variables))
      )
      if (nzchar(mv$strategy %||% "")) add_args$strategy <- mv$strategy
      if (nzchar(mv$group %||% "")) add_args$group <- mv$group
      if (mv_type == "dirichlet") {
        if (!is.null(mv$n)) add_args$n <- as.numeric(mv$n)
      } else if (mv_type == "mvnormal") {
        if (!is.null(mv$covariance)) add_args$covariance <- mv$covariance
      }
      model <- do.call(openqaly::add_multivariate_sampling, add_args)
    }
  }
  model
}

#' Threshold Parameters Dependency
#'
#' Returns the HTML dependency for the threshold parameter table JS and CSS assets.
#'
#' @return An htmltools htmlDependency object.
#' @export
threshold_params_dependency <- function() {
  list(grid_core_dependency(), grid_spec_dependency("threshold-spec"))
}

#' Normalize Threshold Parameters from Shiny Input
#'
#' Shiny/jsonlite can deserialize a JS array of objects in various formats.
#' This function normalizes all cases to a list of proper R lists.
#'
#' @param params Raw threshold parameter input from Shiny.
#'
#' @return A list of lists, each representing one threshold analysis.
#' @keywords internal
normalize_threshold_params <- function(params) {
  if (is.null(params) || length(params) == 0) return(list())
  if (is.data.frame(params)) {
    return(lapply(seq_len(nrow(params)), function(i) as.list(params[i, , drop = FALSE])))
  }
  if (is.list(params) && !is.null(names(params)) && "name" %in% names(params)) {
    if (length(params$name) > 1) {
      n <- length(params$name)
      return(lapply(seq_len(n), function(i) {
        lapply(params, function(col) col[[i]])
      }))
    }
    return(list(as.list(params)))
  }
  if (is.list(params)) {
    return(lapply(params, as.list))
  }
  list()
}

#' Apply Threshold Parameters to a Model
#'
#' Takes a list of threshold analysis specifications from the UI and applies
#' them to the model, replacing any existing threshold analyses.
#'
#' @param model An openqaly model object.
#' @param analyses A list of threshold analysis lists with name, variable,
#'   lower, upper, condition, variable_strategy, variable_group, and active.
#' @return The modified model object.
#' @export
apply_threshold_params <- function(model, analyses) {
  analyses <- normalize_threshold_params(analyses)
  # Remove existing threshold analyses
  if (length(model$threshold_analyses) > 0) {
    for (ta in rev(model$threshold_analyses)) {
      model <- openqaly::remove_threshold_analysis(model, ta$name)
    }
  }
  # Add from UI
  for (a in analyses) {
    cond <- a$condition
    if (is.null(cond) || is.null(cond$output)) next

    condition <- switch(cond$output,
      "outcomes" = openqaly::threshold_condition_outcomes(
        summary = if (nzchar(cond$summary %||% "")) cond$summary else NULL,
        value = if (nzchar(cond$value %||% "")) cond$value else NULL,
        type = cond$type %||% "absolute",
        strategy = if ((cond$type %||% "absolute") == "absolute") cond$strategy else NULL,
        referent = if ((cond$type %||% "absolute") == "difference") cond$referent else NULL,
        comparator = if ((cond$type %||% "absolute") == "difference") cond$comparator else NULL,
        discounted = if (is.null(cond$discounted)) TRUE else as.logical(cond$discounted),
        target_value = as.numeric(cond$target_value %||% 0),
        group = cond$group %||% ""
      ),
      "costs" = openqaly::threshold_condition_costs(
        summary = if (nzchar(cond$summary %||% "")) cond$summary else NULL,
        value = if (nzchar(cond$value %||% "")) cond$value else NULL,
        type = cond$type %||% "absolute",
        strategy = if ((cond$type %||% "absolute") == "absolute") cond$strategy else NULL,
        referent = if ((cond$type %||% "absolute") == "difference") cond$referent else NULL,
        comparator = if ((cond$type %||% "absolute") == "difference") cond$comparator else NULL,
        discounted = if (is.null(cond$discounted)) TRUE else as.logical(cond$discounted),
        target_value = as.numeric(cond$target_value %||% 0),
        group = cond$group %||% ""
      ),
      "nmb" = openqaly::threshold_condition_nmb(
        health_summary = cond$health_summary,
        cost_summary = cond$cost_summary,
        referent = cond$referent,
        comparator = cond$comparator,
        discounted = if (is.null(cond$discounted)) TRUE else as.logical(cond$discounted),
        target_value = as.numeric(cond$target_value %||% 0),
        group = cond$group %||% "",
        wtp = if (!is.null(cond$wtp) && nzchar(cond$wtp)) as.numeric(cond$wtp) else NULL
      ),
      "ce" = openqaly::threshold_condition_ce(
        health_summary = cond$health_summary,
        cost_summary = cond$cost_summary,
        referent = cond$referent,
        comparator = cond$comparator,
        discounted = if (is.null(cond$discounted)) TRUE else as.logical(cond$discounted),
        group = cond$group %||% "",
        wtp = if (!is.null(cond$wtp) && nzchar(cond$wtp)) as.numeric(cond$wtp) else NULL
      ),
      "trace" = openqaly::threshold_condition_trace(
        state = cond$state,
        time = as.numeric(cond$time),
        time_unit = cond$time_unit %||% "cycle",
        type = cond$type %||% "absolute",
        strategy = if ((cond$type %||% "absolute") == "absolute") cond$strategy else NULL,
        referent = if ((cond$type %||% "absolute") == "difference") cond$referent else NULL,
        comparator = if ((cond$type %||% "absolute") == "difference") cond$comparator else NULL,
        target_value = as.numeric(cond$target_value),
        group = cond$group %||% ""
      )
    )
    if (is.null(condition)) next

    model <- openqaly::add_threshold_analysis(
      model,
      name = a$name,
      variable = a$variable,
      lower = as.numeric(a$lower),
      upper = as.numeric(a$upper),
      condition = condition,
      variable_strategy = a$variable_strategy %||% "",
      variable_group = a$variable_group %||% "",
      active = if (is.null(a$active)) TRUE else as.logical(a$active)
    )
  }
  model
}
