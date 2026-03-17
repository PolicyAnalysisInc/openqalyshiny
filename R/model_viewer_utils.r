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

#' Get Variable Choices
#'
#' Extract unique variable names from a model object, filtered to only include
#' global (unsegmented) variables — those not defined for a specific strategy
#' or group. This is used for VBP price variable selection, which requires
#' variables that apply across all strategies and groups.
#'
#' @param model An openqaly model object.
#'
#' @return A character vector of variable names.
#' @keywords internal
get_variable_choices <- function(model) {
  global_vars <- openqaly::get_global_variables(model)
  if (nrow(global_vars) == 0) return(character(0))
  unique(global_vars$name)
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
dsa_params_dependency <- function() {
  htmltools::htmlDependency(
    name = "dsa-params",
    version = "3.0.0",
    src = c(file = system.file("www", package = "openqalyshiny")),
    script = "dsa-params.js",
    stylesheet = "dsa-params.css",
    all_files = FALSE
  )
}

#' Variables Table Dependency
#'
#' Returns the HTML dependency for the variables table JS and CSS assets.
#'
#' @return An htmltools htmlDependency object.
#' @keywords internal
variables_table_dependency <- function() {
  htmltools::htmlDependency(
    name = "variables-table",
    version = "1.0.0",
    src = c(file = system.file("www", package = "openqalyshiny")),
    script = "variables-table.js",
    stylesheet = "variables-table.css",
    all_files = FALSE
  )
}

#' States Table Dependency
#'
#' Returns the HTML dependency for the states table JS and CSS assets.
#'
#' @return An htmltools htmlDependency object.
#' @keywords internal
states_table_dependency <- function() {
  htmltools::htmlDependency(
    name = "states-table",
    version = "1.0.0",
    src = c(file = system.file("www", package = "openqalyshiny")),
    script = "states-table.js",
    stylesheet = "states-table.css",
    all_files = FALSE
  )
}

#' Transitions Table Dependency
#'
#' Returns the HTML dependency for the transitions table JS and CSS assets.
#'
#' @return An htmltools htmlDependency object.
#' @keywords internal
transitions_table_dependency <- function() {
  htmltools::htmlDependency(
    name = "transitions-table",
    version = "1.0.0",
    src = c(file = system.file("www", package = "openqalyshiny")),
    script = "transitions-table.js",
    stylesheet = "transitions-table.css",
    all_files = FALSE
  )
}

#' Values Table Dependency
#'
#' Returns the HTML dependency for the values table JS and CSS assets.
#'
#' @return An htmltools htmlDependency object.
#' @keywords internal
values_table_dependency <- function() {
  htmltools::htmlDependency(
    name = "values-table",
    version = "1.0.0",
    src = c(file = system.file("www", package = "openqalyshiny")),
    script = "values-table.js",
    stylesheet = "values-table.css",
    all_files = FALSE
  )
}

#' Summaries Table Dependency
#'
#' Returns the HTML dependency for the summaries table JS and CSS assets.
#'
#' @return An htmltools htmlDependency object.
#' @keywords internal
summaries_table_dependency <- function() {
  htmltools::htmlDependency(
    name = "summaries-table",
    version = "1.0.0",
    src = c(file = system.file("www", package = "openqalyshiny")),
    script = "summaries-table.js",
    stylesheet = "summaries-table.css",
    all_files = FALSE
  )
}

#' Groups Table Dependency
#'
#' Returns the HTML dependency for the groups table JS and CSS assets.
#'
#' @return An htmltools htmlDependency object.
#' @keywords internal
groups_table_dependency <- function() {
  htmltools::htmlDependency(
    name = "groups-table",
    version = "1.0.0",
    src = c(file = system.file("www", package = "openqalyshiny")),
    script = "groups-table.js",
    stylesheet = "groups-table.css",
    all_files = FALSE
  )
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
  htmltools::htmlDependency(
    name = "trees-table",
    version = "1.0.0",
    src = c(file = system.file("www", package = "openqalyshiny")),
    script = "trees-table.js",
    stylesheet = "trees-table.css",
    all_files = FALSE
  )
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
  htmltools::htmlDependency(
    name = "strategies-table",
    version = "1.0.0",
    src = c(file = system.file("www", package = "openqalyshiny")),
    script = "strategies-table.js",
    stylesheet = "strategies-table.css",
    all_files = FALSE
  )
}
