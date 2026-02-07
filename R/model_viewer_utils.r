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
