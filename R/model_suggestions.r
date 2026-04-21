#' Model-Aware Formula Suggestions
#'
#' Functions for extracting context-appropriate terms and suggestions from
#' openqaly models for use with \code{\link{formulaInput}}.
#'
#' @name model_suggestions
#' @importFrom jsonlite toJSON
NULL

# -----------------------------------------------------------------------------
# Internal keyword definitions (verified against openqaly namespaces)
# -----------------------------------------------------------------------------

# Time keywords (always available in formula contexts)
.time_keywords <- c(
  "cycle", "day", "week", "month", "year",
  "cycle_lag", "day_lag", "week_lag", "month_lag", "year_lag"
)

# Cycle length keywords (always available)
.cycle_length_keywords <- c(
  "cycle_length_days", "cycle_length_weeks",
  "cycle_length_months", "cycle_length_years"
)

# Time unit constants (always available)
.time_unit_keywords <- c("days_per_year", "days_per_month")

# Segment keywords (always available)
.segment_keywords <- c("group", "strategy")

# State time keywords (Markov models only)
.state_time_keywords <- c(
  "state_cycle", "state_day", "state_week", "state_month", "state_year",
  "state_cycle_lag", "state_day_lag", "state_week_lag",
  "state_month_lag", "state_year_lag"
)

# Base keywords (without state time)
.base_keywords <- c(
  .time_keywords, .cycle_length_keywords, .time_unit_keywords, .segment_keywords
)

# Keyword descriptions for suggestions
.keyword_descriptions <- list(
  cycle = "Current cycle number (0-indexed)",
  day = "Time at end of cycle in days",
  week = "Time at end of cycle in weeks",

  month = "Time at end of cycle in months",
  year = "Time at end of cycle in years",
  cycle_lag = "Time at start of cycle (cycles)",
  day_lag = "Time at start of cycle in days",
  week_lag = "Time at start of cycle in weeks",
  month_lag = "Time at start of cycle in months",
  year_lag = "Time at start of cycle in years",
  state_cycle = "Cycles in current state",
  state_day = "Days in current state (end of cycle)",
  state_week = "Weeks in current state (end of cycle)",
  state_month = "Months in current state (end of cycle)",
  state_year = "Years in current state (end of cycle)",
  state_cycle_lag = "Cycles in current state (start of cycle)",
  state_day_lag = "Days in current state (start of cycle)",
  state_week_lag = "Weeks in current state (start of cycle)",
  state_month_lag = "Months in current state (start of cycle)",
  state_year_lag = "Years in current state (start of cycle)",
  cycle_length_days = "Cycle length in days",
  cycle_length_weeks = "Cycle length in weeks",
  cycle_length_months = "Cycle length in months",
  cycle_length_years = "Cycle length in years",
  days_per_year = "Days per year (365)",
  days_per_month = "Days per month (30.4167)",
  group = "Current patient group",
  strategy = "Current treatment strategy",
  bc = "Base case value of the parameter being varied"
)

# Context-to-component mapping
.context_components <- list(
  variable = list(
    keywords = "oq_vars",
    components = c("variables", "tables", "trees")
  ),
  transition_markov = list(
    keywords = "oq_all",
    components = c("variables", "tables", "trees")
  ),
  transition_psm = list(
    keywords = "oq_all",
    components = c("variables", "tables")
  ),
  transition_custom_psm = list(
    keywords = "oq_all",
    components = c("variables", "tables")
  ),
  value = list(
    keywords = "oq_all",
    components = c("variables", "tables", "values")
  ),
  initial_probability = list(
    keywords = "none",
    components = c("variables", "tables")
  ),
  group_weight = list(
    keywords = "none",
    components = c("variables", "tables")
  ),
  variable_sampling = list(
    keywords = "bc_only",
    components = c("variables", "tables")
  ),
  multivariate_sampling = list(
    keywords = "bc_only",
    components = c("variables", "tables")
  ),
  dsa_bound = list(
    keywords = "bc_only",
    components = c("variables", "tables")
  ),
  scenario_override = list(
    keywords = "bc_only",
    components = c("variables", "tables")
  ),
  twsa_bound = list(
    keywords = "bc_only",
    components = c("variables", "tables")
  ),
  override = list(
    keywords = "bc_only",
    components = c("variables", "tables")
  ),
  tree_node = list(
    keywords = "oq_all",
    components = c("variables", "tables")
  )
)

# -----------------------------------------------------------------------------
# Exported functions
# -----------------------------------------------------------------------------

#' Get Model Terms for Syntax Highlighting
#'
#' Extracts terms from an openqaly model for syntax highlighting in formula
#' inputs, filtered by the formula context.
#'
#' @param model An openqaly model object (\code{oq_model} or
#'   \code{oq_model_builder})
#' @param context Character string specifying the formula context. Valid values:
#'   \itemize{
#'     \item \code{"variable"}: Variable formula (add_variable formula param)
#'     \item \code{"variable_sampling"}: Variable sampling distribution
#'     \item \code{"transition_markov"}: Markov transition formula
#'     \item \code{"transition_psm"}: PSM transition formula
#'     \item \code{"transition_custom_psm"}: Custom PSM state probability
#'     \item \code{"value"}: Value formula
#'     \item \code{"initial_probability"}: State initial probability
#'     \item \code{"group_weight"}: Group weight formula
#'     \item \code{"multivariate_sampling"}: Multivariate sampling distribution
#'     \item \code{"dsa_bound"}: DSA low/high bounds
#'     \item \code{"scenario_override"}: Scenario variable override
#'     \item \code{"twsa_bound"}: TWSA bounds
#'     \item \code{"override"}: Override expression
#'     \item \code{"tree_node"}: Decision tree node formula
#'   }
#'
#' @return A named list suitable for the \code{terms} parameter of
#'   \code{\link{formulaInput}}. Contains character vectors for each token
#'   type: \code{keyword}, \code{variable}, \code{table}, \code{value},
#'   \code{tree} (as applicable to the context).
#'
#' @examples
#' \dontrun{
#' # Get terms for a variable formula context
#' terms <- get_model_terms(my_model, "variable")
#'
#' # Use in formulaInput
#' formulaInput("formula1", terms = terms)
#' }
#'
#' @seealso \code{\link{get_model_suggestions}} for autocomplete suggestions,
#'   \code{\link{formulaInput}} for the input component
#'
#' @export
get_model_terms <- function(model, context) {
  .validate_model(model)
  .validate_context(context)

  context_spec <- .context_components[[context]]
  model_type <- .get_model_type(model)

  terms <- list()

  # Add keywords based on context
  keywords <- .get_keywords_for_context(context_spec$keywords, model_type)
  if (length(keywords) > 0) {
    terms$keyword <- keywords
  }

  # Add model components based on context
  components <- context_spec$components

  if ("variables" %in% components) {
    var_names <- .extract_variable_names(model)
    if (length(var_names) > 0) {
      terms$variable <- var_names
    }
  }

  if ("tables" %in% components) {
    table_names <- .extract_table_names(model)
    if (length(table_names) > 0) {
      terms$table <- table_names
    }
  }

  if ("values" %in% components) {
    value_names <- .extract_value_names(model)
    if (length(value_names) > 0) {
      terms$value <- value_names
    }
  }

  if ("trees" %in% components) {
    tree_names <- .extract_tree_names(model)
    if (length(tree_names) > 0) {
      terms$tree <- tree_names
    }
  }

  terms
}

#' Get Model Suggestions for Autocomplete
#'
#' Extracts suggestions from an openqaly model for autocomplete in formula
#' inputs, filtered by the formula context.
#'
#' @param model An openqaly model object (\code{oq_model} or
#'   \code{oq_model_builder})
#' @param context Character string specifying the formula context. See
#'   \code{\link{get_model_terms}} for valid values.
#' @param include_r_functions Logical. If TRUE (default), includes R function
#'   suggestions from loaded packages.
#' @param r_packages Character vector of package names to include R functions
#'   from. If NULL (default), uses c("base", "stats").
#'
#' @return A named list suitable for the \code{suggestions} parameter of
#'   \code{\link{formulaInput}}. Each element is a data frame with columns:
#'   \code{name}, \code{label}, \code{description}, \code{signature},
#'   \code{package} (for R functions) or similar metadata.
#'
#' @examples
#' \dontrun{
#' # Get suggestions for a variable formula context
#' suggestions <- get_model_suggestions(my_model, "variable")
#'
#' # Use in formulaInput
#' formulaInput("formula1", suggestions = suggestions)
#'
#' # Without R function suggestions
#' suggestions <- get_model_suggestions(my_model, "variable",
#'                                       include_r_functions = FALSE)
#'
#' # With specific R packages
#' suggestions <- get_model_suggestions(my_model, "variable",
#'                                       r_packages = c("base", "stats", "utils"))
#' }
#'
#' @seealso \code{\link{get_model_terms}} for syntax highlighting,
#'   \code{\link{get_r_function_suggestions}} for R function extraction,
#'   \code{\link{formulaInput}} for the input component
#'
#' @export
get_model_suggestions <- function(model, context, include_r_functions = TRUE,
                                   r_packages = NULL) {
  .validate_model(model)
  .validate_context(context)

  context_spec <- .context_components[[context]]
  model_type <- .get_model_type(model)

  suggestions <- list()

  # Add R function suggestions if requested
  if (include_r_functions) {
    r_funcs <- get_r_function_suggestions(r_packages)
    if (nrow(r_funcs) > 0) {
      suggestions$r_function <- r_funcs
    }
  }

  # Add keyword suggestions based on context
  keywords <- .get_keywords_for_context(context_spec$keywords, model_type)
  if (length(keywords) > 0) {
    suggestions$keyword <- .build_keyword_suggestions(keywords)
  }

  # Add model component suggestions based on context
  components <- context_spec$components

  if ("variables" %in% components) {
    var_suggestions <- .build_variable_suggestions(model)
    if (nrow(var_suggestions) > 0) {
      suggestions$variable <- var_suggestions
    }
  }

  if ("tables" %in% components) {
    table_suggestions <- .build_table_suggestions(model)
    if (nrow(table_suggestions) > 0) {
      suggestions$table <- table_suggestions
    }
  }

  if ("values" %in% components) {
    value_suggestions <- .build_value_suggestions(model)
    if (nrow(value_suggestions) > 0) {
      suggestions$value <- value_suggestions
    }
  }

  if ("trees" %in% components) {
    tree_suggestions <- .build_tree_suggestions(model)
    if (nrow(tree_suggestions) > 0) {
      suggestions$tree <- tree_suggestions
    }
  }

  suggestions
}

# -----------------------------------------------------------------------------
# Internal helper functions
# -----------------------------------------------------------------------------

#' Validate model object
#' @keywords internal
.validate_model <- function(model) {
  if (!inherits(model, c("oq_model", "oq_model_builder"))) {
    stop("model must be an oq_model or oq_model_builder object")
  }
}

#' Validate context parameter
#' @keywords internal
.validate_context <- function(context) {
  valid_contexts <- names(.context_components)
  if (!context %in% valid_contexts) {
    stop(
      "Invalid context '", context, "'. Valid contexts are: ",
      paste(valid_contexts, collapse = ", ")
    )
  }
}

#' Get model type from model object
#' @keywords internal
.get_model_type <- function(model) {
  openqaly::get_model_type(model)
}

#' Get keywords for a specific context and model type
#' @keywords internal
.get_keywords_for_context <- function(keyword_set, model_type) {
  if (keyword_set == "none") {
    return(character(0))
  }

  if (keyword_set == "bc_only") {
    return("bc")
  }

  # oq_vars or oq_all - start with base keywords
  keywords <- .base_keywords

  # Add state time keywords for Markov models
  if (model_type == "markov") {
    keywords <- c(keywords, .state_time_keywords)
  }

  keywords
}

#' Build keyword suggestions data frame
#' @keywords internal
.build_keyword_suggestions <- function(keywords) {
  data.frame(
    name = keywords,
    label = keywords,
    description = vapply(keywords, function(k) {
      .keyword_descriptions[[k]] %||% ""
    }, character(1), USE.NAMES = FALSE),
    signature = character(length(keywords)),
    stringsAsFactors = FALSE
  )
}

#' Extract variable names from model
#' @keywords internal
.extract_variable_names <- function(model) {
  openqaly::get_variable_names(model)
}

#' Extract table names from model
#' @keywords internal
.extract_table_names <- function(model) {
  openqaly::get_table_names(model)
}

#' Extract value names from model
#' @keywords internal
.extract_value_names <- function(model) {
  openqaly::get_model_value_names(model)
}

#' Extract tree names from model
#' @keywords internal
.extract_tree_names <- function(model) {
  openqaly::get_tree_names(model)
}

#' Build variable suggestions with strategy/group-specific display
#' @keywords internal
.build_variable_suggestions <- function(model) {
  vars <- openqaly::get_variables(model)
  if (nrow(vars) == 0) {
    return(.empty_suggestions_df())
  }

  var_names <- openqaly::get_variable_names(model)

  if (length(var_names) == 0) {
    return(.empty_suggestions_df())
  }

  do.call(rbind, lapply(var_names, function(vname) {
    var_rows <- vars[vars$name == vname, , drop = FALSE]
    .build_variable_entry(vname, var_rows)
  }))
}

#' Build a single variable suggestion entry
#' @keywords internal
.build_variable_entry <- function(var_name, var_rows) {
  # Check for strategy/group-specific variants
  has_strategy <- "strategy" %in% names(var_rows) &&
    any(!is.na(var_rows$strategy) & nzchar(var_rows$strategy))
  has_group <- "group" %in% names(var_rows) &&
    any(!is.na(var_rows$group) & nzchar(var_rows$group))
  is_stratified <- nrow(var_rows) > 1 && (has_strategy || has_group)

  if (is_stratified) {
    # Stratified variable - show variants in tooltip
    description <- .build_stratified_description(var_rows)
    signature <- ""
  } else {
    # Single definition - show description and formula in tooltip
    # Filter out auto-generated junk descriptions like "var_name, strategy"
    description <- ""
    if ("description" %in% names(var_rows) &&
        !is.na(var_rows$description[1]) &&
        nzchar(var_rows$description[1])) {
      desc <- var_rows$description[1]
      if (!grepl(paste0("^", var_name, ","), desc)) {
        description <- desc
      }
    }

    # Use formula as signature
    signature <- ""
    if ("formula" %in% names(var_rows) && !is.na(var_rows$formula[1])) {
      signature <- as.character(var_rows$formula[1])
    }
  }

  data.frame(
    name = var_name,
    label = var_name,  # Always use name - that's what goes in the formula
    description = description,
    signature = signature,
    stringsAsFactors = FALSE
  )
}

#' Build stratified variable description showing all variants
#' @keywords internal
.build_stratified_description <- function(var_rows) {
  sections <- character(0)

  for (i in seq_len(nrow(var_rows))) {
    row <- var_rows[i, ]

    # Build context header (strategy/group)
    context_parts <- character(0)
    if ("strategy" %in% names(row) && !is.na(row$strategy) && nzchar(row$strategy)) {
      context_parts <- c(context_parts, row$strategy)
    }
    if ("group" %in% names(row) && !is.na(row$group) && nzchar(row$group)) {
      context_parts <- c(context_parts, row$group)
    }
    context_header <- if (length(context_parts) > 0) {
      paste(context_parts, collapse = "/")
    } else {
      "default"
    }

    # Build section with display_name, description, formula
    section_lines <- paste0("[", context_header, "]")

    if ("display_name" %in% names(row) && !is.na(row$display_name) &&
        nzchar(row$display_name)) {
      section_lines <- c(section_lines, row$display_name)
    }

    if ("description" %in% names(row) && !is.na(row$description) &&
        nzchar(row$description)) {
      section_lines <- c(section_lines, row$description)
    }

    if ("formula" %in% names(row) && !is.na(row$formula)) {
      section_lines <- c(section_lines, paste0("= ", row$formula))
    }

    sections <- c(sections, paste(section_lines, collapse = "\n"))
  }

  paste(sections, collapse = "\n\n")
}

#' Build table suggestions
#' @keywords internal
.build_table_suggestions <- function(model) {
  tables <- openqaly::get_tables(model)
  if (length(tables) == 0) {
    return(.empty_suggestions_df())
  }

  table_names <- names(tables)

  do.call(rbind, lapply(table_names, function(tname) {
    tbl_entry <- tables[[tname]]

    # Get table data - handle both list format and direct data.frame
    tbl_data <- NULL
    if (is.list(tbl_entry) && "data" %in% names(tbl_entry)) {
      tbl_data <- tbl_entry$data
    } else if (is.data.frame(tbl_entry)) {
      tbl_data <- tbl_entry
    }

    # Build description with dimensions
    description <- ""
    if (!is.null(tbl_data) && is.data.frame(tbl_data)) {
      dims <- paste0(nrow(tbl_data), " rows x ", ncol(tbl_data), " cols")
      cols <- paste(names(tbl_data), collapse = ", ")
      description <- paste0(dims, "\nColumns: ", cols)
    }

    # Build signature with data preview (first 3 rows)
    signature <- ""
    if (!is.null(tbl_data) && is.data.frame(tbl_data) && nrow(tbl_data) > 0) {
      n_preview <- min(3, nrow(tbl_data))
      header <- paste(names(tbl_data), collapse = " | ")
      preview_lines <- c(header, paste(rep("-", nchar(header)), collapse = ""))

      for (i in seq_len(n_preview)) {
        row_vals <- vapply(tbl_data[i, ], function(x) {
          if (is.numeric(x)) format(x, digits = 3, nsmall = 0) else as.character(x)
        }, character(1))
        preview_lines <- c(preview_lines, paste(row_vals, collapse = " | "))
      }

      if (nrow(tbl_data) > n_preview) {
        preview_lines <- c(preview_lines, "...")
      }
      signature <- paste(preview_lines, collapse = "\n")
    }

    data.frame(
      name = tname,
      label = tname,
      description = description,
      signature = signature,
      stringsAsFactors = FALSE
    )
  }))
}

#' Build value suggestions
#' @keywords internal
.build_value_suggestions <- function(model) {
  vals <- openqaly::get_model_values(model)
  if (nrow(vals) == 0) {
    return(.empty_suggestions_df())
  }

  value_names <- openqaly::get_model_value_names(model)

  if (length(value_names) == 0) {
    return(.empty_suggestions_df())
  }

  do.call(rbind, lapply(value_names, function(vname) {
    val_rows <- vals[vals$name == vname, , drop = FALSE]
    val_row <- val_rows[1, ]

    display_name <- if ("display_name" %in% names(val_row) &&
                        !is.na(val_row$display_name)) {
      val_row$display_name
    } else {
      vname
    }

    description <- if ("description" %in% names(val_row) &&
                       !is.na(val_row$description)) {
      val_row$description
    } else {
      ""
    }

    # Add type info if available
    if ("type" %in% names(val_row) && !is.na(val_row$type) &&
        val_row$type != "") {
      description <- paste0("[", val_row$type, "] ", description)
    }

    formula_text <- if ("formula" %in% names(val_row) &&
                        !is.na(val_row$formula)) {
      val_row$formula
    } else {
      ""
    }

    data.frame(
      name = vname,
      label = display_name,
      description = description,
      signature = formula_text,
      stringsAsFactors = FALSE
    )
  }))
}

#' Build tree suggestions
#' @keywords internal
.build_tree_suggestions <- function(model) {
  trees <- openqaly::get_trees(model)
  if (is.null(trees) || !is.data.frame(trees) || nrow(trees) == 0) {
    return(.empty_suggestions_df())
  }

  tree_names <- openqaly::get_tree_names(model)

  if (length(tree_names) == 0) {
    return(.empty_suggestions_df())
  }

  do.call(rbind, lapply(tree_names, function(tname) {
    tree_rows <- trees[trees$name == tname, , drop = FALSE]

    # Count nodes
    n_nodes <- nrow(tree_rows)

    # Get root node tags if available
    root_tags <- ""
    if ("tags" %in% names(tree_rows) && "parent" %in% names(tree_rows)) {
      root_rows <- tree_rows[is.na(tree_rows$parent) | tree_rows$parent == "", ]
      if (nrow(root_rows) > 0 && !is.na(root_rows$tags[1])) {
        root_tags <- root_rows$tags[1]
      }
    }

    description <- paste0("Decision tree with ", n_nodes, " nodes")
    signature <- paste0("p(", tname, ")")

    data.frame(
      name = tname,
      label = paste0(tname, " (tree)"),
      description = description,
      signature = signature,
      stringsAsFactors = FALSE
    )
  }))
}

#' Create empty suggestions data frame
#' @keywords internal
.empty_suggestions_df <- function() {
  data.frame(
    name = character(0),
    label = character(0),
    description = character(0),
    signature = character(0),
    stringsAsFactors = FALSE
  )
}

#' Null-coalescing operator
#' @keywords internal
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
