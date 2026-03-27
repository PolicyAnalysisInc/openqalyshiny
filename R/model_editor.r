#' Dispatch a model action to the appropriate openqaly reducer
#'
#' @param model The current model object
#' @param action A list with \code{type} (string) and action-specific payload fields
#' @return The updated model
#' @keywords internal
# Safely extract a scalar string from action fields.
# Shiny's JSON parser converts "" to list(), not "".
.str <- function(x) if (is.character(x) && length(x) == 1) x else ""

.resolve_variable_target <- function(model, name, strategy = "", group = "") {
  vars <- openqaly::get_variables(model)
  if (is.null(vars) || nrow(vars) == 0) {
    stop(sprintf("No variable found matching: %s", name), call. = FALSE)
  }

  strategy <- .str(strategy)
  group <- .str(group)

  match_mask <- vars$name == name
  match_mask <- match_mask &
    if (nzchar(strategy)) {
      !is.na(vars$strategy) & vars$strategy == strategy
    } else {
      is.na(vars$strategy) | vars$strategy == ""
    }
  match_mask <- match_mask &
    if (nzchar(group)) {
      !is.na(vars$group) & vars$group == group
    } else {
      is.na(vars$group) | vars$group == ""
    }

  match_idx <- which(match_mask)
  if (length(match_idx) == 0) {
    target_desc <- name
    if (nzchar(strategy)) target_desc <- paste0(target_desc, ", strategy='", strategy, "'")
    if (nzchar(group)) target_desc <- paste0(target_desc, ", group='", group, "'")
    stop(sprintf("No variable found matching: %s", target_desc), call. = FALSE)
  }

  row <- vars[match_idx[1], , drop = FALSE]
  list(
    strategy = if (is.na(row$strategy[[1]])) NA_character_ else row$strategy[[1]],
    group = if (is.na(row$group[[1]])) NA_character_ else row$group[[1]]
  )
}

.validate_multivariate_distribution <- function(distribution) {
  distribution <- trimws(distribution %||% "")
  if (!nzchar(distribution)) {
    return(list(valid = FALSE, message = "Distribution is required."))
  }

  expr <- tryCatch(
    rlang::parse_expr(distribution),
    error = function(e) e
  )
  if (inherits(expr, "error")) {
    return(list(valid = FALSE, message = "Distribution must be a valid R expression."))
  }

  if (!rlang::is_call(expr)) {
    return(list(valid = FALSE, message = "Distribution must be a function call."))
  }

  fn_name <- rlang::call_name(expr)
  if (!fn_name %in% c("dirichlet", "mvnormal", "multinomial")) {
    return(list(
      valid = FALSE,
      message = "Distribution must use dirichlet(), mvnormal(), or multinomial()."
    ))
  }

  args <- rlang::call_args(expr)
  arg_names <- names(args) %||% rep("", length(args))
  arg_names[arg_names == ""] <- NA_character_

  if (fn_name == "dirichlet") {
    if (length(args) != 1) {
      return(list(valid = FALSE, message = "dirichlet() requires exactly one alpha argument."))
    }
    if (!all(is.na(arg_names) | arg_names == "alpha")) {
      return(list(valid = FALSE, message = "dirichlet() only accepts an alpha argument."))
    }
  }

  if (fn_name == "multinomial") {
    has_size <- "size" %in% arg_names
    has_prob <- "prob" %in% arg_names
    if (all(is.na(arg_names))) {
      if (length(args) != 2) {
        return(list(valid = FALSE, message = "multinomial() requires size and prob."))
      }
    } else if (!(has_size && has_prob)) {
      return(list(valid = FALSE, message = "multinomial() requires size and prob."))
    }
  }

  if (fn_name == "mvnormal") {
    has_mean <- "mean" %in% arg_names
    has_sd <- "sd" %in% arg_names
    has_cor <- "cor" %in% arg_names
    has_cov <- "cov" %in% arg_names

    if (all(is.na(arg_names))) {
      if (length(args) < 2 || length(args) > 4) {
        return(list(
          valid = FALSE,
          message = "mvnormal() requires mean plus either cov or sd/cor."
        ))
      }
    } else {
      if (!has_mean) {
        return(list(valid = FALSE, message = "mvnormal() requires a mean argument."))
      }
      if (!(has_cov || (has_sd && has_cor))) {
        return(list(
          valid = FALSE,
          message = "mvnormal() requires either cov or both sd and cor."
        ))
      }
    }
  }

  list(valid = TRUE, message = NULL)
}

dispatch_model_action <- function(model, action) {
  switch(action$type,
    "edit_variable" = {
      if (action$field %in% c("strategy", "group")) {
        # Cannot change targeting via edit_variable — must remove + re-add
        old_strategy <- if (nzchar(.str(action$strategy))) action$strategy else ""
        old_group <- if (nzchar(.str(action$group))) action$group else ""

        # Get current variable data before removing
        vars <- openqaly::get_variables(model)
        match_idx <- which(
          vars$name == action$name &
          (if (nzchar(old_strategy)) vars$strategy == old_strategy else (is.na(vars$strategy) | vars$strategy == "")) &
          (if (nzchar(old_group)) vars$group == old_group else (is.na(vars$group) | vars$group == ""))
        )
        if (length(match_idx) == 0) stop("Variable not found")
        var_row <- vars[match_idx[1], ]

        # Remove old
        model <- openqaly::remove_variable(
          model, name = action$name,
          strategy = if (nzchar(old_strategy)) old_strategy else NULL,
          group = if (nzchar(old_group)) old_group else NULL
        )

        # Re-add with new targeting
        new_strategy <- if (action$field == "strategy") (action$value %||% "") else old_strategy
        new_group <- if (action$field == "group") (action$value %||% "") else old_group

        add_args <- list(model = model, name = action$name)
        if (nzchar(new_strategy)) add_args$strategy <- new_strategy
        if (nzchar(new_group)) add_args$group <- new_group
        if (!is.na(var_row$formula) && nzchar(var_row$formula)) {
          add_args$formula <- rlang::parse_expr(as.character(var_row$formula))
        }
        if ("display_name" %in% names(var_row) && !is.na(var_row$display_name) && nzchar(var_row$display_name)) {
          add_args$display_name <- var_row$display_name
        }
        if ("description" %in% names(var_row) && !is.na(var_row$description) && nzchar(var_row$description)) {
          add_args$description <- var_row$description
        }
        rlang::inject(openqaly::add_variable(!!!add_args))
      } else {
        args <- list(
          model = model,
          name = action$name,
          strategy = if (nzchar(.str(action$strategy))) action$strategy else NA_character_,
          group = if (nzchar(.str(action$group))) action$group else NA_character_
        )
        field <- action$field
        value <- action$value
        if (field == "formula") {
          args$formula <- rlang::parse_expr(value)
        } else if (field == "name") {
          args$new_name <- value
        } else {
          args[[field]] <- value
        }
        rlang::inject(openqaly::edit_variable(!!!args))
      }
    },
    "add_variable" = {
      args <- list(model = model, name = action$name)
      if (nzchar(action$formula %||% "")) {
        args$formula <- rlang::parse_expr(action$formula)
      }
      if (nzchar(action$display_name %||% "")) {
        args$display_name <- action$display_name
      }
      if (nzchar(action$description %||% "")) {
        args$description <- action$description
      }
      if (nzchar(.str(action$strategy))) args$strategy <- action$strategy
      if (nzchar(.str(action$group))) args$group <- action$group
      rlang::inject(openqaly::add_variable(!!!args))
    },
    "remove_variable" = {
      openqaly::remove_variable(
        model,
        name = action$name,
        strategy = if (nzchar(.str(action$strategy))) action$strategy else NULL,
        group = if (nzchar(.str(action$group))) action$group else NULL
      )
    },
    "add_table" = {
      openqaly::add_table(model, name = action$name, data = action$data,
                          description = action$description)
    },
    "edit_table" = {
      openqaly::edit_table(model, name = action$name, data = action$data)
    },
    "edit_table_meta" = {
      args <- list(model = model, name = action$name)
      if (!is.null(action$new_name)) args$new_name <- action$new_name
      if (!is.null(action$description)) args$description <- action$description
      rlang::inject(openqaly::edit_table(!!!args))
    },
    "remove_table" = {
      openqaly::remove_table(model, name = action$name)
    },
    "add_script" = {
      openqaly::add_script(model, name = action$name, code = action$code,
                           description = action$description)
    },
    "edit_script" = {
      args <- list(model = model, name = action$name)
      if (!is.null(action$code)) args$code <- action$code
      if (!is.null(action$description)) args$description <- action$description
      if (!is.null(action$new_name)) args$new_name <- action$new_name
      rlang::inject(openqaly::edit_script(!!!args))
    },
    "remove_script" = {
      openqaly::remove_script(model, name = action$name)
    },
    "add_group" = {
      args <- list(model = model, name = action$name)
      if (nzchar(action$display_name %||% "")) args$display_name <- action$display_name
      if (nzchar(action$description %||% "")) args$description <- action$description
      args$weight <- action$weight %||% "1"
      args$enabled <- action$enabled %||% 1
      rlang::inject(openqaly::add_group(!!!args))
    },
    "edit_group" = {
      args <- list(model = model, name = action$name)
      field <- action$field
      value <- action$value
      if (field == "name") {
        args$new_name <- value
      } else {
        args[[field]] <- value
      }
      rlang::inject(openqaly::edit_group(!!!args))
    },
    "remove_group" = {
      openqaly::remove_group(model, name = action$name, error_on_dependencies = TRUE)
    },
    "force_remove_group" = {
      openqaly::remove_group(model, name = action$name, error_on_dependencies = FALSE)
    },
    "add_strategy" = {
      args <- list(model = model, name = action$name)
      if (nzchar(action$display_name %||% "")) args$display_name <- action$display_name
      if (nzchar(action$description %||% "")) args$description <- action$description
      args$enabled <- action$enabled %||% 1
      rlang::inject(openqaly::add_strategy(!!!args))
    },
    "edit_strategy" = {
      args <- list(model = model, name = action$name)
      field <- action$field
      value <- action$value
      if (field == "name") {
        args$new_name <- value
      } else {
        args[[field]] <- value
      }
      rlang::inject(openqaly::edit_strategy(!!!args))
    },
    "remove_strategy" = {
      openqaly::remove_strategy(model, name = action$name, error_on_dependencies = TRUE)
    },
    "force_remove_strategy" = {
      openqaly::remove_strategy(model, name = action$name, error_on_dependencies = FALSE)
    },
    "set_settings" = {
      args <- c(list(model = model), action$settings)
      rlang::inject(openqaly::set_settings(!!!args))
    },
    "add_state" = {
      args <- list(model = model, name = action$name)
      if (nzchar(action$display_name %||% "")) args$display_name <- action$display_name
      if (nzchar(action$description %||% "")) args$description <- action$description
      model_type <- openqaly::get_model_type(model)
      if (model_type == "markov") {
        if (nzchar(action$initial_probability %||% "")) {
          args$initial_prob <- rlang::parse_expr(action$initial_probability)
        }
        if (nzchar(action$state_group %||% "")) args$state_group <- action$state_group
        args$share_state_time <- identical(action$share_state_time, TRUE) ||
          identical(action$share_state_time, "Yes")
        if (nzchar(action$state_cycle_limit %||% "")) {
          args$state_cycle_limit <- as.numeric(action$state_cycle_limit)
        }
        if (nzchar(action$state_cycle_limit_unit %||% "")) {
          args$state_cycle_limit_unit <- action$state_cycle_limit_unit
        }
      }
      rlang::inject(openqaly::add_state(!!!args))
    },
    "edit_state" = {
      args <- list(model = model, name = action$name)
      field <- action$field
      value <- action$value
      if (field == "name") {
        args$new_name <- value
      } else if (field == "initial_probability") {
        args$initial_prob <- rlang::parse_expr(value)
      } else if (field == "share_state_time") {
        args$share_state_time <- identical(value, "Yes") || identical(value, TRUE)
      } else if (field == "state_cycle_limit") {
        args$state_cycle_limit <- if (nzchar(value %||% "")) as.numeric(value) else Inf
      } else {
        args[[field]] <- value
      }
      rlang::inject(openqaly::edit_state(!!!args))
    },
    "remove_state" = {
      openqaly::remove_state(model, name = action$name, error_on_dependencies = TRUE)
    },
    "force_remove_state" = {
      openqaly::remove_state(model, name = action$name, error_on_dependencies = FALSE)
    },
    "add_transition" = {
      model_type <- action$model_type %||% openqaly::get_model_type(model)
      if (model_type == "markov") {
        openqaly::add_transition(
          model,
          from_state = action$from_state,
          to_state = action$to_state,
          formula = rlang::parse_expr(action$formula)
        )
      } else if (model_type == "psm") {
        openqaly::add_psm_transition(
          model,
          endpoint = action$endpoint,
          time_unit = action$time_unit,
          formula = rlang::parse_expr(action$formula)
        )
      } else if (model_type == "custom_psm") {
        openqaly::add_custom_psm_transition(
          model,
          state = action$state,
          formula = rlang::parse_expr(action$formula)
        )
      } else {
        stop("Unsupported model type for transitions: ", model_type)
      }
    },
    "edit_transition" = {
      model_type <- action$model_type %||% openqaly::get_model_type(model)
      if (model_type == "markov") {
        args <- list(model = model, from_state = action$from_state, to_state = action$to_state)
        if (action$field == "formula") {
          args$formula <- rlang::parse_expr(action$value)
        }
        rlang::inject(openqaly::edit_transition(!!!args))
      } else if (model_type == "psm") {
        args <- list(model = model, endpoint = action$endpoint)
        if (action$field == "formula") {
          args$formula <- rlang::parse_expr(action$value)
        } else if (action$field == "time_unit") {
          args$time_unit <- action$value
        }
        rlang::inject(openqaly::edit_psm_transition(!!!args))
      } else if (model_type == "custom_psm") {
        args <- list(model = model, state = action$state)
        if (action$field == "formula") {
          args$formula <- rlang::parse_expr(action$value)
        }
        rlang::inject(openqaly::edit_custom_psm_transition(!!!args))
      }
    },
    "remove_transition" = {
      model_type <- action$model_type %||% openqaly::get_model_type(model)
      if (model_type == "markov") {
        openqaly::remove_transition(model, from_state = action$from_state, to_state = action$to_state)
      } else if (model_type == "psm") {
        openqaly::remove_psm_transition(model, endpoint = action$endpoint)
      } else if (model_type == "custom_psm") {
        openqaly::remove_custom_psm_transition(model, state = action$state)
      }
    },
    "add_value" = {
      args <- list(model = model, name = action$name)
      if (nzchar(action$formula %||% "")) {
        args$formula <- rlang::parse_expr(action$formula)
      }
      if (nzchar(action$state %||% "")) args$state <- action$state
      if (nzchar(action$destination %||% "")) args$destination <- action$destination
      if (nzchar(action$value_type %||% "")) args$type <- action$value_type
      if (nzchar(action$display_name %||% "")) args$display_name <- action$display_name
      if (nzchar(action$description %||% "")) args$description <- action$description
      if (nzchar(action$discounting_override %||% "")) {
        args$discounting_override <- rlang::parse_expr(action$discounting_override)
      }
      rlang::inject(openqaly::add_value(!!!args))
    },
    "edit_value" = {
      old_state <- action$state %||% ""
      old_dest <- action$destination %||% ""
      field <- action$field

      if (field %in% c("state", "destination")) {
        # Remove + re-add pattern for state/destination changes
        vals <- openqaly::get_model_values(model)
        match_idx <- which(
          vals$name == action$name &
          (if (nzchar(old_state)) vals$state == old_state else (is.na(vals$state) | vals$state == "")) &
          (if (nzchar(old_dest)) vals$destination == old_dest else (is.na(vals$destination) | vals$destination == ""))
        )
        if (length(match_idx) == 0) stop("Value not found")
        val_row <- vals[match_idx[1], ]

        # Remove old
        model <- openqaly::remove_value(
          model, name = action$name,
          state = if (nzchar(old_state)) old_state else NULL,
          destination = if (nzchar(old_dest)) old_dest else NULL,
          error_on_dependencies = FALSE
        )

        # Re-add with updated field
        new_state <- if (field == "state") (action$value %||% "") else old_state
        new_dest <- if (field == "destination") (action$value %||% "") else old_dest

        add_args <- list(model = model, name = action$name)
        if (nzchar(new_state)) add_args$state <- new_state
        if (nzchar(new_dest)) add_args$destination <- new_dest
        if (!is.na(val_row$formula) && nzchar(as.character(val_row$formula))) {
          add_args$formula <- rlang::parse_expr(as.character(val_row$formula))
        }
        if ("type" %in% names(val_row) && !is.na(val_row$type) && nzchar(val_row$type)) {
          add_args$type <- val_row$type
        }
        if ("display_name" %in% names(val_row) && !is.na(val_row$display_name) && nzchar(val_row$display_name)) {
          add_args$display_name <- val_row$display_name
        }
        if ("description" %in% names(val_row) && !is.na(val_row$description) && nzchar(val_row$description)) {
          add_args$description <- val_row$description
        }
        if ("discounting_override" %in% names(val_row) && !is.na(val_row$discounting_override) && nzchar(as.character(val_row$discounting_override))) {
          add_args$discounting_override <- rlang::parse_expr(as.character(val_row$discounting_override))
        }
        rlang::inject(openqaly::add_value(!!!add_args))
      } else {
        args <- list(
          model = model,
          name = action$name,
          state = if (nzchar(old_state)) old_state else NA_character_,
          destination = if (nzchar(old_dest)) old_dest else NA_character_
        )
        if (field == "formula") {
          args$formula <- rlang::parse_expr(action$value)
        } else if (field == "name") {
          args$new_name <- action$value
          if (isTRUE(action$error_on_name_sharing)) args$error_on_name_sharing <- TRUE
          if (isTRUE(action$error_on_field_changes)) args$error_on_field_changes <- TRUE
          if (isTRUE(action$rename_all)) args$rename_all <- TRUE
        } else if (field == "discounting_override") {
          if (nzchar(action$value %||% "")) {
            args$discounting_override <- rlang::parse_expr(action$value)
          } else {
            args$discounting_override <- NA
          }
        } else {
          args[[field]] <- action$value
        }
        rlang::inject(openqaly::edit_value(!!!args))
      }
    },
    "remove_value" = {
      openqaly::remove_value(
        model,
        name = action$name,
        state = if (nzchar(action$state %||% "")) action$state else NULL,
        destination = if (nzchar(action$destination %||% "")) action$destination else NULL,
        error_on_dependencies = TRUE
      )
    },
    "force_remove_value" = {
      openqaly::remove_value(
        model,
        name = action$name,
        state = if (nzchar(action$state %||% "")) action$state else NULL,
        destination = if (nzchar(action$destination %||% "")) action$destination else NULL,
        error_on_dependencies = FALSE
      )
    },
    "rename_value_single" = {
      openqaly::edit_value(
        model,
        name = action$name,
        state = if (nzchar(action$state %||% "")) action$state else NA_character_,
        destination = if (nzchar(action$destination %||% "")) action$destination else NA_character_,
        new_name = action$new_name,
        error_on_name_sharing = FALSE,
        error_on_field_changes = TRUE
      )
    },
    "rename_value_all" = {
      openqaly::edit_value(
        model,
        name = action$name,
        state = if (nzchar(action$state %||% "")) action$state else NA_character_,
        destination = if (nzchar(action$destination %||% "")) action$destination else NA_character_,
        new_name = action$new_name,
        rename_all = TRUE
      )
    },
    "add_summary" = {
      args <- list(model = model, name = action$name, values = action$values)
      if (nzchar(action$display_name %||% "")) args$display_name <- action$display_name
      if (nzchar(action$description %||% "")) args$description <- action$description
      if (nzchar(action$summary_type %||% "")) args$type <- action$summary_type
      if (nzchar(action$wtp %||% "")) args$wtp <- as.numeric(action$wtp)
      rlang::inject(openqaly::add_summary(!!!args))
    },
    "edit_summary" = {
      args <- list(model = model, name = action$name)
      field <- action$field
      value <- action$value
      if (field == "name") {
        args$new_name <- value
      } else if (field == "type") {
        args$type <- value
      } else if (field == "wtp") {
        args$wtp <- if (nzchar(value %||% "")) as.numeric(value) else NA_real_
      } else if (field == "values") {
        args$values <- value
      } else {
        args[[field]] <- value
      }
      rlang::inject(openqaly::edit_summary(!!!args))
    },
    "remove_summary" = {
      openqaly::remove_summary(model, name = action$name, error_on_dependencies = TRUE)
    },
    "force_remove_summary" = {
      openqaly::remove_summary(model, name = action$name, error_on_dependencies = FALSE)
    },
    "add_tree_node" = {
      args <- list(model = model, tree_name = action$tree_name, node = action$node)
      if (nzchar(action$parent %||% "")) args$parent <- action$parent
      if (nzchar(action$formula %||% "")) {
        args$formula <- rlang::parse_expr(action$formula)
      }
      if (nzchar(action$tags %||% "")) args$tags <- action$tags
      rlang::inject(openqaly::add_tree_node(!!!args))
    },
    "edit_tree_node" = {
      args <- list(model = model, tree_name = action$tree_name, node = action$node)
      field <- action$field
      value <- action$value
      if (field == "node") {
        args$new_node_name <- value
      } else if (field == "tree_name") {
        args$new_tree_name <- value
      } else if (field == "formula") {
        args$formula <- rlang::parse_expr(value)
      } else {
        args[[field]] <- value
      }
      rlang::inject(openqaly::edit_tree_node(!!!args))
    },
    "remove_tree_node" = {
      openqaly::remove_tree_node(model, tree_name = action$tree_name, node = action$node)
    },
    "remove_tree" = {
      openqaly::remove_tree(model, tree_name = action$tree_name)
    },
    "edit_decision_tree" = {
      args <- list(model = model)
      if (!is.null(action$duration)) {
        args$duration <- rlang::parse_expr(action$duration)
      }
      if (!is.null(action$duration_unit)) args$duration_unit <- action$duration_unit
      rlang::inject(openqaly::edit_decision_tree(!!!args))
    },
    "set_decision_tree" = {
      args <- list(model = model, tree_name = action$tree_name)
      if (nzchar(action$duration %||% "")) {
        args$duration <- rlang::parse_expr(action$duration)
      }
      if (nzchar(action$duration_unit %||% "")) {
        args$duration_unit <- action$duration_unit
      }
      rlang::inject(openqaly::set_decision_tree(!!!args))
    },
    "remove_decision_tree" = {
      openqaly::remove_decision_tree(model)
    },
    "set_documentation" = {
      openqaly::set_documentation(model, text = action$text)
    },
    "set_override_categories" = {
      openqaly::set_override_categories(model, action$categories)
    },
    "add_override_category" = {
      openqaly::add_override_category(
        model, name = action$name, general = isTRUE(action$general)
      )
    },
    "edit_override_category" = {
      args <- list(model = model, name = action$name)
      if (!is.null(action$new_name)) args$new_name <- action$new_name
      if (!is.null(action$general)) args$general <- action$general
      rlang::inject(openqaly::edit_override_category(!!!args))
    },
    "remove_override_category" = {
      openqaly::remove_override_category(model, name = action$name)
    },
    "add_override" = {
      args <- list(
        model = model,
        category = action$category,
        title = action$title,
        name = action$name,
        type = action$override_type %||% "variable",
        input_type = action$input_type %||% "numeric",
        expression = action$expression %||% "0",
        description = action$description,
        strategy = .str(action$strategy),
        group = .str(action$group)
      )
      if (!is.null(action$min)) args$min <- action$min
      if (!is.null(action$max)) args$max <- action$max
      if (!is.null(action$step_size)) args$step_size <- action$step_size
      if (!is.null(action$options)) args$options <- action$options
      rlang::inject(openqaly::add_override(!!!args))
    },
    "edit_override" = {
      args <- list(
        model = model,
        category = action$category,
        type = action$override_type,
        name = action$name,
        strategy = .str(action$strategy),
        group = .str(action$group)
      )
      if (!is.null(action$new_type)) args$new_type <- action$new_type
      if (!is.null(action$new_name)) args$new_name <- action$new_name
      if (!is.null(action$new_strategy)) args$new_strategy <- action$new_strategy
      if (!is.null(action$new_group)) args$new_group <- action$new_group
      if (!is.null(action$title)) args$title <- action$title
      if (!is.null(action$description)) args$description <- action$description
      if (!is.null(action$expression)) args$expression <- action$expression
      if (!is.null(action$input_type)) args$input_type <- action$input_type
      if (!is.null(action$min)) args$min <- action$min
      if (!is.null(action$max)) args$max <- action$max
      if (!is.null(action$step_size)) args$step_size <- action$step_size
      if (!is.null(action$options)) args$options <- action$options
      rlang::inject(openqaly::edit_override(!!!args))
    },
    "remove_override" = {
      openqaly::remove_override(
        model,
        category = action$category,
        type = action$override_type,
        name = action$name,
        strategy = .str(action$strategy),
        group = .str(action$group)
      )
    },
    "add_scenario" = {
      openqaly::add_scenario(model,
        name = action$name,
        description = action$description
      )
    },
    "edit_scenario" = {
      openqaly::edit_scenario(model,
        name = action$name,
        new_name = action$new_name,
        description = action$description
      )
    },
    "remove_scenario" = {
      openqaly::remove_scenario(model, name = action$name)
    },
    "set_vbp" = {
      openqaly::set_vbp(model,
        price_variable = action$price_variable,
        intervention_strategy = action$intervention_strategy,
        outcome_summary = action$outcome_summary,
        cost_summary = action$cost_summary
      )
    },
    "clear_vbp" = {
      openqaly::remove_vbp(model)
    },
    # DSA individual actions
    "add_dsa_variable" = {
      low_expr <- rlang::parse_expr(action$low)
      high_expr <- rlang::parse_expr(action$high)
      rlang::inject(openqaly::add_dsa_variable(model,
        variable = action$variable, low = !!low_expr, high = !!high_expr,
        strategy = .str(action$strategy), group = .str(action$group),
        display_name = action$display_name
      ))
    },
    "edit_dsa_variable" = {
      args <- list(model, variable = action$variable,
        strategy = .str(action$strategy), group = .str(action$group))
      if (!is.null(action$new_variable)) args$new_variable <- action$new_variable
      if (!is.null(action$new_strategy)) args$new_strategy <- action$new_strategy
      if (!is.null(action$new_group)) args$new_group <- action$new_group
      if (!is.null(action$low)) args$low <- rlang::parse_expr(action$low)
      if (!is.null(action$high)) args$high <- rlang::parse_expr(action$high)
      if (!is.null(action$display_name)) args$display_name <- action$display_name
      do.call(openqaly::edit_dsa_variable, args)
    },
    "remove_dsa_variable" = {
      openqaly::remove_dsa_variable(model, variable = action$variable,
        strategy = .str(action$strategy), group = .str(action$group))
    },
    "add_dsa_setting" = {
      openqaly::add_dsa_setting(model, setting = action$setting,
        low = action$low, high = action$high,
        display_name = action$display_name)
    },
    "edit_dsa_setting" = {
      args <- list(model, setting = action$setting)
      if (!is.null(action$new_setting)) args$new_setting <- action$new_setting
      if (!is.null(action$low)) args$low <- action$low
      if (!is.null(action$high)) args$high <- action$high
      if (!is.null(action$display_name)) args$display_name <- action$display_name
      do.call(openqaly::edit_dsa_setting, args)
    },
    "remove_dsa_setting" = {
      openqaly::remove_dsa_setting(model, setting = action$setting)
    },
    # Scenario individual actions
    "add_scenario_variable" = {
      expr <- rlang::parse_expr(action$value)
      rlang::inject(openqaly::add_scenario_variable(model,
        scenario = action$scenario, variable = action$variable,
        value = !!expr,
        strategy = .str(action$strategy),
        group = .str(action$group)))
    },
    "edit_scenario_variable" = {
      args <- list(model, scenario = action$scenario,
        variable = action$variable,
        strategy = .str(action$strategy),
        group = .str(action$group))
      if (!is.null(action$new_variable)) args$new_variable <- action$new_variable
      if (!is.null(action$new_strategy)) args$new_strategy <- .str(action$new_strategy)
      if (!is.null(action$new_group)) args$new_group <- .str(action$new_group)
      if (!is.null(action$value)) {
        args$value <- rlang::parse_expr(action$value)
      }
      do.call(openqaly::edit_scenario_variable, args)
    },
    "remove_scenario_variable" = {
      openqaly::remove_scenario_variable(model, scenario = action$scenario,
        variable = action$variable,
        strategy = .str(action$strategy),
        group = .str(action$group))
    },
    "add_scenario_setting" = {
      val <- action$value
      if (is.character(val)) {
        nv <- suppressWarnings(as.numeric(val))
        if (!is.na(nv)) val <- nv
      }
      openqaly::add_scenario_setting(model, scenario = action$scenario,
        setting = action$setting, value = val)
    },
    "edit_scenario_setting" = {
      args <- list(model, scenario = action$scenario, setting = action$setting)
      if (!is.null(action$new_setting)) args$new_setting <- action$new_setting
      if (!is.null(action$value)) {
        val <- action$value
        if (is.character(val)) {
          nv <- suppressWarnings(as.numeric(val))
          if (!is.na(nv)) val <- nv
        }
        args$value <- val
      }
      do.call(openqaly::edit_scenario_setting, args)
    },
    "remove_scenario_setting" = {
      openqaly::remove_scenario_setting(model, scenario = action$scenario,
        setting = action$setting)
    },
    # TWSA analysis-level actions
    "add_twsa" = {
      openqaly::add_twsa(model,
        name = action$name,
        description = action$description
      )
    },
    "edit_twsa" = {
      openqaly::edit_twsa(model,
        name = action$name,
        new_name = action$new_name,
        description = action$description
      )
    },
    "remove_twsa" = {
      openqaly::remove_twsa(model, name = action$name)
    },
    # TWSA variable parameter actions
    "add_twsa_variable" = {
      args <- list(model = model, twsa_name = action$twsa_name,
        variable = action$variable, type = action$method_type,
        strategy = .str(action$strategy), group = .str(action$group))
      if (!is.null(action$display_name)) args$display_name <- action$display_name
      if (!is.null(action$include_base_case)) args$include_base_case <- action$include_base_case
      if (!is.null(action$steps)) args$steps <- as.integer(action$steps)
      if (!is.null(action$min)) args$min <- rlang::parse_expr(action$min)
      if (!is.null(action$max)) args$max <- rlang::parse_expr(action$max)
      if (!is.null(action$radius)) args$radius <- rlang::parse_expr(action$radius)
      if (!is.null(action$values)) args$values <- rlang::parse_expr(action$values)
      rlang::inject(openqaly::add_twsa_variable(!!!args))
    },
    "edit_twsa_variable" = {
      args <- list(model, twsa_name = action$twsa_name,
        variable = action$variable,
        strategy = .str(action$strategy), group = .str(action$group))
      if (!is.null(action$new_variable)) args$new_variable <- action$new_variable
      if (!is.null(action$new_strategy)) args$new_strategy <- .str(action$new_strategy)
      if (!is.null(action$new_group)) args$new_group <- .str(action$new_group)
      if (!is.null(action$method_type)) args$type <- action$method_type
      if (!is.null(action$steps)) args$steps <- as.integer(action$steps)
      if (!is.null(action$display_name)) args$display_name <- action$display_name
      if (!is.null(action$include_base_case)) args$include_base_case <- action$include_base_case
      if (!is.null(action$min) && nzchar(action$min)) args$min <- rlang::parse_expr(action$min)
      if (!is.null(action$max) && nzchar(action$max)) args$max <- rlang::parse_expr(action$max)
      if (!is.null(action$radius) && nzchar(action$radius)) args$radius <- rlang::parse_expr(action$radius)
      if (!is.null(action$values) && nzchar(action$values)) args$values <- rlang::parse_expr(action$values)
      do.call(openqaly::edit_twsa_variable, args)
    },
    "remove_twsa_variable" = {
      openqaly::remove_twsa_variable(model, twsa_name = action$twsa_name,
        variable = action$variable,
        strategy = .str(action$strategy), group = .str(action$group))
    },
    # TWSA setting parameter actions
    "add_twsa_setting" = {
      args <- list(model = model, twsa_name = action$twsa_name,
        setting = action$setting, type = action$method_type)
      if (!is.null(action$display_name)) args$display_name <- action$display_name
      if (!is.null(action$include_base_case)) args$include_base_case <- action$include_base_case
      if (!is.null(action$steps)) args$steps <- as.integer(action$steps)
      if (!is.null(action$min)) args$min <- as.numeric(action$min)
      if (!is.null(action$max)) args$max <- as.numeric(action$max)
      if (!is.null(action$radius)) args$radius <- as.numeric(action$radius)
      if (!is.null(action$values)) args$values <- as.numeric(unlist(action$values))
      do.call(openqaly::add_twsa_setting, args)
    },
    "edit_twsa_setting" = {
      args <- list(model, twsa_name = action$twsa_name,
        setting = action$setting)
      if (!is.null(action$new_setting)) args$new_setting <- action$new_setting
      if (!is.null(action$method_type)) args$type <- action$method_type
      if (!is.null(action$steps)) args$steps <- as.integer(action$steps)
      if (!is.null(action$display_name)) args$display_name <- action$display_name
      if (!is.null(action$include_base_case)) args$include_base_case <- action$include_base_case
      if (!is.null(action$min)) args$min <- as.numeric(action$min)
      if (!is.null(action$max)) args$max <- as.numeric(action$max)
      if (!is.null(action$radius)) args$radius <- as.numeric(action$radius)
      if (!is.null(action$values)) args$values <- as.numeric(unlist(action$values))
      do.call(openqaly::edit_twsa_setting, args)
    },
    "remove_twsa_setting" = {
      openqaly::remove_twsa_setting(model, twsa_name = action$twsa_name,
        setting = action$setting)
    },
    # PSA individual actions
    "edit_variable_sampling" = {
      target <- .resolve_variable_target(
        model,
        action$variable,
        strategy = .str(action$strategy),
        group = .str(action$group)
      )
      if (!is.null(action$sampling) && nzchar(action$sampling)) {
        sampling_expr <- rlang::parse_expr(action$sampling)
        rlang::inject(openqaly::edit_variable(model,
          name = action$variable,
          strategy = target$strategy,
          group = target$group,
          sampling = !!sampling_expr
        ))
      } else {
        openqaly::edit_variable(model,
          name = action$variable,
          strategy = target$strategy,
          group = target$group,
          sampling = ""
        )
      }
    },
    "rename_variable_sampling" = {
      old_target <- .resolve_variable_target(
        model,
        action$variable,
        strategy = .str(action$strategy),
        group = .str(action$group)
      )
      new_target <- .resolve_variable_target(
        model,
        action$new_variable,
        strategy = .str(action$new_strategy),
        group = .str(action$new_group)
      )
      # Remove sampling from old variable/strategy/group combo
      m <- openqaly::edit_variable(model,
        name = action$variable,
        strategy = old_target$strategy,
        group = old_target$group,
        sampling = ""
      )
      # Set sampling on new variable/strategy/group combo
      if (!is.null(action$sampling) && nzchar(action$sampling)) {
        sampling_expr <- rlang::parse_expr(action$sampling)
        rlang::inject(openqaly::edit_variable(m,
          name = action$new_variable,
          strategy = new_target$strategy,
          group = new_target$group,
          sampling = !!sampling_expr
        ))
      } else {
        openqaly::edit_variable(m,
          name = action$new_variable,
          strategy = new_target$strategy,
          group = new_target$group,
          sampling = ""
        )
      }
    },
    "remove_variable_sampling" = {
      target <- .resolve_variable_target(
        model,
        action$variable,
        strategy = .str(action$strategy),
        group = .str(action$group)
      )
      openqaly::edit_variable(model,
        name = action$variable,
        strategy = target$strategy,
        group = target$group,
        sampling = ""
      )
    },
    "set_psa_settings" = {
      args <- list(model)
      if (!is.null(action$n_sim)) args$n_sim <- as.integer(action$n_sim)
      if (!is.null(action$seed) && nzchar(action$seed)) {
        args$seed <- as.integer(action$seed)
      } else {
        args$seed <- NULL
      }
      do.call(openqaly::set_psa, args)
    },
    "add_multivariate_sampling" = {
      if (nzchar(action$distribution %||% "")) {
        dist_expr <- rlang::parse_expr(action$distribution)
        rlang::inject(openqaly::add_multivariate_sampling(model,
          name = action$name,
          distribution = !!dist_expr,
          variables = action$variables,
          description = action$description %||% ""
        ))
      } else {
        model
      }
    },
    "edit_multivariate_sampling" = {
      args <- list(model, name = action$name)
      if (!is.null(action$new_name)) args$new_name <- action$new_name
      if (!is.null(action$distribution) && nzchar(action$distribution)) {
        args$distribution <- rlang::parse_expr(action$distribution)
      }
      if (!is.null(action$variables)) args$variables <- action$variables
      if (!is.null(action$description)) args$description <- action$description
      do.call(openqaly::edit_multivariate_sampling, args)
    },
    "remove_multivariate_sampling" = {
      openqaly::remove_multivariate_sampling(model, name = action$name)
    },
    stop("Unknown action type: ", action$type)
  )
}

#' Create a reactive that only invalidates downstream when its value changes
#'
#' Uses \code{rlang::hash()} to compare new vs previous values. If the hash
#' matches, returns the cached previous value (same reference), preventing
#' downstream reactive invalidation from propagating to renderers.
#'
#' @param expr An expression to evaluate reactively
#' @return A reactive that only propagates changes when the hash differs
#' @keywords internal
hashed_reactive <- function(expr) {
  expr <- rlang::enquo(expr)
  last_hash <- NULL
  last_value <- NULL
  shiny::reactive({
    new_value <- rlang::eval_tidy(expr)
    new_hash <- rlang::hash(new_value)
    if (!identical(new_hash, last_hash)) {
      last_hash <<- new_hash
      last_value <<- new_value
    }
    last_value
  })
}

scripts_editor_dependency <- function() {
  htmltools::htmlDependency(
    name = "scripts-editor",
    version = "1.0.0",
    src = c(file = system.file("www", package = "openqalyshiny")),
    script = "scripts-editor.js",
    stylesheet = "scripts-editor.css",
    all_files = FALSE
  )
}

#' Model Editor UI
#'
#' @description A Shiny application for editing openqaly models.
#'
#' @param path Optional path to a model file (.json, .yml, .yaml) to load on
#'   startup. If \code{NULL} (default), no model is loaded until the user
#'   opens one via the UI.
#' @keywords internal
create_history_manager <- function(max_size = 5) {
  rv <- shiny::reactiveValues(past = list(), future = list())

  list(
    push = function(old_model) {
      rv$past <- c(tail(rv$past, max_size - 1), list(old_model))
      rv$future <- list()
    },
    undo = function(current_model) {
      n <- length(rv$past)
      if (n == 0) return(NULL)
      restored <- rv$past[[n]]
      rv$past <- rv$past[-n]
      rv$future <- c(rv$future, list(current_model))
      restored
    },
    redo = function(current_model) {
      n <- length(rv$future)
      if (n == 0) return(NULL)
      restored <- rv$future[[n]]
      rv$future <- rv$future[-n]
      rv$past <- c(rv$past, list(current_model))
      restored
    },
    can_undo = function() length(rv$past) > 0,
    can_redo = function() length(rv$future) > 0,
    clear = function() {
      rv$past <- list()
      rv$future <- list()
    }
  )
}

#' Launch the model editor Shiny app
#'
#' @param path Optional path to a model file (.json, .yml, .yaml) to load on
#'   startup. If \code{NULL} (default), no model is loaded until the user
#'   opens one via the UI.
#' @param options A list of options passed to shiny::shinyApp().
#' @return A Shiny app object
#' @export
run_model_editor <- function(path = NULL, options = list()) {
  ui <- bslib::page_fillable(
    padding = 0,
    htmltools::htmlDependency(
      name = "ace-editor",
      version = "1.32.6",
      src = c(href = "https://cdnjs.cloudflare.com/ajax/libs/ace/1.32.6"),
      script = c("ace.min.js", "mode-r.min.js", "theme-chrome.min.js",
                 "ext-language_tools.min.js"),
      all_files = FALSE
    ),
    scripts_editor_dependency(),
    documentation_editor_dependency(),
    tags$head(
      tags$script(shiny::HTML("
        // Ctrl+S / Cmd+S save shortcut
        document.addEventListener('keydown', function(e) {
          if ((e.ctrlKey || e.metaKey) && e.key === 's') {
            e.preventDefault();
            Shiny.setInputValue('save_file', Math.random(), {priority: 'event'});
          }
        });
        // Dirty state tracking for beforeunload
        Shiny.addCustomMessageHandler('set_dirty', function(dirty) {
          window._modelDirty = dirty;
        });
        window.addEventListener('beforeunload', function(e) {
          if (window._modelDirty) {
            e.preventDefault();
            e.returnValue = '';
          }
        });
        // Version history initial selection (registered globally to avoid duplicate registration)
        Shiny.addCustomMessageHandler('set_initial_version', function(version) {
          var attempts = 0;
          function trySet() {
            attempts++;
            var items = document.querySelectorAll('.version-item');
            if (items.length > 0) {
              Shiny.setInputValue('version_select', version, {priority: 'event'});
            } else if (attempts < 100) {
              requestAnimationFrame(trySet);
            }
          }
          requestAnimationFrame(trySet);
        });
      ")),
      tags$style(shiny::HTML("
        body {
          overflow: hidden;
        }
        .app-bar {
          display: flex;
          align-items: center;
          background-color: #2c3e50;
          color: white;
          padding: 0 16px;
          height: 48px;
          flex-shrink: 0;
        }
        .app-bar-title {
          font-size: 18px;
          font-weight: 600;
          margin-right: 24px;
        }
        .app-bar .dropdown .btn {
          color: white;
          background: transparent;
          border: none;
        }
        .app-bar .dropdown .btn:hover,
        .app-bar .dropdown .btn:focus {
          background: rgba(255,255,255,0.1);
        }
        .no-model-message {
          display: flex;
          align-items: center;
          justify-content: center;
          height: 100%;
          color: #999;
          font-size: 18px;
        }
        [data-display-if='output.model_loaded'] {
          flex: 1 1 auto;
          min-height: 0;
          display: flex;
          flex-direction: column;
        }
        .tab-pane.active > .shiny-html-output {
          flex: 1 1 auto;
          display: flex;
          flex-direction: column;
          min-height: 0;
        }
        .model-content {
          padding: 16px;
          overflow-x: auto; overflow-y: hidden;
          flex: 1 1 auto;
          min-height: 0;
          display: flex;
          flex-direction: column;
        }
        .model-content > div,
        .model-content .tab-content,
        .model-content .tab-pane.active {
          flex: 1 1 auto;
          display: flex;
          flex-direction: column;
          min-height: 0;
        }
        .tables-tab-container {
          display: flex;
          flex-direction: row;
          flex: 1 1 auto;
          min-height: 0;
        }
        .tables-sidebar {
          width: 220px;
          min-width: 220px;
          border-right: 1px solid #dee2e6;
          overflow-y: auto;
          padding: 8px;
        }
        .tables-sidebar-item {
          display: flex;
          align-items: center;
          padding: 6px 8px;
          cursor: pointer;
          border-radius: 4px;
          margin-bottom: 2px;
        }
        .tables-sidebar-item:hover {
          background-color: #f0f0f0;
        }
        .tables-sidebar-item.active {
          background-color: #e2e6ea;
          font-weight: 600;
        }
        .tables-sidebar-item .table-name {
          flex-grow: 1;
          overflow: hidden;
          text-overflow: ellipsis;
          white-space: nowrap;
        }
        .tables-sidebar-item .table-actions {
          display: flex;
          gap: 4px;
          margin-left: 4px;
        }
        .tables-sidebar-item .table-actions .btn {
          padding: 0 4px;
          font-size: 12px;
          line-height: 1;
          border: none;
          background: transparent;
          color: #666;
        }
        .tables-sidebar-item .table-actions .btn:hover {
          color: #333;
        }
        .tables-editor {
          flex-grow: 1;
          overflow-x: auto; overflow-y: hidden;
          min-height: 0;
          padding: 8px;
        }
        .scripts-tab-container {
          display: flex;
          flex-direction: row;
          flex: 1 1 auto;
          min-height: 0;
          overflow: hidden;
        }
        .scripts-sidebar {
          width: 220px;
          min-width: 220px;
          border-right: 1px solid #dee2e6;
          overflow-y: auto;
          padding: 8px;
        }
        .scripts-sidebar-item {
          display: flex;
          align-items: center;
          padding: 6px 8px;
          cursor: pointer;
          border-radius: 4px;
          margin-bottom: 2px;
        }
        .scripts-sidebar-item:hover {
          background-color: #f0f0f0;
        }
        .scripts-sidebar-item.active {
          background-color: #e2e6ea;
          font-weight: 600;
        }
        .scripts-sidebar-item .script-name {
          flex-grow: 1;
          overflow: hidden;
          text-overflow: ellipsis;
          white-space: nowrap;
        }
        .scripts-sidebar-item .script-actions {
          display: flex;
          gap: 4px;
          margin-left: 4px;
        }
        .scripts-sidebar-item .script-actions .btn {
          padding: 0 4px;
          font-size: 12px;
          line-height: 1;
          border: none;
          background: transparent;
          color: #666;
        }
        .scripts-sidebar-item .script-actions .btn:hover {
          color: #333;
        }
        .scripts-editor {
          flex-grow: 1;
          display: flex;
          flex-direction: column;
          padding: 8px;
          min-height: 0;
          overflow: hidden;
        }
        .trees-tab-container {
          display: flex;
          flex-direction: row;
          flex: 1 1 auto;
          min-height: 0;
        }
        .trees-sidebar {
          width: 220px;
          min-width: 220px;
          border-right: 1px solid #dee2e6;
          overflow-y: auto;
          padding: 8px;
        }
        .trees-sidebar-item {
          display: flex;
          align-items: center;
          padding: 6px 8px;
          cursor: pointer;
          border-radius: 4px;
          margin-bottom: 2px;
        }
        .trees-sidebar-item:hover {
          background-color: #f0f0f0;
        }
        .trees-sidebar-item.active {
          background-color: #e2e6ea;
          font-weight: 600;
        }
        .trees-sidebar-item .tree-name {
          flex-grow: 1;
          overflow: hidden;
          text-overflow: ellipsis;
          white-space: nowrap;
        }
        .trees-sidebar-item .tree-actions {
          display: flex;
          gap: 4px;
          margin-left: 4px;
        }
        .trees-sidebar-item .tree-actions .btn {
          padding: 0 4px;
          font-size: 12px;
          line-height: 1;
          border: none;
          background: transparent;
          color: #666;
        }
        .trees-sidebar-item .tree-actions .btn:hover {
          color: #333;
        }
        .trees-editor {
          flex-grow: 1;
          display: flex;
          flex-direction: column;
          min-height: 0;
          padding: 8px;
          overflow: hidden;
        }
        .trees-toolbar {
          display: flex;
          align-items: center;
          justify-content: space-between;
          padding-bottom: 6px;
          flex-shrink: 0;
        }
        .trees-toolbar-left {
          display: flex;
          align-items: center;
          gap: 6px;
        }
        .trees-toolbar-right {
          display: flex;
          align-items: center;
          gap: 8px;
        }
        .trees-preview-toggle {
          display: inline-flex;
          align-items: center;
          gap: 6px;
          cursor: pointer;
          font-size: 0.8rem;
          color: #666;
          user-select: none;
        }
        .trees-preview-toggle input { display: none; }
        .trees-toggle-slider {
          width: 32px; height: 18px;
          background: #ccc;
          border-radius: 9px;
          position: relative;
          transition: background 0.2s;
        }
        .trees-toggle-slider::after {
          content: '';
          position: absolute;
          width: 14px; height: 14px;
          background: #fff;
          border-radius: 50%;
          top: 2px; left: 2px;
          transition: transform 0.2s;
        }
        .trees-preview-toggle input:checked + .trees-toggle-slider {
          background: var(--bs-primary, #0d6efd);
        }
        .trees-preview-toggle input:checked + .trees-toggle-slider::after {
          transform: translateX(14px);
        }
        .trees-content-split {
          display: flex;
          flex: 1 1 auto;
          min-height: 0;
          gap: 0;
        }
        .trees-table-wrapper {
          flex: 1 1 auto;
          min-width: 0;
          min-height: 0;
          display: flex;
          flex-direction: column;
        }
        .trees-preview-panel {
          flex: 0 0 40%;
          min-width: 0;
          min-height: 0;
          border-left: 1px solid #dee2e6;
          padding: 8px;
          overflow: auto;
        }
        .trees-preview-panel .shiny-plot-output {
          width: 100% !important;
          height: 100% !important;
        }
        #scripts_ace_editor {
          flex: 1 1 auto;
          min-height: 200px;
        }
        .settings-form {
          max-width: 600px;
          padding: 16px;
        }
        .settings-form .form-group {
          margin-bottom: 12px;
        }
        .app-bar .btn:disabled {
          opacity: 0.3;
          cursor: default;
          pointer-events: none;
        }
        .app-bar-hamburger {
          color: white;
          background: transparent;
          border: none;
          font-size: 18px;
          padding: 4px 8px;
          margin-right: 8px;
        }
        .app-bar-hamburger:hover,
        .app-bar-hamburger:focus {
          color: white;
          background: rgba(255,255,255,0.1);
        }
        .app-page-link.active {
          font-weight: 600;
          background-color: #e9ecef;
        }
        .app-page { display: none; }
        .app-page.active {
          display: flex;
          flex-direction: column;
          flex: 1 1 auto;
          min-height: 0;
        }
.results-page .bslib-sidebar-layout {
          flex: 1 1 auto;
          min-height: 0;
          border: none;
        }
      ")),
      tags$script(shiny::HTML("
        Shiny.addCustomMessageHandler('toggle_undo_redo', function(msg) {
          var undo = document.getElementById('undo_btn');
          var redo = document.getElementById('redo_btn');
          if (undo) undo.disabled = !msg.can_undo;
          if (redo) redo.disabled = !msg.can_redo;
        });
        function switchAppPage(page) {
          document.querySelectorAll('.app-page-link').forEach(function(btn) {
            btn.classList.remove('active');
          });
          var active = document.querySelector('.app-page-link[data-page=\"' + page + '\"]');
          if (active) active.classList.add('active');

          document.querySelectorAll('.app-page').forEach(function(el) {
            el.classList.remove('active');
          });
          var target = document.getElementById('page_' + page);
          if (target) target.classList.add('active');

          Shiny.setInputValue('active_page', page, {priority: 'event'});
        }

        Shiny.addCustomMessageHandler('editor_progress', function(data) {
          var snackbar = document.getElementById('editor_progress_snackbar');
          var bar = document.getElementById('editor_progress_bar');
          var pct = document.getElementById('editor_progress_pct');
          var msg = document.getElementById('editor_no_results_msg');
          if (data.state === 'running') {
            if (msg) msg.style.display = 'none';
            if (bar) bar.style.width = data.pct + '%';
            if (pct) pct.textContent = data.pct + '%';
            if (snackbar) snackbar.classList.add('visible');
          } else {
            if (snackbar) snackbar.classList.remove('visible');
            if (bar) bar.style.width = '0%';
            if (pct) pct.textContent = '0%';
            if (msg) msg.style.display = data.state === 'done' ? 'none' : 'block';
          }
        });

        Shiny.addCustomMessageHandler('dsa_progress', function(data) {
          var snackbar = document.getElementById('dsa_progress_snackbar');
          var bar = document.getElementById('dsa_progress_bar');
          var pct = document.getElementById('dsa_progress_pct');
          if (data.state === 'running') {
            if (bar) bar.style.width = data.pct + '%';
            if (pct) pct.textContent = data.pct + '%';
            if (snackbar) snackbar.classList.add('visible');
          } else {
            if (snackbar) snackbar.classList.remove('visible');
            if (bar) bar.style.width = '0%';
            if (pct) pct.textContent = '0%';
          }
        });

        Shiny.addCustomMessageHandler('vbp_progress', function(data) {
          var snackbar = document.getElementById('vbp_progress_snackbar');
          var bar = document.getElementById('vbp_progress_bar');
          var pct = document.getElementById('vbp_progress_pct');
          if (data.state === 'running') {
            if (bar) bar.style.width = data.pct + '%';
            if (pct) pct.textContent = data.pct + '%';
            if (snackbar) snackbar.classList.add('visible');
          } else {
            if (snackbar) snackbar.classList.remove('visible');
            if (bar) bar.style.width = '0%';
            if (pct) pct.textContent = '0%';
          }
        });

        Shiny.addCustomMessageHandler('scenario_progress', function(data) {
          var snackbar = document.getElementById('scenario_progress_snackbar');
          var bar = document.getElementById('scenario_progress_bar');
          var pct = document.getElementById('scenario_progress_pct');
          if (data.state === 'running') {
            if (bar) bar.style.width = data.pct + '%';
            if (pct) pct.textContent = data.pct + '%';
            if (snackbar) snackbar.classList.add('visible');
          } else {
            if (snackbar) snackbar.classList.remove('visible');
            if (bar) bar.style.width = '0%';
            if (pct) pct.textContent = '0%';
          }
        });

        Shiny.addCustomMessageHandler('psa_progress', function(data) {
          var snackbar = document.getElementById('psa_progress_snackbar');
          var bar = document.getElementById('psa_progress_bar');
          var pct = document.getElementById('psa_progress_pct');
          if (data.state === 'running') {
            if (bar) bar.style.width = data.pct + '%';
            if (pct) pct.textContent = data.pct + '%';
            if (snackbar) snackbar.classList.add('visible');
          } else {
            if (snackbar) snackbar.classList.remove('visible');
            if (bar) bar.style.width = '0%';
            if (pct) pct.textContent = '0%';
          }
        });

        Shiny.addCustomMessageHandler('twsa_progress', function(data) {
          var snackbar = document.getElementById('twsa_progress_snackbar');
          var bar = document.getElementById('twsa_progress_bar');
          var pct = document.getElementById('twsa_progress_pct');
          if (data.state === 'running') {
            if (bar) bar.style.width = data.pct + '%';
            if (pct) pct.textContent = data.pct + '%';
            if (snackbar) snackbar.classList.add('visible');
          } else {
            if (snackbar) snackbar.classList.remove('visible');
            if (bar) bar.style.width = '0%';
            if (pct) pct.textContent = '0%';
          }
        });

        Shiny.addCustomMessageHandler('threshold_progress', function(data) {
          var snackbar = document.getElementById('threshold_progress_snackbar');
          var bar = document.getElementById('threshold_progress_bar');
          var pct = document.getElementById('threshold_progress_pct');
          if (data.state === 'running') {
            if (bar) bar.style.width = data.pct + '%';
            if (pct) pct.textContent = data.pct + '%';
            if (snackbar) snackbar.classList.add('visible');
          } else {
            if (snackbar) snackbar.classList.remove('visible');
            if (bar) bar.style.width = '0%';
            if (pct) pct.textContent = '0%';
          }
        });

        Shiny.addCustomMessageHandler('trees_toggle_preview', function(msg) {
          var panel = document.querySelector('.trees-preview-panel');
          if (panel) {
            panel.style.display = msg.show ? '' : 'none';
            if (msg.show) {
              // Notify Shiny that the plot output is now visible
              var plot = document.getElementById('trees_preview_plot');
              if (plot) $(plot).trigger('shown');
            }
          }
        });
      "))
    ),
    tags$div(
      class = "app-bar",
      tags$div(
        class = "dropdown",
        tags$button(
          class = "btn app-bar-hamburger",
          type = "button",
          `data-bs-toggle` = "dropdown",
          `aria-expanded` = "false",
          shiny::icon("bars")
        ),
        tags$ul(
          class = "dropdown-menu",
          tags$li(
            tags$button(
              class = "dropdown-item app-page-link active",
              `data-page` = "documentation",
              onclick = "switchAppPage('documentation')",
              "Documentation"
            )
          ),
          tags$li(
            tags$button(
              class = "dropdown-item app-page-link",
              `data-page` = "editor",
              onclick = "switchAppPage('editor')",
              "Model Inputs"
            )
          ),
          tags$li(
            tags$button(
              class = "dropdown-item app-page-link",
              `data-page` = "overrides",
              onclick = "switchAppPage('overrides')",
              "Overrides"
            )
          ),
          tags$li(
            tags$button(
              class = "dropdown-item app-page-link",
              `data-page` = "results",
              onclick = "switchAppPage('results')",
              "Base Case"
            )
          ),
          tags$li(
            tags$button(
              class = "dropdown-item app-page-link",
              `data-page` = "vbp",
              onclick = "switchAppPage('vbp')",
              "VBP"
            )
          ),
          tags$li(
            tags$button(
              class = "dropdown-item app-page-link",
              `data-page` = "dsa",
              onclick = "switchAppPage('dsa')",
              "DSA"
            )
          ),
          tags$li(
            tags$button(
              class = "dropdown-item app-page-link",
              `data-page` = "scenario",
              onclick = "switchAppPage('scenario')",
              "Scenario Analysis"
            )
          ),
          tags$li(
            tags$button(
              class = "dropdown-item app-page-link",
              `data-page` = "twsa",
              onclick = "switchAppPage('twsa')",
              "TWSA"
            )
          ),
          tags$li(
            tags$button(
              class = "dropdown-item app-page-link",
              `data-page` = "threshold",
              onclick = "switchAppPage('threshold')",
              "Threshold"
            )
          ),
          tags$li(
            tags$button(
              class = "dropdown-item app-page-link",
              `data-page` = "psa",
              onclick = "switchAppPage('psa')",
              "PSA"
            )
          )
        )
      ),
      tags$span(class = "app-bar-title", shiny::textOutput("editor_title", inline = TRUE)),
      tags$div(
        class = "dropdown",
        tags$button(
          class = "btn dropdown-toggle",
          type = "button",
          `data-bs-toggle` = "dropdown",
          `aria-expanded` = "false",
          "File"
        ),
        tags$ul(
          class = "dropdown-menu",
          tags$li(
            shinyFiles::shinyFilesButton(
              "open_file",
              "Open",
              "Select a model file",
              multiple = FALSE,
              class = "dropdown-item",
              style = "background: none; border: none; width: 100%; text-align: left; color: inherit;"
            )
          ),
          tags$li(tags$hr(class = "dropdown-divider")),
          tags$li(
            tags$button(
              class = "dropdown-item", type = "button",
              onclick = "Shiny.setInputValue('save_file', Math.random(), {priority: 'event'});",
              "Save")
          ),
          tags$li(
            shinyFiles::shinySaveButton("save_as_file", "Save As...", "Save model as...",
              filetype = list(JSON = "json", YAML = c("yaml", "yml")),
              class = "dropdown-item",
              style = "background: none; border: none; width: 100%; text-align: left; color: inherit;")
          ),
          tags$li(tags$hr(class = "dropdown-divider")),
          tags$li(
            tags$button(
              class = "dropdown-item", type = "button",
              onclick = "Shiny.setInputValue('version_history', Math.random(), {priority: 'event'});",
              "Version History...")
          )
        )
      ),
      tags$div(
        class = "dropdown",
        tags$button(
          class = "btn dropdown-toggle",
          type = "button",
          `data-bs-toggle` = "dropdown",
          `aria-expanded` = "false",
          "View"
        ),
        tags$ul(
          class = "dropdown-menu",
          tags$li(
            tags$button(
              class = "dropdown-item",
              type = "button",
              onclick = "Shiny.setInputValue('show_diff_modal', Math.random(), {priority: 'event'})",
              "Model Diff"
            )
          )
        )
      ),
      tags$div(
        style = "margin-left: auto; display: flex; gap: 4px;",
        shiny::actionButton("undo_btn", NULL,
          icon = shiny::icon("rotate-left"),
          class = "btn btn-sm",
          style = "color: white; background: transparent; border: none;",
          title = "Undo (Ctrl+Z)",
          disabled = "disabled"
        ),
        shiny::actionButton("redo_btn", NULL,
          icon = shiny::icon("rotate-right"),
          class = "btn btn-sm",
          style = "color: white; background: transparent; border: none;",
          title = "Redo (Ctrl+Shift+Z)",
          disabled = "disabled"
        )
      )
    ),
    shiny::conditionalPanel(
      condition = "!output.model_loaded",
      tags$div(class = "no-model-message", "No model loaded")
    ),
    shiny::conditionalPanel(
      condition = "output.model_loaded",
      # Documentation page
      tags$div(
        id = "page_documentation",
        class = "app-page active",
        style = "overflow: auto; padding: 16px;",
        shiny::uiOutput("documentation_tab")
      ),
      # Model Inputs page (formerly Editor)
      tags$div(
        id = "page_editor",
        class = "app-page model-content",
        bslib::navset_underline(
          bslib::nav_panel("Tables",
            tags$div(
              class = "tables-tab-container",
              shiny::uiOutput("tables_sidebar"),
              tags$div(
                class = "tables-editor",
                rhandsontable::rHandsontableOutput("tables_hot_output", height = "100%")
              )
            )
          ),
          bslib::nav_panel("Scripts", shiny::uiOutput("scripts_tab")),
          bslib::nav_panel("Variables", shiny::uiOutput("variables_table")),
          bslib::nav_panel("Settings", shiny::uiOutput("settings_panel")),
          bslib::nav_panel("Groups", shiny::uiOutput("groups_table")),
          bslib::nav_panel("States", shiny::uiOutput("states_table")),
          bslib::nav_panel("Strategies", shiny::uiOutput("strategies_table")),
          bslib::nav_panel("Transitions", shiny::uiOutput("transitions_table")),
          bslib::nav_panel("Values", shiny::uiOutput("values_table")),
          bslib::nav_panel("Summaries", shiny::uiOutput("summaries_table")),
          bslib::nav_panel("Decision Trees",
            tags$div(
              class = "trees-tab-container",
              shiny::uiOutput("trees_sidebar"),
              tags$div(
                class = "trees-editor",
                tags$div(
                  class = "trees-toolbar",
                  tags$div(class = "trees-toolbar-left"),
                  tags$div(
                    class = "trees-toolbar-right",
                    tags$label(
                      class = "trees-preview-toggle",
                      tags$input(
                        type = "checkbox",
                        id = "trees_preview_toggle",
                        onchange = "Shiny.setInputValue('trees_preview_on', this.checked, {priority: 'event'})"
                      ),
                      tags$span(class = "trees-toggle-slider"),
                      tags$span(class = "trees-toggle-label", "Preview")
                    )
                  )
                ),
                tags$div(
                  class = "trees-content-split",
                  tags$div(
                    class = "trees-table-wrapper",
                    shiny::uiOutput("trees_table")
                  ),
                  tags$div(
                    class = "trees-preview-panel",
                    style = "display: none;",
                    shiny::plotOutput("trees_preview_plot", height = "100%", width = "100%")
                  )
                )
              )
            )
          ),
        )
      ),
      # Overrides page
      tags$div(
        id = "page_overrides",
        class = "app-page",
        style = "overflow: auto; padding: 16px;",
        tags$div(
          style = "max-width: 600px;",
          shiny::uiOutput("override_panel")
        )
      ),
      # Results page
      tags$div(
        id = "page_results",
        class = "app-page results-page",
        style = "overflow: auto;",
        bslib::layout_sidebar(
          sidebar = bslib::sidebar(
            id = "results_sidebar",
            width = "250px",
            tags$button(
              type = "button",
              class = "btn btn-outline-secondary btn-sm w-100",
              onclick = "switchAppPage('overrides')",
              "Manage Overrides"
            )
          ),
          shiny::conditionalPanel(
            condition = "!output.has_editor_results",
            tags$div(
              id = "editor_no_results_msg", class = "text-muted p-3",
              "Run the model to see results."
            )
          ),
          shiny::conditionalPanel(
            condition = "output.has_editor_results",
            bslib::navset_card_tab(
              bslib::nav_panel("Trace", traceResultsUI("editor_trace")),
              bslib::nav_panel("Outcomes", outcomesResultsUI("editor_outcomes")),
              bslib::nav_panel("Costs", costsResultsUI("editor_costs")),
              bslib::nav_panel("NMB", nmbResultsUI("editor_nmb")),
              bslib::nav_panel("Pairwise CE", pairwiseCeResultsUI("editor_pairwise_ce")),
              bslib::nav_panel("Incremental CE", incrementalCeResultsUI("editor_incremental_ce")),
              bslib::nav_panel("Variables",
                variableDiagnosticsUI("editor_variable_diagnostics")),
              bslib::nav_panel("Decision Trees",
                decisionTreeResultsUI("editor_decision_trees")),
              bslib::nav_panel("Transitions",
                transitionHeatmapUI("editor_transitions"))
            )
          )
        )
      ),
      # DSA page
      tags$div(
        id = "page_dsa",
        class = "app-page results-page",
        style = "overflow: auto;",
        bslib::layout_sidebar(
          sidebar = bslib::sidebar(
            id = "dsa_sidebar",
            width = "250px",
            tags$button(
              type = "button",
              class = "btn btn-outline-secondary btn-sm w-100",
              onclick = "switchAppPage('overrides')",
              "Manage Overrides"
            )
          ),
          bslib::navset_card_tab(
            bslib::nav_panel("Inputs",
              shiny::uiOutput("dsa_inputs_panel")
            ),
            bslib::nav_panel("Outcomes", dsaResultTabUI("editor_dsa_outcomes")),
            bslib::nav_panel("Costs", dsaResultTabUI("editor_dsa_costs")),
            bslib::nav_panel("NMB", dsaResultTabUI("editor_dsa_nmb")),
            bslib::nav_panel("CE", dsaResultTabUI("editor_dsa_ce")),
            bslib::nav_panel("VBP", dsaResultTabUI("editor_dsa_vbp"))
          )
        )
      ),
      # VBP page
      tags$div(
        id = "page_vbp",
        class = "app-page results-page",
        style = "overflow: auto;",
        bslib::navset_card_tab(
          bslib::nav_panel("Inputs",
            shiny::uiOutput("vbp_inputs_panel")
          ),
          bslib::nav_panel("Results", vbpResultsUI("editor_vbp"))
        )
      ),
      # Scenario page
      tags$div(
        id = "page_scenario",
        class = "app-page results-page",
        style = "overflow: auto;",
        bslib::layout_sidebar(
          sidebar = bslib::sidebar(
            id = "scenario_sidebar",
            width = "250px",
            tags$button(
              type = "button",
              class = "btn btn-outline-secondary btn-sm w-100",
              onclick = "switchAppPage('overrides')",
              "Manage Overrides"
            )
          ),
          bslib::navset_card_tab(
            bslib::nav_panel("Inputs",
              shiny::uiOutput("scenario_inputs_panel")
            ),
            bslib::nav_panel("Outcomes", scenarioResultTabUI("editor_scenario_outcomes")),
            bslib::nav_panel("Costs", scenarioResultTabUI("editor_scenario_costs")),
            bslib::nav_panel("NMB", scenarioResultTabUI("editor_scenario_nmb")),
            bslib::nav_panel("CE", scenarioResultTabUI("editor_scenario_ce")),
            bslib::nav_panel("VBP", scenarioResultTabUI("editor_scenario_vbp"))
          )
        )
      ),
      # TWSA page
      tags$div(
        id = "page_twsa",
        class = "app-page results-page",
        style = "overflow: auto;",
        bslib::layout_sidebar(
          sidebar = bslib::sidebar(
            id = "twsa_sidebar",
            width = "250px",
            tags$button(
              type = "button",
              class = "btn btn-outline-secondary btn-sm w-100",
              onclick = "switchAppPage('overrides')",
              "Manage Overrides"
            )
          ),
          bslib::navset_card_tab(
            bslib::nav_panel("Inputs",
              shiny::uiOutput("twsa_inputs_panel")
            ),
            bslib::nav_panel("Outcomes", twsaResultTabUI("editor_twsa_outcomes")),
            bslib::nav_panel("Costs", twsaResultTabUI("editor_twsa_costs")),
            bslib::nav_panel("NMB", twsaResultTabUI("editor_twsa_nmb")),
            bslib::nav_panel("CE", twsaResultTabUI("editor_twsa_ce")),
            bslib::nav_panel("VBP", twsaResultTabUI("editor_twsa_vbp"))
          )
        )
      ),
      # Threshold page
      tags$div(
        id = "page_threshold",
        class = "app-page results-page",
        style = "overflow: auto;",
        bslib::layout_sidebar(
          sidebar = bslib::sidebar(
            id = "threshold_sidebar",
            width = "250px",
            tags$button(
              type = "button",
              class = "btn btn-outline-secondary btn-sm w-100",
              onclick = "switchAppPage('overrides')",
              "Manage Overrides"
            )
          ),
          bslib::navset_card_tab(
            bslib::nav_panel("Inputs",
              shiny::uiOutput("threshold_inputs_panel")
            ),
            bslib::nav_panel("Summary", thresholdSummaryUI("editor_threshold_summary")),
            bslib::nav_panel("Detail", thresholdResultTabUI("editor_threshold_detail")),
            bslib::nav_panel("Convergence", thresholdResultTabUI("editor_threshold_convergence"))
          )
        )
      ),
      # PSA page
      tags$div(
        id = "page_psa",
        class = "app-page results-page",
        style = "overflow: auto;",
        bslib::layout_sidebar(
          sidebar = bslib::sidebar(
            id = "psa_sidebar",
            width = "250px",
            tags$button(
              type = "button",
              class = "btn btn-outline-secondary btn-sm w-100",
              onclick = "switchAppPage('overrides')",
              "Manage Overrides"
            )
          ),
          bslib::navset_card_tab(
            bslib::nav_panel("Settings",
              shiny::uiOutput("psa_settings_panel")
            ),
            bslib::nav_panel("Univariate Sampling",
              shiny::uiOutput("psa_univariate_panel")
            ),
            bslib::nav_panel("Multivariate Sampling",
              shiny::uiOutput("psa_multivariate_panel")
            ),
            bslib::nav_panel("Outcomes", psaResultTabUI("editor_psa_outcomes")),
            bslib::nav_panel("Costs", psaResultTabUI("editor_psa_costs")),
            bslib::nav_panel("NMB", psaResultTabUI("editor_psa_nmb")),
            bslib::nav_panel("Incremental CE", psaResultTabUI("editor_psa_incremental_ce")),
            bslib::nav_panel("Pairwise CE", psaResultTabUI("editor_psa_pairwise_ce")),
            bslib::nav_panel("EVPI", psaResultTabUI("editor_psa_evpi")),
            bslib::nav_panel("Parameters", psaResultTabUI("editor_psa_parameters"))
          )
        )
      )
    ),
    # Progress snackbar
    tags$div(
      id = "editor_progress_snackbar",
      class = "progress-snackbar",
      tags$span(class = "progress-snackbar-label", "Running model..."),
      tags$div(
        class = "progress-snackbar-track",
        tags$div(id = "editor_progress_bar", class = "progress-snackbar-fill")
      ),
      tags$span(id = "editor_progress_pct", class = "progress-snackbar-pct", "0%")
    ),
    # DSA progress snackbar
    tags$div(
      id = "dsa_progress_snackbar",
      class = "progress-snackbar",
      tags$span(class = "progress-snackbar-label", "Running DSA..."),
      tags$div(
        class = "progress-snackbar-track",
        tags$div(id = "dsa_progress_bar", class = "progress-snackbar-fill")
      ),
      tags$span(id = "dsa_progress_pct", class = "progress-snackbar-pct", "0%")
    ),
    # VBP progress snackbar
    tags$div(
      id = "vbp_progress_snackbar",
      class = "progress-snackbar",
      tags$span(class = "progress-snackbar-label", "Running VBP..."),
      tags$div(
        class = "progress-snackbar-track",
        tags$div(id = "vbp_progress_bar", class = "progress-snackbar-fill")
      ),
      tags$span(id = "vbp_progress_pct", class = "progress-snackbar-pct", "0%")
    ),
    # Scenario progress snackbar
    tags$div(
      id = "scenario_progress_snackbar",
      class = "progress-snackbar",
      tags$span(class = "progress-snackbar-label", "Running Scenario Analysis..."),
      tags$div(
        class = "progress-snackbar-track",
        tags$div(id = "scenario_progress_bar", class = "progress-snackbar-fill")
      ),
      tags$span(id = "scenario_progress_pct", class = "progress-snackbar-pct", "0%")
    ),
    # TWSA progress snackbar
    tags$div(
      id = "twsa_progress_snackbar",
      class = "progress-snackbar",
      tags$span(class = "progress-snackbar-label", "Running TWSA..."),
      tags$div(
        class = "progress-snackbar-track",
        tags$div(id = "twsa_progress_bar", class = "progress-snackbar-fill")
      ),
      tags$span(id = "twsa_progress_pct", class = "progress-snackbar-pct", "0%")
    ),
    # PSA progress snackbar
    tags$div(
      id = "psa_progress_snackbar",
      class = "progress-snackbar",
      tags$span(class = "progress-snackbar-label", "Running PSA..."),
      tags$div(
        class = "progress-snackbar-track",
        tags$div(id = "psa_progress_bar", class = "progress-snackbar-fill")
      ),
      tags$span(id = "psa_progress_pct", class = "progress-snackbar-pct", "0%")
    ),
    # Threshold progress snackbar
    tags$div(
      id = "threshold_progress_snackbar",
      class = "progress-snackbar",
      tags$span(class = "progress-snackbar-label", "Running Threshold Analysis..."),
      tags$div(
        class = "progress-snackbar-track",
        tags$div(id = "threshold_progress_bar", class = "progress-snackbar-fill")
      ),
      tags$span(id = "threshold_progress_pct", class = "progress-snackbar-pct", "0%")
    )
  )

  server <- function(input, output, session) {
    library(openqalyregimen)
    # Pre-warm R function suggestions cache at startup
    get_r_function_suggestions(c("base", "stats"))

    # Generate Excel-style column labels: A, B, ..., Z, AA, AB, ...
    excel_col_labels <- function(n) {
      labels <- character(n)
      for (i in seq_len(n)) {
        label <- ""
        num <- i
        while (num > 0) {
          num <- num - 1
          label <- paste0(LETTERS[num %% 26 + 1], label)
          num <- num %/% 26
        }
        labels[i] <- label
      }
      labels
    }

    volumes <- c(Working = getwd(), Home = path.expand("~"))
    shinyFiles::shinyFileChoose(
      input, "open_file", roots = volumes,
      filetypes = c("json", "yml", "yaml")
    )
    shinyFiles::shinyFileSave(input, "save_as_file", roots = volumes,
      filetypes = c("json", "yaml", "yml"))

    model <- shiny::reactiveVal(NULL)
    original_model <- shiny::reactiveVal(NULL)
    file_load_counter <- shiny::reactiveVal(0L)
    table_render_trigger <- shiny::reactiveVal(0L)
    current_file_path <- shiny::reactiveVal(NULL)
    saved_model <- shiny::reactiveVal(NULL)
    pending_restore_version <- shiny::reactiveVal(NULL)
    pending_open_file <- shiny::reactiveVal(NULL)

    is_dirty <- shiny::reactive({
      m <- model()
      s <- saved_model()
      if (is.null(m) || is.null(s)) return(FALSE)
      !identical(m, s)
    })

    history <- create_history_manager(max_size = 5)

    # --- File I/O helpers ---

    escape_regex <- function(x) gsub("([.\\\\|()\\[\\]{}^$+*?])", "\\\\\\1", x, perl = TRUE)

    history_file_path <- function(fpath) {
      paste0(tools::file_path_sans_ext(fpath), "_history.json")
    }

    read_history_file <- function(fpath) {
      hpath <- history_file_path(fpath)
      if (!file.exists(hpath)) return(list(current_message = "", versions = list()))
      tryCatch({
        h <- jsonlite::fromJSON(hpath, simplifyVector = FALSE)
        if (is.null(h$versions)) h$versions <- list()
        if (is.null(h$current_message)) h$current_message <- ""
        h
      }, error = function(e) list(current_message = "", versions = list()))
    }

    write_history_file <- function(fpath, history) {
      hpath <- history_file_path(fpath)
      writeLines(jsonlite::toJSON(history, auto_unbox = TRUE, pretty = TRUE), hpath)
    }

    save_model_to_file <- function(m, fpath, message = "", skip_backup = FALSE) {
      ext <- tolower(tools::file_ext(fpath))
      format <- if (ext == "json") "json" else "yaml"
      if (!skip_backup && file.exists(fpath)) {
        old_content <- paste(readLines(fpath, warn = FALSE), collapse = "\n")
        history <- read_history_file(fpath)
        prev_message <- if (nzchar(history$current_message)) history$current_message else "Initial version"
        new_entry <- list(
          timestamp = as.character(Sys.time()),
          message = prev_message,
          content = old_content
        )
        history$versions <- c(list(new_entry), history$versions)
        history$current_message <- message
        write_history_file(fpath, history)
      }
      openqaly::write_model(m, fpath, format = format)
    }

    do_open_file <- function(fpath) {
      ext <- tolower(tools::file_ext(fpath))
      loaded <- if (ext == "json") {
        openqaly::read_model_json(paste(readLines(fpath), collapse = "\n"))
      } else if (ext %in% c("yaml", "yml")) {
        openqaly::read_model_yaml(fpath)
      } else {
        openqaly::read_model(fpath)
      }
      model(loaded)
      original_model(loaded)
      saved_model(loaded)
      current_file_path(fpath)
      history$clear()
      file_load_counter(file_load_counter() + 1L)
      table_render_trigger(table_render_trigger() + 1L)
    }

    list_versions <- function(fpath) {
      empty <- data.frame(id = character(), timestamp = character(),
        relative = character(), message = character(), stringsAsFactors = FALSE)
      if (is.null(fpath)) return(empty)
      history <- read_history_file(fpath)
      if (length(history$versions) == 0) return(empty)
      ids <- as.character(seq_along(history$versions))
      display_times <- as.POSIXct(
        vapply(history$versions, function(v) v$timestamp, character(1))
      )
      display_labels <- format(display_times, "%x %X")
      age_secs <- as.numeric(difftime(Sys.time(), display_times, units = "secs"))
      relative <- ifelse(age_secs < 60, "just now",
        ifelse(age_secs < 3600, paste0(round(age_secs / 60), " min ago"),
        ifelse(age_secs < 86400, paste0(round(age_secs / 3600), " hours ago"),
        paste0(round(age_secs / 86400), " days ago"))))
      messages <- vapply(history$versions, function(v) {
        if (!is.null(v$message) && nzchar(v$message)) v$message else ""
      }, character(1))
      data.frame(id = ids, timestamp = display_labels,
        relative = relative, message = messages, stringsAsFactors = FALSE)
    }

    load_version_model <- function(fpath, version_id) {
      history <- read_history_file(fpath)
      idx <- as.integer(version_id)
      if (is.na(idx) || idx < 1 || idx > length(history$versions)) {
        stop("Invalid version ID: ", version_id)
      }
      content <- history$versions[[idx]]$content
      # Detect JSON vs YAML by checking if content starts with '{'
      if (grepl("^\\s*\\{", content)) {
        openqaly::read_model_json(content)
      } else {
        tmp <- tempfile(fileext = ".yaml")
        on.exit(unlink(tmp), add = TRUE)
        writeLines(content, tmp)
        openqaly::read_model_yaml(tmp)
      }
    }

    show_version_history_modal <- function(selected_version = NULL) {
      fpath <- shiny::isolate(current_file_path())
      if (is.null(fpath)) {
        shiny::showNotification("No file open. Open a file first to view version history.",
          type = "warning", duration = 3)
        return()
      }
      versions <- list_versions(fpath)
      if (nrow(versions) == 0) {
        shiny::showNotification("No saved versions found.", type = "warning", duration = 3)
        return()
      }
      default_version <- selected_version %||% versions$id[1]
      version_items <- lapply(seq_len(nrow(versions)), function(i) {
        msg <- versions$message[i]
        vid <- versions$id[i]
        is_selected <- vid == default_version
        tags$div(
          class = paste("version-item", if (is_selected) "version-item-active" else ""),
          `data-version` = vid,
          onclick = paste0(
            "document.querySelectorAll('.version-item').forEach(el => el.classList.remove('version-item-active'));",
            "this.classList.add('version-item-active');",
            "Shiny.setInputValue('version_select', '", vid, "', {priority: 'event'});"
          ),
          tags$div(class = "version-timestamp", versions$timestamp[i]),
          tags$div(class = "version-relative", versions$relative[i]),
          if (nzchar(msg)) tags$div(class = "version-message", msg) else NULL
        )
      })
      session$sendCustomMessage("set_initial_version", default_version)
      shiny::showModal(shiny::modalDialog(
        title = "Version History",
        size = "xl",
        easyClose = TRUE,
        tags$script(shiny::HTML(
          "setTimeout(function(){ var d = document.querySelector('.modal-dialog'); if(d) d.classList.add('modal-dialog-scrollable'); }, 0);"
        )),
        tags$style(shiny::HTML("
          .modal-body { display: flex; flex-direction: column; }
          .version-history-layout { flex: 1; min-height: 0; display: flex; gap: 16px; }
          .version-list { min-width: 200px; overflow-y: auto;
            border-right: 1px solid #dee2e6; padding-right: 12px; }
          .version-diff-pane { flex: 1; min-width: 0; min-height: 0; overflow-y: auto;
            display: flex; flex-direction: column; }
          .version-item { padding: 8px 12px; cursor: pointer; border-radius: 4px; margin-bottom: 4px; }
          .version-item:hover { background: #f0f0f0; }
          .version-item-active { background: #e7f1ff; border-left: 3px solid #0d6efd; }
          .version-timestamp { font-weight: 500; font-size: 0.9em; }
          .version-relative { color: #6c757d; font-size: 0.8em; }
          .version-message { font-size: 0.85em; color: #495057; margin-top: 2px;
            white-space: nowrap; overflow: hidden; text-overflow: ellipsis; max-width: 180px; }
        ")),
        tags$div(
          class = "version-history-layout",
          tags$div(class = "version-list", version_items),
          tags$div(
            class = "version-diff-pane",
            shiny::selectInput("version_diff_mode", NULL,
              choices = c("Side by Side" = "sidebyside", "Unified" = "unified"),
              selected = "sidebyside", width = "180px"),
            shiny::uiOutput("version_diff_output")
          )
        ),
        footer = shiny::tagList(
          tags$button(class = "btn btn-warning", type = "button",
            onclick = "Shiny.setInputValue('restore_version', Math.random(), {priority: 'event'});",
            "Restore This Version"),
          shiny::modalButton("Close")
        )
      ))
    }

    do_restore_version <- function(skip_backup = FALSE) {
      fpath <- shiny::isolate(current_file_path())
      version_id <- shiny::isolate(pending_restore_version())
      if (is.null(fpath) || is.null(version_id)) {
        shiny::showNotification("Could not restore version.", type = "error", duration = 3)
        return()
      }
      tryCatch({
        restored <- load_version_model(fpath, version_id)
        history$push(shiny::isolate(model()))
        model(restored)
        hist <- read_history_file(fpath)
        idx <- as.integer(version_id)
        restore_ts <- hist$versions[[idx]]$timestamp
        save_model_to_file(restored, fpath,
          message = paste("Restored version from", restore_ts),
          skip_backup = skip_backup)
        saved_model(restored)
        file_load_counter(file_load_counter() + 1L)
        table_render_trigger(table_render_trigger() + 1L)
        pending_restore_version(NULL)
        shiny::removeModal()
        shiny::showNotification("Version restored and saved.", type = "message", duration = 3)
      }, error = function(e) {
        shiny::showNotification(paste("Restore failed:", conditionMessage(e)),
          type = "error", duration = 5)
      })
    }

    # --- End file I/O helpers ---

    send_values_update <- function() {
      m <- shiny::isolate(model())
      if (is.null(m)) return()
      values_df <- openqaly::get_model_values(m)
      initial_data <- lapply(seq_len(nrow(values_df)), function(i) {
        row <- as.list(values_df[i, ])
        for (nm in names(row)) {
          if (is.na(row[[nm]])) row[[nm]] <- ""
        }
        row$formula <- as.character(row$formula)
        if ("discounting_override" %in% names(row)) {
          row$discounting_override <- as.character(row$discounting_override)
          if (row$discounting_override == "NA") row$discounting_override <- ""
        }
        row[["_id"]] <- paste(row$name, row$state, row$destination, sep = "|")
        row
      })
      session$sendCustomMessage("values-table-update", list(
        inputId = "model_action",
        data = initial_data
      ))
    }

    apply_action <- function(action) {
      old <- model()
      new <- dispatch_model_action(old, action)
      if (identical(old, new)) {
        return(list(status = "noop", model = new))
      }
      history$push(old)
      model(new)
      list(status = "ok", model = new)
    }

    perform_undo <- function() {
      restored <- history$undo(model())
      if (!is.null(restored)) {
        model(restored)
        file_load_counter(file_load_counter() + 1L)
        table_render_trigger(table_render_trigger() + 1L)
      }
    }

    perform_redo <- function() {
      restored <- history$redo(model())
      if (!is.null(restored)) {
        model(restored)
        file_load_counter(file_load_counter() + 1L)
        table_render_trigger(table_render_trigger() + 1L)
      }
    }

    # --- Title bar + dirty state ---

    output$editor_title <- shiny::renderText({
      fpath <- current_file_path()
      dirty <- is_dirty()
      if (is.null(fpath)) return("Model Editor")
      fname <- basename(fpath)
      if (dirty) paste("Model Editor -", fname, "*") else paste("Model Editor -", fname)
    })

    shiny::observe({
      dirty <- is_dirty()
      session$sendCustomMessage("set_dirty", dirty)
    })

    # --- Save observer (prompt modal for description) ---

    shiny::observeEvent(input$save_file, {
      m <- model()
      fpath <- current_file_path()
      if (is.null(m)) return()
      if (is.null(fpath)) {
        shiny::showNotification("No file open. Use Save As to save to a new file.",
          type = "warning", duration = 3)
        return()
      }
      if (!is_dirty()) return()
      shiny::showModal(shiny::modalDialog(
        title = "Save Model",
        shiny::textInput("save_message", "Description (optional):",
          placeholder = "e.g., Updated transition probabilities", width = "100%"),
        easyClose = TRUE,
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          tags$button(class = "btn btn-primary", type = "button",
            onclick = "Shiny.setInputValue('confirm_save', Math.random(), {priority: 'event'});",
            "Save")
        )
      ))
    })

    shiny::observeEvent(input$confirm_save, {
      m <- model()
      fpath <- current_file_path()
      if (is.null(m) || is.null(fpath)) return()
      msg <- input$save_message %||% ""
      shiny::removeModal()
      tryCatch({
        save_model_to_file(m, fpath, message = msg)
        saved_model(m)
        shiny::showNotification("Model saved.", type = "message", duration = 2, id = "save_notify")
      }, error = function(e) {
        shiny::showNotification(paste("Save failed:", conditionMessage(e)),
          type = "error", duration = NULL, id = "save_notify")
      })
    })

    # --- Save As observer ---

    shiny::observeEvent(input$save_as_file, {
      m <- model()
      shiny::req(m)
      save_path <- shinyFiles::parseSavePath(volumes, input$save_as_file)
      shiny::req(nrow(save_path) > 0)
      fpath <- as.character(save_path$datapath)
      tryCatch({
        save_model_to_file(m, fpath, message = "")
        current_file_path(fpath)
        saved_model(m)
        shiny::showNotification(paste("Model saved to", basename(fpath)),
          type = "message", duration = 3, id = "save_notify")
      }, error = function(e) {
        shiny::showNotification(paste("Save failed:", conditionMessage(e)),
          type = "error", duration = NULL, id = "save_notify")
      })
    })

    # --- File Open with unsaved-changes check ---

    shiny::observeEvent(input$open_file, {
      shiny::req(is.list(input$open_file))
      file_path <- shinyFiles::parseFilePaths(volumes, input$open_file)
      shiny::req(nrow(file_path) > 0)
      fpath <- as.character(file_path$datapath)

      if (is_dirty()) {
        pending_open_file(fpath)
        has_save_path <- !is.null(current_file_path())
        footer_buttons <- if (has_save_path) {
          shiny::tagList(
            shiny::modalButton("Cancel"),
            tags$button(class = "btn btn-primary", type = "button",
              onclick = "Shiny.setInputValue('open_save_first', Math.random(), {priority: 'event'});",
              "Save & Open"),
            tags$button(class = "btn btn-danger", type = "button",
              onclick = "Shiny.setInputValue('open_discard', Math.random(), {priority: 'event'});",
              "Discard & Open")
          )
        } else {
          shiny::tagList(
            shiny::modalButton("Cancel"),
            tags$button(class = "btn btn-danger", type = "button",
              onclick = "Shiny.setInputValue('open_discard', Math.random(), {priority: 'event'});",
              "Discard & Open")
          )
        }
        shiny::showModal(shiny::modalDialog(
          title = "Unsaved Changes",
          shiny::tags$p("You have unsaved changes. Opening a new file will discard them."),
          footer = footer_buttons
        ))
      } else {
        tryCatch(
          do_open_file(fpath),
          error = function(e) {
            shiny::showNotification(paste("Failed to open file:", conditionMessage(e)),
              type = "error", duration = 5)
          }
        )
      }
    })

    shiny::observeEvent(input$open_save_first, {
      fpath <- current_file_path()
      if (is.null(fpath)) return()
      pending_fpath <- pending_open_file()
      tryCatch({
        save_model_to_file(model(), fpath, message = "Auto-save before open")
        saved_model(model())
        shiny::removeModal()
        do_open_file(pending_fpath)
      }, error = function(e) {
        shiny::showNotification(paste("Save failed:", conditionMessage(e)), type = "error", duration = 5)
      }, finally = {
        pending_open_file(NULL)
      })
    })

    shiny::observeEvent(input$open_discard, {
      pending_fpath <- pending_open_file()
      shiny::removeModal()
      tryCatch({
        do_open_file(pending_fpath)
      }, error = function(e) {
        shiny::showNotification(paste("Failed to open file:", conditionMessage(e)), type = "error", duration = 5)
      }, finally = {
        pending_open_file(NULL)
      })
    })

    # --- Startup path loader ---

    if (!is.null(path)) {
      shiny::observeEvent(TRUE, {
        do_open_file(path)
      }, once = TRUE)
    }

    # --- Version History ---

    shiny::observeEvent(input$version_history, {
      show_version_history_modal()
    })

    output$version_diff_output <- shiny::renderUI({
      shiny::req(input$version_select)
      fpath <- current_file_path()
      shiny::req(fpath)
      tryCatch({
        versions <- list_versions(fpath)
        selected_idx <- which(versions$id == input$version_select)
        # Versions are sorted newest first; the previous (older) version is at idx+1
        if (length(selected_idx) == 1 && selected_idx < nrow(versions)) {
          prev_id <- versions$id[selected_idx + 1]
          prev_model <- load_version_model(fpath, prev_id)
          prev_lines <- model_to_yaml_lines(prev_model)
        } else {
          # Oldest version (initial) — no previous, show no diff
          return(shiny::tags$div(class = "text-muted p-3", "Initial version — no prior changes."))
        }
        version_model <- load_version_model(fpath, input$version_select)
        version_lines <- model_to_yaml_lines(version_model)
        if (identical(prev_lines, version_lines)) {
          return(shiny::tags$div(class = "text-muted p-3", "No differences from previous version."))
        }
        mode <- input$version_diff_mode %||% "sidebyside"
        diff_result <- diffobj::diffChr(
          prev_lines, version_lines,
          format = "html", context = 7L,
          style = list(html.output = "diff.w.style"),
          mode = mode,
          tar.banner = "Previous Version",
          cur.banner = "This Version"
        )
        shiny::tags$div(style = "overflow-y: auto; flex: 1; min-height: 0;", shiny::HTML(as.character(diff_result)))
      }, error = function(e) {
        shiny::tags$div(class = "alert alert-danger", paste("Diff failed:", conditionMessage(e)))
      })
    })

    shiny::observeEvent(input$restore_version, {
      fpath <- current_file_path()
      if (is.null(fpath) || is.null(input$version_select)) return()
      pending_restore_version(input$version_select)

      if (is_dirty()) {
        shiny::showModal(shiny::modalDialog(
          title = "Unsaved Changes",
          shiny::tags$p("Your current changes will be saved as a new version before restoring."),
          footer = shiny::tagList(
            tags$button(class = "btn btn-secondary", type = "button",
              onclick = "Shiny.setInputValue('restore_cancel', Math.random(), {priority: 'event'});",
              "Cancel"),
            tags$button(class = "btn btn-primary", type = "button",
              onclick = "Shiny.setInputValue('confirm_restore', Math.random(), {priority: 'event'});",
              "Save & Restore"),
            tags$button(class = "btn btn-danger", type = "button",
              onclick = "Shiny.setInputValue('discard_restore', Math.random(), {priority: 'event'});",
              "Discard & Restore")
          )
        ))
      } else {
        do_restore_version(skip_backup = FALSE)
      }
    })

    shiny::observeEvent(input$confirm_restore, {
      fpath <- current_file_path()
      if (is.null(fpath)) return()
      tryCatch({
        save_model_to_file(model(), fpath, message = "Auto-save before restore")
        do_restore_version(skip_backup = TRUE)
      }, error = function(e) {
        shiny::showNotification(paste("Save failed:", conditionMessage(e)),
          type = "error", duration = 5)
      })
    })

    shiny::observeEvent(input$discard_restore, {
      do_restore_version(skip_backup = FALSE)
    })

    shiny::observeEvent(input$restore_cancel, {
      selected <- pending_restore_version()
      pending_restore_version(NULL)
      show_version_history_modal(selected_version = selected)
    })

    # Central dispatcher for all frontend change events
    shiny::observeEvent(input$model_action, {
      action <- input$model_action
      shiny::req(action$type)

      # Handle remove_value dependency errors with force confirmation
      if (action$type == "remove_value") {
        tryCatch({
          result <- apply_action(action)
          send_values_update()
        }, value_has_dependencies = function(e) {
          deps <- e$dependencies
          dep_items <- paste0("<li>", htmltools::htmlEscape(unlist(deps)), "</li>", collapse = "")
          escaped_name <- gsub("'", "\\\\'", action$name)
          escaped_state <- gsub("'", "\\\\'", action$state %||% "")
          escaped_dest <- gsub("'", "\\\\'", action$destination %||% "")
          shiny::showModal(shiny::modalDialog(
            title = "Value has dependencies",
            shiny::tags$p(conditionMessage(e)),
            shiny::tags$p("Do you want to force remove this value and all its dependencies?"),
            footer = shiny::tagList(
              shiny::modalButton("Cancel"),
              shiny::actionButton("confirm_force_remove_value", "Remove Anyway",
                                  class = "btn-danger",
                                  onclick = paste0(
                                    "Shiny.setInputValue('model_action', ",
                                    "{type: 'force_remove_value', name: '", escaped_name,
                                    "', state: '", escaped_state,
                                    "', destination: '", escaped_dest, "'}, ",
                                    "{priority: 'event'}); ",
                                    "Shiny.onInputChange('close_modal', Math.random());"
                                  ))
            )
          ))
          send_values_update()
        }, error = function(e) {
          shiny::showNotification(
            paste("Action failed:", conditionMessage(e)),
            type = "error"
          )
          file_load_counter(file_load_counter() + 1L)
        })
        return()
      }

      # Handle edit_value name field — name sharing and field changes modals
      if (action$type == "edit_value" && identical(action$field, "name")) {
        tryCatch({
          result <- apply_action(action)
          send_values_update()
        }, value_name_shared = function(e) {
          escaped_old_name <- gsub("'", "\\\\'", action$name)
          escaped_new_name <- gsub("'", "\\\\'", action$value)
          escaped_state <- gsub("'", "\\\\'", action$state %||% "")
          escaped_dest <- gsub("'", "\\\\'", action$destination %||% "")
          shiny::showModal(shiny::modalDialog(
            title = "Rename shared value",
            shiny::tags$p(conditionMessage(e)),
            footer = shiny::tagList(
              shiny::modalButton("Cancel"),
              shiny::actionButton("rename_value_single_btn", "Just This Row",
                                  class = "btn-warning",
                                  onclick = paste0(
                                    "Shiny.setInputValue('model_action', ",
                                    "{type: 'rename_value_single', name: '", escaped_old_name,
                                    "', new_name: '", escaped_new_name,
                                    "', state: '", escaped_state,
                                    "', destination: '", escaped_dest, "'}, ",
                                    "{priority: 'event'}); ",
                                    "Shiny.onInputChange('close_modal', Math.random());"
                                  )),
              shiny::actionButton("rename_value_all_btn", "All Rows",
                                  class = "btn-primary",
                                  onclick = paste0(
                                    "Shiny.setInputValue('model_action', ",
                                    "{type: 'rename_value_all', name: '", escaped_old_name,
                                    "', new_name: '", escaped_new_name,
                                    "', state: '", escaped_state,
                                    "', destination: '", escaped_dest, "'}, ",
                                    "{priority: 'event'}); ",
                                    "Shiny.onInputChange('close_modal', Math.random());"
                                  ))
            )
          ))
          send_values_update()
        }, value_field_changes = function(e) {
          escaped_name <- gsub("'", "\\\\'", action$name)
          escaped_new_name <- gsub("'", "\\\\'", action$value)
          escaped_state <- gsub("'", "\\\\'", action$state %||% "")
          escaped_dest <- gsub("'", "\\\\'", action$destination %||% "")
          shiny::showModal(shiny::modalDialog(
            title = "Field changes",
            shiny::tags$p(conditionMessage(e)),
            footer = shiny::tagList(
              shiny::modalButton("Cancel"),
              shiny::actionButton("confirm_value_field_changes", "Proceed",
                                  class = "btn-primary",
                                  onclick = paste0(
                                    "Shiny.setInputValue('model_action', ",
                                    "{type: 'edit_value', name: '", escaped_name,
                                    "', state: '", escaped_state,
                                    "', destination: '", escaped_dest,
                                    "', field: 'name', value: '", escaped_new_name,
                                    "', error_on_name_sharing: false, error_on_field_changes: false}, ",
                                    "{priority: 'event'}); ",
                                    "Shiny.onInputChange('close_modal', Math.random());"
                                  ))
            )
          ))
          send_values_update()
        }, error = function(e) {
          shiny::showNotification(
            paste("Action failed:", conditionMessage(e)),
            type = "error"
          )
          file_load_counter(file_load_counter() + 1L)
        })
        return()
      }

      # Handle rename_value_single — catches field changes
      if (action$type == "rename_value_single") {
        tryCatch({
          result <- apply_action(action)
          send_values_update()
        }, value_field_changes = function(e) {
          escaped_name <- gsub("'", "\\\\'", action$name)
          escaped_new_name <- gsub("'", "\\\\'", action$new_name)
          escaped_state <- gsub("'", "\\\\'", action$state %||% "")
          escaped_dest <- gsub("'", "\\\\'", action$destination %||% "")
          shiny::showModal(shiny::modalDialog(
            title = "Field changes",
            shiny::tags$p(conditionMessage(e)),
            footer = shiny::tagList(
              shiny::modalButton("Cancel"),
              shiny::actionButton("confirm_rename_value_field_changes", "Proceed",
                                  class = "btn-primary",
                                  onclick = paste0(
                                    "Shiny.setInputValue('model_action', ",
                                    "{type: 'edit_value', name: '", escaped_name,
                                    "', state: '", escaped_state,
                                    "', destination: '", escaped_dest,
                                    "', field: 'name', value: '", escaped_new_name,
                                    "', error_on_name_sharing: false, error_on_field_changes: false}, ",
                                    "{priority: 'event'}); ",
                                    "Shiny.onInputChange('close_modal', Math.random());"
                                  ))
            )
          ))
          send_values_update()
        }, error = function(e) {
          shiny::showNotification(
            paste("Action failed:", conditionMessage(e)),
            type = "error"
          )
          file_load_counter(file_load_counter() + 1L)
        })
        return()
      }

      # Handle remove_summary dependency errors with force confirmation
      if (action$type == "remove_summary") {
        tryCatch({
          result <- apply_action(action)
          file_load_counter(file_load_counter() + 1L)
        }, summary_has_dependencies = function(e) {
          escaped_name <- gsub("'", "\\\\'", action$name)
          shiny::showModal(shiny::modalDialog(
            title = "Summary has dependencies",
            shiny::tags$p(conditionMessage(e)),
            shiny::tags$p("Do you want to force remove this summary and all its dependencies?"),
            footer = shiny::tagList(
              shiny::modalButton("Cancel"),
              shiny::actionButton("confirm_force_remove_summary", "Remove Anyway",
                                  class = "btn-danger",
                                  onclick = paste0(
                                    "Shiny.setInputValue('model_action', ",
                                    "{type: 'force_remove_summary', name: '", escaped_name, "'}, ",
                                    "{priority: 'event'}); ",
                                    "Shiny.onInputChange('close_modal', Math.random());"
                                  ))
            )
          ))
        }, error = function(e) {
          shiny::showNotification(paste("Action failed:", conditionMessage(e)), type = "error")
          file_load_counter(file_load_counter() + 1L)
        })
        return()
      }

      # Handle remove_state dependency errors with force confirmation
      if (action$type == "remove_state") {
        tryCatch({
          result <- apply_action(action)
          if (result$status == "noop") {
            file_load_counter(file_load_counter() + 1L)
          } else {
            file_load_counter(file_load_counter() + 1L)
          }
        }, error = function(e) {
          msg <- conditionMessage(e)
          if (grepl("dependenc", msg, ignore.case = TRUE)) {
            shiny::showModal(shiny::modalDialog(
              title = "State has dependencies",
              shiny::tags$p(msg),
              shiny::tags$p("Do you want to force remove this state and all its dependencies?"),
              footer = shiny::tagList(
                shiny::modalButton("Cancel"),
                shiny::actionButton("confirm_force_remove_state", "Force Remove",
                                    class = "btn-danger",
                                    onclick = paste0(
                                      "Shiny.setInputValue('model_action', ",
                                      "{type: 'force_remove_state', name: '",
                                      gsub("'", "\\\\'", action$name), "'}, ",
                                      "{priority: 'event'}); ",
                                      "Shiny.onInputChange('close_modal', Math.random());"
                                    ))
              )
            ))
          } else {
            shiny::showNotification(
              paste("Action failed:", msg),
              type = "error"
            )
            file_load_counter(file_load_counter() + 1L)
          }
        })
        return()
      }

      tryCatch({
        result <- apply_action(action)
        value_types <- c("add_value", "edit_value", "remove_value",
                         "force_remove_value", "rename_value_single",
                         "rename_value_all")
        if (result$status == "noop") {
          file_load_counter(file_load_counter() + 1L)
        } else if (action$type %in% value_types) {
          send_values_update()
        } else if (action$type %in% c("add_variable", "add_state", "add_transition",
                                        "force_remove_state", "remove_transition",
                                        "add_summary", "edit_summary",
                                        "force_remove_summary")) {
          file_load_counter(file_load_counter() + 1L)
        }
      }, error = function(e) {
        shiny::showNotification(
          paste("Action failed:", conditionMessage(e)),
          type = "error"
        )
        file_load_counter(file_load_counter() + 1L)
      })
    })

    shiny::observeEvent(input$close_modal, {
      shiny::removeModal()
    })

    output$model_loaded <- shiny::reactive({
      !is.null(model())
    })
    shiny::outputOptions(output, "model_loaded", suspendWhenHidden = FALSE)

    shiny::observeEvent(input$show_diff_modal, {
      m <- original_model()
      cm <- model()
      shiny::req(m, cm)

      diff_content <- tryCatch({
        original_lines <- model_to_yaml_lines(m)
        current_lines <- model_to_yaml_lines(cm)

        if (identical(original_lines, current_lines)) {
          shiny::tags$div(
            class = "text-muted p-3",
            "No changes from original model."
          )
        } else {
          diff_result <- diffobj::diffChr(
            original_lines,
            current_lines,
            format = "html",
            context = 7L,
            style = list(html.output = "diff.w.style"),
            mode = "sidebyside",
            tar.banner = "Original Model",
            cur.banner = "Current"
          )
          shiny::tags$div(
            style = "max-height: 70vh; overflow: auto;",
            shiny::HTML(as.character(diff_result))
          )
        }
      }, error = function(e) {
        shiny::tags$div(
          class = "alert alert-danger",
          paste("Failed to generate diff:", conditionMessage(e))
        )
      })

      shiny::showModal(shiny::modalDialog(
        title = "Model Diff",
        diff_content,
        size = "xl",
        easyClose = TRUE,
        footer = shiny::modalButton("Close")
      ))
    })

    # --- Results page ---
    editor_results_rv <- shiny::reactiveValues(results = NULL, metadata = NULL)

    output$override_panel <- shiny::renderUI({
      m <- model()
      if (is.null(m)) return(NULL)
      cats <- openqaly::get_override_categories(m)
      if (length(cats) == 0) {
        return(htmltools::tagList(
          override_input_dependency(),
          override_manager_dependency(),
          tags$div(class = "text-muted p-3",
            tags$p("This model has no overrides."),
            tags$button(
              type = "button",
              class = "override-manage-btn btn btn-outline-secondary btn-sm",
              `data-input-id` = "editor_overrides",
              "\u2699 Manage Overrides"
            )
          )
        ))
      }
      overrideInput("editor_overrides", m)
    })

    overrideManagerServer("editor_overrides",
      model = shiny::reactive(model()),
      on_action = function(action) {
        apply_action(action)
      }
    )

    editor_override_values <- shiny::reactive({
      m <- model()
      if (is.null(m)) return(NULL)
      cats <- openqaly::get_override_categories(m)
      if (length(cats) == 0) return(NULL)
      values <- list()
      for (cat in cats) {
        for (override in cat$overrides) {
          input_id <- .build_override_id("editor_overrides", override)
          val <- input[[input_id]]
          if (!is.null(val)) {
            values <- c(values, list(list(
              name = override$name,
              expression = as.character(val),
              strategy = override$strategy %||% "",
              group = override$group %||% ""
            )))
          }
        }
      }
      values
    })
    editor_override_values_debounced <- shiny::debounce(editor_override_values, 1000)

    build_editor_model <- function() {
      m <- model()
      vals <- editor_override_values_debounced()
      if (is.null(m)) return(NULL)
      if (!is.null(vals) && length(vals) > 0) {
        m <- openqaly::set_override_expressions(m, vals)
      }
      m
    }

    # --- Async model run with progress reporting ---
    run_state <- shiny::reactiveValues(
      running = FALSE,
      progress_file = NULL,
      needs_rerun = FALSE,
      last_error = FALSE
    )
    rerun_trigger <- shiny::reactiveVal(0L)

    # Open the results sidebar when navigating to the results page
    shiny::observeEvent(input$active_page, {
      if (input$active_page == "results") {
        bslib::toggle_sidebar("results_sidebar", open = TRUE)
      }
      if (input$active_page == "dsa") {
        bslib::toggle_sidebar("dsa_sidebar", open = TRUE)
      }
      if (input$active_page == "scenario") {
        bslib::toggle_sidebar("scenario_sidebar", open = TRUE)
      }
    })

    # Track whether we're on a run-eligible page using a reactiveVal.
    # Only update when the boolean actually changes.
    on_run_page <- shiny::reactiveVal(FALSE)
    shiny::observeEvent(input$active_page, {
      is_run_page <- input$active_page == "results"
      if (!identical(is_run_page, shiny::isolate(on_run_page()))) {
        on_run_page(is_run_page)
      }
    })

    # Main run observer
    shiny::observe({
      shiny::req(on_run_page())
      m <- build_editor_model()
      rerun_trigger()
      shiny::req(m)

      if (shiny::isolate(run_state$running)) {
        run_state$needs_rerun <- TRUE
        return(NULL)
      }

      pf <- create_progress_file()
      run_state$running <- TRUE
      run_state$progress_file <- pf
      run_state$last_error <- FALSE
      session$sendCustomMessage("editor_progress", list(state = "running", pct = 0))

      p <- suppressWarnings(promises::future_promise({
        cb <- make_file_progress_callback(pf)
        openqaly::run_model(m, progress = cb)
      }, seed = TRUE))

      promises::then(p,
        onFulfilled = function(res) {
          editor_results_rv$results <- res
          editor_results_rv$metadata <- res$metadata
          session$sendCustomMessage("editor_progress", list(state = "done"))
          run_state$running <- FALSE
          run_state$progress_file <- NULL
          unlink(pf)
          if (run_state$needs_rerun) {
            run_state$needs_rerun <- FALSE
            rerun_trigger(shiny::isolate(rerun_trigger()) + 1L)
          }
        },
        onRejected = function(err) {
          editor_results_rv$results <- NULL
          editor_results_rv$metadata <- NULL
          run_state$last_error <- TRUE
          session$sendCustomMessage("editor_progress", list(state = "error"))
          shiny::showNotification(
            paste("Model run failed:", conditionMessage(err)),
            type = "error", duration = 10
          )
          run_state$running <- FALSE
          run_state$progress_file <- NULL
          unlink(pf)
          if (run_state$needs_rerun) {
            run_state$needs_rerun <- FALSE
            rerun_trigger(shiny::isolate(rerun_trigger()) + 1L)
          }
        }
      )

      NULL
    })

    # Progress polling observer
    shiny::observe({
      shiny::req(run_state$running, run_state$progress_file)
      shiny::invalidateLater(250)
      prog <- read_file_progress(run_state$progress_file)
      if (!is.null(prog) && prog$total > 0) {
        session$sendCustomMessage("editor_progress", list(
          state = "running",
          pct = round(prog$pct * 100)
        ))
      }
    })

    # DSA reactive state (defined here so onSessionEnded can reference it)
    dsa_results_rv <- shiny::reactiveValues(results = NULL, metadata = NULL)
    dsa_run_state <- shiny::reactiveValues(running = FALSE, progress_file = NULL)

    # VBP reactive state
    vbp_results_rv <- shiny::reactiveValues(results = NULL)
    vbp_run_state <- shiny::reactiveValues(running = FALSE, progress_file = NULL)

    # Scenario reactive state
    scenario_results_rv <- shiny::reactiveValues(results = NULL, metadata = NULL)
    scenario_run_state <- shiny::reactiveValues(running = FALSE, progress_file = NULL)

    # TWSA reactive state
    twsa_results_rv <- shiny::reactiveValues(results = NULL, metadata = NULL)
    twsa_run_state <- shiny::reactiveValues(running = FALSE, progress_file = NULL)

    # Clean up progress files on session end
    shiny::onSessionEnded(function() {
      pf <- shiny::isolate(run_state$progress_file)
      if (!is.null(pf)) unlink(pf)
      dpf <- shiny::isolate(dsa_run_state$progress_file)
      if (!is.null(dpf)) unlink(dpf)
      vpf <- shiny::isolate(vbp_run_state$progress_file)
      if (!is.null(vpf)) unlink(vpf)
      spf <- shiny::isolate(scenario_run_state$progress_file)
      if (!is.null(spf)) unlink(spf)
      tpf <- shiny::isolate(twsa_run_state$progress_file)
      if (!is.null(tpf)) unlink(tpf)
      ppf <- shiny::isolate(psa_run_state$progress_file)
      if (!is.null(ppf)) unlink(ppf)
    })

    output$has_editor_results <- shiny::reactive(!is.null(editor_results_rv$results))
    shiny::outputOptions(output, "has_editor_results", suspendWhenHidden = FALSE)

    editor_results_reactive <- shiny::reactive(editor_results_rv$results)
    editor_metadata_reactive <- shiny::reactive(editor_results_rv$metadata)

    traceResultsServer("editor_trace", editor_results_reactive, editor_metadata_reactive)
    outcomesResultsServer("editor_outcomes", editor_results_reactive, editor_metadata_reactive)
    costsResultsServer("editor_costs", editor_results_reactive, editor_metadata_reactive)
    nmbResultsServer("editor_nmb", editor_results_reactive, editor_metadata_reactive)
    pairwiseCeResultsServer("editor_pairwise_ce", editor_results_reactive, editor_metadata_reactive)
    incrementalCeResultsServer("editor_incremental_ce", editor_results_reactive, editor_metadata_reactive)

    # --- Diagnostics page ---
    variableDiagnosticsServer("editor_variable_diagnostics",
      editor_results_reactive, editor_metadata_reactive)

    decisionTreeResultsServer("editor_decision_trees",
      editor_results_reactive, editor_metadata_reactive)

    transitionHeatmapServer("editor_transitions",
      editor_results_reactive, editor_metadata_reactive)

    # --- DSA page ---
    output$dsa_inputs_panel <- shiny::renderUI({
      file_load_counter()
      m <- shiny::isolate(model())
      if (is.null(m)) {
        return(tags$div(class = "text-muted p-3",
          "Load a model first."
        ))
      }

      var_choices <- openqaly::get_variable_names(m)
      var_targeting <- openqaly::get_variable_targeting(m)

      strategies_df <- openqaly::get_strategies(m)
      strategy_choices <- if (nrow(strategies_df) > 0) {
        setNames(strategies_df$name, strategies_df$display_name)
      } else {
        character(0)
      }

      groups_df <- openqaly::get_groups(m)
      group_choices <- if (nrow(groups_df) > 0) {
        c(
          "Overall" = "overall",
          "All (Overall + Groups)" = "all",
          "All Groups" = "all_groups",
          setNames(groups_df$name, groups_df$display_name)
        )
      } else {
        character(0)
      }
      individual_groups <- group_choices[
        !group_choices %in% c("overall", "all", "all_groups")
      ]
      setting_choices <- get_dsa_setting_choices()

      # Build variable formulas keyed by "name|strategy|group" for base case column
      vars_df <- openqaly::get_variables(m)
      variable_formulas <- list()
      for (i in seq_len(nrow(vars_df))) {
        strat_val <- if (is.null(vars_df$strategy[i]) || is.na(vars_df$strategy[i])) "" else vars_df$strategy[i]
        grp_val <- if (is.null(vars_df$group[i]) || is.na(vars_df$group[i])) "" else vars_df$group[i]
        key <- paste0(vars_df$name[i], "|", strat_val, "|", grp_val)
        variable_formulas[[key]] <- as.character(vars_df$formula[i])
      }
      setting_values <- openqaly::get_settings(m)
      setting_values$discount_rate <- setting_values$discount_outcomes

      param_table <- shiny::tags$div(
        dsa_params_dependency(),
        formula_input_dependency(),
        shiny::tags$div(
          style = "display: inline-flex; gap: 6px; margin-bottom: 6px;",
          shiny::tags$button(
            type = "button",
            class = "btn btn-sm btn-outline-secondary dsa-add-variable-btn",
            "+ Add Variable"
          ),
          shiny::tags$button(
            type = "button",
            class = "btn btn-sm btn-outline-secondary dsa-add-setting-btn",
            "+ Add Setting"
          )
        ),
        shiny::tags$div(
          id = "dsa_params_grid",
          class = "dsa-params-container",
          `data-input-id` = "editor_dsa_params",
          `data-variables` = jsonlite::toJSON(var_choices, auto_unbox = TRUE),
          `data-settings` = jsonlite::toJSON(
            as.list(setting_choices), auto_unbox = TRUE
          ),
          `data-strategies` = jsonlite::toJSON(
            as.list(strategy_choices), auto_unbox = TRUE
          ),
          `data-groups` = jsonlite::toJSON(
            as.list(individual_groups), auto_unbox = TRUE
          ),
          `data-variable-targeting` = jsonlite::toJSON(
            var_targeting, auto_unbox = TRUE
          ),
          `data-initial` = jsonlite::toJSON(
            lapply(openqaly::get_dsa_parameters(m), function(p) {
              list(
                type = p$type,
                name = p$name,
                display_name = p$display_name %||% p$name,
                strategy = p$strategy %||% "",
                group = p$group %||% "",
                low = as.character(p$low),
                high = as.character(p$high)
              )
            }), auto_unbox = TRUE
          ),
          `data-variable-formulas` = jsonlite::toJSON(
            variable_formulas, auto_unbox = TRUE
          ),
          `data-setting-values` = jsonlite::toJSON(
            as.list(setting_values), auto_unbox = TRUE
          ),
          `data-terms` = jsonlite::toJSON(
            get_model_terms(m, "dsa_bound"), auto_unbox = FALSE
          ),
          `data-suggestions` = jsonlite::toJSON(
            get_model_suggestions(m, "dsa_bound"), auto_unbox = FALSE
          )
        )
      )

      shiny::tagList(
        param_table,
        shiny::tags$button(
          type = "button",
          class = "btn btn-primary mt-2 w-100 dsa-run-btn",
          "Run DSA Analysis"
        )
      )
    })

    shiny::observeEvent(input$run_dsa_action, {
      params <- normalize_dsa_params(input$run_dsa_action$params)
      if (length(params) == 0) {
        params <- normalize_dsa_params(input$editor_dsa_params)
      }
      if (length(params) == 0) {
        shiny::showNotification("Please add at least one parameter.",
          type = "warning")
        return()
      }

      m <- build_editor_model()
      if (is.null(m)) {
        shiny::showNotification("No model loaded.", type = "warning")
        return()
      }

      m <- apply_dsa_params(m, params)

      dsa_args <- list(m)

      pf <- create_progress_file()
      dsa_run_state$running <- TRUE
      dsa_run_state$progress_file <- pf
      session$sendCustomMessage("dsa_progress", list(state = "running", pct = 0))

      dsa_args$progress <- make_file_progress_callback(pf)

      p <- suppressWarnings(promises::future_promise({
        do.call(openqaly::run_dsa, dsa_args)
      }, seed = TRUE))

      promises::then(p,
        onFulfilled = function(res) {
          dsa_results_rv$results <- res
          dsa_results_rv$metadata <- res$metadata
          session$sendCustomMessage("dsa_progress", list(state = "done"))
          dsa_run_state$running <- FALSE
          dsa_run_state$progress_file <- NULL
          unlink(pf)
          shiny::showNotification("DSA analysis complete.", type = "message")
        },
        onRejected = function(err) {
          dsa_results_rv$results <- NULL
          dsa_results_rv$metadata <- NULL
          session$sendCustomMessage("dsa_progress", list(state = "error"))
          dsa_run_state$running <- FALSE
          dsa_run_state$progress_file <- NULL
          unlink(pf)
          shiny::showNotification(
            paste("DSA analysis failed:", conditionMessage(err)),
            type = "error", duration = 10
          )
        }
      )

      NULL
    })

    # DSA progress polling
    shiny::observe({
      shiny::req(dsa_run_state$running, dsa_run_state$progress_file)
      shiny::invalidateLater(250)
      prog <- read_file_progress(dsa_run_state$progress_file)
      if (!is.null(prog) && prog$total > 0) {
        session$sendCustomMessage("dsa_progress", list(
          state = "running",
          pct = round(prog$pct * 100)
        ))
      }
    })

    output$has_dsa_results <- shiny::reactive(!is.null(dsa_results_rv$results))
    shiny::outputOptions(output, "has_dsa_results", suspendWhenHidden = FALSE)

    dsa_results_reactive <- shiny::reactive(dsa_results_rv$results)
    dsa_metadata_reactive <- shiny::reactive(dsa_results_rv$metadata)
    dsaResultTabServer("editor_dsa_outcomes", "outcomes",
      dsa_results_reactive, dsa_metadata_reactive)
    dsaResultTabServer("editor_dsa_costs", "costs",
      dsa_results_reactive, dsa_metadata_reactive)
    dsaResultTabServer("editor_dsa_nmb", "nmb",
      dsa_results_reactive, dsa_metadata_reactive)
    dsaResultTabServer("editor_dsa_ce", "ce",
      dsa_results_reactive, dsa_metadata_reactive)
    dsaResultTabServer("editor_dsa_vbp", "vbp",
      dsa_results_reactive, dsa_metadata_reactive)

    # --- VBP page ---
    output$vbp_inputs_panel <- shiny::renderUI({
      m <- model()
      if (is.null(m)) {
        return(tags$div(class = "text-muted p-3", "Load a model first."))
      }

      var_choices <- get_variable_choices(m)

      strategies_df <- openqaly::get_strategies(m)
      strat_choices <- if (nrow(strategies_df) > 0) {
        setNames(strategies_df$name, strategies_df$display_name)
      } else {
        character(0)
      }

      sdf <- openqaly::get_model_summaries(m)
      outcome_choices <- if (!is.null(sdf) && nrow(sdf) > 0) {
        odf <- sdf[sdf$type == "outcome", , drop = FALSE]
        if (nrow(odf) > 0) setNames(odf$name, odf$display_name) else character(0)
      } else {
        character(0)
      }
      cost_choices <- if (!is.null(sdf) && nrow(sdf) > 0) {
        cdf <- sdf[sdf$type == "cost", , drop = FALSE]
        if (nrow(cdf) > 0) setNames(cdf$name, cdf$display_name) else character(0)
      } else {
        character(0)
      }

      # Pre-populate from VBP config if available
      vbp_config <- openqaly::get_vbp(m)

      default_price <- if (!is.null(vbp_config$price_variable) &&
                           vbp_config$price_variable %in% var_choices) {
        vbp_config$price_variable
      } else if (length(var_choices) > 0) {
        var_choices[1]
      } else NULL

      default_intervention <- if (!is.null(vbp_config$intervention_strategy) &&
                                  vbp_config$intervention_strategy %in% strat_choices) {
        vbp_config$intervention_strategy
      } else if (length(strat_choices) > 1) {
        strat_choices[2]
      } else if (length(strat_choices) > 0) {
        strat_choices[1]
      } else NULL

      default_outcome <- if (!is.null(vbp_config$outcome_summary) &&
                             vbp_config$outcome_summary %in% outcome_choices) {
        vbp_config$outcome_summary
      } else if (length(outcome_choices) > 0) {
        outcome_choices[1]
      } else NULL

      default_cost <- if (!is.null(vbp_config$cost_summary) &&
                          vbp_config$cost_summary %in% cost_choices) {
        vbp_config$cost_summary
      } else if (length(cost_choices) > 0) {
        cost_choices[1]
      } else NULL

      shiny::tagList(
        shiny::checkboxInput("editor_vbp_enabled", "Enable VBP",
          value = !is.null(vbp_config)),
        shiny::conditionalPanel(
          condition = "input.editor_vbp_enabled == true",
          shiny::selectInput("editor_vbp_price_variable", "Price Variable",
            choices = var_choices,
            selected = default_price
          ),
          shiny::selectInput("editor_vbp_intervention", "Intervention Strategy",
            choices = strat_choices,
            selected = default_intervention
          ),
          shiny::selectInput("editor_vbp_outcome", "Outcome Summary",
            choices = outcome_choices,
            selected = default_outcome
          ),
          shiny::selectInput("editor_vbp_cost", "Cost Summary",
            choices = cost_choices,
            selected = default_cost
          ),
          shiny::actionButton("editor_run_vbp", "Run VBP Analysis",
            class = "btn-primary mt-2 w-100"
          )
        )
      )
    })

    # Persist VBP config on every dropdown change (only when enabled)
    shiny::observe({
      if (!isTRUE(input$editor_vbp_enabled)) return()
      pv <- input$editor_vbp_price_variable
      int <- input$editor_vbp_intervention
      out <- input$editor_vbp_outcome
      cost <- input$editor_vbp_cost
      if (is.null(pv) || is.null(int) || is.null(out) || is.null(cost)) return()
      if (!nzchar(pv) || !nzchar(int) || !nzchar(out) || !nzchar(cost)) return()
      apply_action(list(
        type = "set_vbp",
        price_variable = pv,
        intervention_strategy = int,
        outcome_summary = out,
        cost_summary = cost
      ))
    })

    # Clear VBP config when toggle is turned off
    shiny::observeEvent(input$editor_vbp_enabled, {
      if (!isTRUE(input$editor_vbp_enabled)) {
        apply_action(list(type = "clear_vbp"))
      }
    })

    shiny::observeEvent(input$editor_run_vbp, {
      shiny::req(
        input$editor_vbp_price_variable,
        input$editor_vbp_intervention,
        input$editor_vbp_outcome,
        input$editor_vbp_cost
      )

      m <- build_editor_model()
      if (is.null(m)) {
        shiny::showNotification("No model loaded.", type = "warning")
        return()
      }

      vbp_args <- list(
        m,
        price_variable = input$editor_vbp_price_variable,
        intervention_strategy = input$editor_vbp_intervention,
        outcome_summary = input$editor_vbp_outcome,
        cost_summary = input$editor_vbp_cost
      )

      pf <- create_progress_file()
      vbp_run_state$running <- TRUE
      vbp_run_state$progress_file <- pf
      session$sendCustomMessage("vbp_progress", list(state = "running", pct = 0))

      vbp_args$progress <- make_file_progress_callback(pf)

      p <- suppressWarnings(promises::future_promise({
        do.call(openqaly::run_vbp, vbp_args)
      }, seed = TRUE))

      promises::then(p,
        onFulfilled = function(res) {
          vbp_results_rv$results <- res
          session$sendCustomMessage("vbp_progress", list(state = "done"))
          vbp_run_state$running <- FALSE
          vbp_run_state$progress_file <- NULL
          unlink(pf)
          shiny::showNotification("VBP analysis complete.", type = "message")
        },
        onRejected = function(err) {
          vbp_results_rv$results <- NULL
          session$sendCustomMessage("vbp_progress", list(state = "error"))
          vbp_run_state$running <- FALSE
          vbp_run_state$progress_file <- NULL
          unlink(pf)
          shiny::showNotification(
            paste("VBP analysis failed:", conditionMessage(err)),
            type = "error", duration = 10
          )
        }
      )

      NULL
    })

    # VBP progress polling
    shiny::observe({
      shiny::req(vbp_run_state$running, vbp_run_state$progress_file)
      shiny::invalidateLater(250)
      prog <- read_file_progress(vbp_run_state$progress_file)
      if (!is.null(prog) && prog$total > 0) {
        session$sendCustomMessage("vbp_progress", list(
          state = "running",
          pct = round(prog$pct * 100)
        ))
      }
    })

    vbp_results_reactive <- shiny::reactive(vbp_results_rv$results)
    vbpResultsServer("editor_vbp", vbp_results_reactive, editor_metadata_reactive)

    # --- Scenario page ---
    output$scenario_inputs_panel <- shiny::renderUI({
      file_load_counter()
      m <- shiny::isolate(model())
      if (is.null(m)) {
        return(tags$div(class = "text-muted p-3", "Load a model first."))
      }

      var_choices <- openqaly::get_variable_names(m)
      var_targeting <- openqaly::get_variable_targeting(m)

      strategies_df <- openqaly::get_strategies(m)
      strategy_choices <- if (nrow(strategies_df) > 0) {
        setNames(strategies_df$name, strategies_df$display_name)
      } else {
        character(0)
      }

      groups_df <- openqaly::get_groups(m)
      group_choices <- if (nrow(groups_df) > 0) {
        c(
          "Overall" = "overall",
          "All (Overall + Groups)" = "all",
          "All Groups" = "all_groups",
          setNames(groups_df$name, groups_df$display_name)
        )
      } else {
        character(0)
      }
      individual_groups <- group_choices[
        !group_choices %in% c("overall", "all", "all_groups")
      ]
      setting_choices <- get_dsa_setting_choices()

      # Build variable formulas for base case column
      vars_df <- openqaly::get_variables(m)
      variable_formulas <- list()
      for (i in seq_len(nrow(vars_df))) {
        strat_val <- if (is.null(vars_df$strategy[i]) || is.na(vars_df$strategy[i])) "" else vars_df$strategy[i]
        grp_val <- if (is.null(vars_df$group[i]) || is.na(vars_df$group[i])) "" else vars_df$group[i]
        key <- paste0(vars_df$name[i], "|", strat_val, "|", grp_val)
        variable_formulas[[key]] <- as.character(vars_df$formula[i])
      }
      setting_values <- openqaly::get_settings(m)
      setting_values$discount_rate <- setting_values$discount_outcomes

      # Build initial scenarios from model
      initial_scenarios <- lapply(openqaly::get_scenarios(m), function(sc) {
        var_ovs <- lapply(sc$variable_overrides %||% list(), function(v) {
          list(
            name = v$name,
            value = as.character(v$value),
            strategy = v$strategy %||% "",
            group = v$group %||% ""
          )
        })
        set_ovs <- lapply(sc$setting_overrides %||% list(), function(st) {
          list(name = st$name, value = as.character(st$value))
        })
        list(
          name = sc$name,
          description = sc$description %||% "",
          variable_overrides = var_ovs,
          setting_overrides = set_ovs
        )
      })

      shiny::tags$div(
        scenario_params_dependency(),
        formula_input_dependency(),
        class = "scenario-params-wrapper",
        # Left panel: scenario list
        tags$div(
          class = "scenario-list-panel",
          tags$div(
            class = "scenario-list-toolbar",
            tags$button(
              type = "button",
              class = "btn btn-sm btn-outline-secondary scenario-add-btn",
              "+ Add"
            ),
            tags$button(
              type = "button",
              class = "btn btn-sm btn-outline-danger scenario-remove-btn",
              "Remove"
            )
          ),
          tags$div(class = "scenario-list")
        ),
        # Right panel: grid
        tags$div(
          class = "scenario-grid-panel",
          tags$div(
            style = "display: inline-flex; gap: 6px; margin-bottom: 6px;",
            tags$button(
              type = "button",
              class = "btn btn-sm btn-outline-secondary scenario-add-variable-btn",
              "+ Add Variable"
            ),
            tags$button(
              type = "button",
              class = "btn btn-sm btn-outline-secondary scenario-add-setting-btn",
              "+ Add Setting"
            )
          ),
          tags$div(
            class = "scenario-params-container",
            `data-input-id` = "editor_scenario_params",
            `data-variables` = jsonlite::toJSON(var_choices, auto_unbox = TRUE),
            `data-settings` = jsonlite::toJSON(
              as.list(setting_choices), auto_unbox = TRUE
            ),
            `data-strategies` = jsonlite::toJSON(
              as.list(strategy_choices), auto_unbox = TRUE
            ),
            `data-groups` = jsonlite::toJSON(
              as.list(individual_groups), auto_unbox = TRUE
            ),
            `data-variable-targeting` = jsonlite::toJSON(
              var_targeting, auto_unbox = TRUE
            ),
            `data-variable-formulas` = jsonlite::toJSON(
              variable_formulas, auto_unbox = TRUE
            ),
            `data-setting-values` = jsonlite::toJSON(
              as.list(setting_values), auto_unbox = TRUE
            ),
            `data-terms` = jsonlite::toJSON(
              get_model_terms(m, "dsa_bound"), auto_unbox = FALSE
            ),
            `data-suggestions` = jsonlite::toJSON(
              get_model_suggestions(m, "dsa_bound"), auto_unbox = FALSE
            ),
            `data-initial-scenarios` = jsonlite::toJSON(
              initial_scenarios, auto_unbox = TRUE
            )
          ),
          tags$button(
            type = "button",
            class = "btn btn-primary mt-2 w-100 scenario-run-btn",
            "Run Scenario Analysis"
          )
        )
      )
    })

    # Scenario add modal
    shiny::observeEvent(input$show_add_scenario_modal, {
      shiny::showModal(shiny::modalDialog(
        title = "Add Scenario",
        bslib::layout_columns(
          col_widths = c(6, 6),
          shiny::textInput("add_scenario_name", "Name", value = ""),
          shiny::textInput("add_scenario_description", "Description", value = "")
        ),
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton("add_scenario_confirm", "Create Scenario",
            class = "btn-primary")
        )
      ))
    })

    shiny::observeEvent(input$add_scenario_confirm, {
      name <- trimws(input$add_scenario_name)
      if (!nzchar(name)) {
        shiny::showNotification("Scenario name is required.", type = "warning")
        return()
      }
      shiny::removeModal()
      apply_action(list(
        type = "add_scenario",
        name = name,
        description = input$add_scenario_description %||% ""
      ))
    })

    # Scenario edit modal
    shiny::observeEvent(input$show_edit_scenario_modal, {
      data <- input$show_edit_scenario_modal
      shiny::showModal(shiny::modalDialog(
        title = "Edit Scenario",
        bslib::layout_columns(
          col_widths = c(6, 6),
          shiny::textInput("edit_scenario_name", "Name", value = data$name),
          shiny::textInput("edit_scenario_description", "Description",
            value = data$description)
        ),
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton("edit_scenario_confirm", "Save",
            class = "btn-primary")
        )
      ))
      session$userData$editing_scenario_name <- data$name
    })

    shiny::observeEvent(input$edit_scenario_confirm, {
      name <- trimws(input$edit_scenario_name)
      if (!nzchar(name)) {
        shiny::showNotification("Scenario name is required.", type = "warning")
        return()
      }
      shiny::removeModal()
      apply_action(list(
        type = "edit_scenario",
        name = session$userData$editing_scenario_name,
        new_name = name,
        description = input$edit_scenario_description %||% ""
      ))
    })

    shiny::observeEvent(input$remove_scenario_action, {
      apply_action(list(
        type = "remove_scenario",
        name = input$remove_scenario_action$name
      ))
    })

    shiny::observeEvent(input$run_scenario_action, {
      scenarios <- input$run_scenario_action$scenarios
      if (is.null(scenarios) || length(scenarios) == 0) {
        shiny::showNotification("Please add at least one scenario.", type = "warning")
        return()
      }

      m <- build_editor_model()
      if (is.null(m)) {
        shiny::showNotification("No model loaded.", type = "warning")
        return()
      }

      scenario_args <- list(m)

      pf <- create_progress_file()
      scenario_run_state$running <- TRUE
      scenario_run_state$progress_file <- pf
      session$sendCustomMessage("scenario_progress", list(state = "running", pct = 0))

      scenario_args$progress <- make_file_progress_callback(pf)

      p <- suppressWarnings(promises::future_promise({
        do.call(openqaly::run_scenario, scenario_args)
      }, seed = TRUE))

      promises::then(p,
        onFulfilled = function(res) {
          scenario_results_rv$results <- res
          scenario_results_rv$metadata <- res$metadata
          session$sendCustomMessage("scenario_progress", list(state = "done"))
          scenario_run_state$running <- FALSE
          scenario_run_state$progress_file <- NULL
          unlink(pf)
          shiny::showNotification("Scenario analysis complete.", type = "message")
        },
        onRejected = function(err) {
          scenario_results_rv$results <- NULL
          scenario_results_rv$metadata <- NULL
          session$sendCustomMessage("scenario_progress", list(state = "error"))
          scenario_run_state$running <- FALSE
          scenario_run_state$progress_file <- NULL
          unlink(pf)
          shiny::showNotification(
            paste("Scenario analysis failed:", conditionMessage(err)),
            type = "error", duration = 10
          )
        }
      )

      NULL
    })

    # Scenario progress polling
    shiny::observe({
      shiny::req(scenario_run_state$running, scenario_run_state$progress_file)
      shiny::invalidateLater(250)
      prog <- read_file_progress(scenario_run_state$progress_file)
      if (!is.null(prog) && prog$total > 0) {
        session$sendCustomMessage("scenario_progress", list(
          state = "running",
          pct = round(prog$pct * 100)
        ))
      }
    })

    scenario_results_reactive <- shiny::reactive(scenario_results_rv$results)
    scenario_metadata_reactive <- shiny::reactive(scenario_results_rv$metadata)
    scenarioResultTabServer("editor_scenario_outcomes", "outcomes",
      scenario_results_reactive, scenario_metadata_reactive)
    scenarioResultTabServer("editor_scenario_costs", "costs",
      scenario_results_reactive, scenario_metadata_reactive)
    scenarioResultTabServer("editor_scenario_nmb", "nmb",
      scenario_results_reactive, scenario_metadata_reactive)
    scenarioResultTabServer("editor_scenario_ce", "ce",
      scenario_results_reactive, scenario_metadata_reactive)
    scenarioResultTabServer("editor_scenario_vbp", "vbp",
      scenario_results_reactive, scenario_metadata_reactive)

    # --- TWSA page ---
    output$twsa_inputs_panel <- shiny::renderUI({
      file_load_counter()
      m <- shiny::isolate(model())
      if (is.null(m)) {
        return(tags$div(class = "text-muted p-3", "Load a model first."))
      }

      var_choices <- openqaly::get_variable_names(m)
      var_targeting <- openqaly::get_variable_targeting(m)

      strategies_df <- openqaly::get_strategies(m)
      strategy_choices <- if (nrow(strategies_df) > 0) {
        setNames(strategies_df$name, strategies_df$display_name)
      } else {
        character(0)
      }

      groups_df <- openqaly::get_groups(m)
      group_choices <- if (nrow(groups_df) > 0) {
        c(
          "Overall" = "overall",
          "All (Overall + Groups)" = "all",
          "All Groups" = "all_groups",
          setNames(groups_df$name, groups_df$display_name)
        )
      } else {
        character(0)
      }
      individual_groups <- group_choices[
        !group_choices %in% c("overall", "all", "all_groups")
      ]
      setting_choices <- get_dsa_setting_choices()

      # Build variable formulas for base case column
      vars_df <- openqaly::get_variables(m)
      variable_formulas <- list()
      for (i in seq_len(nrow(vars_df))) {
        strat_val <- if (is.null(vars_df$strategy[i]) || is.na(vars_df$strategy[i])) "" else vars_df$strategy[i]
        grp_val <- if (is.null(vars_df$group[i]) || is.na(vars_df$group[i])) "" else vars_df$group[i]
        key <- paste0(vars_df$name[i], "|", strat_val, "|", grp_val)
        variable_formulas[[key]] <- as.character(vars_df$formula[i])
      }
      setting_values <- openqaly::get_settings(m)
      setting_values$discount_rate <- setting_values$discount_outcomes

      # Build initial TWSA analyses from model
      initial_twsa <- lapply(m$twsa_analyses %||% list(), function(tw) {
        params <- lapply(tw$parameters %||% list(), function(p) {
          data_obj <- list()
          if (!is.null(p$min)) data_obj$min <- as.character(p$min)
          if (!is.null(p$max)) data_obj$max <- as.character(p$max)
          if (!is.null(p$radius)) data_obj$radius <- as.character(p$radius)
          if (!is.null(p$steps)) data_obj$steps <- p$steps
          if (!is.null(p$values)) data_obj$values <- as.character(p$values)
          list(
            param_type = p$param_type %||% "variable",
            name = p$name,
            type = p$type %||% "radius",
            data = data_obj,
            strategy = p$strategy %||% "",
            group = p$group %||% "",
            display_name = p$display_name,
            include_base_case = p$include_base_case %||% TRUE
          )
        })
        is_var <- vapply(params, function(p) {
          identical(p$param_type, "variable")
        }, logical(1))
        list(
          name = tw$name,
          description = tw$description %||% "",
          variable_params = params[is_var],
          setting_params = params[!is_var]
        )
      })

      shiny::tags$div(
        twsa_params_dependency(),
        formula_input_dependency(),
        class = "twsa-params-wrapper",
        # Left panel: TWSA analysis list
        tags$div(
          class = "twsa-list-panel",
          tags$div(
            class = "twsa-list-toolbar",
            tags$button(
              type = "button",
              class = "btn btn-sm btn-outline-secondary twsa-add-btn",
              "+ Add"
            ),
            tags$button(
              type = "button",
              class = "btn btn-sm btn-outline-danger twsa-remove-btn",
              "Remove"
            )
          ),
          tags$div(class = "twsa-list")
        ),
        # Right panel: grid
        tags$div(
          class = "twsa-grid-panel",
          tags$div(
            class = "twsa-params-container",
            `data-input-id` = "editor_twsa_params",
            `data-variables` = jsonlite::toJSON(var_choices, auto_unbox = TRUE),
            `data-settings` = jsonlite::toJSON(
              as.list(setting_choices), auto_unbox = TRUE
            ),
            `data-strategies` = jsonlite::toJSON(
              as.list(strategy_choices), auto_unbox = TRUE
            ),
            `data-groups` = jsonlite::toJSON(
              as.list(individual_groups), auto_unbox = TRUE
            ),
            `data-variable-targeting` = jsonlite::toJSON(
              var_targeting, auto_unbox = TRUE
            ),
            `data-variable-formulas` = jsonlite::toJSON(
              variable_formulas, auto_unbox = TRUE
            ),
            `data-setting-values` = jsonlite::toJSON(
              as.list(setting_values), auto_unbox = TRUE
            ),
            `data-terms` = jsonlite::toJSON(
              get_model_terms(m, "dsa_bound"), auto_unbox = FALSE
            ),
            `data-suggestions` = jsonlite::toJSON(
              get_model_suggestions(m, "dsa_bound"), auto_unbox = FALSE
            ),
            `data-initial-twsa` = jsonlite::toJSON(
              initial_twsa, auto_unbox = TRUE
            )
          ),
          tags$button(
            type = "button",
            class = "btn btn-primary mt-2 w-100 twsa-run-btn",
            "Run TWSA"
          )
        )
      )
    })

    # TWSA add modal
    shiny::observeEvent(input$show_add_twsa_modal, {
      shiny::showModal(shiny::modalDialog(
        title = "Add TWSA Analysis",
        bslib::layout_columns(
          col_widths = c(6, 6),
          shiny::textInput("add_twsa_name", "Name", value = ""),
          shiny::textInput("add_twsa_description", "Description", value = "")
        ),
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton("add_twsa_confirm", "Create TWSA",
            class = "btn-primary")
        )
      ))
    })

    shiny::observeEvent(input$add_twsa_confirm, {
      name <- trimws(input$add_twsa_name)
      if (!nzchar(name)) {
        shiny::showNotification("TWSA name is required.", type = "warning")
        return()
      }
      shiny::removeModal()

      # Atomic batch: add_twsa + 2x add_twsa_variable as one undo step
      old <- model()
      tryCatch({
        history$push(old)
        # 1. Create the TWSA analysis
        result <- dispatch_model_action(old, list(
          type = "add_twsa",
          name = name,
          description = input$add_twsa_description %||% ""
        ))
        # 2. Add X and Y parameters with first two available variables
        var_names <- openqaly::get_variable_names(old)
        if (length(var_names) >= 1) {
          result <- dispatch_model_action(result, list(
            type = "add_twsa_variable",
            twsa_name = name,
            variable = var_names[1],
            method_type = "radius",
            radius = "value * 0.2",
            steps = 3
          ))
        }
        if (length(var_names) >= 2) {
          result <- dispatch_model_action(result, list(
            type = "add_twsa_variable",
            twsa_name = name,
            variable = var_names[2],
            method_type = "radius",
            radius = "value * 0.2",
            steps = 3
          ))
        }
        model(result)
        file_load_counter(file_load_counter() + 1L)
      }, error = function(e) {
        history$undo(old)
        model(old)
        shiny::showNotification(
          paste("Failed to create TWSA:", conditionMessage(e)),
          type = "error"
        )
      })
    })

    # TWSA edit modal
    shiny::observeEvent(input$show_edit_twsa_modal, {
      data <- input$show_edit_twsa_modal
      shiny::showModal(shiny::modalDialog(
        title = "Edit TWSA Analysis",
        bslib::layout_columns(
          col_widths = c(6, 6),
          shiny::textInput("edit_twsa_name", "Name", value = data$name),
          shiny::textInput("edit_twsa_description", "Description",
            value = data$description)
        ),
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton("edit_twsa_confirm", "Save",
            class = "btn-primary")
        )
      ))
      session$userData$editing_twsa_name <- data$name
    })

    shiny::observeEvent(input$edit_twsa_confirm, {
      name <- trimws(input$edit_twsa_name)
      if (!nzchar(name)) {
        shiny::showNotification("TWSA name is required.", type = "warning")
        return()
      }
      shiny::removeModal()
      apply_action(list(
        type = "edit_twsa",
        name = session$userData$editing_twsa_name,
        new_name = name,
        description = input$edit_twsa_description %||% ""
      ))
    })

    shiny::observeEvent(input$remove_twsa_action, {
      apply_action(list(
        type = "remove_twsa",
        name = input$remove_twsa_action$name
      ))
    })

    shiny::observeEvent(input$run_twsa_action, {
      m <- build_editor_model()
      if (is.null(m)) {
        shiny::showNotification("No model loaded.", type = "warning")
        return()
      }

      if (is.null(m$twsa_analyses) || length(m$twsa_analyses) == 0) {
        shiny::showNotification("Please add at least one TWSA analysis.",
          type = "warning")
        return()
      }

      twsa_args <- list(m)

      pf <- create_progress_file()
      twsa_run_state$running <- TRUE
      twsa_run_state$progress_file <- pf
      session$sendCustomMessage("twsa_progress", list(state = "running", pct = 0))

      twsa_args$progress <- make_file_progress_callback(pf)

      p <- suppressWarnings(promises::future_promise({
        do.call(openqaly::run_twsa, twsa_args)
      }, seed = TRUE))

      promises::then(p,
        onFulfilled = function(res) {
          twsa_results_rv$results <- res
          twsa_results_rv$metadata <- res$metadata
          session$sendCustomMessage("twsa_progress", list(state = "done"))
          twsa_run_state$running <- FALSE
          twsa_run_state$progress_file <- NULL
          unlink(pf)
          shiny::showNotification("TWSA analysis complete.", type = "message")
        },
        onRejected = function(err) {
          twsa_results_rv$results <- NULL
          twsa_results_rv$metadata <- NULL
          session$sendCustomMessage("twsa_progress", list(state = "error"))
          twsa_run_state$running <- FALSE
          twsa_run_state$progress_file <- NULL
          unlink(pf)
          shiny::showNotification(
            paste("TWSA analysis failed:", conditionMessage(err)),
            type = "error", duration = 10
          )
        }
      )

      NULL
    })

    # TWSA progress polling
    shiny::observe({
      shiny::req(twsa_run_state$running, twsa_run_state$progress_file)
      shiny::invalidateLater(250)
      prog <- read_file_progress(twsa_run_state$progress_file)
      if (!is.null(prog) && prog$total > 0) {
        session$sendCustomMessage("twsa_progress", list(
          state = "running",
          pct = round(prog$pct * 100)
        ))
      }
    })

    twsa_results_reactive <- shiny::reactive(twsa_results_rv$results)
    twsa_metadata_reactive <- shiny::reactive(twsa_results_rv$metadata)
    twsaResultTabServer("editor_twsa_outcomes", "outcomes",
      twsa_results_reactive, twsa_metadata_reactive)
    twsaResultTabServer("editor_twsa_costs", "costs",
      twsa_results_reactive, twsa_metadata_reactive)
    twsaResultTabServer("editor_twsa_nmb", "nmb",
      twsa_results_reactive, twsa_metadata_reactive)
    twsaResultTabServer("editor_twsa_ce", "ce",
      twsa_results_reactive, twsa_metadata_reactive)
    twsaResultTabServer("editor_twsa_vbp", "vbp",
      twsa_results_reactive, twsa_metadata_reactive)

    # --- PSA page ---
    psa_results_rv <- shiny::reactiveValues(results = NULL, metadata = NULL)
    psa_run_state <- shiny::reactiveValues(running = FALSE, progress_file = NULL)

    psa_input_data <- shiny::reactive({
      file_load_counter()
      m <- shiny::isolate(model())
      if (is.null(m)) return(NULL)

      var_choices <- openqaly::get_variable_names(m)
      var_targeting <- openqaly::get_variable_targeting(m)

      strategies_df <- openqaly::get_strategies(m)
      strategy_choices <- if (nrow(strategies_df) > 0) {
        setNames(strategies_df$name, strategies_df$display_name)
      } else character(0)

      groups_df <- openqaly::get_groups(m)
      group_choices <- if (nrow(groups_df) > 0) {
        c(
          "Overall" = "overall",
          "All (Overall + Groups)" = "all",
          "All Groups" = "all_groups",
          setNames(groups_df$name, groups_df$display_name)
        )
      } else character(0)
      individual_groups <- group_choices[
        !group_choices %in% c("overall", "all", "all_groups")
      ]

      # Build variable formulas keyed by "name|strategy|group"
      vars_df <- openqaly::get_variables(m)
      variable_formulas <- list()
      variable_instances <- list()
      seen_instance_keys <- character(0)
      for (i in seq_len(nrow(vars_df))) {
        strat_val <- if (is.null(vars_df$strategy[i]) || is.na(vars_df$strategy[i])) "" else vars_df$strategy[i]
        grp_val <- if (is.null(vars_df$group[i]) || is.na(vars_df$group[i])) "" else vars_df$group[i]
        key <- paste0(vars_df$name[i], "|", strat_val, "|", grp_val)
        variable_formulas[[key]] <- as.character(vars_df$formula[i])
        if (!key %in% seen_instance_keys) {
          variable_instances[[length(variable_instances) + 1]] <- list(
            name = as.character(vars_df$name[i]),
            strategy = strat_val,
            group = grp_val,
            formula = as.character(vars_df$formula[i]),
            sampling = as.character(vars_df$sampling[i] %||% "")
          )
          seen_instance_keys <- c(seen_instance_keys, key)
        }
      }

      # Get variables with sampling distributions for initial data
      psa_vars <- vars_df[!is.na(vars_df$sampling) & nzchar(vars_df$sampling), , drop = FALSE]
      initial_data <- if (nrow(psa_vars) > 0) {
        lapply(seq_len(nrow(psa_vars)), function(i) {
          list(
            name = psa_vars$name[i],
            display_name = psa_vars$display_name[i] %||% psa_vars$name[i],
            strategy = if (!is.na(psa_vars$strategy[i])) psa_vars$strategy[i] else "",
            group = if (!is.na(psa_vars$group[i])) psa_vars$group[i] else "",
            sampling = as.character(psa_vars$sampling[i])
          )
        })
      } else list()

      # PSA settings
      psa_config <- m$psa
      n_sim_default <- if (!is.null(psa_config$n_sim)) psa_config$n_sim else 1000
      seed_default <- if (!is.null(psa_config$seed)) psa_config$seed else ""

      # Multivariate sampling specs
      mv_specs <- m$multivariate_sampling
      mv_initial <- if (!is.null(mv_specs) && length(mv_specs) > 0) {
        lapply(mv_specs, function(spec) {
          vars_list <- if (is.data.frame(spec$variables)) {
            spec$variables$variable
          } else if (is.character(spec$variables)) {
            spec$variables
          } else character(0)
          list(
            name = spec$name,
            distribution = as.character(spec$distribution %||% ""),
            variables = vars_list,
            description = spec$description %||% ""
          )
        })
      } else list()

      list(
        m = m,
        var_choices = var_choices,
        var_targeting = var_targeting,
        strategy_choices = strategy_choices,
        individual_groups = individual_groups,
        variable_formulas = variable_formulas,
        variable_instances = variable_instances,
        initial_data = initial_data,
        n_sim_default = n_sim_default,
        seed_default = seed_default,
        mv_initial = mv_initial
      )
    })

    output$psa_settings_panel <- shiny::renderUI({
      d <- psa_input_data()
      if (is.null(d)) {
        return(tags$div(class = "text-muted p-3", "Load a model first."))
      }
      shiny::tagList(
        psa_params_dependency(),
        tags$div(
          class = "psa-inputs-wrapper",
          tags$div(
            class = "psa-settings-container psa-settings-row",
            tags$div(
              class = "form-group",
              tags$label("Number of Simulations"),
              tags$input(
                type = "number", class = "form-control form-control-sm psa-nsim-input",
                value = d$n_sim_default, min = "1", step = "100"
              )
            ),
            tags$div(
              class = "form-group",
              tags$label("Seed (optional)"),
              tags$input(
                type = "number", class = "form-control form-control-sm psa-seed-input",
                value = d$seed_default
              )
            )
          ),
          tags$button(
            type = "button",
            class = "btn btn-primary mt-2 w-100 psa-run-btn",
            "Run PSA"
          )
        )
      )
    })

    output$psa_univariate_panel <- shiny::renderUI({
      d <- psa_input_data()
      if (is.null(d)) {
        return(tags$div(class = "text-muted p-3", "Load a model first."))
      }
      shiny::tagList(
        psa_params_dependency(),
        formula_input_dependency(),
        tags$div(
          class = "psa-inputs-wrapper",
          tags$div(
            style = "display: inline-flex; gap: 6px; margin-bottom: 6px;",
            tags$button(
              type = "button",
              class = "btn btn-sm btn-outline-secondary psa-add-variable-btn",
              "+ Add Variable"
            )
          ),
          tags$div(
            class = "psa-params-container",
            `data-input-id` = "editor_psa_params",
            `data-variables` = jsonlite::toJSON(d$var_choices, auto_unbox = TRUE),
            `data-strategies` = jsonlite::toJSON(
              as.list(d$strategy_choices), auto_unbox = TRUE
            ),
            `data-groups` = jsonlite::toJSON(
              as.list(d$individual_groups), auto_unbox = TRUE
            ),
            `data-variable-targeting` = jsonlite::toJSON(
              d$var_targeting, auto_unbox = TRUE
            ),
            `data-variable-instances` = jsonlite::toJSON(
              d$variable_instances, auto_unbox = TRUE
            ),
            `data-variable-formulas` = jsonlite::toJSON(
              d$variable_formulas, auto_unbox = TRUE
            ),
            `data-initial` = jsonlite::toJSON(
              d$initial_data, auto_unbox = TRUE
            ),
            `data-terms` = jsonlite::toJSON(
              get_model_terms(d$m, "variable_sampling"), auto_unbox = FALSE
            ),
            `data-suggestions` = jsonlite::toJSON(
              get_model_suggestions(d$m, "variable_sampling"), auto_unbox = FALSE
            )
          )
        )
      )
    })

    output$psa_multivariate_panel <- shiny::renderUI({
      d <- psa_input_data()
      if (is.null(d)) {
        return(tags$div(class = "text-muted p-3", "Load a model first."))
      }
      shiny::tagList(
        psa_params_dependency(),
        formula_input_dependency(),
        tags$div(
          class = "psa-inputs-wrapper",
          tags$div(
            style = "display: inline-flex; gap: 6px; margin-bottom: 6px;",
            tags$button(
              type = "button",
              class = "btn btn-sm btn-outline-secondary psa-add-mv-btn",
              "+ Add Multivariate"
            )
          ),
          tags$div(
            class = "psa-multivariate-container",
            `data-input-id` = "editor_psa_multivariate",
            `data-variables` = jsonlite::toJSON(d$var_choices, auto_unbox = TRUE),
            `data-strategies` = jsonlite::toJSON(
              as.list(d$strategy_choices), auto_unbox = TRUE
            ),
            `data-groups` = jsonlite::toJSON(
              as.list(d$individual_groups), auto_unbox = TRUE
            ),
            `data-variable-targeting` = jsonlite::toJSON(
              d$var_targeting, auto_unbox = TRUE
            ),
            `data-variable-instances` = jsonlite::toJSON(
              d$variable_instances, auto_unbox = TRUE
            ),
            `data-variable-formulas` = jsonlite::toJSON(
              d$variable_formulas, auto_unbox = TRUE
            ),
            `data-initial` = jsonlite::toJSON(
              d$mv_initial, auto_unbox = TRUE
            ),
            `data-terms` = jsonlite::toJSON(
              get_model_terms(d$m, "multivariate_sampling"), auto_unbox = FALSE
            ),
            `data-suggestions` = jsonlite::toJSON(
              get_model_suggestions(d$m, "multivariate_sampling"), auto_unbox = FALSE
            )
          )
        )
      )
    })

    shiny::observeEvent(input$show_add_multivariate_modal, {
      d <- psa_input_data()
      if (is.null(d)) {
        shiny::showNotification("Load a model first.", type = "warning")
        return()
      }

      shiny::showModal(shiny::modalDialog(
        title = "Add Multivariate Sampling",
        easyClose = FALSE,
        shiny::tags$div(
          class = "mb-3",
          shiny::textInput("add_mv_name", "Name", value = "")
        ),
        shiny::tags$div(
          class = "mb-3",
          shiny::tags$label("Distribution"),
          shiny::tags$div(
            id = "add_mv_distribution_builder",
            class = "psa-add-mv-builder",
            `data-terms` = jsonlite::toJSON(
              get_model_terms(d$m, "multivariate_sampling"),
              auto_unbox = FALSE
            ),
            `data-suggestions` = jsonlite::toJSON(
              get_model_suggestions(d$m, "multivariate_sampling"),
              auto_unbox = FALSE
            )
          ),
          shiny::tags$div(
            class = "form-text",
            "Choose variables first, then configure the distribution with the structured editor."
          )
        ),
        shiny::tags$div(
          class = "mb-3",
          shiny::selectizeInput(
            "add_mv_variables",
            "Variables",
            choices = d$var_choices,
            selected = character(0),
            multiple = TRUE,
            options = list(plugins = list("remove_button"))
          )
        ),
        shiny::tags$div(
          class = "mb-0",
          shiny::textInput("add_mv_description", "Description", value = "")
        ),
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(
            "add_mv_confirm",
            "Add Multivariate",
            class = "btn-primary"
          )
        )
      ))
    })

    shiny::observeEvent(input$add_mv_confirm, {
      d <- psa_input_data()
      if (is.null(d)) {
        shiny::showNotification("Load a model first.", type = "warning")
        return()
      }

      name <- trimws(input$add_mv_name %||% "")
      distribution <- trimws(.str(input$add_mv_distribution))
      variables <- input$add_mv_variables %||% character(0)
      description <- input$add_mv_description %||% ""

      existing_specs <- d$m$multivariate_sampling %||% list()
      existing_names <- vapply(
        existing_specs,
        function(spec) as.character(spec$name %||% ""),
        character(1)
      )

      if (!nzchar(name)) {
        shiny::showNotification("Multivariate name is required.", type = "warning")
        return()
      }
      if (name %in% existing_names) {
        shiny::showNotification("A multivariate sampling spec with that name already exists.", type = "warning")
        return()
      }

      validation <- .validate_multivariate_distribution(distribution)
      if (!isTRUE(validation$valid)) {
        shiny::showNotification(validation$message, type = "warning")
        return()
      }
      if (length(variables) == 0) {
        shiny::showNotification("Select at least one variable.", type = "warning")
        return()
      }

      shiny::removeModal()

      result <- tryCatch(
        apply_action(list(
          type = "add_multivariate_sampling",
          name = name,
          distribution = distribution,
          variables = variables,
          description = description
        )),
        error = function(e) e
      )

      if (inherits(result, "error")) {
        shiny::showNotification(
          paste("Failed to add multivariate sampling:", conditionMessage(result)),
          type = "error"
        )
      }
    })

    # PSA run handler
    shiny::observeEvent(input$run_psa_action, {
      n_sim <- input$run_psa_action$n_sim
      seed_val <- input$run_psa_action$seed
      params <- input$run_psa_action$params
      multivariate <- input$run_psa_action$multivariate

      if (is.null(n_sim) || n_sim < 1) {
        shiny::showNotification("Please set number of simulations.", type = "warning")
        return()
      }

      m <- build_editor_model()
      if (is.null(m)) {
        shiny::showNotification("No model loaded.", type = "warning")
        return()
      }

      m <- apply_psa_params(m, params, multivariate)

      psa_args <- list(m, n_sim = as.integer(n_sim))
      if (!is.null(seed_val) && nzchar(seed_val)) {
        psa_args$seed <- as.integer(seed_val)
      }

      pf <- create_progress_file()
      psa_run_state$running <- TRUE
      psa_run_state$progress_file <- pf
      session$sendCustomMessage("psa_progress", list(state = "running", pct = 0))

      psa_args$progress <- make_file_progress_callback(pf)

      p <- suppressWarnings(promises::future_promise({
        do.call(openqaly::run_psa, psa_args)
      }, seed = TRUE))

      promises::then(p,
        onFulfilled = function(res) {
          psa_results_rv$results <- res
          psa_results_rv$metadata <- res$metadata
          session$sendCustomMessage("psa_progress", list(state = "done"))
          psa_run_state$running <- FALSE
          psa_run_state$progress_file <- NULL
          unlink(pf)
          shiny::showNotification("PSA analysis complete.", type = "message")
        },
        onRejected = function(err) {
          psa_results_rv$results <- NULL
          psa_results_rv$metadata <- NULL
          session$sendCustomMessage("psa_progress", list(state = "error"))
          psa_run_state$running <- FALSE
          psa_run_state$progress_file <- NULL
          unlink(pf)
          shiny::showNotification(
            paste("PSA analysis failed:", conditionMessage(err)),
            type = "error", duration = 10
          )
        }
      )

      NULL
    })

    # PSA progress polling
    shiny::observe({
      shiny::req(psa_run_state$running, psa_run_state$progress_file)
      shiny::invalidateLater(250)
      prog <- read_file_progress(psa_run_state$progress_file)
      if (!is.null(prog) && prog$total > 0) {
        session$sendCustomMessage("psa_progress", list(
          state = "running",
          pct = round(prog$pct * 100)
        ))
      }
    })

    output$has_psa_results <- shiny::reactive(!is.null(psa_results_rv$results))
    shiny::outputOptions(output, "has_psa_results", suspendWhenHidden = FALSE)

    psa_results_reactive <- shiny::reactive(psa_results_rv$results)
    psa_metadata_reactive <- shiny::reactive(psa_results_rv$metadata)
    psaResultTabServer("editor_psa_outcomes", "outcomes",
      psa_results_reactive, psa_metadata_reactive)
    psaResultTabServer("editor_psa_costs", "costs",
      psa_results_reactive, psa_metadata_reactive)
    psaResultTabServer("editor_psa_nmb", "nmb",
      psa_results_reactive, psa_metadata_reactive)
    psaResultTabServer("editor_psa_incremental_ce", "incremental_ce",
      psa_results_reactive, psa_metadata_reactive)
    psaResultTabServer("editor_psa_pairwise_ce", "pairwise_ce",
      psa_results_reactive, psa_metadata_reactive)
    psaResultTabServer("editor_psa_evpi", "evpi",
      psa_results_reactive, psa_metadata_reactive)
    psaResultTabServer("editor_psa_parameters", "parameters",
      psa_results_reactive, psa_metadata_reactive)

    # --- Threshold page ---
    threshold_results_rv <- shiny::reactiveValues(results = NULL)
    threshold_run_state <- shiny::reactiveValues(running = FALSE, progress_file = NULL)

    output$threshold_inputs_panel <- shiny::renderUI({
      file_load_counter()
      m <- shiny::isolate(model())
      if (is.null(m)) {
        return(tags$div(class = "text-muted p-3", "Load a model first."))
      }

      var_choices <- openqaly::get_variable_names(m)
      var_targeting <- openqaly::get_variable_targeting(m)

      strategies_df <- openqaly::get_strategies(m)
      strategy_choices <- if (nrow(strategies_df) > 0) {
        setNames(strategies_df$name, strategies_df$display_name)
      } else {
        character(0)
      }

      groups_df <- openqaly::get_groups(m)
      individual_groups <- if (nrow(groups_df) > 0) {
        setNames(groups_df$name, groups_df$display_name)
      } else {
        character(0)
      }

      sdf <- openqaly::get_model_summaries(m)
      outcome_summaries <- if (!is.null(sdf) && nrow(sdf) > 0) {
        odf <- sdf[sdf$type == "outcome", , drop = FALSE]
        if (nrow(odf) > 0) setNames(odf$name, odf$display_name) else character(0)
      } else {
        character(0)
      }
      cost_summaries <- if (!is.null(sdf) && nrow(sdf) > 0) {
        cdf <- sdf[sdf$type == "cost", , drop = FALSE]
        if (nrow(cdf) > 0) setNames(cdf$name, cdf$display_name) else character(0)
      } else {
        character(0)
      }

      # Values for condition editor
      values_df <- openqaly::get_model_values(m)
      outcome_values <- if (!is.null(values_df) && nrow(values_df) > 0) {
        unique(values_df$name[values_df$type == "outcome"])
      } else {
        character(0)
      }
      cost_values <- if (!is.null(values_df) && nrow(values_df) > 0) {
        unique(values_df$name[values_df$type == "cost"])
      } else {
        character(0)
      }

      # States for trace condition
      states_df <- openqaly::get_states(m)
      state_choices <- if (nrow(states_df) > 0) states_df$name else character(0)

      # Variable formulas for base case display
      vars_df <- openqaly::get_variables(m)
      variable_formulas <- list()
      for (i in seq_len(nrow(vars_df))) {
        strat_val <- if (is.null(vars_df$strategy[i]) || is.na(vars_df$strategy[i])) "" else vars_df$strategy[i]
        grp_val <- if (is.null(vars_df$group[i]) || is.na(vars_df$group[i])) "" else vars_df$group[i]
        key <- paste0(vars_df$name[i], "|", strat_val, "|", grp_val)
        variable_formulas[[key]] <- as.character(vars_df$formula[i])
      }

      # Build initial analyses from model
      initial_analyses <- lapply(m$threshold_analyses %||% list(), function(ta) {
        list(
          active = ta$active %||% TRUE,
          name = ta$name,
          variable = ta$variable,
          variable_strategy = ta$variable_strategy %||% "",
          variable_group = ta$variable_group %||% "",
          lower = ta$lower,
          upper = ta$upper,
          condition = ta$condition %||% list()
        )
      })

      shiny::tagList(
        threshold_params_dependency(),
        shiny::tags$div(
          style = "display: inline-flex; gap: 6px; margin-bottom: 6px;",
          shiny::tags$button(
            type = "button",
            class = "btn btn-sm btn-outline-secondary threshold-add-btn",
            "+ Add Analysis"
          )
        ),
        shiny::tags$div(
          class = "threshold-params-container",
          `data-input-id` = "editor_threshold_params",
          `data-variables` = jsonlite::toJSON(var_choices, auto_unbox = TRUE),
          `data-strategies` = jsonlite::toJSON(
            as.list(strategy_choices), auto_unbox = TRUE
          ),
          `data-groups` = jsonlite::toJSON(
            as.list(individual_groups), auto_unbox = TRUE
          ),
          `data-variable-targeting` = jsonlite::toJSON(
            var_targeting, auto_unbox = TRUE
          ),
          `data-variable-formulas` = jsonlite::toJSON(
            variable_formulas, auto_unbox = TRUE
          ),
          `data-outcome-summaries` = jsonlite::toJSON(
            as.list(outcome_summaries), auto_unbox = TRUE
          ),
          `data-cost-summaries` = jsonlite::toJSON(
            as.list(cost_summaries), auto_unbox = TRUE
          ),
          `data-outcome-values` = jsonlite::toJSON(
            outcome_values, auto_unbox = TRUE
          ),
          `data-cost-values` = jsonlite::toJSON(
            cost_values, auto_unbox = TRUE
          ),
          `data-states` = jsonlite::toJSON(
            state_choices, auto_unbox = TRUE
          ),
          `data-initial` = jsonlite::toJSON(
            initial_analyses, auto_unbox = TRUE
          )
        ),
        shiny::tags$button(
          type = "button",
          class = "btn btn-primary mt-2 w-100 threshold-run-btn",
          "Run Threshold Analysis"
        )
      )
    })

    shiny::observeEvent(input$run_threshold_action, {
      analyses <- normalize_threshold_params(input$run_threshold_action$analyses)
      if (length(analyses) == 0) {
        analyses <- normalize_threshold_params(input$editor_threshold_params)
      }
      if (length(analyses) == 0) {
        shiny::showNotification("Please add at least one analysis.",
          type = "warning")
        return()
      }

      m <- build_editor_model()
      if (is.null(m)) {
        shiny::showNotification("No model loaded.", type = "warning")
        return()
      }

      m <- apply_threshold_params(m, analyses)

      threshold_args <- list(m)

      pf <- create_progress_file()
      threshold_run_state$running <- TRUE
      threshold_run_state$progress_file <- pf
      session$sendCustomMessage("threshold_progress", list(state = "running", pct = 0))

      threshold_args$progress <- make_file_progress_callback(pf)

      p <- suppressWarnings(promises::future_promise({
        do.call(openqaly::run_threshold, threshold_args)
      }, seed = TRUE))

      promises::then(p,
        onFulfilled = function(res) {
          threshold_results_rv$results <- res
          session$sendCustomMessage("threshold_progress", list(state = "done"))
          threshold_run_state$running <- FALSE
          threshold_run_state$progress_file <- NULL
          unlink(pf)
          shiny::showNotification("Threshold analysis complete.", type = "message")
        },
        onRejected = function(err) {
          threshold_results_rv$results <- NULL
          session$sendCustomMessage("threshold_progress", list(state = "error"))
          threshold_run_state$running <- FALSE
          threshold_run_state$progress_file <- NULL
          unlink(pf)
          shiny::showNotification(
            paste("Threshold analysis failed:", conditionMessage(err)),
            type = "error", duration = 10
          )
        }
      )

      NULL
    })

    # Threshold progress polling
    shiny::observe({
      shiny::req(threshold_run_state$running, threshold_run_state$progress_file)
      shiny::invalidateLater(250)
      prog <- read_file_progress(threshold_run_state$progress_file)
      if (!is.null(prog) && prog$total > 0) {
        session$sendCustomMessage("threshold_progress", list(
          state = "running",
          pct = round(prog$pct * 100)
        ))
      }
    })

    threshold_results_reactive <- shiny::reactive(threshold_results_rv$results)
    thresholdSummaryServer("editor_threshold_summary", threshold_results_reactive)
    thresholdResultTabServer("editor_threshold_detail", "detail", threshold_results_reactive)
    thresholdResultTabServer("editor_threshold_convergence", "convergence", threshold_results_reactive)

    # --- Documentation tab ---
    output$documentation_tab <- shiny::renderUI({
      file_load_counter()
      m <- shiny::isolate(model())
      shiny::req(m)
      tags$div(
        class = "documentation-editor-container",
        `data-input-id` = "model_action",
        `data-initial` = openqaly::get_documentation(m)
      )
    })

    # --- Settings tab ---
    output$settings_panel <- shiny::renderUI({
      m <- model()
      shiny::req(m)
      s <- openqaly::get_settings(m)

      tags$div(
        class = "settings-form",
        style = "overflow-y: auto; height: 100%; padding: 1px;",
        bslib::card(
          bslib::card_header("Model Settings"),
          bslib::card_body(
            shiny::textInput("setting_model_type", "Model Type",
                             value = {
                               model_type_labels <- c(markov = "Markov", psm = "PSM", custom_psm = "Custom PSM", decision_tree = "Decision Tree")
                               mt <- openqaly::get_model_type(m)
                               model_type_labels[mt] %||% mt
                             }),
            tags$script(shiny::HTML(
              "document.getElementById('setting_model_type').setAttribute('disabled', 'disabled');"
            )),
            bslib::layout_columns(
              col_widths = c(6, 6),
              shiny::numericInput("setting_timeframe", "Timeframe",
                                  value = s$timeframe %||% 10, min = 1),
              shiny::selectInput("setting_timeframe_unit", "Timeframe Unit",
                                  choices = c("Years" = "years", "Months" = "months", "Weeks" = "weeks", "Days" = "days", "Cycles" = "cycles"),
                                  selected = s$timeframe_unit %||% "years")
            ),
            bslib::layout_columns(
              col_widths = c(6, 6),
              shiny::numericInput("setting_cycle_length", "Cycle Length",
                                  value = s$cycle_length %||% 1, min = 0, step = 0.1),
              shiny::selectInput("setting_cycle_length_unit", "Cycle Length Unit",
                                  choices = c("Years" = "years", "Months" = "months", "Weeks" = "weeks", "Days" = "days"),
                                  selected = s$cycle_length_unit %||% "years")
            ),
            bslib::layout_columns(
              col_widths = c(6, 6),
              shiny::numericInput("setting_discount_cost", "Discount Rate - Costs (%)",
                                  value = s$discount_cost %||% 0, min = 0, max = 100, step = 0.5),
              shiny::numericInput("setting_discount_outcomes", "Discount Rate - Outcomes (%)",
                                  value = s$discount_outcomes %||% 0, min = 0, max = 100, step = 0.5)
            ),
            shiny::selectInput("setting_half_cycle_method", "Half-Cycle Method",
                                choices = c("Start" = "start", "End" = "end", "Life-Table" = "life-table"),
                                selected = s$half_cycle_method %||% "start"),
            shiny::numericInput("setting_days_per_year", "Days Per Year",
                                value = s$days_per_year %||% 365.25, min = 1),
            shiny::checkboxInput("setting_reduce_state_cycle", "Reduce State Cycle",
                                  value = s$reduce_state_cycle %||% FALSE),
            bslib::layout_columns(
              col_widths = c(6, 6),
              shiny::selectInput("setting_country", "Country",
                                  choices = c(
                                    "United States (US)" = "US",
                                    "United Kingdom (GB)" = "GB",
                                    "Canada (CA)" = "CA",
                                    "India (IN)" = "IN",
                                    "Germany (DE)" = "DE",
                                    "Switzerland (CH)" = "CH",
                                    "France (FR)" = "FR",
                                    "Italy (IT)" = "IT",
                                    "Spain (ES)" = "ES",
                                    "Mexico (MX)" = "MX",
                                    "Netherlands (NL)" = "NL",
                                    "Norway (NO)" = "NO",
                                    "Sweden (SE)" = "SE",
                                    "Denmark (DK)" = "DK",
                                    "Finland (FI)" = "FI",
                                    "Japan (JP)" = "JP",
                                    "China (CN)" = "CN",
                                    "South Korea (KR)" = "KR",
                                    "Brazil (BR)" = "BR",
                                    "Poland (PL)" = "PL",
                                    "Czech Republic (CZ)" = "CZ",
                                    "Hungary (HU)" = "HU",
                                    "Russia (RU)" = "RU",
                                    "Ukraine (UA)" = "UA",
                                    "Israel (IL)" = "IL"
                                  ),
                                  selected = s$country %||% "US"),
              shiny::selectInput("setting_number_country", "Number Country",
                                  choices = c(
                                    "Same as Country" = "",
                                    "United States (US)" = "US",
                                    "United Kingdom (GB)" = "GB",
                                    "Canada (CA)" = "CA",
                                    "India (IN)" = "IN",
                                    "Germany (DE)" = "DE",
                                    "Switzerland (CH)" = "CH",
                                    "France (FR)" = "FR",
                                    "Italy (IT)" = "IT",
                                    "Spain (ES)" = "ES",
                                    "Mexico (MX)" = "MX",
                                    "Netherlands (NL)" = "NL",
                                    "Norway (NO)" = "NO",
                                    "Sweden (SE)" = "SE",
                                    "Denmark (DK)" = "DK",
                                    "Finland (FI)" = "FI",
                                    "Japan (JP)" = "JP",
                                    "China (CN)" = "CN",
                                    "South Korea (KR)" = "KR",
                                    "Brazil (BR)" = "BR",
                                    "Poland (PL)" = "PL",
                                    "Czech Republic (CZ)" = "CZ",
                                    "Hungary (HU)" = "HU",
                                    "Russia (RU)" = "RU",
                                    "Ukraine (UA)" = "UA",
                                    "Israel (IL)" = "IL"
                                  ),
                                  selected = s$number_country %||% "")
            )
          )
        ),
        if (openqaly::get_model_type(m) != "decision_tree") {
          tnames <- openqaly::get_tree_names(m)
          tree_choices <- stats::setNames(tnames, tnames)
          dt_config <- openqaly::get_decision_tree(m)
          current_tree <- dt_config$tree_name %||% ""
          current_duration <- dt_config$duration %||% ""
          current_unit <- dt_config$duration_unit %||% "days"
          bslib::card(
            bslib::card_header("Decision Tree"),
            bslib::card_body(
              shiny::selectInput("setting_dt_tree", "Active Tree",
                                 choices = c("None" = "", tree_choices),
                                 selected = current_tree),
              bslib::layout_columns(
                col_widths = c(6, 6),
                shiny::textInput("setting_dt_duration", "Duration",
                                 value = current_duration,
                                 placeholder = "e.g. 30 or variable name"),
                shiny::selectInput("setting_dt_duration_unit", "Duration Unit",
                                   choices = c("Days" = "days", "Weeks" = "weeks",
                                               "Months" = "months", "Years" = "years"),
                                   selected = current_unit)
              )
            )
          )
        }
      )
    })

    # Sync settings inputs when model changes externally
    shiny::observe({
      m <- model()
      shiny::req(m)
      s <- openqaly::get_settings(m)

      shiny::freezeReactiveValue(input, "setting_timeframe")
      shiny::freezeReactiveValue(input, "setting_timeframe_unit")
      shiny::freezeReactiveValue(input, "setting_cycle_length")
      shiny::freezeReactiveValue(input, "setting_cycle_length_unit")
      shiny::freezeReactiveValue(input, "setting_discount_cost")
      shiny::freezeReactiveValue(input, "setting_discount_outcomes")
      shiny::freezeReactiveValue(input, "setting_half_cycle_method")
      shiny::freezeReactiveValue(input, "setting_days_per_year")
      shiny::freezeReactiveValue(input, "setting_reduce_state_cycle")
      shiny::freezeReactiveValue(input, "setting_country")
      shiny::freezeReactiveValue(input, "setting_number_country")

      shiny::updateNumericInput(session, "setting_timeframe",
                                value = s$timeframe %||% 10)
      shiny::updateSelectInput(session, "setting_timeframe_unit",
                               selected = s$timeframe_unit %||% "years")
      shiny::updateNumericInput(session, "setting_cycle_length",
                                value = s$cycle_length %||% 1)
      shiny::updateSelectInput(session, "setting_cycle_length_unit",
                               selected = s$cycle_length_unit %||% "years")
      shiny::updateNumericInput(session, "setting_discount_cost",
                                value = s$discount_cost %||% 0)
      shiny::updateNumericInput(session, "setting_discount_outcomes",
                                value = s$discount_outcomes %||% 0)
      shiny::updateSelectInput(session, "setting_half_cycle_method",
                               selected = s$half_cycle_method %||% "start")
      shiny::updateNumericInput(session, "setting_days_per_year",
                                value = s$days_per_year %||% 365.25)
      shiny::updateCheckboxInput(session, "setting_reduce_state_cycle",
                                 value = s$reduce_state_cycle %||% FALSE)
      shiny::updateSelectInput(session, "setting_country",
                               selected = s$country %||% "US")
      shiny::updateSelectInput(session, "setting_number_country",
                               selected = s$number_country %||% "")

      # Sync decision tree inputs (skip for standalone decision_tree models)
      if (openqaly::get_model_type(m) != "decision_tree") {
        tnames <- openqaly::get_tree_names(m)
        tree_choices <- stats::setNames(tnames, tnames)

        shiny::freezeReactiveValue(input, "setting_dt_tree")
        shiny::freezeReactiveValue(input, "setting_dt_duration")
        shiny::freezeReactiveValue(input, "setting_dt_duration_unit")

        dt_config <- openqaly::get_decision_tree(m)
        shiny::updateSelectInput(session, "setting_dt_tree",
                                 choices = c("None" = "", tree_choices),
                                 selected = dt_config$tree_name %||% "")
        shiny::updateTextInput(session, "setting_dt_duration",
                               value = dt_config$duration %||% "")
        shiny::updateSelectInput(session, "setting_dt_duration_unit",
                                 selected = dt_config$duration_unit %||% "days")
      }
    })

    # Setting observers - each dispatches set_settings with one setting
    # Idempotency checks prevent feedback loops during undo/redo
    shiny::observeEvent(input$setting_timeframe, {
      new_val <- as.double(input$setting_timeframe)
      current <- openqaly::get_settings(model())$timeframe
      if (identical(new_val, current)) return()
      tryCatch({
        apply_action(list(type = "set_settings", settings = list(timeframe = new_val)))
      }, error = function(e) {
        shiny::showNotification(paste("Failed:", conditionMessage(e)), type = "error")
      })
    })

    shiny::observeEvent(input$setting_timeframe_unit, {
      new_val <- input$setting_timeframe_unit
      current <- openqaly::get_settings(model())$timeframe_unit
      if (identical(new_val, current)) return()
      tryCatch({
        apply_action(list(type = "set_settings", settings = list(timeframe_unit = new_val)))
      }, error = function(e) {
        shiny::showNotification(paste("Failed:", conditionMessage(e)), type = "error")
      })
    })

    shiny::observeEvent(input$setting_cycle_length, {
      new_val <- as.double(input$setting_cycle_length)
      current <- openqaly::get_settings(model())$cycle_length
      if (identical(new_val, current)) return()
      tryCatch({
        apply_action(list(type = "set_settings", settings = list(cycle_length = new_val)))
      }, error = function(e) {
        shiny::showNotification(paste("Failed:", conditionMessage(e)), type = "error")
      })
    })

    shiny::observeEvent(input$setting_cycle_length_unit, {
      new_val <- input$setting_cycle_length_unit
      current <- openqaly::get_settings(model())$cycle_length_unit
      if (identical(new_val, current)) return()
      tryCatch({
        apply_action(list(type = "set_settings", settings = list(cycle_length_unit = new_val)))
      }, error = function(e) {
        shiny::showNotification(paste("Failed:", conditionMessage(e)), type = "error")
      })
    })

    shiny::observeEvent(input$setting_discount_cost, {
      new_val <- as.double(input$setting_discount_cost)
      current <- openqaly::get_settings(model())$discount_cost
      if (identical(new_val, current)) return()
      tryCatch({
        apply_action(list(type = "set_settings", settings = list(discount_cost = new_val)))
      }, error = function(e) {
        shiny::showNotification(paste("Failed:", conditionMessage(e)), type = "error")
      })
    })

    shiny::observeEvent(input$setting_discount_outcomes, {
      new_val <- as.double(input$setting_discount_outcomes)
      current <- openqaly::get_settings(model())$discount_outcomes
      if (identical(new_val, current)) return()
      tryCatch({
        apply_action(list(type = "set_settings", settings = list(discount_outcomes = new_val)))
      }, error = function(e) {
        shiny::showNotification(paste("Failed:", conditionMessage(e)), type = "error")
      })
    })

    shiny::observeEvent(input$setting_half_cycle_method, {
      new_val <- input$setting_half_cycle_method
      current <- openqaly::get_settings(model())$half_cycle_method
      if (identical(new_val, current)) return()
      tryCatch({
        apply_action(list(type = "set_settings", settings = list(half_cycle_method = new_val)))
      }, error = function(e) {
        shiny::showNotification(paste("Failed:", conditionMessage(e)), type = "error")
      })
    })

    shiny::observeEvent(input$setting_days_per_year, {
      new_val <- as.double(input$setting_days_per_year)
      current <- openqaly::get_settings(model())$days_per_year
      if (identical(new_val, current)) return()
      tryCatch({
        apply_action(list(type = "set_settings", settings = list(days_per_year = new_val)))
      }, error = function(e) {
        shiny::showNotification(paste("Failed:", conditionMessage(e)), type = "error")
      })
    })

    shiny::observeEvent(input$setting_reduce_state_cycle, {
      new_val <- input$setting_reduce_state_cycle
      current <- openqaly::get_settings(model())$reduce_state_cycle
      if (identical(new_val, current)) return()
      tryCatch({
        apply_action(list(type = "set_settings", settings = list(reduce_state_cycle = new_val)))
      }, error = function(e) {
        shiny::showNotification(paste("Failed:", conditionMessage(e)), type = "error")
      })
    })

    shiny::observeEvent(input$setting_country, {
      new_val <- input$setting_country
      current <- openqaly::get_settings(model())$country
      if (identical(new_val, current)) return()
      tryCatch({
        apply_action(list(type = "set_settings", settings = list(country = new_val)))
      }, error = function(e) {
        shiny::showNotification(paste("Failed:", conditionMessage(e)), type = "error")
      })
    })

    shiny::observeEvent(input$setting_number_country, {
      new_val <- input$setting_number_country
      current <- openqaly::get_settings(model())$number_country
      setting_val <- if (nzchar(new_val)) new_val else NULL
      if (identical(setting_val, current)) return()
      tryCatch({
        apply_action(list(type = "set_settings", settings = list(number_country = setting_val)))
      }, error = function(e) {
        shiny::showNotification(paste("Failed:", conditionMessage(e)), type = "error")
      })
    })

    # Decision tree setting observers
    shiny::observeEvent(input$setting_dt_tree, {
      new_val <- input$setting_dt_tree
      m <- model()
      dt_config <- openqaly::get_decision_tree(m)
      current <- dt_config$tree_name %||% ""
      if (identical(new_val, current)) return()
      tryCatch({
        if (!nzchar(new_val)) {
          apply_action(list(type = "remove_decision_tree"))
        } else {
          dur <- dt_config$duration %||% ""
          dur_unit <- dt_config$duration_unit %||% ""
          apply_action(list(
            type = "set_decision_tree",
            tree_name = new_val,
            duration = dur,
            duration_unit = dur_unit
          ))
        }
      }, error = function(e) {
        shiny::showNotification(paste("Failed:", conditionMessage(e)), type = "error")
      })
    })

    shiny::observeEvent(input$setting_dt_duration, {
      new_val <- input$setting_dt_duration
      m <- model()
      dt_config <- openqaly::get_decision_tree(m)
      shiny::req(dt_config)
      current <- dt_config$duration %||% ""
      if (identical(new_val, current)) return()
      if (!nzchar(new_val)) return()
      tryCatch({
        apply_action(list(type = "edit_decision_tree", duration = new_val))
      }, error = function(e) {
        shiny::showNotification(paste("Failed:", conditionMessage(e)), type = "error")
      })
    })

    shiny::observeEvent(input$setting_dt_duration_unit, {
      new_val <- input$setting_dt_duration_unit
      m <- model()
      dt_config <- openqaly::get_decision_tree(m)
      shiny::req(dt_config)
      current <- dt_config$duration_unit %||% ""
      if (identical(new_val, current)) return()
      tryCatch({
        apply_action(list(type = "edit_decision_tree", duration_unit = new_val))
      }, error = function(e) {
        shiny::showNotification(paste("Failed:", conditionMessage(e)), type = "error")
      })
    })

    # --- Tables tab state ---
    selected_table <- shiny::reactiveVal(NULL)
    table_names_val <- shiny::reactiveVal(character(0))
    shiny::observe({
      m <- model()
      shiny::req(m)
      new_names <- openqaly::get_table_names(m)
      if (!identical(new_names, shiny::isolate(table_names_val()))) {
        table_names_val(new_names)
      }
    })

    # Auto-select first table when none selected or selected was deleted
    shiny::observe({
      m <- model()
      shiny::req(m)
      tnames <- openqaly::get_table_names(m)
      sel <- selected_table()
      if (is.null(sel) || !(sel %in% tnames)) {
        selected_table(if (length(tnames) > 0) tnames[1] else NULL)
      }
    })

    # Sidebar click handlers
    shiny::observeEvent(input$tables_select_click, {
      selected_table(input$tables_select_click)
    })

    # Add table modal
    shiny::observeEvent(input$tables_add_click, {
      shiny::showModal(shiny::modalDialog(
        title = "Add Table",
        shiny::textInput("tables_add_name", "Table Name"),
        shiny::textInput("tables_add_desc", "Description (optional)"),
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton("tables_add_confirm", "Create", class = "btn-primary")
        )
      ))
    })

    shiny::observeEvent(input$tables_add_confirm, {
      name <- trimws(input$tables_add_name)
      shiny::req(nzchar(name))
      desc <- input$tables_add_desc
      # Create large empty data.frame: first row will be headers in the grid
      empty_data <- as.data.frame(
        matrix("", nrow = 20, ncol = 26),
        stringsAsFactors = FALSE
      )
      colnames(empty_data) <- paste0("V", seq_len(26))
      tryCatch({
        apply_action(list(
          type = "add_table", name = name, data = empty_data,
          description = if (nzchar(desc %||% "")) desc else NULL
        ))
        selected_table(name)
        table_render_trigger(table_render_trigger() + 1L)
        shiny::removeModal()
      }, error = function(e) {
        shiny::showNotification(paste("Failed:", conditionMessage(e)), type = "error")
      })
    })

    # Edit table metadata modal
    shiny::observeEvent(input$tables_edit_click, {
      tname <- input$tables_edit_click
      shiny::req(tname)
      tables <- openqaly::get_tables(model())
      tbl <- tables[[tname]]
      shiny::showModal(shiny::modalDialog(
        title = "Edit Table Metadata",
        shiny::textInput("tables_edit_name", "Table Name", value = tname),
        shiny::textInput("tables_edit_desc", "Description",
                         value = tbl$description %||% ""),
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton("tables_edit_confirm", "Save", class = "btn-primary")
        )
      ))
    })

    shiny::observeEvent(input$tables_edit_confirm, {
      old_name <- input$tables_edit_click
      new_name <- trimws(input$tables_edit_name)
      desc <- input$tables_edit_desc
      shiny::req(nzchar(new_name))
      tryCatch({
        apply_action(list(
          type = "edit_table_meta", name = old_name,
          new_name = if (new_name != old_name) new_name else NULL,
          description = desc
        ))
        selected_table(new_name)
        table_render_trigger(table_render_trigger() + 1L)
        shiny::removeModal()
      }, error = function(e) {
        shiny::showNotification(paste("Failed:", conditionMessage(e)), type = "error")
      })
    })

    # Delete table
    shiny::observeEvent(input$tables_delete_click, {
      tname <- input$tables_delete_click
      shiny::req(tname)
      tryCatch({
        apply_action(list(
          type = "remove_table", name = tname
        ))
        table_render_trigger(table_render_trigger() + 1L)
      }, error = function(e) {
        shiny::showNotification(paste("Failed:", conditionMessage(e)), type = "error")
      })
    })

    # Tables tab UI
    output$tables_sidebar <- shiny::renderUI({
      tnames <- table_names_val()
      sel <- selected_table()

      sidebar_items <- lapply(tnames, function(tname) {
        active_class <- if (identical(tname, sel)) " active" else ""
        tags$div(
          class = paste0("tables-sidebar-item", active_class),
          onclick = sprintf(
            "Shiny.setInputValue('tables_select_click', '%s', {priority: 'event'})",
            gsub("'", "\\\\'", tname)
          ),
          tags$span(class = "table-name", tname),
          tags$div(
            class = "table-actions",
            tags$button(
              class = "btn",
              title = "Edit metadata",
              onclick = sprintf(
                "event.stopPropagation(); Shiny.setInputValue('tables_edit_click', '%s', {priority: 'event'})",
                gsub("'", "\\\\'", tname)
              ),
              shiny::icon("pencil")
            ),
            tags$button(
              class = "btn",
              title = "Delete table",
              onclick = sprintf(
                "event.stopPropagation(); Shiny.setInputValue('tables_delete_click', '%s', {priority: 'event'})",
                gsub("'", "\\\\'", tname)
              ),
              shiny::icon("times")
            )
          )
        )
      })

      tags$div(
        class = "tables-sidebar",
        shiny::actionButton("tables_add_click", "Add Table",
                            icon = shiny::icon("plus"),
                            class = "btn-outline-primary btn-sm mb-2 w-100"),
        sidebar_items
      )
    })

    # Render handsontable for selected table
    # Depends on selected_table() and table_render_trigger() only.
    # Uses isolate(model()) so cell edits do NOT cause re-render.
    output$tables_hot_output <- rhandsontable::renderRHandsontable({
      table_render_trigger()
      sel <- selected_table()
      m <- shiny::isolate(model())
      shiny::req(m, sel)
      tables <- openqaly::get_tables(m)
      shiny::req(sel %in% names(tables))
      tbl_data <- tables[[sel]]$data

      # Convert all columns to character for consistent editing
      tbl_data[] <- lapply(tbl_data, as.character)
      tbl_data[is.na(tbl_data)] <- ""

      # Prepend column names as row 1 (editable header)
      header_row <- as.data.frame(
        as.list(colnames(tbl_data)),
        stringsAsFactors = FALSE
      )
      colnames(header_row) <- colnames(tbl_data)
      tbl_data <- rbind(header_row, tbl_data)

      # Pad to large canvas
      target_rows <- max(50, nrow(tbl_data) + 20)
      target_cols <- max(26, ncol(tbl_data) + 5)

      # Add extra columns
      if (ncol(tbl_data) < target_cols) {
        extra <- as.data.frame(
          matrix("", nrow = nrow(tbl_data), ncol = target_cols - ncol(tbl_data)),
          stringsAsFactors = FALSE
        )
        colnames(extra) <- paste0("V", seq(ncol(tbl_data) + 1, target_cols))
        tbl_data <- cbind(tbl_data, extra)
      }

      # Add extra rows
      if (nrow(tbl_data) < target_rows) {
        empty_rows <- as.data.frame(
          matrix("", nrow = target_rows - nrow(tbl_data), ncol = ncol(tbl_data)),
          stringsAsFactors = FALSE
        )
        colnames(empty_rows) <- colnames(tbl_data)
        tbl_data <- rbind(tbl_data, empty_rows)
      }

      # Replace column names with generic Excel-style labels
      colnames(tbl_data) <- excel_col_labels(ncol(tbl_data))

      hot <- rhandsontable::rhandsontable(tbl_data, stretchH = "all",
                                          rowHeaders = TRUE, contextMenu = TRUE,
                                          height = 400)
      # Style header row
      htmlwidgets::onRender(hot, "
        function(el, x) {
          var hot = this.hot;
          hot.updateSettings({
            cells: function(row, col) {
              if (row === 0) {
                return { renderer: function(instance, td, row, col, prop, value, cellProperties) {
                  Handsontable.renderers.TextRenderer.apply(this, arguments);
                  td.style.fontWeight = 'bold';
                  td.style.textDecoration = 'underline';
                  td.style.backgroundColor = '#f0f0f0';
                }};
              }
              return {};
            }
          });
          var lastWidth = 0;
          var setHeight = function() {
            var h = window.innerHeight - el.getBoundingClientRect().top - 16;
            if (h > 50) hot.updateSettings({ height: h });
          };
          if (typeof ResizeObserver !== 'undefined') {
            new ResizeObserver(function(entries) {
              var w = entries[0].contentRect.width;
              setHeight();
              if (w !== lastWidth && w > 0) {
                lastWidth = w;
                hot.render();
              }
            }).observe(el);
          } else {
            requestAnimationFrame(setHeight);
            window.addEventListener('resize', setHeight);
          }
        }
      ")
    })

    # Handle cell edits from handsontable
    shiny::observeEvent(input$tables_hot_output, {
      sel <- selected_table()
      shiny::req(sel)
      hot_data <- rhandsontable::hot_to_r(input$tables_hot_output)
      shiny::req(is.data.frame(hot_data))

      # Convert to character and trim
      hot_data[] <- lapply(hot_data, as.character)
      hot_data[is.na(hot_data)] <- ""

      # Row 1 = column names, rows 2+ = data
      if (nrow(hot_data) < 1) return()
      new_colnames <- as.character(hot_data[1, ])
      hot_data <- hot_data[-1, , drop = FALSE]
      rownames(hot_data) <- NULL

      # Trim trailing empty rows
      non_empty_rows <- which(apply(hot_data, 1, function(r) any(nzchar(r))))
      if (length(non_empty_rows) == 0) return()
      hot_data <- hot_data[seq_len(max(non_empty_rows)), , drop = FALSE]

      # Trim trailing empty columns (check both data and header row)
      non_empty_cols <- which(
        nzchar(new_colnames) |
        apply(hot_data, 2, function(c) any(nzchar(c)))
      )
      if (length(non_empty_cols) == 0) return()
      hot_data <- hot_data[, seq_len(max(non_empty_cols)), drop = FALSE]
      new_colnames <- new_colnames[seq_len(max(non_empty_cols))]

      # Apply column names from row 1; use generic names for blanks
      for (i in seq_along(new_colnames)) {
        if (!nzchar(new_colnames[i])) new_colnames[i] <- paste0("V", i)
      }
      colnames(hot_data) <- new_colnames

      # Compare with current model data to avoid circular updates
      # (compare as character before type inference to avoid NA issues)
      m <- model()
      tables <- openqaly::get_tables(m)
      if (sel %in% names(tables)) {
        current <- tables[[sel]]$data
        current[] <- lapply(current, as.character)
        current[is.na(current)] <- ""
        if (identical(dim(hot_data), dim(current)) &&
            identical(colnames(hot_data), colnames(current)) &&
            all(hot_data == current)) {
          return()
        }
      }

      # Infer types (numeric, integer, logical) from character columns
      hot_data[] <- lapply(hot_data, function(col) {
        utils::type.convert(col, as.is = TRUE)
      })

      tryCatch({
        apply_action(list(
          type = "edit_table", name = sel, data = hot_data
        ))
      }, error = function(e) {
        shiny::showNotification(paste("Save failed:", conditionMessage(e)), type = "error")
      })
    })

    # --- Scripts tab state ---
    selected_script <- shiny::reactiveVal(NULL)
    script_names_val <- shiny::reactiveVal(character(0))
    shiny::observe({
      m <- model()
      shiny::req(m)
      new_names <- openqaly::get_script_names(m)
      if (is.null(new_names)) new_names <- character(0)
      if (!identical(new_names, shiny::isolate(script_names_val()))) {
        script_names_val(new_names)
      }
    })

    # Auto-select first script when none selected or selected was deleted
    shiny::observe({
      m <- model()
      shiny::req(m)
      snames <- openqaly::get_script_names(m)
      if (is.null(snames)) snames <- character(0)
      sel <- selected_script()
      if (is.null(sel) || !(sel %in% snames)) {
        selected_script(if (length(snames) > 0) snames[1] else NULL)
      }
    })

    # Sidebar click handlers
    shiny::observeEvent(input$scripts_select_click, {
      selected_script(input$scripts_select_click)
    })

    # Add script modal
    shiny::observeEvent(input$scripts_add_click, {
      shiny::showModal(shiny::modalDialog(
        title = "Add Script",
        shiny::textInput("scripts_add_name", "Script Name"),
        shiny::textInput("scripts_add_desc", "Description (optional)"),
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton("scripts_add_confirm", "Create", class = "btn-primary")
        )
      ))
    })

    shiny::observeEvent(input$scripts_add_confirm, {
      name <- trimws(input$scripts_add_name)
      shiny::req(nzchar(name))
      desc <- input$scripts_add_desc
      tryCatch({
        apply_action(list(
          type = "add_script", name = name, code = "",
          description = if (nzchar(desc %||% "")) desc else NULL
        ))
        selected_script(name)
        shiny::removeModal()
      }, error = function(e) {
        shiny::showNotification(paste("Failed:", conditionMessage(e)), type = "error")
      })
    })

    # Edit script metadata modal
    shiny::observeEvent(input$scripts_edit_click, {
      sname <- input$scripts_edit_click
      shiny::req(sname)
      script <- model()$scripts[[sname]]
      shiny::showModal(shiny::modalDialog(
        title = "Edit Script Metadata",
        shiny::textInput("scripts_edit_name", "Script Name", value = sname),
        shiny::textInput("scripts_edit_desc", "Description",
                         value = script$description %||% ""),
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton("scripts_edit_confirm", "Save", class = "btn-primary")
        )
      ))
    })

    shiny::observeEvent(input$scripts_edit_confirm, {
      old_name <- input$scripts_edit_click
      new_name <- trimws(input$scripts_edit_name)
      desc <- input$scripts_edit_desc
      shiny::req(nzchar(new_name))
      tryCatch({
        apply_action(list(
          type = "edit_script", name = old_name,
          new_name = if (new_name != old_name) new_name else NULL,
          description = desc
        ))
        selected_script(new_name)
        shiny::removeModal()
      }, error = function(e) {
        shiny::showNotification(paste("Failed:", conditionMessage(e)), type = "error")
      })
    })

    # Delete script
    shiny::observeEvent(input$scripts_delete_click, {
      sname <- input$scripts_delete_click
      shiny::req(sname)
      tryCatch({
        apply_action(list(
          type = "remove_script", name = sname
        ))
      }, error = function(e) {
        shiny::showNotification(paste("Failed:", conditionMessage(e)), type = "error")
      })
    })

    # Load script content into Ace when selection changes or undo/redo occurs
    shiny::observe({
      sel <- selected_script()
      file_load_counter()  # React to undo/redo and file loads
      m <- shiny::isolate(model())
      scripts <- openqaly::get_scripts(m)
      if (is.null(sel) || is.null(m) || is.null(scripts[[sel]])) return()
      script <- scripts[[sel]]
      session$sendCustomMessage("scripts_load_content", list(
        code = script$code %||% ""
      ))
    })

    # Handle code changes from Ace editor
    shiny::observeEvent(input$scripts_ace_code, {
      sel <- shiny::isolate(selected_script())
      shiny::req(sel)
      m <- shiny::isolate(model())
      current_code <- openqaly::get_scripts(m)[[sel]]$code %||% ""
      if (identical(input$scripts_ace_code, current_code)) return()
      tryCatch({
        apply_action(list(
          type = "edit_script", name = sel, code = input$scripts_ace_code
        ))
      }, error = function(e) {
        shiny::showNotification(paste("Save failed:", conditionMessage(e)), type = "error")
        file_load_counter(file_load_counter() + 1L)
      })
    })

    # Scripts tab UI
    output$scripts_tab <- shiny::renderUI({
      snames <- script_names_val()
      sel <- selected_script()

      sidebar_items <- lapply(snames, function(sname) {
        active_class <- if (identical(sname, sel)) " active" else ""
        tags$div(
          class = paste0("scripts-sidebar-item", active_class),
          onclick = sprintf(
            "Shiny.setInputValue('scripts_select_click', '%s', {priority: 'event'})",
            gsub("'", "\\\\'", sname)
          ),
          tags$span(class = "script-name", sname),
          tags$div(
            class = "script-actions",
            tags$button(
              class = "btn",
              title = "Edit metadata",
              onclick = sprintf(
                "event.stopPropagation(); Shiny.setInputValue('scripts_edit_click', '%s', {priority: 'event'})",
                gsub("'", "\\\\'", sname)
              ),
              shiny::icon("pencil")
            ),
            tags$button(
              class = "btn",
              title = "Delete script",
              onclick = sprintf(
                "event.stopPropagation(); Shiny.setInputValue('scripts_delete_click', '%s', {priority: 'event'})",
                gsub("'", "\\\\'", sname)
              ),
              shiny::icon("times")
            )
          )
        )
      })

      # Build right panel content
      right_panel <- if (!is.null(sel)) {
        tags$div(
          class = "scripts-editor",
          tags$div(id = "scripts_ace_editor")
        )
      } else {
        tags$div(
          class = "scripts-editor",
          tags$div(
            style = "display:flex;align-items:center;justify-content:center;height:100%;color:#999;",
            "No script selected"
          )
        )
      }

      ui <- tags$div(
        class = "scripts-tab-container",
        tags$div(
          class = "scripts-sidebar",
          shiny::actionButton("scripts_add_click", "Add Script",
                              icon = shiny::icon("plus"),
                              class = "btn-outline-primary btn-sm mb-2 w-100"),
          sidebar_items
        ),
        right_panel
      )

      ui
    })

    # --- Strategies tab (Tabulator grid) ---
    pending_remove_strategy <- shiny::reactiveVal(NULL)

    output$strategies_table <- shiny::renderUI({
      file_load_counter()
      m <- shiny::isolate(model())
      shiny::req(m)
      strategies_df <- openqaly::get_strategies(m)

      # Get strategy-specific variables
      vars_df <- openqaly::get_variables(m)
      strategy_vars <- vars_df[!is.na(vars_df$strategy) & nzchar(vars_df$strategy), , drop = FALSE]
      strat_var_names <- unique(strategy_vars$name)

      # Build initial data with var__<varname> fields
      initial_data <- lapply(seq_len(nrow(strategies_df)), function(i) {
        row <- as.list(strategies_df[i, ])
        strat_name <- row$name
        for (vname in strat_var_names) {
          match_idx <- which(strategy_vars$name == vname & strategy_vars$strategy == strat_name)
          row[[paste0("var__", vname)]] <- if (length(match_idx) > 0) {
            as.character(strategy_vars$formula[match_idx[1]])
          } else {
            ""
          }
        }
        row[["_id"]] <- row$name
        row
      })

      # Build var_columns metadata
      var_columns <- lapply(strat_var_names, function(vname) {
        dn <- strategy_vars$display_name[strategy_vars$name == vname][1]
        if (is.na(dn) || !nzchar(dn)) dn <- vname
        list(name = vname, display_name = dn)
      })
      terms <- get_model_terms(m, "variable")
      suggestions <- get_model_suggestions(m, "variable")

      shiny::tagList(
        strategies_table_dependency(),
        add_strategy_modal_dependency(),
        formula_input_dependency(),
        variables_table_dependency(),
        shiny::tags$div(
          class = "strategies-table-container",
          `data-input-id` = "strategies_action",
          `data-initial` = jsonlite::toJSON(initial_data, auto_unbox = TRUE),
          `data-var-columns` = jsonlite::toJSON(var_columns, auto_unbox = TRUE),
          `data-terms` = jsonlite::toJSON(terms, auto_unbox = FALSE),
          `data-suggestions` = jsonlite::toJSON(suggestions, auto_unbox = FALSE)
        )
      )
    })

    # Reactive to track whether modal has strategy-specific vars
    add_strategy_has_vars <- shiny::reactiveVal(FALSE)

    shiny::observeEvent(input$strategies_action, {
      action <- input$strategies_action
      shiny::req(action$type)

      # Intercept modal trigger
      if (action$type == "show_add_strategy_modal") {
        m <- model()
        shiny::req(m)

        # Find strategy-specific variables
        vars_df <- openqaly::get_variables(m)
        strategy_vars <- vars_df[!is.na(vars_df$strategy) & nzchar(vars_df$strategy), , drop = FALSE]
        # Get unique variable names that have strategy-specific rows
        strat_var_names <- unique(strategy_vars$name)
        has_vars <- length(strat_var_names) > 0
        add_strategy_has_vars(has_vars)

        # Build modal content — 2-column grid for strategy fields
        modal_body <- shiny::tagList(
          shiny::tags$div(
            class = "row",
            shiny::tags$div(
              class = "col-md-6",
              shiny::textInput("add_strategy_name", "Name", value = "")
            ),
            shiny::tags$div(
              class = "col-md-6",
              shiny::textInput("add_strategy_display_name", "Display Name", value = "")
            )
          ),
          shiny::tags$div(
            class = "row",
            shiny::tags$div(
              class = "col-md-6",
              shiny::textInput("add_strategy_description", "Description", value = "")
            ),
            shiny::tags$div(
              class = "col-md-6",
              shiny::selectInput("add_strategy_enabled", "Enabled",
                                 choices = c("Yes" = "1", "No" = "0"),
                                 selected = "1")
            )
          )
        )

        if (has_vars) {
          modal_body <- shiny::tagList(
            modal_body,
            shiny::tags$hr(),
            shiny::tags$h5("Strategy-Specific Variables"),
            shiny::tags$p(
              class = "text-muted small",
              "These variables have strategy-specific formulas. ",
              "Enter formulas for the new strategy, or leave blank to skip."
            ),
            shiny::tags$div(id = "add-strategy-vars-container")
          )
        }

        shiny::showModal(shiny::tags$div(
          class = "add-strategy-modal",
          shiny::modalDialog(
            title = "Add Strategy",
            modal_body,
            size = "l",
            footer = shiny::tagList(
              shiny::modalButton("Cancel"),
              shiny::actionButton("add_strategy_confirm", "Create Strategy",
                                  class = "btn-primary")
            )
          )
        ))

        # Send variable data to JS for table initialization
        if (has_vars) {
          terms <- get_model_terms(m, "variable")
          suggestions <- get_model_suggestions(m, "variable")

          # Build initial rows: one per strategy-specific variable name
          var_rows <- lapply(strat_var_names, function(vname) {
            list(name = vname, display_name = "", description = "", formula = "")
          })

          # Pre-serialize terms/suggestions with auto_unbox = FALSE to preserve
          # array structure (single-element vectors must stay as JSON arrays).
          # sendCustomMessage uses auto_unbox = TRUE which would break the
          # FormulaHighlighter/FormulaCompleter APIs.
          session$sendCustomMessage("init_add_strategy_vars_table", list(
            variables = var_rows,
            terms_json = jsonlite::toJSON(terms, auto_unbox = FALSE),
            suggestions_json = jsonlite::toJSON(suggestions, auto_unbox = FALSE)
          ))
        }

        return()
      }

      tryCatch({
        result <- apply_action(action)
        if (result$status == "noop") {
          file_load_counter(file_load_counter() + 1L)
        } else if (action$type %in% c("add_strategy", "remove_strategy",
                                        "force_remove_strategy", "add_variable")) {
          file_load_counter(file_load_counter() + 1L)
        }
      }, strategy_has_dependencies = function(e) {
        pending_remove_strategy(action$name)
        deps <- e$dependencies

        dep_html <- lapply(names(deps), function(key) {
          dep <- deps[[key]]
          label <- gsub("_", " ", key)
          label <- paste0(toupper(substring(label, 1, 1)), substring(label, 2))
          if (is.list(dep) && !is.null(names(dep))) {
            items <- lapply(names(dep), function(parent) {
              children <- paste0("<li>", htmltools::htmlEscape(dep[[parent]]), "</li>",
                                 collapse = "")
              tags$li(
                htmltools::htmlEscape(parent),
                tags$ul(shiny::HTML(children))
              )
            })
            tags$div(
              tags$strong(label),
              tags$ul(items)
            )
          } else {
            items <- paste0("<li>", htmltools::htmlEscape(dep), "</li>", collapse = "")
            tags$div(
              tags$strong(label),
              tags$ul(shiny::HTML(items))
            )
          }
        })

        shiny::showModal(shiny::modalDialog(
          title = paste0("Remove strategy \"", action$name, "\"?"),
          tags$p(paste0("Removing strategy \"", action$name,
                        "\" will also remove the following:")),
          tags$div(
            style = "max-height: 300px; overflow-y: auto;",
            dep_html
          ),
          footer = shiny::tagList(
            shiny::modalButton("Cancel"),
            shiny::actionButton("strategies_force_remove_confirm", "Remove Anyway",
                                class = "btn-danger")
          )
        ))
      }, error = function(e) {
        shiny::showNotification(
          paste("Action failed:", conditionMessage(e)),
          type = "error"
        )
        file_load_counter(file_load_counter() + 1L)
      })
    })

    shiny::observeEvent(input$strategies_force_remove_confirm, {
      sname <- pending_remove_strategy()
      shiny::req(sname)
      tryCatch({
        apply_action(list(
          type = "force_remove_strategy", name = sname
        ))
        pending_remove_strategy(NULL)
        file_load_counter(file_load_counter() + 1L)
        shiny::removeModal()
      }, error = function(e) {
        shiny::showNotification(paste("Failed:", conditionMessage(e)), type = "error")
      })
    })

    # --- Add Strategy modal confirm ---
    shiny::observeEvent(input$add_strategy_confirm, {
      name <- trimws(input$add_strategy_name %||% "")
      if (!nzchar(name)) {
        shiny::showNotification("Strategy name is required.", type = "error")
        return()
      }

      if (add_strategy_has_vars()) {
        # Ask JS to collect the table data; the vars handler below will finish
        session$sendCustomMessage("collect_add_strategy_vars", list())
        return()
      }

      # No strategy-specific vars — just add the strategy directly
      tryCatch({
        apply_action(list(
          type = "add_strategy",
          name = name,
          display_name = trimws(input$add_strategy_display_name %||% ""),
          description = trimws(input$add_strategy_description %||% ""),
          enabled = if (input$add_strategy_enabled == "0") 0 else 1
        ))
        file_load_counter(file_load_counter() + 1L)
        shiny::removeModal()
      }, error = function(e) {
        shiny::showNotification(
          paste("Failed to add strategy:", conditionMessage(e)),
          type = "error"
        )
      })
    })

    # --- Add Strategy modal vars data handler (atomic batch) ---
    shiny::observeEvent(input$add_strategy_modal_vars, {
      name <- trimws(input$add_strategy_name %||% "")
      if (!nzchar(name)) {
        shiny::showNotification("Strategy name is required.", type = "error")
        return()
      }

      var_rows <- input$add_strategy_modal_vars
      old <- model()

      tryCatch({
        # Push history once for atomic undo
        history$push(old)

        # 1. Add the strategy
        result <- dispatch_model_action(old, list(
          type = "add_strategy",
          name = name,
          display_name = trimws(input$add_strategy_display_name %||% ""),
          description = trimws(input$add_strategy_description %||% ""),
          enabled = if (input$add_strategy_enabled == "0") 0 else 1
        ))

        # 2. Add variable rows with non-empty formulas
        for (row in var_rows) {
          formula <- trimws(row$formula %||% "")
          if (!nzchar(formula)) next
          result <- dispatch_model_action(result, list(
            type = "add_variable",
            name = row$name,
            formula = formula,
            display_name = trimws(row$display_name %||% ""),
            description = trimws(row$description %||% ""),
            strategy = name
          ))
        }

        model(result)
        file_load_counter(file_load_counter() + 1L)
        shiny::removeModal()
      }, error = function(e) {
        # Revert on any error — pop the history entry we just pushed
        history$undo(old)
        model(old)
        shiny::showNotification(
          paste("Failed to add strategy:", conditionMessage(e)),
          type = "error"
        )
      })
    })

    # --- Groups tab (Tabulator grid) ---
    pending_remove_group <- shiny::reactiveVal(NULL)

    output$groups_table <- shiny::renderUI({
      file_load_counter()
      m <- shiny::isolate(model())
      shiny::req(m)
      groups_df <- openqaly::get_groups(m)

      # Get group-specific variables
      vars_df <- openqaly::get_variables(m)
      group_vars <- vars_df[!is.na(vars_df$group) & nzchar(vars_df$group), , drop = FALSE]
      grp_var_names <- unique(group_vars$name)

      # Build initial data with var__<varname> fields
      initial_data <- lapply(seq_len(nrow(groups_df)), function(i) {
        row <- as.list(groups_df[i, ])
        grp_name <- row$name
        for (vname in grp_var_names) {
          match_idx <- which(group_vars$name == vname & group_vars$group == grp_name)
          row[[paste0("var__", vname)]] <- if (length(match_idx) > 0) {
            as.character(group_vars$formula[match_idx[1]])
          } else {
            ""
          }
        }
        row[["_id"]] <- row$name
        row
      })

      # Build var_columns metadata
      var_columns <- lapply(grp_var_names, function(vname) {
        dn <- group_vars$display_name[group_vars$name == vname][1]
        if (is.na(dn) || !nzchar(dn)) dn <- vname
        list(name = vname, display_name = dn)
      })
      terms <- get_model_terms(m, "variable")
      suggestions <- get_model_suggestions(m, "variable")

      shiny::tagList(
        groups_table_dependency(),
        add_group_modal_dependency(),
        formula_input_dependency(),
        variables_table_dependency(),
        shiny::tags$div(
          class = "groups-table-container",
          `data-input-id` = "groups_action",
          `data-initial` = jsonlite::toJSON(initial_data, auto_unbox = TRUE),
          `data-var-columns` = jsonlite::toJSON(var_columns, auto_unbox = TRUE),
          `data-terms` = jsonlite::toJSON(terms, auto_unbox = FALSE),
          `data-suggestions` = jsonlite::toJSON(suggestions, auto_unbox = FALSE)
        )
      )
    })

    # Reactive to track whether modal has group-specific vars
    add_group_has_vars <- shiny::reactiveVal(FALSE)

    shiny::observeEvent(input$groups_action, {
      action <- input$groups_action
      shiny::req(action$type)

      # Intercept modal trigger
      if (action$type == "show_add_group_modal") {
        m <- model()
        shiny::req(m)

        # Find group-specific variables
        vars_df <- openqaly::get_variables(m)
        group_vars <- vars_df[!is.na(vars_df$group) & nzchar(vars_df$group), , drop = FALSE]
        grp_var_names <- unique(group_vars$name)
        has_vars <- length(grp_var_names) > 0
        add_group_has_vars(has_vars)

        # Build modal content — 2-column grid for group fields
        modal_body <- shiny::tagList(
          shiny::tags$div(
            class = "row",
            shiny::tags$div(
              class = "col-md-6",
              shiny::textInput("add_group_name", "Name", value = "")
            ),
            shiny::tags$div(
              class = "col-md-6",
              shiny::textInput("add_group_display_name", "Display Name", value = "")
            )
          ),
          shiny::tags$div(
            class = "row",
            shiny::tags$div(
              class = "col-md-4",
              shiny::textInput("add_group_description", "Description", value = "")
            ),
            shiny::tags$div(
              class = "col-md-4",
              shiny::textInput("add_group_weight", "Weight", value = "1")
            ),
            shiny::tags$div(
              class = "col-md-4",
              shiny::selectInput("add_group_enabled", "Enabled",
                                 choices = c("Yes" = "1", "No" = "0"),
                                 selected = "1")
            )
          )
        )

        if (has_vars) {
          modal_body <- shiny::tagList(
            modal_body,
            shiny::tags$hr(),
            shiny::tags$h5("Group-Specific Variables"),
            shiny::tags$p(
              class = "text-muted small",
              "These variables have group-specific formulas. ",
              "Enter formulas for the new group, or leave blank to skip."
            ),
            shiny::tags$div(id = "add-group-vars-container")
          )
        }

        shiny::showModal(shiny::tags$div(
          class = "add-group-modal",
          shiny::modalDialog(
            title = "Add Group",
            modal_body,
            size = "l",
            footer = shiny::tagList(
              shiny::modalButton("Cancel"),
              shiny::actionButton("add_group_confirm", "Create Group",
                                  class = "btn-primary")
            )
          )
        ))

        # Send variable data to JS for table initialization
        if (has_vars) {
          terms <- get_model_terms(m, "variable")
          suggestions <- get_model_suggestions(m, "variable")

          # Build initial rows: one per group-specific variable name
          var_rows <- lapply(grp_var_names, function(vname) {
            list(name = vname, display_name = "", description = "", formula = "")
          })

          session$sendCustomMessage("init_add_group_vars_table", list(
            variables = var_rows,
            terms_json = jsonlite::toJSON(terms, auto_unbox = FALSE),
            suggestions_json = jsonlite::toJSON(suggestions, auto_unbox = FALSE)
          ))
        }

        return()
      }

      tryCatch({
        result <- apply_action(action)
        if (result$status == "noop") {
          file_load_counter(file_load_counter() + 1L)
        } else if (action$type %in% c("add_group", "remove_group",
                                        "force_remove_group", "add_variable",
                                        "edit_variable")) {
          file_load_counter(file_load_counter() + 1L)
        }
      }, group_has_dependencies = function(e) {
        pending_remove_group(action$name)
        deps <- e$dependencies

        dep_html <- lapply(names(deps), function(key) {
          dep <- deps[[key]]
          label <- gsub("_", " ", key)
          label <- paste0(toupper(substring(label, 1, 1)), substring(label, 2))
          if (is.list(dep) && !is.null(names(dep))) {
            items <- lapply(names(dep), function(parent) {
              children <- paste0("<li>", htmltools::htmlEscape(dep[[parent]]), "</li>",
                                 collapse = "")
              tags$li(
                htmltools::htmlEscape(parent),
                tags$ul(shiny::HTML(children))
              )
            })
            tags$div(
              tags$strong(label),
              tags$ul(items)
            )
          } else {
            items <- paste0("<li>", htmltools::htmlEscape(dep), "</li>", collapse = "")
            tags$div(
              tags$strong(label),
              tags$ul(shiny::HTML(items))
            )
          }
        })

        shiny::showModal(shiny::modalDialog(
          title = paste0("Remove group \"", action$name, "\"?"),
          tags$p(paste0("Removing group \"", action$name,
                        "\" will also remove the following:")),
          tags$div(
            style = "max-height: 300px; overflow-y: auto;",
            dep_html
          ),
          footer = shiny::tagList(
            shiny::modalButton("Cancel"),
            shiny::actionButton("groups_force_remove_confirm", "Remove Anyway",
                                class = "btn-danger")
          )
        ))
      }, error = function(e) {
        shiny::showNotification(
          paste("Action failed:", conditionMessage(e)),
          type = "error"
        )
        file_load_counter(file_load_counter() + 1L)
      })
    })

    shiny::observeEvent(input$groups_force_remove_confirm, {
      gname <- pending_remove_group()
      shiny::req(gname)
      tryCatch({
        apply_action(list(
          type = "force_remove_group", name = gname
        ))
        pending_remove_group(NULL)
        file_load_counter(file_load_counter() + 1L)
        shiny::removeModal()
      }, error = function(e) {
        shiny::showNotification(paste("Failed:", conditionMessage(e)), type = "error")
      })
    })

    # --- Add Group modal confirm ---
    shiny::observeEvent(input$add_group_confirm, {
      name <- trimws(input$add_group_name %||% "")
      if (!nzchar(name)) {
        shiny::showNotification("Group name is required.", type = "error")
        return()
      }

      if (add_group_has_vars()) {
        # Ask JS to collect the table data; the vars handler below will finish
        session$sendCustomMessage("collect_add_group_vars", list())
        return()
      }

      # No group-specific vars — just add the group directly
      tryCatch({
        apply_action(list(
          type = "add_group",
          name = name,
          display_name = trimws(input$add_group_display_name %||% ""),
          description = trimws(input$add_group_description %||% ""),
          weight = trimws(input$add_group_weight %||% "1"),
          enabled = if (input$add_group_enabled == "0") 0 else 1
        ))
        file_load_counter(file_load_counter() + 1L)
        shiny::removeModal()
      }, error = function(e) {
        shiny::showNotification(
          paste("Failed to add group:", conditionMessage(e)),
          type = "error"
        )
      })
    })

    # --- Add Group modal vars data handler (atomic batch) ---
    shiny::observeEvent(input$add_group_modal_vars, {
      name <- trimws(input$add_group_name %||% "")
      if (!nzchar(name)) {
        shiny::showNotification("Group name is required.", type = "error")
        return()
      }

      var_rows <- input$add_group_modal_vars
      old <- model()

      tryCatch({
        # Push history once for atomic undo
        history$push(old)

        # 1. Add the group
        result <- dispatch_model_action(old, list(
          type = "add_group",
          name = name,
          display_name = trimws(input$add_group_display_name %||% ""),
          description = trimws(input$add_group_description %||% ""),
          weight = trimws(input$add_group_weight %||% "1"),
          enabled = if (input$add_group_enabled == "0") 0 else 1
        ))

        # 2. Add variable rows with non-empty formulas
        for (row in var_rows) {
          formula <- trimws(row$formula %||% "")
          if (!nzchar(formula)) next
          result <- dispatch_model_action(result, list(
            type = "add_variable",
            name = row$name,
            formula = formula,
            display_name = trimws(row$display_name %||% ""),
            description = trimws(row$description %||% ""),
            group = name
          ))
        }

        model(result)
        file_load_counter(file_load_counter() + 1L)
        shiny::removeModal()
      }, error = function(e) {
        # Revert on any error — pop the history entry we just pushed
        history$undo(old)
        model(old)
        shiny::showNotification(
          paste("Failed to add group:", conditionMessage(e)),
          type = "error"
        )
      })
    })

    # --- States tab ---
    output$states_table <- shiny::renderUI({
      file_load_counter()
      m <- shiny::isolate(model())
      shiny::req(m)
      model_type <- openqaly::get_model_type(m)
      if (model_type == "decision_tree") {
        return(shiny::tags$p("Decision tree models do not use states."))
      }
      states_df <- openqaly::get_states(m)
      initial_data <- lapply(seq_len(nrow(states_df)), function(i) {
        row <- as.list(states_df[i, ])
        # Convert logicals to Yes/No for JS
        if (!is.null(row$share_state_time)) {
          row$share_state_time <- if (isTRUE(row$share_state_time)) "Yes" else "No"
        }
        row[["_id"]] <- row$name
        row
      })
      tag_args <- list(
        class = "states-table-container",
        `data-input-id` = "model_action",
        `data-initial` = jsonlite::toJSON(initial_data, auto_unbox = TRUE),
        `data-model-type` = model_type
      )
      deps <- list(states_table_dependency())
      if (model_type == "markov") {
        terms <- get_model_terms(m, "variable")
        suggestions <- get_model_suggestions(m, "variable")
        tag_args[["data-terms"]] <- jsonlite::toJSON(terms, auto_unbox = FALSE)
        tag_args[["data-suggestions"]] <- jsonlite::toJSON(suggestions, auto_unbox = FALSE)
        deps <- c(deps, list(formula_input_dependency()))
      }
      shiny::tagList(
        deps,
        rlang::inject(shiny::tags$div(!!!tag_args))
      )
    })

    # --- Transitions tab ---
    output$transitions_table <- shiny::renderUI({
      file_load_counter()
      m <- shiny::isolate(model())
      shiny::req(m)
      model_type <- openqaly::get_model_type(m)
      if (model_type == "decision_tree") {
        return(shiny::tags$p("Decision tree models do not use transitions."))
      }
      trans_df <- openqaly::get_model_transitions(m)
      state_names <- openqaly::get_states(m)$name
      terms <- get_model_terms(m, "variable")
      suggestions <- get_model_suggestions(m, "variable")
      initial_data <- lapply(seq_len(nrow(trans_df)), function(i) {
        row <- as.list(trans_df[i, ])
        if (model_type == "markov") {
          row[["_id"]] <- paste(row$from_state, row$to_state, sep = "|")
        } else if (model_type == "psm") {
          row[["_id"]] <- row$endpoint
        } else if (model_type == "custom_psm") {
          row[["_id"]] <- row$state
        }
        row
      })
      shiny::tagList(
        transitions_table_dependency(),
        formula_input_dependency(),
        shiny::tags$div(
          class = "transitions-table-container",
          `data-input-id` = "model_action",
          `data-initial` = jsonlite::toJSON(initial_data, auto_unbox = TRUE),
          `data-model-type` = model_type,
          `data-state-names` = jsonlite::toJSON(state_names, auto_unbox = TRUE),
          `data-terms` = jsonlite::toJSON(terms, auto_unbox = FALSE),
          `data-suggestions` = jsonlite::toJSON(suggestions, auto_unbox = FALSE)
        )
      )
    })

    output$variables_table <- shiny::renderUI({
      file_load_counter()
      m <- shiny::isolate(model())
      shiny::req(m)

      vars_df <- openqaly::get_variables(m)
      strategies <- openqaly::get_strategies(m)
      groups <- openqaly::get_groups(m)
      terms <- get_model_terms(m, "variable")
      suggestions <- get_model_suggestions(m, "variable")

      initial_data <- lapply(seq_len(nrow(vars_df)), function(i) {
        row <- as.list(vars_df[i, ])
        row[["_id"]] <- paste(row$name, row$strategy %||% "", row$group %||% "", sep = "|")
        row
      })
      strategy_map <- stats::setNames(strategies$name, strategies$display_name)
      group_map <- stats::setNames(groups$name, groups$display_name)

      shiny::tagList(
        variables_table_dependency(),
        formula_input_dependency(),
        shiny::tags$div(
          class = "variables-table-container",
          `data-input-id` = "model_action",
          `data-initial` = jsonlite::toJSON(initial_data, auto_unbox = TRUE),
          `data-strategies` = jsonlite::toJSON(as.list(strategy_map), auto_unbox = TRUE),
          `data-groups` = jsonlite::toJSON(as.list(group_map), auto_unbox = TRUE),
          `data-terms` = jsonlite::toJSON(terms, auto_unbox = FALSE),
          `data-suggestions` = jsonlite::toJSON(suggestions, auto_unbox = FALSE)
        )
      )
    })

    # --- Values tab ---
    output$values_table <- shiny::renderUI({
      file_load_counter()
      m <- shiny::isolate(model())
      shiny::req(m)
      model_type <- openqaly::get_model_type(m)

      values_df <- openqaly::get_model_values(m)
      terms <- get_model_terms(m, "variable")
      suggestions <- get_model_suggestions(m, "variable")

      initial_data <- lapply(seq_len(nrow(values_df)), function(i) {
        row <- as.list(values_df[i, ])
        # Ensure NA fields become empty strings for JS
        for (nm in names(row)) {
          if (is.na(row[[nm]])) row[[nm]] <- ""
        }
        # Convert formula to character
        row$formula <- as.character(row$formula)
        if ("discounting_override" %in% names(row)) {
          row$discounting_override <- as.character(row$discounting_override)
          if (row$discounting_override == "NA") row$discounting_override <- ""
        }
        row[["_id"]] <- paste(row$name, row$state, row$destination, sep = "|")
        row
      })

      state_names <- if (model_type != "decision_tree") openqaly::get_states(m)$name else character(0)

      shiny::tagList(
        values_table_dependency(),
        formula_input_dependency(),
        shiny::tags$div(
          class = "values-table-container",
          `data-input-id` = "model_action",
          `data-initial` = jsonlite::toJSON(initial_data, auto_unbox = TRUE),
          `data-model-type` = model_type,
          `data-state-names` = jsonlite::toJSON(state_names, auto_unbox = TRUE),
          `data-terms` = jsonlite::toJSON(terms, auto_unbox = FALSE),
          `data-suggestions` = jsonlite::toJSON(suggestions, auto_unbox = FALSE)
        )
      )
    })

    output$summaries_table <- shiny::renderUI({
      file_load_counter()
      m <- shiny::isolate(model())
      shiny::req(m)

      summaries_df <- openqaly::get_model_summaries(m)
      if (is.null(summaries_df) || nrow(summaries_df) == 0) {
        initial_data <- list()
      } else {
        initial_data <- lapply(seq_len(nrow(summaries_df)), function(i) {
          row <- as.list(summaries_df[i, ])
          for (nm in names(row)) {
            if (is.na(row[[nm]])) row[[nm]] <- ""
          }
          row[["_id"]] <- row$name
          row
        })
      }

      values_df <- openqaly::get_model_values(m)
      outcome_values <- unique(values_df$name[values_df$type == "outcome"])
      cost_values <- unique(values_df$name[values_df$type == "cost"])

      shiny::tagList(
        summaries_table_dependency(),
        shiny::tags$div(
          class = "summaries-table-container",
          `data-input-id` = "model_action",
          `data-initial` = jsonlite::toJSON(initial_data, auto_unbox = TRUE),
          `data-outcome-values` = jsonlite::toJSON(outcome_values, auto_unbox = FALSE),
          `data-cost-values` = jsonlite::toJSON(cost_values, auto_unbox = FALSE)
        )
      )
    })

    # --- Decision Trees tab state ---
    selected_tree <- shiny::reactiveVal(NULL)
    tree_names_val <- shiny::reactiveVal(character(0))

    shiny::observe({
      m <- model()
      shiny::req(m)
      new_names <- openqaly::get_tree_names(m)
      if (!identical(new_names, shiny::isolate(tree_names_val()))) {
        tree_names_val(new_names)
      }
    })

    shiny::observe({
      m <- model()
      shiny::req(m)
      tnames <- openqaly::get_tree_names(m)
      sel <- selected_tree()
      if (is.null(sel) || !(sel %in% tnames)) {
        selected_tree(if (length(tnames) > 0) tnames[1] else NULL)
      }
    })

    shiny::observeEvent(input$trees_select_click, {
      selected_tree(input$trees_select_click)
    })

    # Add Tree modal
    shiny::observeEvent(input$trees_add_click, {
      shiny::showModal(shiny::modalDialog(
        title = "Add Tree",
        shiny::textInput("trees_add_name", "Tree Name"),
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton("trees_add_confirm", "Create", class = "btn-primary")
        )
      ))
    })

    shiny::observeEvent(input$trees_add_confirm, {
      name <- trimws(input$trees_add_name)
      shiny::req(nzchar(name))
      tryCatch({
        apply_action(list(type = "add_tree_node", tree_name = name, node = "root"))
        selected_tree(name)
        shiny::removeModal()
      }, error = function(e) {
        shiny::showNotification(paste("Failed:", conditionMessage(e)), type = "error")
      })
    })

    # Rename Tree modal
    shiny::observeEvent(input$trees_edit_click, {
      tname <- input$trees_edit_click
      shiny::req(tname)
      shiny::showModal(shiny::modalDialog(
        title = "Rename Tree",
        shiny::textInput("trees_edit_name", "Tree Name", value = tname),
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton("trees_edit_confirm", "Save", class = "btn-primary")
        )
      ))
    })

    shiny::observeEvent(input$trees_edit_confirm, {
      old_name <- input$trees_edit_click
      new_name <- trimws(input$trees_edit_name)
      shiny::req(nzchar(new_name))
      if (new_name != old_name) {
        tryCatch({
          m <- model()
          trees_df <- openqaly::get_trees(m)
          tree_nodes <- trees_df[trees_df$name == old_name, ]
          apply_action(list(
            type = "edit_tree_node", tree_name = old_name,
            node = tree_nodes$node[1], field = "tree_name", value = new_name
          ))
          selected_tree(new_name)
          shiny::removeModal()
        }, error = function(e) {
          shiny::showNotification(paste("Failed:", conditionMessage(e)), type = "error")
        })
      } else {
        shiny::removeModal()
      }
    })

    # Delete Tree
    shiny::observeEvent(input$trees_delete_click, {
      tname <- input$trees_delete_click
      shiny::req(tname)
      tryCatch({
        m <- model()
        # If the deleted tree is the active decision tree, remove it first
        dt_config <- openqaly::get_decision_tree(m)
        if (identical(dt_config$tree_name, tname)) {
          apply_action(list(type = "remove_decision_tree"))
        }
        apply_action(list(type = "remove_tree", tree_name = tname))
      }, error = function(e) {
        shiny::showNotification(paste("Failed:", conditionMessage(e)), type = "error")
      })
    })

    # Toggle show/hide preview panel via custom message
    shiny::observeEvent(input$trees_preview_on, {
      session$sendCustomMessage("trees_toggle_preview",
        list(show = isTRUE(input$trees_preview_on)))
    })

    # Tree preview plot
    output$trees_preview_plot <- shiny::renderPlot({
      shiny::req(isTRUE(input$trees_preview_on))
      sel <- selected_tree()
      m <- model()
      shiny::req(m, sel)
      openqaly::plot_decision_tree(m, tree_name = sel)
    }, bg = "transparent")

    # Trees sidebar
    output$trees_sidebar <- shiny::renderUI({
      tnames <- tree_names_val()
      sel <- selected_tree()
      sidebar_items <- lapply(tnames, function(tname) {
        active_class <- if (identical(tname, sel)) " active" else ""
        tags$div(
          class = paste0("trees-sidebar-item", active_class),
          onclick = sprintf(
            "Shiny.setInputValue('trees_select_click', '%s', {priority: 'event'})",
            gsub("'", "\\\\'", tname)
          ),
          tags$span(class = "tree-name", tname),
          tags$div(
            class = "tree-actions",
            tags$button(
              class = "btn", title = "Rename tree",
              onclick = sprintf(
                "event.stopPropagation(); Shiny.setInputValue('trees_edit_click', '%s', {priority: 'event'})",
                gsub("'", "\\\\'", tname)
              ),
              shiny::icon("pencil")
            ),
            tags$button(
              class = "btn", title = "Delete tree",
              onclick = sprintf(
                "event.stopPropagation(); Shiny.setInputValue('trees_delete_click', '%s', {priority: 'event'})",
                gsub("'", "\\\\'", tname)
              ),
              shiny::icon("times")
            )
          )
        )
      })
      tags$div(
        class = "trees-sidebar",
        shiny::actionButton("trees_add_click", "Add Tree",
                            icon = shiny::icon("plus"),
                            class = "btn-outline-primary btn-sm mb-2 w-100"),
        sidebar_items
      )
    })

    # Trees table (filtered to selected tree)
    output$trees_table <- shiny::renderUI({
      file_load_counter()
      sel <- selected_tree()
      m <- shiny::isolate(model())
      shiny::req(m, sel)
      trees_df <- openqaly::get_trees(m)
      if (is.null(trees_df) || !is.data.frame(trees_df) || nrow(trees_df) == 0) {
        tree_rows <- trees_df[0, ]
      } else {
        tree_rows <- trees_df[trees_df$name == sel, ]
      }
      if (is.null(tree_rows) || nrow(tree_rows) == 0) {
        initial_data <- list()
      } else {
        initial_data <- lapply(seq_len(nrow(tree_rows)), function(i) {
          row <- as.list(tree_rows[i, ])
          for (nm in names(row)) {
            val <- row[[nm]]
            if (is.null(val) || length(val) == 0 || (length(val) == 1 && is.na(val))) {
              row[[nm]] <- ""
            }
          }
          row$formula <- as.character(row$formula)
          row[["_id"]] <- paste(sel, row$node, sep = "|")
          row$name <- NULL
          row
        })
      }
      terms <- get_model_terms(m, "variable")
      suggestions <- get_model_suggestions(m, "variable")
      shiny::tagList(
        trees_table_dependency(),
        formula_input_dependency(),
        shiny::tags$div(
          class = "trees-table-container",
          `data-input-id` = "model_action",
          `data-tree-name` = sel,
          `data-initial` = jsonlite::toJSON(initial_data, auto_unbox = TRUE),
          `data-terms` = jsonlite::toJSON(terms, auto_unbox = FALSE),
          `data-suggestions` = jsonlite::toJSON(suggestions, auto_unbox = FALSE)
        )
      )
    })

    # Undo/redo button observers
    shiny::observeEvent(input$undo_btn, perform_undo())
    shiny::observeEvent(input$redo_btn, perform_redo())

    # Reactive button state sync
    shiny::observe({
      session$sendCustomMessage("toggle_undo_redo", list(
        can_undo = history$can_undo(),
        can_redo = history$can_redo()
      ))
    })
  }

  pkgs <- loadedNamespaces()

  shiny::shinyApp(ui, server, onStart = function() {
    n_cores <- future::availableCores()
    old_plan <- future::plan(list(
      #future::tweak(future::multisession, workers = 1),
      future::tweak(future::multisession,
        workers = I(max(1, n_cores)))
    ))
    # Pre-load all currently loaded packages on outer worker
    f <- future::future({
      for (p in pkgs) {
        suppressPackageStartupMessages(
          requireNamespace(p, quietly = TRUE)
        )
      }
      TRUE
    })
    future::value(f)
    shiny::onStop(function() future::plan(old_plan))
  }, options = options)
}
