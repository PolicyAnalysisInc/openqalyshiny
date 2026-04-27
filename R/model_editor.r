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


dispatch_model_action <- function(model, action) {
  switch(action$type,
    "replace_model" = {
      replacement <- action$model
      if (!inherits(replacement, "oq_model")) {
        stop("Replacement model must be an oq_model object.", call. = FALSE)
      }
      replacement
    },
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
          if (!nzchar(value %||% "")) return(model)
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
        if (!nzchar(value %||% "")) return(model)
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
      if (!nzchar(action$formula %||% "")) return(model)
      parsed_formula <- rlang::parse_expr(action$formula)
      if (model_type == "markov") {
        rlang::inject(openqaly::add_transition(
          model,
          from_state = action$from_state,
          to_state = action$to_state,
          formula = !!parsed_formula
        ))
      } else if (model_type == "psm") {
        rlang::inject(openqaly::add_transition(
          model,
          endpoint = action$endpoint,
          time_unit = action$time_unit,
          formula = !!parsed_formula
        ))
      } else if (model_type == "custom_psm") {
        rlang::inject(openqaly::add_transition(
          model,
          state = action$state,
          formula = !!parsed_formula
        ))
      } else {
        stop("Unsupported model type for transitions: ", model_type)
      }
    },
    "edit_transition" = {
      model_type <- action$model_type %||% openqaly::get_model_type(model)
      if (model_type == "markov") {
        args <- list(model = model, from_state = action$from_state, to_state = action$to_state)
        if (action$field == "formula") {
          if (!nzchar(action$value %||% "")) return(model)
          args$formula <- rlang::parse_expr(action$value)
          rlang::inject(openqaly::edit_transition(!!!args))
        } else if (action$field %in% c("from_state", "to_state")) {
          match_idx <- which(
            model$transitions$from_state == action$from_state &
              model$transitions$to_state == action$to_state
          )
          if (length(match_idx) == 0) stop("Markov transition not found")
          trans_formula <- rlang::parse_expr(as.character(model$transitions$formula[[match_idx[1]]]))
          new_from <- if (action$field == "from_state") action$value else action$from_state
          new_to <- if (action$field == "to_state") action$value else action$to_state
          model <- openqaly::remove_transition(
            model,
            from_state = action$from_state,
            to_state = action$to_state
          )
          rlang::inject(openqaly::add_transition(
            model,
            from_state = new_from,
            to_state = new_to,
            formula = !!trans_formula
          ))
        } else {
          rlang::inject(openqaly::edit_transition(!!!args))
        }
      } else if (model_type == "psm") {
        args <- list(model = model, endpoint = action$endpoint)
        if (action$field == "formula") {
          if (!nzchar(action$value %||% "")) return(model)
          args$formula <- rlang::parse_expr(action$value)
        } else if (action$field == "time_unit") {
          args$time_unit <- action$value
        }
        rlang::inject(openqaly::edit_transition(!!!args))
      } else if (model_type == "custom_psm") {
        args <- list(model = model, state = action$state)
        if (action$field == "formula") {
          if (!nzchar(action$value %||% "")) return(model)
          args$formula <- rlang::parse_expr(action$value)
          rlang::inject(openqaly::edit_transition(!!!args))
        } else if (action$field == "state") {
          match_idx <- which(model$transitions$state == action$state)
          if (length(match_idx) == 0) stop("Custom PSM transition not found")
          trans_formula <- rlang::parse_expr(as.character(model$transitions$formula[[match_idx[1]]]))
          model <- openqaly::remove_transition(model, state = action$state)
          rlang::inject(openqaly::add_transition(
            model,
            state = action$value,
            formula = !!trans_formula
          ))
        } else {
          rlang::inject(openqaly::edit_transition(!!!args))
        }
      }
    },
    "remove_transition" = {
      model_type <- action$model_type %||% openqaly::get_model_type(model)
      if (model_type == "markov") {
        openqaly::remove_transition(model, from_state = action$from_state, to_state = action$to_state)
      } else if (model_type == "psm") {
        openqaly::remove_transition(model, endpoint = action$endpoint)
      } else if (model_type == "custom_psm") {
        openqaly::remove_transition(model, state = action$state)
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
          if (!nzchar(action$value %||% "")) return(model)
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
      args$formula <- if (nzchar(action$formula %||% "")) {
        rlang::parse_expr(action$formula)
      } else {
        NA
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
        if (!nzchar(value %||% "")) return(model)
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
      if (!is.null(action$duration) && nzchar(action$duration)) {
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
    "set_override_expressions" = {
      openqaly::set_override_expressions(model, action$overrides)
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
      if (!nzchar(action$low %||% "") || !nzchar(action$high %||% "")) return(model)
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
      if (!is.null(action$low) && nzchar(action$low)) args$low <- rlang::parse_expr(action$low)
      if (!is.null(action$high) && nzchar(action$high)) args$high <- rlang::parse_expr(action$high)
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
      if (!nzchar(action$value %||% "")) return(model)
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
      if (!is.null(action$value) && nzchar(action$value)) {
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
      if (!is.null(action$min) && nzchar(action$min)) args$min <- rlang::parse_expr(action$min)
      if (!is.null(action$max) && nzchar(action$max)) args$max <- rlang::parse_expr(action$max)
      if (!is.null(action$radius) && nzchar(action$radius)) args$radius <- rlang::parse_expr(action$radius)
      if (!is.null(action$values) && nzchar(action$values)) args$values <- rlang::parse_expr(action$values)
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
      type <- .str(action$mv_dist_type)
      if (!nzchar(type)) return(model)
      add_args <- list(
        model,
        name = action$name,
        type = type,
        variables = as.character(unlist(action$variables))
      )
      if (nzchar(.str(action$strategy))) add_args$strategy <- action$strategy
      if (nzchar(.str(action$group))) add_args$group <- action$group
      if (type == "dirichlet") {
        if (!is.null(action$n)) add_args$n <- as.numeric(action$n)
      } else if (type == "mvnormal") {
        if (!is.null(action$covariance)) add_args$covariance <- .str(action$covariance)
      }
      do.call(openqaly::add_multivariate_sampling, add_args)
    },
    "edit_multivariate_sampling" = {
      args <- list(model, name = action$name)
      if (!is.null(action$new_name)) args$new_name <- action$new_name
      if (!is.null(action$type_value)) args$type <- .str(action$type_value)
      if (!is.null(action$variables)) args$variables <- as.character(unlist(action$variables))
      if (!is.null(action$strategy)) args$strategy <- .str(action$strategy)
      if (!is.null(action$group)) args$group <- .str(action$group)
      # Pass only type-specific params
      edit_type <- args$type %||% {
        idx <- which(vapply(model$multivariate_sampling, function(s) s$name == action$name, logical(1)))
        if (length(idx) > 0) model$multivariate_sampling[[idx[1]]]$type else ""
      }
      if (edit_type == "dirichlet") {
        if (!is.null(action$n)) args$n <- as.numeric(action$n)
      } else if (edit_type == "mvnormal") {
        if (!is.null(action$covariance)) args$covariance <- .str(action$covariance)
      }
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

# Helper: info icon with popover text
info_icon <- function(text) {
  tags$button(class = "info-trigger",
    shiny::icon("circle-info"),
    tags$span(class = "info-popover", text)
  )
}

# Unified sidebar list component used by scripts, tables, and scenarios.
# items: list of named lists with `name` and optional `description`.
# selected: the name of the currently selected item (or NULL).
# select_input, edit_input, delete_input, add_input: Shiny input ids
# that receive click events (select/edit/delete receive the item name;
# add receives a button click count).
oq_sidebar_list_ui <- function(items,
                                selected = NULL,
                                add_input,
                                select_input,
                                edit_input,
                                delete_input,
                                add_label = "Add") {
  item_tags <- lapply(items, function(it) {
    nm <- it$name
    desc <- it$description %||% ""
    active_class <- if (identical(nm, selected)) " active" else ""
    nm_js <- gsub("'", "\\\\'", nm)
    tags$div(
      class = paste0("oq-sidebar-item", active_class),
      onclick = sprintf(
        "Shiny.setInputValue('%s', '%s', {priority: 'event'})",
        select_input, nm_js
      ),
      tags$div(
        class = "oq-sidebar-item-content",
        tags$div(class = "oq-sidebar-item-name", nm),
        if (nzchar(desc)) tags$div(class = "oq-sidebar-item-desc", desc)
      ),
      tags$div(
        class = "oq-sidebar-item-actions",
        tags$button(
          type = "button",
          class = "btn",
          title = "Edit",
          onclick = sprintf(
            "event.stopPropagation(); Shiny.setInputValue('%s', '%s', {priority: 'event'})",
            edit_input, nm_js
          ),
          shiny::icon("pencil")
        ),
        tags$button(
          type = "button",
          class = "btn",
          title = "Delete",
          onclick = sprintf(
            "event.stopPropagation(); Shiny.setInputValue('%s', '%s', {priority: 'event'})",
            delete_input, nm_js
          ),
          shiny::icon("times")
        )
      )
    )
  })

  tags$div(
    class = "oq-sidebar",
    shiny::actionButton(
      add_input, add_label,
      icon = shiny::icon("plus"),
      class = "btn-outline-primary btn-sm oq-sidebar-add"
    ),
    tags$div(class = "oq-sidebar-list", item_tags)
  )
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
  tab_value_panel <- function(navset_id, value, ...) {
    shiny::conditionalPanel(
      condition = sprintf("input['%s'] == '%s'", navset_id, value),
      ...
    )
  }

  empty_state <- function(icon, title, subtitle = NULL) {
    tags$div(
      class = "empty-state-placeholder",
      tags$div(class = "empty-state-icon", icon),
      tags$div(class = "empty-state-title", title),
      if (!is.null(subtitle)) tags$div(class = "empty-state-subtitle", subtitle)
    )
  }

  analysis_sidebar <- function(sidebar_id,
                               visualization_content,
                               overrides_output_id,
                               footer_content,
                               width = "400px") {
    bslib::sidebar(
      id = sidebar_id,
      width = width,
      class = "results-analysis-sidebar",
      fillable = TRUE,
      tags$div(
        class = "results-sidebar-shell",
        tags$div(
          class = "results-sidebar-tabs",
          bslib::navset_card_tab(
            id = paste0(sidebar_id, "_sections"),
            selected = "visualization",
            bslib::nav_panel(
              "Visualization",
              value = "visualization",
              tags$div(
                class = "results-sidebar-panel-scroll",
                .editorize_selectize_tag(visualization_content)
              )
            ),
            bslib::nav_panel(
              "Overrides",
              value = "overrides",
              tags$div(
                class = "results-sidebar-panel-scroll",
                shiny::uiOutput(overrides_output_id)
              )
            )
          )
        ),
        tags$div(
          class = "results-sidebar-footer",
          footer_content
        )
      )
    )
  }

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
    htmltools::htmlDependency(
      name = "design-tokens",
      version = "1.0.0",
      src = system.file("www", package = "openqalyshiny"),
      stylesheet = "design-tokens.css",
      all_files = FALSE
    ),
    info_hover_dependency(),
    scripts_editor_dependency(),
    documentation_editor_dependency(),
    formula_input_dependency(),
    override_input_dependency(),
    tags$head(
      tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
      tags$link(rel = "preconnect", href = "https://fonts.gstatic.com", crossorigin = NA),
      tags$link(
        rel = "stylesheet",
        href = "https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&family=JetBrains+Mono:wght@400;500&display=swap"
      ),
      tags$script(shiny::HTML("
        // Ctrl+S / Cmd+S save shortcut
        document.addEventListener('keydown', function(e) {
          if ((e.ctrlKey || e.metaKey) && e.key === 's') {
            e.preventDefault();
            Shiny.setInputValue('save_file', Math.random(), {priority: 'event'});
          }
          // Ctrl+N / Cmd+N new model shortcut
          if ((e.ctrlKey || e.metaKey) && e.key === 'n') {
            e.preventDefault();
            Shiny.setInputValue('new_model', Math.random(), {priority: 'event'});
          }
        });
        // Programmatic Save As trigger
        Shiny.addCustomMessageHandler('trigger_save_as', function(msg) {
          var btn = document.getElementById('save_as_file');
          if (btn) btn.click();
        });
        // Programmatic page navigation
        Shiny.addCustomMessageHandler('navigate_page', function(msg) {
          if (typeof switchAppPage === 'function') switchAppPage(msg.page);
        });
        // Re-initialize widgets after override panel renderUI
        Shiny.addCustomMessageHandler('override-rebind', function(data) {
          setTimeout(function() {
            var container = document.querySelector('.override-input-container');
            if (!container) return;
            // Rebind formula inputs (binding is pre-loaded in page head)
            $(container).find('.formula-input').each(function() {
              if (!this._formulaEditor) {
                Shiny.unbindAll(this.parentNode);
                Shiny.bindAll(this.parentNode);
              }
            });
          }, 300);
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
        (function() {
          var refreshScheduled = false;

          function inModelEditor(element) {
            return !!(element && element.closest('.bslib-page-fill'));
          }

          function repositionOpenSelectize() {
            if (!window.jQuery) {
              return;
            }

            window.jQuery('.selectized, select.shiny-bound-input').each(function() {
              if (!this.selectize) {
                return;
              }

              var instance = this.selectize;
              if (!instance.isOpen || !inModelEditor(instance.$wrapper && instance.$wrapper[0])) {
                return;
              }

              if (typeof instance.positionDropdown === 'function') {
                instance.positionDropdown();
              }
            });
          }

          function refreshFormulaEditors() {
            var formulaInputs = document.querySelectorAll('.bslib-page-fill .formula-input');

            formulaInputs.forEach(function(el) {
              if (typeof el._formulaResizeHandler === 'function') {
                el._formulaResizeHandler();
              }
            });
          }

          function scheduleOverlayRefresh() {
            if (refreshScheduled) {
              return;
            }

            refreshScheduled = true;
            window.requestAnimationFrame(function() {
              refreshScheduled = false;
              repositionOpenSelectize();
              refreshFormulaEditors();
            });
          }
          document.addEventListener('shiny:bound', scheduleOverlayRefresh);
          document.addEventListener('shown.bs.tab', function() {
            scheduleOverlayRefresh();
          });
          document.addEventListener('scroll', function(event) {
            if (event.target && event.target.closest &&
                event.target.closest('.results-sidebar-panel-scroll')) {
              scheduleOverlayRefresh();
            }
          });
          window.addEventListener('resize', scheduleOverlayRefresh);
        })();
      ")),
      tags$style(shiny::HTML("
        html, body {
          height: 100%;
        }
        body {
          overflow: hidden;
        }
        .bslib-page-fill {
          height: 100vh;
          height: 100dvh;
          min-height: 100vh;
          min-height: 100dvh;
        }
        .app-bar {
          display: flex;
          align-items: center;
          background-color: var(--oq-surface);
          color: var(--oq-text);
          padding: 0 16px;
          height: var(--oq-topbar-h);
          flex-shrink: 0;
          border-bottom: 1px solid var(--oq-border);
          box-shadow: var(--oq-shadow-xs);
        }
        .app-bar-logo {
          width: 28px;
          height: 28px;
          border-radius: 7px;
          background: var(--oq-primary);
          color: var(--oq-text-inverse);
          font-size: 15px;
          font-weight: 700;
          display: flex;
          align-items: center;
          justify-content: center;
          margin-right: 10px;
          flex-shrink: 0;
        }
        .app-bar-sep {
          width: 1px;
          height: 24px;
          background: var(--oq-border);
          margin: 0 12px;
          flex-shrink: 0;
        }
        .app-bar-title {
          font-size: var(--oq-text-md);
          font-weight: 600;
          margin-right: 24px;
        }
        .app-bar .dropdown .btn {
          color: var(--oq-text);
          background: transparent;
          border: none;
        }
        .app-bar .dropdown .btn:hover,
        .app-bar .dropdown .btn:focus {
          background: var(--oq-surface-hover);
        }
        .no-model-message {
          display: flex;
          align-items: center;
          justify-content: center;
          height: 100%;
          color: var(--oq-text-tertiary);
          font-size: var(--oq-text-md);
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
          overflow-x: auto; overflow-y: auto;
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
        /* Make uiOutput wrappers inside sidebar containers behave as
           flex items so their .oq-sidebar child can stretch to full
           height and enable list scrolling. */
        .tables-tab-container > .shiny-html-output,
        .scripts-tab-container > .shiny-html-output,
        .trees-tab-container > .shiny-html-output {
          display: flex;
          flex-direction: column;
          min-height: 0;
        }
        /* Unified sidebar component (oq-sidebar)
           Used by scripts, tables, and scenario inputs */
        .oq-sidebar {
          width: 240px;
          min-width: 240px;
          border-right: 1px solid var(--oq-border);
          display: flex;
          flex-direction: column;
          min-height: 0;
          height: 100%;
        }
        .oq-sidebar-add {
          margin: 8px 8px 4px 8px;
        }
        .oq-sidebar-list {
          flex: 1 1 auto;
          overflow-y: auto;
          padding: 4px 8px 8px 8px;
        }
        .oq-sidebar-item {
          display: flex;
          align-items: flex-start;
          padding: 8px 10px;
          cursor: pointer;
          border-radius: 6px;
          margin-bottom: 2px;
        }
        .oq-sidebar-item:hover {
          background-color: var(--oq-surface-hover);
        }
        .oq-sidebar-item.active,
        .oq-sidebar-item--active {
          background-color: var(--oq-surface-active);
        }
        .oq-sidebar-item-content {
          flex: 1 1 auto;
          min-width: 0;
          overflow: hidden;
          display: flex;
          flex-direction: column;
          justify-content: center;
          min-height: 24px;
        }
        .oq-sidebar-item-name {
          display: block;
          overflow: hidden;
          text-overflow: ellipsis;
          white-space: nowrap;
          font-weight: 500;
          line-height: 1.3;
        }
        .oq-sidebar-item.active .oq-sidebar-item-name,
        .oq-sidebar-item--active .oq-sidebar-item-name {
          font-weight: 600;
          color: var(--oq-primary, #6366f1);
        }
        .oq-sidebar-item-desc {
          font-size: 0.75rem;
          color: var(--oq-text-tertiary, #9ca3af);
          margin-top: 1px;
          overflow: hidden;
          text-overflow: ellipsis;
          white-space: nowrap;
          font-weight: 400;
          line-height: 1.3;
        }
        .oq-sidebar-item-actions {
          display: flex;
          gap: 4px;
          margin-left: 4px;
          flex-shrink: 0;
          align-self: center;
        }
        .oq-sidebar-item-actions .btn {
          padding: 0 4px;
          font-size: 12px;
          line-height: 1;
          border: none;
          background: transparent;
          color: var(--oq-text-tertiary);
          opacity: 0;
          transition: opacity var(--oq-transition), color var(--oq-transition);
        }
        .oq-sidebar-item:hover .oq-sidebar-item-actions .btn,
        .oq-sidebar-item.active .oq-sidebar-item-actions .btn,
        .oq-sidebar-item--active .oq-sidebar-item-actions .btn {
          opacity: 1;
        }
        .oq-sidebar-item-actions .btn:hover {
          color: var(--oq-text);
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
          box-shadow: var(--oq-shadow-xs);
        }
        .trees-preview-toggle input:checked + .trees-toggle-slider {
          background: var(--oq-primary);
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
          border: 1px solid var(--oq-border);
          border-radius: 8px;
        }
        /* ── Settings page ── */
        .settings-scroll {
          flex: 1;
          overflow-y: auto;
          padding: 20px 24px;
        }
        .settings-container {
          max-width: 720px;
          background: var(--oq-surface);
          border: 1px solid var(--oq-border);
          border-radius: var(--oq-radius-md);
          box-shadow: var(--oq-shadow-xs);
          padding: 20px 24px;
        }
        .settings-section-header {
          font-weight: 600;
          font-size: var(--oq-text-sm);
          color: var(--oq-text);
          margin: 0 0 12px;
          padding-bottom: 6px;
          border-bottom: 1px solid var(--oq-border-light);
        }
        .settings-section-header:not(:first-child) {
          margin-top: 24px;
        }
        .settings-grid {
          display: grid;
          grid-template-columns: 1fr 1fr 1fr;
          gap: 12px 16px;
          min-width: 0;
        }
        .settings-grid > * {
          min-width: 0;
        }
        .settings-grid .form-group {
          margin-bottom: 0;
        }
        .settings-grid .form-group label,
        .settings-grid .form-group .control-label {
          font-size: var(--oq-text-xs);
          font-weight: 600;
          color: var(--oq-text-secondary);
          text-transform: uppercase;
          letter-spacing: 0.3px;
          margin-bottom: 4px;
        }
        .settings-grid .form-group .form-control {
          width: 100%;
          max-width: none;
        }
        .settings-grid .field-checkbox {
          display: flex;
          flex-direction: row;
          align-items: center;
          gap: 8px;
          padding-top: 22px;
        }
        .settings-grid .field-checkbox .checkbox {
          margin: 0;
        }
        .settings-grid .field-checkbox .checkbox label {
          font-size: var(--oq-text-xs);
          font-weight: 600;
          color: var(--oq-text-secondary);
          text-transform: uppercase;
          letter-spacing: 0.3px;
        }
        .settings-grid .field-checkbox input[type='checkbox'] {
          accent-color: var(--oq-primary);
        }
        .dt-card {
          margin-top: 16px;
          max-width: 720px;
          background: var(--oq-surface);
          border: 1px solid var(--oq-border);
          border-radius: var(--oq-radius-md);
          box-shadow: var(--oq-shadow-xs);
          padding: 20px 24px;
        }
        .dt-card .settings-section-header:first-child {
          margin-top: 0;
        }
        .app-bar .btn:disabled {
          opacity: 0.3;
          cursor: default;
          pointer-events: none;
        }
        .app-bar .btn-undo,
        .app-bar .btn-redo {
          width: 32px;
          height: 32px;
          padding: 0;
          display: inline-flex;
          align-items: center;
          justify-content: center;
          color: var(--oq-text-secondary);
          background: transparent;
          border: none;
          border-radius: 4px;
          transition: all var(--oq-transition);
        }
        .app-bar .btn-undo:hover,
        .app-bar .btn-redo:hover {
          background: var(--oq-surface-hover);
          color: var(--oq-text);
        }
        .app-bar-hamburger {
          color: var(--oq-text);
          background: transparent;
          border: none;
          font-size: 18px;
          padding: 4px 8px;
          margin-right: 8px;
        }
        .app-bar-hamburger:hover,
        .app-bar-hamburger:focus {
          color: var(--oq-text);
          background: var(--oq-surface-hover);
        }
        .app-page-link.active {
          font-weight: 600;
          background-color: #e9ecef;
        }
        .dropdown-menu .dropdown-item.app-page-link {
          display: flex;
          align-items: center;
          gap: 0.5em;
        }
        .dropdown-menu .dropdown-item.app-page-link > i,
        .dropdown-menu .dropdown-item.app-page-link > svg {
          width: 1.25em;
          text-align: center;
          flex-shrink: 0;
          display: inline-flex;
          justify-content: center;
        }
        .app-page { display: none; }
        .app-page.active {
          display: flex;
          flex-direction: column;
          flex: 1 1 auto;
          min-height: 0;
        }
        .results-page {
          min-height: 500px;
          height: calc(100vh - var(--oq-topbar-h, 52px));
          height: calc(100dvh - var(--oq-topbar-h, 52px));
        }
        .results-page .bslib-sidebar-layout {
          flex: 1 1 auto;
          min-height: 0;
          height: 100%;
          border: none;
          gap: 12px;
          padding: 12px;
          --_padding: 0px;
          --_padding-icon: 0px;
          --_vert-border: none;
          --_sidebar-bg: transparent;
          --_main-bg: transparent;
        }
        .results-page .bslib-sidebar-layout > .collapse-toggle {
          display: none !important;
        }
        .results-page .bslib-sidebar-layout > .sidebar,
        .results-page .bslib-sidebar-layout > .main {
          display: flex;
          flex-direction: column;
          min-height: 0;
          border: 1px solid var(--oq-border);
          border-radius: var(--oq-radius-md, 8px);
          box-shadow: var(--oq-shadow-xs);
          overflow: hidden;
          background: var(--oq-surface);
        }
        .results-page .bslib-sidebar-layout > .main {
          padding: 0;
        }
        .results-page .bslib-sidebar-layout > .main > .card,
        .results-page .bslib-sidebar-layout > .main > .shiny-panel-conditional,
        .results-page .bslib-sidebar-layout > .main > .shiny-panel-conditional > .card {
          flex: 1 1 auto;
          min-height: 0;
        }
        .results-page .bslib-sidebar-layout > .main > .shiny-panel-conditional {
          display: flex;
          flex-direction: column;
        }
        .results-page .bslib-sidebar-layout > .main > .card,
        .results-page .bslib-sidebar-layout > .main > .shiny-panel-conditional > .card,
        .results-page .bslib-sidebar-layout > .main > .card > .tab-content,
        .results-page .bslib-sidebar-layout > .main > .shiny-panel-conditional > .card > .tab-content,
        .results-page .bslib-sidebar-layout > .main > .card > .tab-content > .tab-pane,
        .results-page .bslib-sidebar-layout > .main > .shiny-panel-conditional > .card > .tab-content > .tab-pane,
        .results-page .bslib-sidebar-layout > .main > .card > .tab-content > .tab-pane > .card-body,
        .results-page .bslib-sidebar-layout > .main > .shiny-panel-conditional > .card > .tab-content > .tab-pane > .card-body {
          height: 100%;
          min-height: 0;
        }
        .results-page .bslib-sidebar-layout > .main > .card > .tab-content > .tab-pane > .card-body,
        .results-page .bslib-sidebar-layout > .main > .shiny-panel-conditional > .card > .tab-content > .tab-pane > .card-body {
          display: flex;
          flex-direction: column;
        }
        .results-page .results-content-shell {
          display: flex;
          flex: 1 1 auto;
          flex-direction: column;
          min-height: 0;
          height: 100%;
        }
        .results-page .results-content-shell > .shiny-panel-conditional {
          display: flex;
          flex: 1 1 auto;
          flex-direction: column;
          min-height: 0;
        }
        .results-page .results-content-shell > .results-content-output {
          display: flex;
          flex: 1 1 auto;
          flex-direction: column;
          min-height: 0;
        }
        .results-page .results-content-shell > .results-plot-region,
        .results-page .results-content-shell > .results-content-output > .results-plot-region,
        .results-page .results-content-shell > .shiny-panel-conditional > .results-plot-region {
          display: flex;
          flex: 1 1 auto;
          flex-direction: column;
          min-height: 0;
          height: 100%;
        }
        .results-page .results-content-shell .results-plot-region > .shiny-plot-output {
          flex: 1 1 auto;
          min-height: 0;
        }
        .results-analysis-sidebar {
          overflow: hidden !important;
          background: var(--oq-surface);
        }
        .results-analysis-sidebar > .sidebar-content {
          display: flex;
          flex-direction: column;
          height: 100%;
          min-height: 0;
          overflow: hidden;
          padding: 0;
          gap: 0;
        }
        .results-sidebar-shell {
          display: flex;
          flex-direction: column;
          gap: 0;
          flex: 1 1 auto;
          min-height: 0;
        }
        .results-sidebar-tabs,
        .results-sidebar-tabs > .card,
        .results-sidebar-tabs > .card > .tab-content {
          display: flex;
          flex: 1 1 auto;
          flex-direction: column;
          min-height: 0;
        }
        .results-sidebar-tabs > .card {
          margin-bottom: 0;
          overflow: visible;
          border: none;
          border-radius: 0;
          box-shadow: none;
        }
        .results-sidebar-tabs > .card > .card-header {
          background: transparent;
          border: none;
          border-bottom: 1px solid var(--oq-border);
          padding: 0 16px;
          min-height: 40px;
        }
        .results-sidebar-tabs > .card > .card-header .nav {
          display: flex;
          align-items: stretch;
          margin-bottom: -1px;
        }
        .results-sidebar-tabs > .card > .card-header .nav-link {
          display: flex;
          align-items: center;
          padding: 10px 14px;
          font-size: 0.8125rem;
          font-weight: 500;
          color: #6b7280;
          border: none;
          background: transparent;
          position: relative;
          transition: color var(--oq-transition);
        }
        .results-sidebar-tabs > .card > .card-header .nav-link:hover {
          color: var(--oq-text);
        }
        .results-sidebar-tabs > .card > .card-header .nav-link.active {
          color: var(--oq-primary);
          background: transparent;
          border: none;
        }
        .results-sidebar-tabs > .card > .card-header .nav-link.active::after {
          content: '';
          position: absolute;
          bottom: -1px;
          left: 8px;
          right: 8px;
          height: 2px;
          background: var(--oq-primary);
        }
        .results-sidebar-tabs > .card > .tab-content {
          overflow: hidden;
          position: relative;
        }
        .results-sidebar-tabs > .card > .tab-content > .tab-pane {
          display: flex !important;
          flex: 1 1 auto;
          flex-direction: column;
          min-height: 0;
        }
        .results-sidebar-tabs > .card > .tab-content > .tab-pane:not(.active) {
          position: absolute;
          width: 0;
          height: 0;
          overflow: hidden;
          opacity: 0;
          pointer-events: none;
        }
        .results-sidebar-tabs > .card > .tab-content > .tab-pane.active > .card-body {
          display: flex;
          flex: 1 1 auto;
          flex-direction: column;
          min-height: 0;
          padding: 0;
        }
        .results-sidebar-panel-scroll {
          display: flex;
          flex: 1 1 auto;
          flex-direction: column;
          min-height: 0;
          overflow-y: auto;
          overflow-x: visible;
          padding: 12px 16px;
        }
        .results-sidebar-controls {
          display: flex;
          flex-direction: column;
          gap: 12px;
        }
        .results-sidebar-control > .shiny-input-container,
        .results-sidebar-control .form-group {
          width: 100%;
          margin-bottom: 0;
        }
        .results-sidebar-footer {
          flex: 0 0 auto;
          background: var(--oq-surface);
          border-top: 1px solid var(--oq-border);
          padding: 12px 16px;
        }
        .results-sidebar-footer .shiny-input-container,
        .results-sidebar-footer .form-group {
          margin-bottom: 10px;
          font-size: 0.8125rem;
          color: var(--oq-text-secondary);
        }
        .results-sidebar-footer input[type='checkbox'] {
          accent-color: var(--oq-primary);
        }
        .results-sidebar-footer .shiny-input-container:last-child,
        .results-sidebar-footer .form-group:last-child {
          margin-bottom: 0;
        }
        .results-sidebar-footer .btn {
          width: 100%;
          display: flex;
          align-items: center;
          justify-content: center;
          gap: 6px;
          padding: 9px 16px;
          font-size: 0.8125rem;
          font-weight: 600;
          background: var(--oq-primary);
          border: none;
          border-radius: 6px;
          color: var(--oq-text-inverse);
          transition: background 0.15s ease;
        }
        .results-sidebar-footer .btn:hover {
          background: var(--oq-primary-hover);
        }
        .results-footer-grid {
          display: grid;
          grid-template-columns: repeat(2, minmax(0, 1fr));
          gap: 12px;
        }
        .results-footer-grid .shiny-input-container,
        .results-footer-grid .form-group {
          margin-bottom: 0;
        }
        /* -- Empty state placeholders -- */
        .empty-state-placeholder {
          display: flex;
          flex-direction: column;
          align-items: center;
          justify-content: center;
          padding: var(--oq-sp-8) var(--oq-sp-4);
          min-height: 120px;
          text-align: center;
          gap: var(--oq-sp-2);
        }
        .empty-state-icon {
          font-size: 1.75rem;
          color: var(--oq-text-tertiary);
          opacity: 0.6;
        }
        .empty-state-title {
          font-size: var(--oq-text-sm);
          color: var(--oq-text-secondary);
        }
        .empty-state-subtitle {
          font-size: var(--oq-text-xs);
          color: var(--oq-text-tertiary);
        }
        /* -- Underline tab bar overrides (issue 08) -- */
        .nav-underline {
          border-bottom: 1px solid var(--oq-border);
          overflow-x: auto;
          scrollbar-width: none;
          flex-shrink: 0;
        }
        .nav-underline::-webkit-scrollbar {
          display: none;
        }
        .nav-underline .nav-item {
          display: flex;
          align-items: stretch;
          margin-bottom: -1px;
        }
        .nav-underline .nav-link {
          display: flex;
          align-items: center;
          padding: 10px 16px;
          font-size: 0.8125rem;
          font-weight: 500;
          color: #6b7280;
          border: none;
          background: transparent;
          position: relative;
          transition: color var(--oq-transition);
        }
        .nav-underline .nav-link:hover {
          color: var(--oq-text);
        }
        .nav-underline .nav-link.active {
          color: var(--oq-primary);
          background: transparent;
          border: none;
        }
        .nav-underline .nav-link.active::after {
          content: '';
          position: absolute;
          bottom: -1px;
          left: 8px;
          right: 8px;
          height: 2px;
          background: var(--oq-primary);
        }
        /* -- Handsontable styling (issue 20) -- */
        .tables-editor .handsontable th {
          background: #f0f2f5;
          font-size: 0.75rem;
          font-weight: 600;
          color: #6b7280;
          text-align: center;
        }
        .tables-editor .handsontable td {
          font-size: 0.8125rem;
          padding: 5px 10px;
        }
        .tables-editor .handsontable td,
        .tables-editor .handsontable th {
          border-color: #edf0f3;
        }
        .tables-editor .handsontable tbody tr:first-child td {
          font-weight: 700;
        }
        /* -- Results page tab bar overrides (issue 40) -- */
        .results-page .bslib-sidebar-layout > .main > .card,
        .results-page .bslib-sidebar-layout > .main > .shiny-panel-conditional > .card {
          border: none;
          border-radius: 0;
          box-shadow: none;
          overflow: hidden;
          flex: 1 1 auto;
        }
        .results-page .card > .card-header {
          border: none;
          border-bottom: 1px solid var(--oq-border);
          border-radius: 0;
          padding: 0 16px;
          background: var(--oq-surface);
        }
        .results-page .card > .card-header .nav {
          display: flex;
          align-items: stretch;
          margin-bottom: -1px;
        }
        .results-page .card > .card-header .nav-link {
          display: flex;
          align-items: center;
          padding: 10px 14px;
          font-size: 0.8125rem;
          font-weight: 500;
          color: #6b7280;
          border: none;
          background: transparent;
          position: relative;
          transition: color var(--oq-transition);
        }
        .results-page .card > .card-header .nav-link:hover {
          color: var(--oq-text);
        }
        .results-page .card > .card-header .nav-link.active {
          color: var(--oq-primary);
          background: transparent;
          border: none;
        }
        .results-page .card > .card-header .nav-link.active::after {
          content: '';
          position: absolute;
          bottom: -1px;
          left: 8px;
          right: 8px;
          height: 2px;
          background: var(--oq-primary);
        }
        .results-page .card > .tab-content {
          border: none;
        }
        .selectize-dropdown,
        .selectize-dropdown.form-control,
        .ace_autocomplete,
        .ace_tooltip {
          z-index: 11000 !important;
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
          if (target) {
            $(target).find('.shiny-html-output').trigger('shown');
          }
        }

        Shiny.addCustomMessageHandler('editor_progress', function(data) {
          var snackbar = document.getElementById('editor_progress_snackbar');
          var bar = document.getElementById('editor_progress_bar');
          var pct = document.getElementById('editor_progress_pct');
          var msg = document.getElementById('editor_no_results_msg');
          if (data.state === 'running') {
            if (msg) msg.style.display = 'none';
            if (bar) bar.style.width = data.pct + '%';
            if (pct) pct.textContent = Math.round(data.pct) + '%';
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
            if (pct) pct.textContent = Math.round(data.pct) + '%';
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
            if (pct) pct.textContent = Math.round(data.pct) + '%';
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
            if (pct) pct.textContent = Math.round(data.pct) + '%';
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
            if (pct) pct.textContent = Math.round(data.pct) + '%';
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
            if (pct) pct.textContent = Math.round(data.pct) + '%';
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
            if (pct) pct.textContent = Math.round(data.pct) + '%';
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
        rlang::inject(tags$ul(
          class = "dropdown-menu",
          tags$li(
            tags$button(
              class = "dropdown-item app-page-link active",
              `data-page` = "documentation",
              onclick = "switchAppPage('documentation')",
              shiny::icon("file-lines"), "Documentation"
            )
          ),
          tags$li(
            tags$button(
              class = "dropdown-item app-page-link",
              `data-page` = "editor",
              onclick = "switchAppPage('editor')",
              shiny::icon("sliders"), "Model Inputs"
            )
          ),
          tags$li(
            tags$button(
              class = "dropdown-item app-page-link",
              `data-page` = "results",
              onclick = "switchAppPage('results')",
              shiny::icon("play"), "Base Case"
            )
          ),
          tags$li(
            tags$button(
              class = "dropdown-item app-page-link",
              `data-page` = "vbp",
              onclick = "switchAppPage('vbp')",
              shiny::icon("dollar-sign"), "VBP"
            )
          ),
          tags$li(
            tags$button(
              class = "dropdown-item app-page-link",
              `data-page` = "dsa",
              onclick = "switchAppPage('dsa')",
              shiny::icon("chart-bar"), "DSA"
            )
          ),
          tags$li(
            tags$button(
              class = "dropdown-item app-page-link",
              `data-page` = "scenario",
              onclick = "switchAppPage('scenario')",
              shiny::icon("layer-group"), "Scenario Analysis"
            )
          ),
          tags$li(
            tags$button(
              class = "dropdown-item app-page-link",
              `data-page` = "twsa",
              onclick = "switchAppPage('twsa')",
              shiny::icon("grip"), "TWSA"
            )
          ),
          tags$li(
            tags$button(
              class = "dropdown-item app-page-link",
              `data-page` = "threshold",
              onclick = "switchAppPage('threshold')",
              shiny::icon("bullseye"), "Threshold"
            )
          ),
          tags$li(
            tags$button(
              class = "dropdown-item app-page-link",
              `data-page` = "psa",
              onclick = "switchAppPage('psa')",
              shiny::icon("chart-area"), "PSA"
            )
          ),
          !!!unname(lapply(.get_editor_extensions(), function(ext) {
            tags$li(tags$button(
              class = "dropdown-item app-page-link",
              `data-page` = ext$tab_id,
              onclick = sprintf("switchAppPage('%s')", ext$tab_id),
              ext$label
            ))
          }))
        ))
      ),
      tags$div(class = "app-bar-logo", "Q"),
      tags$span(class = "app-bar-title", shiny::textOutput("editor_title", inline = TRUE)),
      tags$div(class = "app-bar-sep"),
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
            tags$button(
              class = "dropdown-item", type = "button",
              onclick = "Shiny.setInputValue('new_model', Math.random(), {priority: 'event'});",
              shiny::icon("file-circle-plus"), "New")
          ),
          tags$li(
            shinyFiles::shinyFilesButton(
              "open_file",
              tagList(shiny::icon("folder-open"), "Open"),
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
              shiny::icon("floppy-disk"), "Save")
          ),
          tags$li(
            shinyFiles::shinySaveButton("save_as_file",
              tagList(shiny::icon("file-export"), "Save As..."),
              "Save model as...",
              filetype = list(JSON = "json", YAML = c("yaml", "yml")),
              class = "dropdown-item",
              style = "background: none; border: none; width: 100%; text-align: left; color: inherit;")
          ),
          tags$li(tags$hr(class = "dropdown-divider")),
          tags$li(
            tags$button(
              class = "dropdown-item", type = "button",
              onclick = "Shiny.setInputValue('version_history', Math.random(), {priority: 'event'});",
              shiny::icon("clock-rotate-left"), "Version History...")
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
              shiny::icon("code-compare"), "Model Diff"
            )
          )
        )
      ),
      tags$div(
        style = "margin-left: auto; display: flex; gap: 4px;",
        shiny::actionButton("undo_btn", NULL,
          icon = shiny::icon("rotate-left"),
          class = "btn btn-sm btn-undo",
          title = "Undo (Ctrl+Z)",
          disabled = "disabled"
        ),
        shiny::actionButton("redo_btn", NULL,
          icon = shiny::icon("rotate-right"),
          class = "btn btn-sm btn-redo",
          title = "Redo (Ctrl+Shift+Z)",
          disabled = "disabled"
        )
      )
    ),
    shiny::conditionalPanel(
      condition = "!output.model_loaded",
      tags$div(class = "no-model-message", "No model loaded")
    ),
    rlang::inject(shiny::conditionalPanel(
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
          id = "model_inputs_tabs",
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
      # Results page
      tags$div(
        id = "page_results",
        class = "app-page results-page",
        bslib::layout_sidebar(
          sidebar = analysis_sidebar(
            sidebar_id = "results_sidebar",
            visualization_content = shiny::tagList(
              shiny::conditionalPanel(
                condition = "output.has_editor_results && input.results_tabs == 'trace'",
                traceResultsSidebarUI("editor_trace")
              ),
              shiny::conditionalPanel(
                condition = "output.has_editor_results && input.results_tabs == 'outcomes'",
                outcomesResultsSidebarUI("editor_outcomes")
              ),
              shiny::conditionalPanel(
                condition = "output.has_editor_results && input.results_tabs == 'costs'",
                costsResultsSidebarUI("editor_costs")
              ),
              shiny::conditionalPanel(
                condition = "output.has_editor_results && input.results_tabs == 'nmb'",
                nmbResultsSidebarUI("editor_nmb")
              ),
              shiny::conditionalPanel(
                condition = "output.has_editor_results && input.results_tabs == 'pairwise_ce'",
                pairwiseCeResultsSidebarUI("editor_pairwise_ce")
              ),
              shiny::conditionalPanel(
                condition = "output.has_editor_results && input.results_tabs == 'incremental_ce'",
                incrementalCeResultsSidebarUI("editor_incremental_ce")
              ),
              shiny::conditionalPanel(
                condition = "output.has_editor_results && input.results_tabs == 'variables'",
                variableDiagnosticsSidebarUI("editor_variable_diagnostics")
              ),
              shiny::conditionalPanel(
                condition = "output.has_editor_results && input.results_tabs == 'decision_trees'",
                decisionTreeResultsSidebarUI("editor_decision_trees")
              ),
              shiny::conditionalPanel(
                condition = "output.has_editor_results && input.results_tabs == 'transitions'",
                transitionHeatmapSidebarUI("editor_transitions")
              ),
              shiny::conditionalPanel(
                condition = "!output.has_editor_results",
                empty_state(
                  icon = "\u2699",
                  title = "No visualization controls",
                  subtitle = "Run the model to configure visualizations"
                )
              )
            ),
            overrides_output_id = "results_sidebar_overrides",
            footer_content = shiny::tagList(
              shiny::checkboxInput(
                "editor_results_auto_run",
                "Run automatically on changes",
                value = TRUE
              ),
              shiny::actionButton(
                "editor_run_model",
                "Run Model",
                class = "btn-primary"
              )
            )
          ),
          shiny::conditionalPanel(
            condition = "!output.has_editor_results",
            tags$div(
              id = "editor_no_results_msg",
              style = "display:flex; align-items:center; justify-content:center; height:100%;",
              empty_state(
                icon = "\u25B6",
                title = "Run the model to see results",
                subtitle = "Results will appear here once the model has been executed"
              )
            )
          ),
          shiny::conditionalPanel(
            condition = "output.has_editor_results",
            bslib::navset_card_tab(
              id = "results_tabs",
              selected = "trace",
              bslib::nav_panel("Trace", value = "trace", traceResultsUI("editor_trace")),
              bslib::nav_panel("Outcomes", value = "outcomes", outcomesResultsUI("editor_outcomes")),
              bslib::nav_panel("Costs", value = "costs", costsResultsUI("editor_costs")),
              bslib::nav_panel("NMB", value = "nmb", nmbResultsUI("editor_nmb")),
              bslib::nav_panel("Pairwise CE", value = "pairwise_ce", pairwiseCeResultsUI("editor_pairwise_ce")),
              bslib::nav_panel("Incremental CE", value = "incremental_ce", incrementalCeResultsUI("editor_incremental_ce")),
              bslib::nav_panel("Variables", value = "variables",
                variableDiagnosticsUI("editor_variable_diagnostics")),
              bslib::nav_panel("Decision Trees", value = "decision_trees",
                decisionTreeResultsUI("editor_decision_trees")),
              bslib::nav_panel("Transitions", value = "transitions",
                transitionHeatmapUI("editor_transitions"))
            )
          )
        )
      ),
      # DSA page
      tags$div(
        id = "page_dsa",
        class = "app-page results-page",
        bslib::layout_sidebar(
          sidebar = analysis_sidebar(
            sidebar_id = "dsa_sidebar",
            visualization_content = shiny::tagList(
              tab_value_panel("dsa_tabs", "outcomes", dsaResultTabSidebarUI("editor_dsa_outcomes")),
              tab_value_panel("dsa_tabs", "costs", dsaResultTabSidebarUI("editor_dsa_costs")),
              tab_value_panel("dsa_tabs", "nmb", dsaResultTabSidebarUI("editor_dsa_nmb")),
              tab_value_panel("dsa_tabs", "ce", dsaResultTabSidebarUI("editor_dsa_ce")),
              tab_value_panel("dsa_tabs", "vbp", dsaResultTabSidebarUI("editor_dsa_vbp")),
              tab_value_panel("dsa_tabs", "inputs",
                empty_state(
                  icon = "\u2699",
                  title = "No visualization controls",
                  subtitle = "Select a result tab to configure visualizations"
                )
              )
            ),
            overrides_output_id = "dsa_sidebar_overrides",
            footer_content = shiny::actionButton(
              "editor_run_dsa",
              "Run DSA Analysis",
              class = "btn-primary"
            )
          ),
          bslib::navset_card_tab(
            id = "dsa_tabs",
            selected = "inputs",
            bslib::nav_panel("Inputs", value = "inputs",
              shiny::uiOutput("dsa_inputs_panel")
            ),
            bslib::nav_panel("Outcomes", value = "outcomes", dsaResultTabUI("editor_dsa_outcomes")),
            bslib::nav_panel("Costs", value = "costs", dsaResultTabUI("editor_dsa_costs")),
            bslib::nav_panel("NMB", value = "nmb", dsaResultTabUI("editor_dsa_nmb")),
            bslib::nav_panel("CE", value = "ce", dsaResultTabUI("editor_dsa_ce")),
            bslib::nav_panel("VBP", value = "vbp", dsaResultTabUI("editor_dsa_vbp"))
          )
        )
      ),
      # VBP page
      tags$div(
        id = "page_vbp",
        class = "app-page results-page",
        bslib::layout_sidebar(
          sidebar = analysis_sidebar(
            sidebar_id = "vbp_sidebar",
            visualization_content = shiny::tagList(
              tab_value_panel("vbp_tabs", "results", vbpResultsSidebarUI("editor_vbp")),
              tab_value_panel("vbp_tabs", "inputs",
                empty_state(
                  icon = "\u2699",
                  title = "No visualization controls",
                  subtitle = "Select a result tab to configure visualizations"
                )
              )
            ),
            overrides_output_id = "vbp_sidebar_overrides",
            footer_content = shiny::actionButton(
              "editor_run_vbp",
              "Run VBP Analysis",
              class = "btn-primary"
            )
          ),
          bslib::navset_card_tab(
            id = "vbp_tabs",
            selected = "inputs",
            bslib::nav_panel("Inputs", value = "inputs",
              shiny::uiOutput("vbp_inputs_panel")
            ),
            bslib::nav_panel("Results", value = "results", vbpResultsUI("editor_vbp"))
          )
        )
      ),
      # Scenario page
      tags$div(
        id = "page_scenario",
        class = "app-page results-page",
        bslib::layout_sidebar(
          sidebar = analysis_sidebar(
            sidebar_id = "scenario_sidebar",
            visualization_content = shiny::tagList(
              tab_value_panel("scenario_tabs", "outcomes", scenarioResultTabSidebarUI("editor_scenario_outcomes")),
              tab_value_panel("scenario_tabs", "costs", scenarioResultTabSidebarUI("editor_scenario_costs")),
              tab_value_panel("scenario_tabs", "nmb", scenarioResultTabSidebarUI("editor_scenario_nmb")),
              tab_value_panel("scenario_tabs", "ce", scenarioResultTabSidebarUI("editor_scenario_ce")),
              tab_value_panel("scenario_tabs", "vbp", scenarioResultTabSidebarUI("editor_scenario_vbp")),
              tab_value_panel("scenario_tabs", "inputs",
                empty_state(
                  icon = "\u2699",
                  title = "No visualization controls",
                  subtitle = "Select a result tab to configure visualizations"
                )
              )
            ),
            overrides_output_id = "scenario_sidebar_overrides",
            footer_content = shiny::actionButton(
              "editor_run_scenario",
              "Run Scenario Analysis",
              class = "btn-primary"
            )
          ),
          bslib::navset_card_tab(
            id = "scenario_tabs",
            selected = "inputs",
            bslib::nav_panel("Inputs", value = "inputs",
              shiny::uiOutput("scenario_inputs_panel")
            ),
            bslib::nav_panel("Outcomes", value = "outcomes", scenarioResultTabUI("editor_scenario_outcomes")),
            bslib::nav_panel("Costs", value = "costs", scenarioResultTabUI("editor_scenario_costs")),
            bslib::nav_panel("NMB", value = "nmb", scenarioResultTabUI("editor_scenario_nmb")),
            bslib::nav_panel("CE", value = "ce", scenarioResultTabUI("editor_scenario_ce")),
            bslib::nav_panel("VBP", value = "vbp", scenarioResultTabUI("editor_scenario_vbp"))
          )
        )
      ),
      # TWSA page
      tags$div(
        id = "page_twsa",
        class = "app-page results-page",
        bslib::layout_sidebar(
          sidebar = analysis_sidebar(
            sidebar_id = "twsa_sidebar",
            visualization_content = shiny::tagList(
              tab_value_panel("twsa_tabs", "outcomes", twsaResultTabSidebarUI("editor_twsa_outcomes")),
              tab_value_panel("twsa_tabs", "costs", twsaResultTabSidebarUI("editor_twsa_costs")),
              tab_value_panel("twsa_tabs", "nmb", twsaResultTabSidebarUI("editor_twsa_nmb")),
              tab_value_panel("twsa_tabs", "ce", twsaResultTabSidebarUI("editor_twsa_ce")),
              tab_value_panel("twsa_tabs", "vbp", twsaResultTabSidebarUI("editor_twsa_vbp")),
              tab_value_panel("twsa_tabs", "inputs",
                empty_state(
                  icon = "\u2699",
                  title = "No visualization controls",
                  subtitle = "Select a result tab to configure visualizations"
                )
              )
            ),
            overrides_output_id = "twsa_sidebar_overrides",
            footer_content = shiny::actionButton(
              "editor_run_twsa",
              "Run TWSA",
              class = "btn-primary"
            )
          ),
          bslib::navset_card_tab(
            id = "twsa_tabs",
            selected = "inputs",
            bslib::nav_panel("Inputs", value = "inputs",
              shiny::uiOutput("twsa_inputs_panel")
            ),
            bslib::nav_panel("Outcomes", value = "outcomes", twsaResultTabUI("editor_twsa_outcomes")),
            bslib::nav_panel("Costs", value = "costs", twsaResultTabUI("editor_twsa_costs")),
            bslib::nav_panel("NMB", value = "nmb", twsaResultTabUI("editor_twsa_nmb")),
            bslib::nav_panel("CE", value = "ce", twsaResultTabUI("editor_twsa_ce")),
            bslib::nav_panel("VBP", value = "vbp", twsaResultTabUI("editor_twsa_vbp"))
          )
        )
      ),
      # Threshold page
      tags$div(
        id = "page_threshold",
        class = "app-page results-page",
        bslib::layout_sidebar(
          sidebar = analysis_sidebar(
            sidebar_id = "threshold_sidebar",
            visualization_content = shiny::tagList(
              tab_value_panel("threshold_tabs", "detail", thresholdResultTabSidebarUI("editor_threshold_detail")),
              tab_value_panel("threshold_tabs", "convergence", thresholdResultTabSidebarUI("editor_threshold_convergence")),
              tab_value_panel("threshold_tabs", "inputs",
                empty_state(
                  icon = "\u2699",
                  title = "No visualization controls",
                  subtitle = "Select a result tab to configure visualizations"
                )
              ),
              tab_value_panel("threshold_tabs", "summary",
                empty_state(
                  icon = "\u2699",
                  title = "No visualization controls",
                  subtitle = "Select a result tab to configure visualizations"
                )
              )
            ),
            overrides_output_id = "threshold_sidebar_overrides",
            footer_content = shiny::actionButton(
              "editor_run_threshold",
              "Run Threshold Analysis",
              class = "btn-primary"
            )
          ),
          bslib::navset_card_tab(
            id = "threshold_tabs",
            selected = "inputs",
            bslib::nav_panel("Inputs", value = "inputs",
              shiny::uiOutput("threshold_inputs_panel")
            ),
            bslib::nav_panel("Summary", value = "summary", thresholdSummaryUI("editor_threshold_summary")),
            bslib::nav_panel("Detail", value = "detail", thresholdResultTabUI("editor_threshold_detail")),
            bslib::nav_panel("Convergence", value = "convergence", thresholdResultTabUI("editor_threshold_convergence"))
          )
        )
      ),
      # PSA page
      tags$div(
        id = "page_psa",
        class = "app-page results-page",
        bslib::layout_sidebar(
          sidebar = analysis_sidebar(
            sidebar_id = "psa_sidebar",
            visualization_content = shiny::tagList(
              tab_value_panel("psa_tabs", "outcomes", psaResultTabSidebarUI("editor_psa_outcomes")),
              tab_value_panel("psa_tabs", "costs", psaResultTabSidebarUI("editor_psa_costs")),
              tab_value_panel("psa_tabs", "nmb", psaResultTabSidebarUI("editor_psa_nmb")),
              tab_value_panel("psa_tabs", "incremental_ce", psaResultTabSidebarUI("editor_psa_incremental_ce")),
              tab_value_panel("psa_tabs", "pairwise_ce", psaResultTabSidebarUI("editor_psa_pairwise_ce")),
              tab_value_panel("psa_tabs", "evpi", psaResultTabSidebarUI("editor_psa_evpi")),
              tab_value_panel("psa_tabs", "parameters", psaResultTabSidebarUI("editor_psa_parameters")),
              tab_value_panel("psa_tabs", "univariate_sampling",
                empty_state(
                  icon = "\u2699",
                  title = "No visualization controls",
                  subtitle = "Select a result tab to configure visualizations"
                )
              ),
              tab_value_panel("psa_tabs", "multivariate_sampling",
                empty_state(
                  icon = "\u2699",
                  title = "No visualization controls",
                  subtitle = "Select a result tab to configure visualizations"
                )
              )
            ),
            overrides_output_id = "psa_sidebar_overrides",
            footer_content = shiny::uiOutput("psa_footer_controls")
          ),
          bslib::navset_card_tab(
            id = "psa_tabs",
            selected = "univariate_sampling",
            bslib::nav_panel("Univariate Sampling", value = "univariate_sampling",
              shiny::uiOutput("psa_univariate_panel")
            ),
            bslib::nav_panel("Multivariate Sampling", value = "multivariate_sampling",
              shiny::uiOutput("psa_multivariate_panel")
            ),
            bslib::nav_panel("Outcomes", value = "outcomes", psaResultTabUI("editor_psa_outcomes")),
            bslib::nav_panel("Costs", value = "costs", psaResultTabUI("editor_psa_costs")),
            bslib::nav_panel("NMB", value = "nmb", psaResultTabUI("editor_psa_nmb")),
            bslib::nav_panel("Incremental CE", value = "incremental_ce", psaResultTabUI("editor_psa_incremental_ce")),
            bslib::nav_panel("Pairwise CE", value = "pairwise_ce", psaResultTabUI("editor_psa_pairwise_ce")),
            bslib::nav_panel("EVPI", value = "evpi", psaResultTabUI("editor_psa_evpi")),
            bslib::nav_panel("Parameters", value = "parameters", psaResultTabUI("editor_psa_parameters"))
          )
        )
      ),
      !!!unname(lapply(.get_editor_extensions(), function(ext) {
        tags$div(
          id = paste0("page_", ext$tab_id),
          class = "app-page",
          ext$ui_fn(ext$tab_id)
        )
      }))
    )),
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
    pending_new_model <- shiny::reactiveVal(FALSE)

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
        openqaly::read_model_json(file = fpath)
      } else if (ext %in% c("yaml", "yml")) {
        openqaly::read_model_yaml(file = fpath)
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
        openqaly::read_model_json(text = content)
      } else {
        openqaly::read_model_yaml(text = content)
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
          .version-list { min-width: 220px; max-width: 260px; overflow-y: auto;
            border-right: 1px solid var(--oq-border); padding-right: 12px; }
          .version-diff-pane { flex: 1; min-width: 0; min-height: 0; overflow-y: auto;
            display: flex; flex-direction: column; }
          .version-diff-pane select { font-family: var(--oq-font); font-size: var(--oq-text-sm);
            border: 1px solid var(--oq-border); border-radius: var(--oq-radius-sm); padding: 4px 8px; }
          .version-diff-pane .diff-container { border: 1px solid var(--oq-border);
            border-radius: var(--oq-radius); font-family: var(--oq-font-mono);
            font-size: var(--oq-text-xs); }
          .version-item { padding: 8px 12px; cursor: pointer; border-radius: 4px; margin-bottom: 4px; }
          .version-item:hover { background: var(--oq-surface-hover); }
          .version-item-active { background: var(--oq-primary-light); border-left: 3px solid var(--oq-primary); }
          .version-timestamp { font-weight: 500; font-size: var(--oq-text-sm); color: var(--oq-text); }
          .version-relative { color: var(--oq-text-tertiary); font-size: var(--oq-text-xs); }
          .version-message { font-size: var(--oq-text-xs); color: var(--oq-text-secondary); margin-top: 2px;
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

      if (identical(action$type, "replace_model")) {
        new <- action$model
        if (!inherits(new, "oq_model")) {
          stop("Replacement model must be an oq_model object.", call. = FALSE)
        }
        if (identical(old, new)) {
          return(list(status = "noop", model = new))
        }
        history$push(old)
        model(new)
        file_load_counter(file_load_counter() + 1L)
        table_render_trigger(table_render_trigger() + 1L)
        return(list(status = "ok", model = new))
      }

      new <- dispatch_model_action(old, action)
      if (identical(old, new)) {
        return(list(status = "noop", model = new))
      }
      history$push(old)
      model(new)
      list(status = "ok", model = new)
    }

    for (ext in .get_editor_extensions()) {
      tryCatch(
        ext$server_fn(
          model = model,
          apply_action = apply_action,
          input = input,
          output = output,
          session = session
        ),
        error = function(e) {
          shiny::showNotification(
            paste0("Extension '", ext$tab_id, "' failed to initialize: ", conditionMessage(e)),
            type = "error"
          )
        }
      )
    }

    apply_batch_edit <- function(action) {
      edits <- action$edits
      # Guard against jsonlite simplifying an array of objects into a data.frame
      if (is.data.frame(edits)) {
        edits <- lapply(seq_len(nrow(edits)), function(i) as.list(edits[i, , drop = FALSE]))
      }
      if (length(edits) == 0) return(list(status = "noop", model = model()))

      old <- model()
      m <- old
      for (edit in edits) {
        m <- dispatch_model_action(m, edit)
      }
      if (identical(old, m)) {
        return(list(status = "noop", model = m))
      }
      history$push(old)
      model(m)
      list(status = "ok", model = m)
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
      m <- model()
      fpath <- current_file_path()
      dirty <- is_dirty()
      if (is.null(m)) return("Model Editor")
      fname <- if (is.null(fpath)) "Untitled" else basename(fpath)
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
        session$sendCustomMessage("trigger_save_as", list())
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

    # --- New Model ---

    show_new_model_type_modal <- function() {
      shiny::showModal(shiny::modalDialog(
        title = "New Model",
        shiny::radioButtons("new_model_type", "Model Type:",
          choices = c(
            "Markov" = "markov",
            "PSM" = "psm",
            "Custom PSM" = "custom_psm",
            "Decision Tree" = "decision_tree"
          ),
          selected = "markov"
        ),
        shiny::helpText(
          shiny::uiOutput("new_model_type_description")
        ),
        easyClose = TRUE,
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          tags$button(class = "btn btn-primary", type = "button",
            onclick = "Shiny.setInputValue('confirm_new_model', Math.random(), {priority: 'event'});",
            "Create")
        )
      ))
    }

    output$new_model_type_description <- shiny::renderUI({
      type <- input$new_model_type
      desc <- switch(type,
        markov = "Cohort simulation with discrete health states and transition probabilities.",
        psm = "Partitioned survival model using parametric survival curves.",
        custom_psm = "Partitioned survival with custom state probability formulas.",
        decision_tree = "One-time decision analysis with branching outcomes.",
        ""
      )
      tags$span(style = "font-style: italic;", desc)
    })

    shiny::observeEvent(input$new_model, {
      m <- model()
      if (is.null(m)) {
        show_new_model_type_modal()
        return()
      }
      if (!is_dirty()) {
        show_new_model_type_modal()
        return()
      }
      pending_new_model(TRUE)
      has_save_path <- !is.null(current_file_path())
      footer_buttons <- if (has_save_path) {
        shiny::tagList(
          shiny::modalButton("Cancel"),
          tags$button(class = "btn btn-primary", type = "button",
            onclick = "Shiny.setInputValue('new_save_first', Math.random(), {priority: 'event'});",
            "Save & Continue"),
          tags$button(class = "btn btn-danger", type = "button",
            onclick = "Shiny.setInputValue('new_discard', Math.random(), {priority: 'event'});",
            "Discard & Continue")
        )
      } else {
        shiny::tagList(
          shiny::modalButton("Cancel"),
          tags$button(class = "btn btn-danger", type = "button",
            onclick = "Shiny.setInputValue('new_discard', Math.random(), {priority: 'event'});",
            "Discard & Continue")
        )
      }
      shiny::showModal(shiny::modalDialog(
        title = "Unsaved Changes",
        shiny::tags$p("You have unsaved changes. Creating a new model will discard them."),
        footer = footer_buttons
      ))
    })

    shiny::observeEvent(input$new_save_first, {
      fpath <- current_file_path()
      if (is.null(fpath)) return()
      tryCatch({
        save_model_to_file(model(), fpath, message = "Auto-save before new model")
        saved_model(model())
        shiny::removeModal()
        show_new_model_type_modal()
      }, error = function(e) {
        shiny::showNotification(paste("Save failed:", conditionMessage(e)), type = "error", duration = 5)
      }, finally = {
        pending_new_model(FALSE)
      })
    })

    shiny::observeEvent(input$new_discard, {
      shiny::removeModal()
      pending_new_model(FALSE)
      show_new_model_type_modal()
    })

    shiny::observeEvent(input$confirm_new_model, {
      type <- input$new_model_type
      shiny::req(type)
      on_run_page(FALSE)
      session$sendCustomMessage("navigate_page", list(page = "documentation"))
      new_m <- openqaly::define_model(type)
      if (identical(type, "psm")) {
        new_m <- new_m |>
          openqaly::add_state(
            "progression_free",
            display_name = "Progression Free",
            description  = "No disease progression"
          ) |>
          openqaly::add_state(
            "progressed",
            display_name = "Progressed",
            description  = "Disease has progressed"
          ) |>
          openqaly::add_state(
            "dead",
            display_name = "Dead",
            description  = "Death"
          ) |>
          openqaly::add_transition(
            endpoint = "PFS",
            time_unit = "months",
            formula = 0
          ) |>
          openqaly::add_transition(
            endpoint = "OS",
            time_unit = "months",
            formula = 0
          )
      }
      model(new_m)
      original_model(new_m)
      saved_model(new_m)
      current_file_path(NULL)
      history$clear()
      file_load_counter(file_load_counter() + 1L)
      table_render_trigger(table_render_trigger() + 1L)
      shiny::removeModal()
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

      # Handle batch_edit (clipboard paste) — process sub-edits, single undo step
      if (action$type == "batch_edit") {
        tryCatch({
          result <- apply_batch_edit(action)
          send_values_update()
          file_load_counter(file_load_counter() + 1L)
        }, error = function(e) {
          shiny::showNotification(
            paste("Action failed:", conditionMessage(e)),
            type = "error"
          )
          file_load_counter(file_load_counter() + 1L)
        })
        return()
      }

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
            title = paste0('Remove "', action$name, '"?'),
            shiny::tags$p(paste0(
              'The value "', action$name, '" is referenced by other items that will also be deleted:'
            )),
            shiny::tags$ul(shiny::HTML(dep_items)),
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
            title = paste0('Remove "', action$name, '"?'),
            shiny::tags$p(paste0(
              'The summary "', action$name, '" has dependent items that will also be deleted.'
            )),
            shiny::tags$p("Remove this summary and all dependent items?"),
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
              title = paste0('Remove "', action$name, '"?'),
              shiny::tags$p(paste0(
                'The state "', action$name, '" has dependent items (such as transitions and values) ',
                'that will also be deleted.'
              )),
              shiny::tags$p("Remove this state and all dependent items?"),
              footer = shiny::tagList(
                shiny::modalButton("Cancel"),
                shiny::actionButton("confirm_force_remove_state", "Remove Anyway",
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
                                        "force_remove_summary",
                                        "add_tree_node", "remove_tree_node",
                                        "remove_variable")) {
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
            style = "max-height: 500px; overflow: auto;",
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

    build_override_sidebar_panel <- function(m) {
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

      input_id <- "editor_overrides"
      sections <- lapply(cats, function(cat) {
        cards <- lapply(cat$overrides, function(override) {
          oid <- .build_override_id(input_id, override)
          input_widget <- .build_override_input(
            oid,
            override,
            m
          )
          default_val <- if (!is.null(override$overridden_expression)) {
            as.character(override$overridden_expression)
          } else if (!is.null(override$default_value)) {
            as.character(override$default_value)
          } else {
            ""
          }

          tags$div(class = "override-card-simple",
            tags$div(class = "override-card-header",
              tags$div(class = "override-card-title-row",
                tags$span(class = "override-card-title",
                  override$title %||% override$display_name %||% override$name
                ),
                tags$button(
                  type = "button",
                  class = "btn btn-sm btn-outline-secondary override-reset-btn",
                  `data-override-id` = oid,
                  `data-default-value` = default_val,
                  `data-input-type` = override$input_type,
                  "Reset"
                )
              )
            ),
            tags$div(class = "override-card-body", input_widget)
          )
        })

        htmltools::tagList(
          tags$div(class = "override-section-header", cat$name),
          cards
        )
      })

      has_formula <- any(vapply(cats, function(cat) {
        any(vapply(cat$overrides, function(o) {
          identical(o$input_type, "formula")
        }, logical(1)))
      }, logical(1)))

      deps <- list(override_input_dependency(), override_manager_dependency())
      if (has_formula) deps <- c(deps, formula_input_dependency())

      gear_btn <- tags$button(
        type = "button",
        class = "override-manage-btn btn btn-outline-secondary btn-sm w-100 mb-3",
        `data-input-id` = input_id,
        "\u2699 Manage Overrides"
      )

      session$onFlushed(function() {
        session$sendCustomMessage("override-rebind", list())
      }, once = FALSE)

      htmltools::tagList(
        deps,
        tags$div(
          id = paste0(input_id, "-container"),
          class = "override-input-container",
          gear_btn,
          sections
        )
      )
    }

    render_override_sidebar_output <- function(output_id, page_name) {
      output[[output_id]] <- shiny::renderUI({
        shiny::req(identical(input$active_page, page_name))
        build_override_sidebar_panel(shiny::isolate(model()))
      })
    }

    render_override_sidebar_output("results_sidebar_overrides", "results")
    render_override_sidebar_output("dsa_sidebar_overrides", "dsa")
    render_override_sidebar_output("vbp_sidebar_overrides", "vbp")
    render_override_sidebar_output("scenario_sidebar_overrides", "scenario")
    render_override_sidebar_output("twsa_sidebar_overrides", "twsa")
    render_override_sidebar_output("threshold_sidebar_overrides", "threshold")
    render_override_sidebar_output("psa_sidebar_overrides", "psa")

    overrideManagerServer("editor_overrides",
      model = shiny::reactive(model()),
      on_action = function(action) {
        apply_action(action)
      }
    )

    # Sync override input values to model state via apply_action.
    # Reads model() (not isolated) to re-enumerate when override structure changes.
    # No-ops when input values match model's overridden_expression (prevents spurious reruns).
    shiny::observe({
      m <- model()
      if (is.null(m)) return()
      cats <- openqaly::get_override_categories(m)
      if (length(cats) == 0) return()

      changed <- list()
      for (cat in cats) {
        for (override in cat$overrides) {
          input_id <- .build_override_id("editor_overrides", override)
          val <- input[[input_id]]
          if (is.null(val)) next
          expr <- if (is.list(val) && !is.null(val$value)) val$value else val
          expr_str <- as.character(expr)
          model_expr <- as.character(override$overridden_expression %||% "")
          if (identical(override$input_type, "timeframe")) {
            parsed_input <- .parse_timeframe_value(expr_str)
            parsed_model <- .parse_timeframe_value(model_expr)
            expr_str <- paste0(parsed_input$number, "|", parsed_input$unit)
            model_expr <- paste0(parsed_model$number, "|", parsed_model$unit)
          }
          if (!identical(expr_str, model_expr)) {
            changed <- c(changed, list(list(
              name = override$name,
              expression = expr_str,
              strategy = override$strategy %||% "",
              group = override$group %||% ""
            )))
          }
        }
      }

      if (length(changed) > 0) {
        apply_action(list(type = "set_override_expressions", overrides = changed))
      }
    })

    build_editor_model <- function() {
      model()
    }

    # --- Async model run with progress reporting ---
    run_state <- shiny::reactiveValues(
      running = FALSE,
      progress_file = NULL,
      needs_rerun = FALSE,
      last_error = FALSE
    )
    rerun_trigger <- shiny::reactiveVal(0L)
    request_editor_rerun <- function() {
      rerun_trigger(shiny::isolate(rerun_trigger()) + 1L)
    }

    # Open the active analysis sidebar when navigating between result pages
    shiny::observeEvent(input$active_page, {
      sidebar_id <- switch(input$active_page,
        "results" = "results_sidebar",
        "dsa" = "dsa_sidebar",
        "vbp" = "vbp_sidebar",
        "scenario" = "scenario_sidebar",
        "twsa" = "twsa_sidebar",
        "threshold" = "threshold_sidebar",
        "psa" = "psa_sidebar",
        NULL
      )
      if (!is.null(sidebar_id)) {
        bslib::toggle_sidebar(sidebar_id, open = TRUE)
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

    shiny::observe({
      shiny::req(on_run_page(), isTRUE(input$editor_results_auto_run))
      model()
      request_editor_rerun()
    })

    shiny::observeEvent(input$editor_run_model, {
      shiny::req(on_run_page())
      request_editor_rerun()
    }, ignoreInit = TRUE)

    # Main run observer
    shiny::observeEvent(rerun_trigger(), {
      shiny::req(on_run_page())
      m <- build_editor_model()
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

      cb <- make_file_progress_callback(pf)
      p <- suppressWarnings(promises::future_promise({
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
    }, ignoreInit = TRUE)

    # Progress polling observer
    shiny::observe({
      shiny::req(run_state$running, run_state$progress_file)
      shiny::invalidateLater(250)
      prog <- read_file_progress(run_state$progress_file)
      if (!is.null(prog) && prog$total > 0) {
        session$sendCustomMessage("editor_progress", list(
          state = "running",
          pct = round(prog$pct * 100, 2)
        ))
      }
    })

    # DSA reactive state (defined here so onSessionEnded can reference it)
    dsa_results_rv <- shiny::reactiveValues(results = NULL, metadata = NULL)
    dsa_run_state <- shiny::reactiveValues(running = FALSE, progress_file = NULL)

    # VBP reactive state
    vbp_results_rv <- shiny::reactiveValues(results = NULL)
    vbp_run_state <- shiny::reactiveValues(running = FALSE, progress_file = NULL)
    vbp_price_choice_rv <- shiny::reactiveVal(NULL)

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
          class = "dsa-params-container oq-grid-themed",
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

      param_table
    })

    run_dsa_analysis <- function() {
      m <- build_editor_model()
      if (is.null(m)) {
        shiny::showNotification("No model loaded.", type = "warning")
        return()
      }

      if (length(openqaly::get_dsa_parameters(m)) == 0) {
        shiny::showNotification("Please add at least one parameter.",
          type = "warning")
        return()
      }

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
    }

    shiny::observeEvent(input$editor_run_dsa, {
      run_dsa_analysis()
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$run_dsa_action, {
      run_dsa_analysis()
    })

    # DSA progress polling
    shiny::observe({
      shiny::req(dsa_run_state$running, dsa_run_state$progress_file)
      shiny::invalidateLater(250)
      prog <- read_file_progress(dsa_run_state$progress_file)
      if (!is.null(prog) && prog$total > 0) {
        session$sendCustomMessage("dsa_progress", list(
          state = "running",
          pct = round(prog$pct * 100, 2)
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

      default_price <- resolve_vbp_variable_choice(
        m,
        vbp_config$price_variable %||% NULL,
        default_intervention,
        preferred_selection = vbp_price_choice_rv()
      )

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
          )
        )
      )
    })

    # Persist VBP config on every dropdown change (only when enabled)
    shiny::observe({
      if (!isTRUE(input$editor_vbp_enabled)) return()
      vbp_price_choice_rv(input$editor_vbp_price_variable %||% NULL)
      pv <- normalize_vbp_variable_choice(input$editor_vbp_price_variable)
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
      if (!isTRUE(input$editor_vbp_enabled)) {
        shiny::showNotification("Enable VBP before running the analysis.", type = "warning")
        return()
      }

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

      invalid_groups <- get_vbp_group_specific_variable_groups(
        m,
        input$editor_vbp_price_variable
      )
      if (length(invalid_groups) > 0) {
        shiny::showNotification(
          sprintf(
            paste0(
              "VBP price variable '%s' is defined for specific group(s): %s. ",
              "VBP does not currently support group-specific price variables."
            ),
            input$editor_vbp_price_variable,
            paste(invalid_groups, collapse = ", ")
          ),
          type = "error",
          duration = 10
        )
        return()
      }

      vbp_args <- list(
        m,
        price_variable = normalize_vbp_variable_choice(input$editor_vbp_price_variable),
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
          pct = round(prog$pct * 100, 2)
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
        # Left panel: scenario list (unified oq-sidebar)
        tags$div(
          class = "oq-sidebar",
          tags$button(
            type = "button",
            class = "btn btn-outline-primary btn-sm oq-sidebar-add scenario-add-btn",
            shiny::icon("plus"),
            " Add Scenario"
          ),
          tags$div(class = "oq-sidebar-list scenario-list")
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
            class = "scenario-params-container oq-grid-themed",
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
      old <- model()
      tryCatch({
        history$push(old)
        result <- dispatch_model_action(old, list(
          type = "add_scenario",
          name = name,
          description = input$add_scenario_description %||% ""
        ))
        model(result)
        file_load_counter(file_load_counter() + 1L)
      }, error = function(e) {
        history$undo(old)
        model(old)
        shiny::showNotification(
          paste("Failed to create scenario:", conditionMessage(e)),
          type = "error"
        )
      })
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
      old <- model()
      tryCatch({
        history$push(old)
        result <- dispatch_model_action(old, list(
          type = "edit_scenario",
          name = session$userData$editing_scenario_name,
          new_name = name,
          description = input$edit_scenario_description %||% ""
        ))
        model(result)
        file_load_counter(file_load_counter() + 1L)
      }, error = function(e) {
        history$undo(old)
        model(old)
        shiny::showNotification(
          paste("Failed to edit scenario:", conditionMessage(e)),
          type = "error"
        )
      })
    })

    shiny::observeEvent(input$remove_scenario_action, {
      old <- model()
      tryCatch({
        history$push(old)
        result <- dispatch_model_action(old, list(
          type = "remove_scenario",
          name = input$remove_scenario_action$name
        ))
        model(result)
        file_load_counter(file_load_counter() + 1L)
      }, error = function(e) {
        history$undo(old)
        model(old)
        shiny::showNotification(
          paste("Failed to remove scenario:", conditionMessage(e)),
          type = "error"
        )
      })
    })

    run_scenario_analysis <- function() {
      m <- build_editor_model()
      if (is.null(m)) {
        shiny::showNotification("No model loaded.", type = "warning")
        return()
      }

      if (length(openqaly::get_scenarios(m)) == 0) {
        shiny::showNotification("Please add at least one scenario.", type = "warning")
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
    }

    shiny::observeEvent(input$editor_run_scenario, {
      run_scenario_analysis()
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$run_scenario_action, {
      run_scenario_analysis()
    })

    # Scenario progress polling
    shiny::observe({
      shiny::req(scenario_run_state$running, scenario_run_state$progress_file)
      shiny::invalidateLater(250)
      prog <- read_file_progress(scenario_run_state$progress_file)
      if (!is.null(prog) && prog$total > 0) {
        session$sendCustomMessage("scenario_progress", list(
          state = "running",
          pct = round(prog$pct * 100, 2)
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
        raw_params <- tw$parameters %||% list()
        params <- lapply(seq_along(raw_params), function(pi) {
          p <- raw_params[[pi]]
          data_obj <- list()
          if (!is.null(p$min)) data_obj$min <- as.character(p$min)
          if (!is.null(p$max)) data_obj$max <- as.character(p$max)
          if (!is.null(p$radius)) data_obj$radius <- as.character(p$radius)
          if (!is.null(p$steps)) data_obj$steps <- p$steps
          if (!is.null(p$values)) data_obj$values <- as.character(p$values)
          list(
            axis = if (pi == 1) "x" else "y",
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
        # Left panel: TWSA analysis list (unified oq-sidebar)
        tags$div(
          class = "oq-sidebar",
          tags$button(
            type = "button",
            class = "btn btn-outline-primary btn-sm oq-sidebar-add twsa-add-btn",
            shiny::icon("plus"),
            " Add Analysis"
          ),
          tags$div(class = "oq-sidebar-list twsa-list")
        ),
        # Right panel: grid
        tags$div(
          class = "twsa-grid-panel",
          tags$div(
            class = "twsa-params-container oq-grid-themed",
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
        # 2. Add X and Y parameters with first two available variable combos
        var_targeting <- openqaly::get_variable_targeting(old)
        combos <- list()
        for (vname in names(var_targeting)) {
          t <- var_targeting[[vname]]
          strat <- if (!is.null(t$strategies)) t$strategies[1] else ""
          grp <- if (!is.null(t$groups)) t$groups[1] else ""
          combos[[length(combos) + 1]] <- list(
            name = vname, strategy = strat, group = grp
          )
          if (length(combos) >= 2) break
        }
        if (length(combos) >= 1) {
          result <- dispatch_model_action(result, list(
            type = "add_twsa_variable",
            twsa_name = name,
            variable = combos[[1]]$name,
            strategy = combos[[1]]$strategy,
            group = combos[[1]]$group,
            method_type = "radius",
            radius = "value * 0.2",
            steps = 3
          ))
        }
        if (length(combos) >= 2) {
          result <- dispatch_model_action(result, list(
            type = "add_twsa_variable",
            twsa_name = name,
            variable = combos[[2]]$name,
            strategy = combos[[2]]$strategy,
            group = combos[[2]]$group,
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

    run_twsa_analysis <- function() {
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
    }

    shiny::observeEvent(input$editor_run_twsa, {
      run_twsa_analysis()
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$run_twsa_action, {
      run_twsa_analysis()
    })

    # TWSA progress polling
    shiny::observe({
      shiny::req(twsa_run_state$running, twsa_run_state$progress_file)
      shiny::invalidateLater(250)
      prog <- read_file_progress(twsa_run_state$progress_file)
      if (!is.null(prog) && prog$total > 0) {
        session$sendCustomMessage("twsa_progress", list(
          state = "running",
          pct = round(prog$pct * 100, 2)
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
          var_names <- if (is.character(spec$variables)) {
            spec$variables
          } else if (is.data.frame(spec$variables)) {
            as.character(spec$variables$variable)
          } else {
            character(0)
          }
          result <- list(
            name = spec$name,
            type = spec$type %||% "",
            variables = as.list(var_names),
            strategy = spec$strategy %||% "",
            group = spec$group %||% ""
          )
          if (!is.null(spec$covariance)) result$covariance <- as.character(spec$covariance)
          if (!is.null(spec$n)) {
            result$n <- spec$n
          } else if (!is.null(spec$alpha)) {
            # Convert legacy alpha to n (effective sample size = sum of alpha)
            result$n <- sum(as.numeric(spec$alpha))
          }
          result
        })
      } else list()

      # Table names for mvnormal covariance dropdown
      table_names <- if (!is.null(m$tables)) names(m$tables) else character(0)

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
        mv_initial = mv_initial,
        table_names = table_names
      )
    })

    output$psa_footer_controls <- shiny::renderUI({
      d <- psa_input_data()
      if (is.null(d)) {
        return(tags$div(class = "text-muted", "Load a model first."))
      }

      shiny::tagList(
        tags$div(
          class = "results-footer-grid",
          shiny::numericInput(
            "editor_psa_n_sim",
            "N",
            value = d$n_sim_default,
            min = 1,
            step = 100
          ),
          shiny::textInput(
            "editor_psa_seed",
            "Seed",
            value = as.character(d$seed_default %||% ""),
            placeholder = "Optional"
          ),
          tags$div(
            style = "grid-column: 1 / -1;",
            shiny::actionButton(
              "editor_run_psa",
              "Run PSA",
              class = "btn-primary"
            )
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
            class = "psa-params-container oq-grid-themed",
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
            class = "psa-multivariate-container oq-grid-themed",
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
            `data-tables` = jsonlite::toJSON(
              d$table_names, auto_unbox = FALSE
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
          tags$div(class = "label-row",
            tags$label(class = "control-label", `for` = "add_mv_type", "Type"),
            info_icon("Dirichlet for proportions, MV Normal for correlated continuous variables, Multinomial for discrete categorical outcomes.")
          ),
          .editor_select_input(
            "add_mv_type",
            label = NULL,
            choices = c("dirichlet", "mvnormal", "multinomial"),
            selected = "dirichlet"
          )
        ),
        shiny::tags$div(
          class = "mb-3",
          tags$div(class = "label-row",
            tags$label(class = "control-label", `for` = "add_mv_variables", "Variables"),
            info_icon("Select the variables to be jointly sampled from this distribution.")
          ),
          .editor_selectize_input(
            "add_mv_variables",
            label = NULL,
            choices = d$var_choices,
            selected = character(0),
            multiple = TRUE,
            options = list(plugins = list("remove_button"))
          )
        ),
        shiny::tags$div(
          class = "mb-3",
          shiny::tags$label("Parameters"),
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
            "Choose variables first, then configure the distribution parameters."
          )
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
      mv_type <- .str(input$add_mv_type)
      mv_params <- input$add_mv_params  # structured object from JS builder
      variables <- input$add_mv_variables %||% character(0)

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
      if (!nzchar(mv_type)) {
        shiny::showNotification("Distribution type is required.", type = "warning")
        return()
      }
      if (length(variables) == 0) {
        shiny::showNotification("Select at least one variable.", type = "warning")
        return()
      }

      # Build the action from structured params
      action_data <- list(
        type = "add_multivariate_sampling",
        name = name,
        mv_dist_type = mv_type,
        variables = variables
      )
      if (!is.null(mv_params)) {
        if (!is.null(mv_params$alpha)) action_data$alpha <- mv_params$alpha
        if (!is.null(mv_params$n)) action_data$n <- mv_params$n
        if (!is.null(mv_params$size)) action_data$size <- mv_params$size
        if (!is.null(mv_params$covariance)) action_data$covariance <- mv_params$covariance
      }

      # Validate mvnormal requires covariance
      if (mv_type == "mvnormal" && !nzchar(action_data$covariance %||% "")) {
        shiny::showNotification("Covariance table is required for mvnormal.", type = "warning")
        return()
      }

      shiny::removeModal()

      result <- tryCatch(
        apply_action(action_data),
        error = function(e) e
      )

      if (inherits(result, "error")) {
        shiny::showNotification(
          paste("Failed to add multivariate sampling:", conditionMessage(result)),
          type = "error"
        )
      }
    })

    normalize_psa_seed <- function(seed_val) {
      seed_chr <- trimws(as.character(seed_val %||% ""))
      if (!nzchar(seed_chr)) return("")
      if (!grepl("^-?\\d+$", seed_chr)) return(NA_character_)
      seed_chr
    }

    sync_psa_settings <- function(n_sim = NULL, seed_val = NULL) {
      m <- model()
      if (is.null(m)) return(FALSE)

      n_sim_value <- as.integer(n_sim %||% input$editor_psa_n_sim %||% NA_integer_)
      if (is.na(n_sim_value) || n_sim_value < 1) {
        shiny::showNotification("Please set a valid number of simulations.", type = "warning")
        return(FALSE)
      }

      seed_chr <- normalize_psa_seed(seed_val %||% input$editor_psa_seed)
      if (is.na(seed_chr)) {
        shiny::showNotification("Seed must be an integer.", type = "warning")
        return(FALSE)
      }

      current_n_sim <- as.integer(m$psa$n_sim %||% 1000L)
      current_seed <- if (is.null(m$psa$seed)) "" else as.character(m$psa$seed)

      if (identical(current_n_sim, n_sim_value) && identical(current_seed, seed_chr)) {
        return(TRUE)
      }

      apply_action(list(
        type = "set_psa_settings",
        n_sim = n_sim_value,
        seed = seed_chr
      ))

      TRUE
    }

    run_psa_analysis <- function(n_sim = NULL, seed_val = NULL) {
      n_sim_value <- as.integer(n_sim %||% input$editor_psa_n_sim %||% NA_integer_)
      if (is.na(n_sim_value) || n_sim_value < 1) {
        shiny::showNotification("Please set number of simulations.", type = "warning")
        return()
      }

      seed_chr <- normalize_psa_seed(seed_val %||% input$editor_psa_seed)
      if (is.na(seed_chr)) {
        shiny::showNotification("Seed must be an integer.", type = "warning")
        return()
      }

      if (!sync_psa_settings(n_sim = n_sim_value, seed_val = seed_chr)) {
        return()
      }

      m <- build_editor_model()
      if (is.null(m)) {
        shiny::showNotification("No model loaded.", type = "warning")
        return()
      }

      psa_args <- list(m, n_sim = n_sim_value)
      if (nzchar(seed_chr)) {
        psa_args$seed <- as.integer(seed_chr)
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
    }

    shiny::observeEvent(input$editor_psa_n_sim, {
      sync_psa_settings()
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$editor_psa_seed, {
      sync_psa_settings()
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$editor_run_psa, {
      run_psa_analysis()
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$run_psa_action, {
      run_psa_analysis(
        n_sim = input$run_psa_action$n_sim,
        seed_val = input$run_psa_action$seed
      )
    }, ignoreInit = TRUE)

    # PSA progress polling
    shiny::observe({
      shiny::req(psa_run_state$running, psa_run_state$progress_file)
      shiny::invalidateLater(250)
      prog <- read_file_progress(psa_run_state$progress_file)
      if (!is.null(prog) && prog$total > 0) {
        session$sendCustomMessage("psa_progress", list(
          state = "running",
          pct = round(prog$pct * 100, 2)
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
          class = "threshold-params-container oq-grid-themed",
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
        )
      )
    })

    run_threshold_analysis <- function() {
      m <- build_editor_model()
      if (is.null(m)) {
        shiny::showNotification("No model loaded.", type = "warning")
        return()
      }

      if (is.null(m$threshold_analyses) || length(m$threshold_analyses) == 0) {
        shiny::showNotification("Please add at least one analysis.",
          type = "warning")
        return()
      }

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
    }

    shiny::observeEvent(input$editor_run_threshold, {
      run_threshold_analysis()
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$run_threshold_action, {
      run_threshold_analysis()
    })

    # Threshold progress polling
    shiny::observe({
      shiny::req(threshold_run_state$running, threshold_run_state$progress_file)
      shiny::invalidateLater(250)
      prog <- read_file_progress(threshold_run_state$progress_file)
      if (!is.null(prog) && prog$total > 0) {
        session$sendCustomMessage("threshold_progress", list(
          state = "running",
          pct = round(prog$pct * 100, 2)
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
      model_type <- openqaly::get_model_type(m)

      model_type_labels <- c(markov = "Markov", psm = "PSM", custom_psm = "Custom PSM", decision_tree = "Decision Tree")

      country_choices <- c(
        "United States (US)" = "US", "United Kingdom (GB)" = "GB",
        "Canada (CA)" = "CA", "India (IN)" = "IN",
        "Germany (DE)" = "DE", "Switzerland (CH)" = "CH",
        "France (FR)" = "FR", "Italy (IT)" = "IT",
        "Spain (ES)" = "ES", "Mexico (MX)" = "MX",
        "Netherlands (NL)" = "NL", "Norway (NO)" = "NO",
        "Sweden (SE)" = "SE", "Denmark (DK)" = "DK",
        "Finland (FI)" = "FI", "Japan (JP)" = "JP",
        "China (CN)" = "CN", "South Korea (KR)" = "KR",
        "Brazil (BR)" = "BR", "Poland (PL)" = "PL",
        "Czech Republic (CZ)" = "CZ", "Hungary (HU)" = "HU",
        "Russia (RU)" = "RU", "Ukraine (UA)" = "UA",
        "Israel (IL)" = "IL"
      )

      tags$div(
        class = "settings-scroll",
        style = "height: 100%;",
        tags$div(
          class = "settings-container",

          # ── Model section ──
          tags$div(class = "settings-section-header", "Model"),
          tags$div(class = "settings-grid",
            tags$div(
              tags$div(class = "label-row",
                tags$label(class = "control-label", "Model Type"),
                info_icon("The model structure. Set at creation and cannot be changed.")
              ),
              shiny::textInput("setting_model_type", label = NULL,
                               value = model_type_labels[model_type] %||% model_type)
            ),
            if (model_type != "decision_tree") {
              tags$div(
                tags$div(class = "label-row",
                  tags$label(class = "control-label", "Half-Cycle Method"),
                  info_icon("Half-cycle correction for value calculations. Start uses state occupancy at the beginning of each cycle, End uses occupancy at the end of each cycle, and Life-Table averages the two.")
                ),
                shiny::selectInput("setting_half_cycle_method", label = NULL,
                                    choices = c("Start" = "start", "End" = "end", "Life-Table" = "life-table"),
                                    selected = s$half_cycle_method %||% "start")
              )
            }
          ),
          tags$script(shiny::HTML(
            "document.getElementById('setting_model_type').setAttribute('disabled', 'disabled');"
          )),

          # ── Time & Cycles section (hidden for decision_tree) ──
          if (model_type != "decision_tree") {
            tagList(
              tags$div(class = "settings-section-header", "Time & Cycles"),
              tags$div(class = "settings-grid",
                tags$div(
                  tags$div(class = "label-row",
                    tags$label(class = "control-label", "Timeframe"),
                    info_icon("Total time horizon for the model simulation.")
                  ),
                  shiny::numericInput("setting_timeframe", label = NULL,
                                      value = s$timeframe %||% 10, min = 1)
                ),
                tags$div(
                  tags$div(class = "label-row",
                    tags$label(class = "control-label", "Timeframe Unit"),
                    info_icon("Unit for the time horizon. \"Cycles\" expresses the timeframe directly in model cycles.")
                  ),
                  shiny::selectInput("setting_timeframe_unit", label = NULL,
                                      choices = c("Years" = "years", "Months" = "months", "Weeks" = "weeks", "Days" = "days", "Cycles" = "cycles"),
                                      selected = s$timeframe_unit %||% "years")
                ),
                tags$div(
                  tags$div(class = "label-row",
                    tags$label(class = "control-label", "Days Per Year"),
                    info_icon("Number of days per year used when converting between time units. Use 365.25 to account for leap years.")
                  ),
                  shiny::numericInput("setting_days_per_year", label = NULL,
                                      value = s$days_per_year %||% 365.25, min = 1)
                ),
                tags$div(
                  tags$div(class = "label-row",
                    tags$label(class = "control-label", "Cycle Length"),
                    info_icon("Duration of each model cycle. Shorter cycles give more precision but increase run time.")
                  ),
                  shiny::numericInput("setting_cycle_length", label = NULL,
                                      value = s$cycle_length %||% 1, min = 0, step = 0.1)
                ),
                tags$div(
                  tags$div(class = "label-row",
                    tags$label(class = "control-label", "Cycle Length Unit"),
                    info_icon("Unit for the cycle length.")
                  ),
                  shiny::selectInput("setting_cycle_length_unit", label = NULL,
                                      choices = c("Years" = "years", "Months" = "months", "Weeks" = "weeks", "Days" = "days"),
                                      selected = s$cycle_length_unit %||% "years")
                ),
                if (model_type == "markov") {
                  tags$div(class = "field-checkbox",
                    shiny::checkboxInput("setting_reduce_state_cycle", "Reduce State Cycle",
                                          value = s$reduce_state_cycle %||% FALSE),
                    info_icon("Optimizes tunnel states by detecting where transitions and values stop changing with time in state, reducing the number of tunnel states needed.")
                  )
                }
              )
            )
          },

          # ── Discounting section ──
          tags$div(class = "settings-section-header", "Discounting"),
          tags$div(class = "settings-grid",
            tags$div(
              tags$div(class = "label-row",
                tags$label(class = "control-label", "Costs (%)"),
                info_icon("Annual discount rate applied to future costs, as a percentage.")
              ),
              shiny::numericInput("setting_discount_cost", label = NULL,
                                  value = s$discount_cost %||% 0, min = 0, max = 100, step = 0.5)
            ),
            tags$div(
              tags$div(class = "label-row",
                tags$label(class = "control-label", "Outcomes (%)"),
                info_icon("Annual discount rate applied to future health outcomes (e.g. QALYs, life years), as a percentage.")
              ),
              shiny::numericInput("setting_discount_outcomes", label = NULL,
                                  value = s$discount_outcomes %||% 0, min = 0, max = 100, step = 0.5)
            )
          ),

          # ── Locale section ──
          tags$div(class = "settings-section-header", "Locale"),
          tags$div(class = "settings-grid",
            tags$div(
              tags$div(class = "label-row",
                tags$label(class = "control-label", "Country"),
                info_icon("Sets the currency symbol and default number formatting used in results.")
              ),
              shiny::selectInput("setting_country", label = NULL,
                                  choices = country_choices,
                                  selected = s$country %||% "US")
            ),
            tags$div(
              tags$div(class = "label-row",
                tags$label(class = "control-label", "Number Country"),
                info_icon("Overrides the decimal and thousands separators independently of the currency. Defaults to the selected country.")
              ),
              shiny::selectInput("setting_number_country", label = NULL,
                                  choices = c("Same as Country" = "", country_choices),
                                  selected = s$number_country %||% "")
            )
          )
        ), # end settings-container

        # ── Decision Tree section (hidden for decision_tree model type) ──
        if (model_type != "decision_tree") {
          tnames <- openqaly::get_tree_names(m)
          tree_choices <- stats::setNames(tnames, tnames)
          dt_config <- openqaly::get_decision_tree(m)
          current_tree <- dt_config$tree_name %||% ""
          current_duration <- dt_config$duration %||% ""
          current_unit <- dt_config$duration_unit %||% "days"
          tags$div(class = "dt-card",
            tags$div(class = "settings-section-header", style = "margin-top: 0;", "Decision Tree"),
            tags$div(class = "settings-grid",
              tags$div(
                tags$div(class = "label-row",
                  tags$label(class = "control-label", "Active Tree"),
                  info_icon("Decision tree to run before the Markov or PSM phase. Its duration is subtracted from the total time horizon.")
                ),
                shiny::selectInput("setting_dt_tree", label = NULL,
                                   choices = c("None" = "", tree_choices),
                                   selected = current_tree)
              ),
              tags$div(
                tags$div(class = "label-row",
                  tags$label(class = "control-label", "Duration"),
                  info_icon("Length of the decision tree phase. Can be a number or a variable name.")
                ),
                shiny::textInput("setting_dt_duration", label = NULL,
                                 value = current_duration,
                                 placeholder = "e.g. 30 or variable name")
              ),
              tags$div(
                tags$div(class = "label-row",
                  tags$label(class = "control-label", "Duration Unit"),
                  info_icon("Unit for the decision tree duration.")
                ),
                shiny::selectInput("setting_dt_duration_unit", label = NULL,
                                   choices = c("Days" = "days", "Weeks" = "weeks",
                                               "Months" = "months", "Years" = "years"),
                                   selected = current_unit)
              )
            )
          )
        },

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
      tables <- openqaly::get_tables(model())

      items <- lapply(tnames, function(tname) {
        list(
          name = tname,
          description = tables[[tname]]$description %||% ""
        )
      })

      oq_sidebar_list_ui(
        items = items,
        selected = sel,
        add_input = "tables_add_click",
        select_input = "tables_select_click",
        edit_input = "tables_edit_click",
        delete_input = "tables_delete_click",
        add_label = "Add Table"
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
      if (is.null(sel) || is.null(m)) return()
      scripts <- openqaly::get_scripts(m)
      if (is.null(scripts[[sel]])) return()
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
      scripts <- model()$scripts

      items <- lapply(snames, function(sname) {
        list(
          name = sname,
          description = scripts[[sname]]$description %||% ""
        )
      })

      sidebar <- oq_sidebar_list_ui(
        items = items,
        selected = sel,
        add_input = "scripts_add_click",
        select_input = "scripts_select_click",
        edit_input = "scripts_edit_click",
        delete_input = "scripts_delete_click",
        add_label = "Add Script"
      )

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

      tags$div(
        class = "scripts-tab-container",
        sidebar,
        right_panel
      )
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
          class = "strategies-table-container oq-grid-themed",
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

      # Handle batch_edit (clipboard paste)
      if (action$type == "batch_edit") {
        tryCatch({
          result <- apply_batch_edit(action)
          file_load_counter(file_load_counter() + 1L)
        }, error = function(e) {
          shiny::showNotification(
            paste("Action failed:", conditionMessage(e)),
            type = "error"
          )
          file_load_counter(file_load_counter() + 1L)
        })
        return()
      }

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

        # Build modal content — CSS grid layout for strategy fields
        modal_body <- shiny::tagList(
          shiny::tags$div(
            class = "add-form-grid",
            shiny::tags$div(
              shiny::textInput("add_strategy_name", "Name", value = "",
                               width = "100%")
            ),
            shiny::tags$div(
              shiny::textInput("add_strategy_display_name", "Display Name",
                               value = "", width = "100%")
            ),
            shiny::tags$div(
              class = "full-width",
              shiny::textInput("add_strategy_description", "Description",
                               value = "", width = "100%")
            )
          )
        )

        if (has_vars) {
          n_vars <- length(strat_var_names)
          modal_body <- shiny::tagList(
            modal_body,
            shiny::tags$div(class = "add-modal-divider"),
            shiny::tags$div(
              class = "add-vars-header",
              shiny::tags$span(class = "add-vars-title",
                               "Strategy-Specific Variables"),
              shiny::tags$span(class = "add-vars-badge",
                               paste0(n_vars, " variable",
                                      if (n_vars != 1) "s"))
            ),
            shiny::tags$p(
              class = "add-vars-desc",
              "These variables have strategy-specific formulas."
            ),
            shiny::tags$div(id = "add-strategy-vars-container", class = "oq-grid-themed")
          )
        }

        shiny::showModal(shiny::tags$div(
          class = "add-strategy-modal",
          shiny::modalDialog(
            title = "Add Strategy",
            modal_body,
            size = "l",
            footer = shiny::tags$div(
              class = "add-modal-footer",
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
          enabled = 1
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
      # Guard against jsonlite simplifying an array of objects into a data.frame
      if (is.data.frame(var_rows)) {
        var_rows <- lapply(seq_len(nrow(var_rows)), function(i) as.list(var_rows[i, , drop = FALSE]))
      } else if (is.list(var_rows) && !is.null(names(var_rows))) {
        # Single object came as a flat named list
        var_rows <- list(var_rows)
      }
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
          enabled = 1
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
          class = "groups-table-container oq-grid-themed",
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

      # Handle batch_edit (clipboard paste)
      if (action$type == "batch_edit") {
        tryCatch({
          result <- apply_batch_edit(action)
          file_load_counter(file_load_counter() + 1L)
        }, error = function(e) {
          shiny::showNotification(
            paste("Action failed:", conditionMessage(e)),
            type = "error"
          )
          file_load_counter(file_load_counter() + 1L)
        })
        return()
      }

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

        # Build modal content — CSS grid layout for group fields
        modal_body <- shiny::tagList(
          shiny::tags$div(
            class = "add-form-grid",
            shiny::tags$div(
              shiny::textInput("add_group_name", "Name", value = "",
                               width = "100%")
            ),
            shiny::tags$div(
              shiny::textInput("add_group_display_name", "Display Name",
                               value = "", width = "100%")
            ),
            shiny::tags$div(
              class = "full-width",
              shiny::textInput("add_group_description", "Description",
                               value = "", width = "100%")
            ),
            shiny::tags$div(
              class = "full-width add-group-weight-field",
              tags$div(class = "label-row",
                shiny::tags$label(
                  class = "control-label",
                  `for` = "add_group_weight",
                  "Weight"
                ),
                info_icon("Relative weight of this group in the total population.")
              ),
              formulaInput("add_group_weight", value = "1",
                           model = m, context = "variable",
                           width = "100%")
            )
          )
        )

        if (has_vars) {
          n_vars <- length(grp_var_names)
          modal_body <- shiny::tagList(
            modal_body,
            shiny::tags$div(class = "add-modal-divider"),
            shiny::tags$div(
              class = "add-vars-header",
              shiny::tags$span(class = "add-vars-title",
                               "Group-Specific Variables"),
              shiny::tags$span(class = "add-vars-badge",
                               paste0(n_vars, " variable",
                                      if (n_vars != 1) "s"))
            ),
            shiny::tags$p(
              class = "add-vars-desc",
              "These variables have group-specific formulas."
            ),
            shiny::tags$div(id = "add-group-vars-container", class = "oq-grid-themed")
          )
        }

        shiny::showModal(shiny::tags$div(
          class = "add-group-modal",
          shiny::modalDialog(
            title = "Add Group",
            modal_body,
            size = "l",
            footer = shiny::tags$div(
              class = "add-modal-footer",
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
          weight = trimws(input$add_group_weight$value %||% "1"),
          enabled = 1
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
      # Guard against jsonlite simplifying an array of objects into a data.frame
      if (is.data.frame(var_rows)) {
        var_rows <- lapply(seq_len(nrow(var_rows)), function(i) as.list(var_rows[i, , drop = FALSE]))
      } else if (is.list(var_rows) && !is.null(names(var_rows))) {
        # Single object came as a flat named list
        var_rows <- list(var_rows)
      }
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
          weight = trimws(input$add_group_weight$value %||% "1"),
          enabled = 1
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
        class = "states-table-container oq-grid-themed",
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
          class = "transitions-table-container oq-grid-themed",
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
          class = "variables-table-container oq-grid-themed",
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
          class = "values-table-container oq-grid-themed",
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
          class = "summaries-table-container oq-grid-themed",
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
      items <- lapply(tnames, function(tname) list(name = tname))
      oq_sidebar_list_ui(
        items = items,
        selected = sel,
        add_input = "trees_add_click",
        select_input = "trees_select_click",
        edit_input = "trees_edit_click",
        delete_input = "trees_delete_click",
        add_label = "Add Tree"
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
          class = "trees-table-container oq-grid-themed",
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
