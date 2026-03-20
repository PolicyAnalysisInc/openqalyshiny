#' Override Manager Server
#'
#' Shiny module server that provides a full-screen modal for managing override
#' categories and their overrides. Users can create, edit, delete, and reorder
#' overrides via a Kanban-style drag-and-drop interface. Each operation is
#' dispatched individually through openqaly builder functions via \code{on_action}.
#'
#' @param id The module namespace ID. Must match the base input ID used by
#'   the corresponding \code{\link{overrideInput}}.
#' @param model A reactive expression returning an openqaly model object that
#'   contains \code{override_categories}.
#' @param on_action A callback function that receives an action list and returns
#'   the result of applying it (e.g., via \code{apply_action}).
#'
#' @return Invisible NULL. Called for side effects.
#'
#' @export
overrideManagerServer <- function(id, model, on_action) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Strip trailing dash from namespace prefix so JS can construct
    # input IDs like "prefix-add_category" without double dashes
    input_prefix <- sub("-$", "", ns(""))

    modal_open <- shiny::reactiveVal(FALSE)

    # Helper: build category data for JS from model
    .build_cat_data <- function(m) {
      categories <- openqaly::get_override_categories(m)
      lapply(categories, function(cat) {
        overrides <- lapply(cat$overrides, function(o) {
          list(
            name = o$name,
            title = o$title %||% o$display_name %||% o$name,
            display_name = o$display_name %||% o$title %||% o$name,
            description = o$description %||% NULL,
            type = o$type %||% "variable",
            strategy = o$strategy %||% "",
            group = o$group %||% "",
            input_type = o$input_type %||% "numeric",
            input_config = o$input_config %||% list(),
            default_value = if (!is.null(o$default_value)) {
              as.character(o$default_value)
            } else {
              ""
            },
            overridden_expression = if (!is.null(o$overridden_expression)) {
              as.character(o$overridden_expression)
            } else {
              NULL
            }
          )
        })

        list(
          name = cat$name,
          general = isTRUE(cat$general),
          overrides = overrides
        )
      })
    }

    # Helper: build model metadata for JS form dropdowns
    .build_model_meta <- function(m) {
      overridable_settings <- c(
        "discount_cost", "discount_outcomes", "timeframe", "cycle_length"
      )
      available_settings <- intersect(
        overridable_settings, names(openqaly::get_settings(m))
      )

      targeting <- openqaly::get_variable_targeting(m)
      vars <- openqaly::get_variables(m)

      var_meta <- lapply(names(targeting), function(vname) {
        rows <- vars[vars$name == vname, , drop = FALSE]
        formulas <- list()
        for (i in seq_len(nrow(rows))) {
          strat_val <- rows$strategy[i]
          grp_val <- rows$group[i]
          if (is.null(strat_val) || is.na(strat_val)) strat_val <- ""
          if (is.null(grp_val) || is.na(grp_val)) grp_val <- ""
          key <- paste0(strat_val, "|", grp_val)
          formulas[[key]] <- as.character(rows$formula[i])
        }
        list(
          name = vname,
          strategies = as.list(targeting[[vname]]$strategies %||% character(0)),
          groups = as.list(targeting[[vname]]$groups %||% character(0)),
          formulas = formulas
        )
      })

      strategies <- openqaly::get_strategies(m)
      groups <- openqaly::get_groups(m)

      settings <- openqaly::get_settings(m)
      setting_values <- lapply(available_settings, function(s) {
        list(name = s, value = as.character(settings[[s]]))
      })

      list(
        variables = var_meta,
        settings = available_settings,
        setting_values = setting_values,
        strategies = if (nrow(strategies) > 0) {
          as.list(setNames(strategies$display_name, strategies$name))
        } else {
          list()
        },
        groups = if (nrow(groups) > 0) {
          as.list(setNames(groups$display_name, groups$name))
        } else {
          list()
        }
      )
    }

    # Helper: send error to JS
    .send_error <- function(msg) {
      session$sendCustomMessage("override-manager-error", list(
        inputId = input_prefix,
        message = msg
      ))
    }

    # Helper: run an action via on_action with error handling
    .run_action <- function(action) {
      tryCatch({
        on_action(action)
      }, error = function(e) {
        .send_error(conditionMessage(e))
        NULL
      })
    }

    # Open modal when gear button is clicked
    shiny::observeEvent(input$manage_click, {
      m <- model()
      if (is.null(m)) return()
      categories <- openqaly::get_override_categories(m)
      modal <- .build_manager_modal(ns, categories)
      shiny::showModal(modal)
      modal_open(TRUE)

      cat_data <- .build_cat_data(m)
      model_meta <- .build_model_meta(m)

      session$sendCustomMessage("override-manager-init", list(
        inputId = input_prefix,
        categories = cat_data,
        model_meta = model_meta
      ))
    })

    # Handle Close
    shiny::observeEvent(input$manager_close, {
      modal_open(FALSE)
      shiny::removeModal()
    })

    # Subscribe: whenever model changes while modal is open, send updated state
    shiny::observe({
      shiny::req(modal_open())
      m <- model()
      shiny::req(m)
      cat_data <- .build_cat_data(m)
      model_meta <- .build_model_meta(m)
      session$sendCustomMessage("override-manager-update", list(
        inputId = input_prefix,
        categories = cat_data,
        model_meta = model_meta
      ))
    })

    # --- Individual operation observers ---

    shiny::observeEvent(input$add_category, {
      data <- input$add_category
      .run_action(list(
        type = "add_override_category",
        name = data$name,
        general = isTRUE(data$general)
      ))
    })

    shiny::observeEvent(input$edit_category, {
      data <- input$edit_category
      action <- list(
        type = "edit_override_category",
        name = data$name
      )
      if (!is.null(data$new_name)) action$new_name <- data$new_name
      if (!is.null(data$general)) action$general <- data$general
      .run_action(action)
    })

    shiny::observeEvent(input$remove_category, {
      data <- input$remove_category
      .run_action(list(
        type = "remove_override_category",
        name = data$name
      ))
    })

    shiny::observeEvent(input$add_override, {
      data <- input$add_override
      action <- list(
        type = "add_override",
        category = data$category,
        title = data$title,
        name = data$name,
        override_type = data$override_type %||% "variable",
        input_type = data$input_type %||% "numeric",
        expression = data$expression %||% "0",
        description = data$description %||% NULL,
        strategy = data$strategy %||% "",
        group = data$group %||% ""
      )
      if (!is.null(data$min)) action$min <- as.numeric(data$min)
      if (!is.null(data$max)) action$max <- as.numeric(data$max)
      if (!is.null(data$step_size)) action$step_size <- as.numeric(data$step_size)
      if (!is.null(data$options)) action$options <- data$options
      .run_action(action)
    })

    shiny::observeEvent(input$edit_override, {
      data <- input$edit_override
      action <- list(
        type = "edit_override",
        category = data$category,
        override_type = data$override_type %||% "variable",
        name = data$name,
        strategy = data$strategy %||% "",
        group = data$group %||% ""
      )
      if (!is.null(data$new_type)) action$new_type <- data$new_type
      if (!is.null(data$new_name)) action$new_name <- data$new_name
      if (!is.null(data$new_strategy)) action$new_strategy <- data$new_strategy
      if (!is.null(data$new_group)) action$new_group <- data$new_group
      if (!is.null(data$title)) action$title <- data$title
      if (!is.null(data$description)) action$description <- data$description
      if (!is.null(data$expression)) action$expression <- data$expression
      if (!is.null(data$input_type)) action$input_type <- data$input_type
      if (!is.null(data$min)) action$min <- as.numeric(data$min)
      if (!is.null(data$max)) action$max <- as.numeric(data$max)
      if (!is.null(data$step_size)) action$step_size <- as.numeric(data$step_size)
      if (!is.null(data$options)) action$options <- data$options
      .run_action(action)
    })

    shiny::observeEvent(input$remove_override, {
      data <- input$remove_override
      .run_action(list(
        type = "remove_override",
        category = data$category,
        override_type = data$override_type %||% "variable",
        name = data$name,
        strategy = data$strategy %||% "",
        group = data$group %||% ""
      ))
    })

    # Reorder uses batch set_override_categories (no individual builder for reorder)
    shiny::observeEvent(input$reorder_overrides, {
      raw_state <- input$reorder_overrides
      if (is.null(raw_state)) return()
      tryCatch({
        new_categories <- .parse_manager_state(raw_state)
        on_action(list(type = "set_override_categories", categories = new_categories))
      }, error = function(e) {
        .send_error(conditionMessage(e))
      })
    })
  })
}

#' Build Manager Modal
#'
#' Constructs the modal dialog for the override manager.
#'
#' @param ns The module namespace function.
#' @param categories The current override categories list.
#'
#' @return A \code{shiny::modalDialog} object.
#' @keywords internal
.build_manager_modal <- function(ns, categories) {
  # Build columns for each category
  columns <- lapply(seq_along(categories), function(i) {
    .build_manager_column(categories[[i]], i - 1)
  })

  body <- htmltools::tags$div(
    class = "override-manager-body",
    columns
  )

  shiny::modalDialog(
    title = "Manage Overrides",
    body,
    size = "xl",
    easyClose = FALSE,
    footer = htmltools::tags$div(
      class = "override-manager-footer",
      htmltools::tags$button(
        type = "button",
        class = "btn btn-secondary",
        `data-action` = "manager-close",
        "Close"
      )
    )
  )
}

#' Build Manager Column
#'
#' Creates a single category column div for the manager modal.
#'
#' @param category A single override category list.
#' @param index Zero-based index of the category.
#'
#' @return An htmltools tag.
#' @keywords internal
.build_manager_column <- function(category, index) {
  cards <- lapply(category$overrides, function(override) {
    .build_manager_card(override)
  })

  htmltools::tags$div(
    class = "override-manager-column",
    `data-category-index` = index,
    htmltools::tags$div(
      class = "override-manager-column-header",
      htmltools::tags$span(
        class = "override-manager-column-title",
        `data-action` = "rename-category",
        `data-category-index` = index,
        category$name
      ),
      htmltools::tags$div(
        class = "override-manager-column-actions",
        htmltools::tags$button(
          type = "button",
          `data-action` = "delete-category",
          `data-category-index` = index,
          title = "Delete category",
          "\u00D7"
        )
      )
    ),
    htmltools::tags$div(
      class = "override-manager-card-list",
      `data-category-index` = index,
      cards
    ),
    htmltools::tags$button(
      type = "button",
      class = "override-manager-add-btn",
      `data-action` = "add-card",
      `data-category-index` = index,
      "+ Add Override"
    )
  )
}

#' Build Manager Card
#'
#' Creates a compact card div for a single override in the manager.
#'
#' @param override A list describing the override.
#'
#' @return An htmltools tag.
#' @keywords internal
.build_manager_card <- function(override) {
  override_json <- jsonlite::toJSON(override, auto_unbox = TRUE)

  htmltools::tags$div(
    class = "override-manager-card",
    `data-override` = override_json,
    htmltools::tags$div(
      class = "override-manager-card-top",
      htmltools::tags$div(
        class = "override-manager-card-info",
        htmltools::tags$div(
          class = "override-manager-card-name",
          override$title %||% override$display_name %||% override$name
        ),
        if (!is.null(override$description)) {
          htmltools::tags$div(
            class = "override-manager-card-title",
            override$description
          )
        },
        htmltools::tags$div(
          class = "override-manager-card-type",
          override$input_type %||% "numeric"
        )
      ),
      htmltools::tags$button(
        type = "button",
        class = "override-manager-card-menu-btn",
        `data-action` = "toggle-menu",
        "\u2026"
      )
    )
  )
}

#' Parse Manager State
#'
#' Converts the JSON state sent from JavaScript back into an R
#' \code{override_categories} list with appropriate type coercion.
#'
#' @param raw_state A JSON string representing the categories array.
#'
#' @return A list of override categories in the format expected by openqaly.
#' @keywords internal
.parse_manager_state <- function(raw_state) {
  parsed <- jsonlite::fromJSON(raw_state, simplifyVector = FALSE)

  lapply(parsed, function(cat) {
    overrides <- lapply(cat$overrides %||% list(), function(o) {
      config <- o$input_config %||% list()

      # Coerce numeric config values
      if (!is.null(config$min)) config$min <- as.numeric(config$min)
      if (!is.null(config$max)) config$max <- as.numeric(config$max)
      if (!is.null(config$step_size)) {
        config$step_size <- as.numeric(config$step_size)
      }

      # Coerce default value for numeric types
      default_val <- o$default_value
      if (o$input_type %in% c("numeric", "slider") && !is.null(default_val)) {
        num_val <- suppressWarnings(as.numeric(default_val))
        if (!is.na(num_val)) default_val <- num_val
      }

      result <- list(
        name = o$name,
        title = o$title,
        display_name = o$display_name %||% o$title,
        description = o$description,
        type = o$type %||% "variable",
        strategy = o$strategy %||% "",
        group = o$group %||% "",
        input_type = o$input_type %||% "numeric",
        input_config = config,
        default_value = default_val
      )

      # Preserve overridden_expression if it was set
      if (!is.null(o$overridden_expression)) {
        result$overridden_expression <- o$overridden_expression
      }

      result
    })

    list(
      name = cat$name,
      general = isTRUE(cat$general),
      overrides = overrides
    )
  })
}

#' Override Manager Dependency
#'
#' Returns the HTML dependencies for the override manager, including SortableJS
#' from CDN and local manager CSS/JS assets.
#'
#' @return A list of htmltools htmlDependency objects.
#' @keywords internal
override_manager_dependency <- function() {
  # SortableJS is loaded dynamically in override-manager.js
  # to avoid timing issues with htmlDependency CDN loading
  htmltools::htmlDependency(
    name = "override-manager",
    version = "1.0.0",
    src = c(file = system.file("www", package = "openqalyshiny")),
    script = "override-manager.js",
    stylesheet = "override-manager.css",
    all_files = FALSE
  )
}
