#' Override Manager Server
#'
#' Shiny module server that provides a full-screen modal for managing override
#' categories and their overrides. Users can create, edit, delete, and reorder
#' overrides via a Kanban-style drag-and-drop interface.
#'
#' @param id The module namespace ID. Must match the base input ID used by
#'   the corresponding \code{\link{overrideInput}}.
#' @param model A reactive expression returning an openqaly model object that
#'   contains \code{override_categories}.
#' @param on_apply A callback function that receives the updated
#'   \code{override_categories} list when the user clicks Apply.
#'
#' @return Invisible NULL. Called for side effects.
#'
#' @export
overrideManagerServer <- function(id, model, on_apply) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Open modal when gear button is clicked
    shiny::observeEvent(input$manage_click, {
      m <- model()
      if (is.null(m)) return()
      categories <- openqaly::get_override_categories(m)
      if (length(categories) == 0) return()

      # Build and show modal
      modal <- .build_manager_modal(ns, categories)
      shiny::showModal(modal)

      # Send categories to JS for rendering
      cat_data <- lapply(categories, function(cat) {
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

      # Strip trailing dash from namespace prefix so JS can construct
      # input IDs like "prefix-manager_state" without double dashes
      input_prefix <- sub("-$", "", ns(""))

      # Build model metadata for form dropdowns
      overridable_settings <- c(
        "discount_cost", "discount_outcomes", "timeframe", "cycle_length"
      )
      available_settings <- intersect(
        overridable_settings, names(openqaly::get_settings(m))
      )

      # Build per-variable targeting info
      targeting <- openqaly::get_variable_targeting(m)
      var_meta <- lapply(names(targeting), function(vname) {
        list(
          name = vname,
          strategies = as.list(targeting[[vname]]$strategies %||% character(0)),
          groups = as.list(targeting[[vname]]$groups %||% character(0))
        )
      })

      strategies <- openqaly::get_strategies(m)
      groups <- openqaly::get_groups(m)
      model_meta <- list(
        variables = var_meta,
        settings = available_settings,
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

      session$sendCustomMessage("override-manager-init", list(
        inputId = input_prefix,
        categories = cat_data,
        model_meta = model_meta
      ))
    })

    # Handle Apply
    shiny::observeEvent(input$manager_state, {
      raw_state <- input$manager_state
      if (is.null(raw_state)) return()

      tryCatch({
        new_categories <- .parse_manager_state(raw_state)
        on_apply(new_categories)
        shiny::removeModal()
      }, error = function(e) {
        shiny::showNotification(
          paste("Failed to apply changes:", conditionMessage(e)),
          type = "error",
          duration = 8
        )
      })
    })

    # Handle Cancel
    shiny::observeEvent(input$manager_cancel, {
      shiny::removeModal()
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
        `data-action` = "manager-cancel",
        "Cancel"
      ),
      htmltools::tags$button(
        type = "button",
        class = "btn btn-primary",
        `data-action` = "manager-apply",
        "Apply"
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
