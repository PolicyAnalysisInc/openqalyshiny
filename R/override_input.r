#' Override Input
#'
#' Generates a tabbed interface for model override categories, with each
#' override displayed as a card containing the appropriate input control.
#' This is a container generator - each override becomes its own independent
#' Shiny input accessible via \code{input$\{inputId\}_{name}}.
#'
#' @param inputId The base input ID. Individual overrides will be accessible
#'   as \code{input$\{inputId\}_{override_name}}.
#' @param model An openqaly model object that contains \code{override_categories}.
#' @param width The width of the container, e.g., '400px' or '100\%'.
#'   Default is NULL which uses the default width.
#'
#' @return A Shiny tag list containing a tabbed interface with override cards.
#'
#' @examples
#' \dontrun{
#' # Run the interactive demo:
#' run_override_input_demo()
#' }
#'
#' @seealso \code{\link{updateOverrideInput}}, \code{\link{run_override_input_demo}}
#'
#' @export
#' @importFrom htmltools htmlDependency tags tagList
overrideInput <- function(inputId, model, width = NULL) {
  categories <- openqaly::get_override_categories(model)
  if (length(categories) == 0) {
    stop("model must contain 'override_categories'")
  }

  # Build nav panels for each category
  nav_panels <- lapply(categories, function(cat) {
    # Build cards for each override in this category
    cards <- lapply(cat$overrides, function(override) {
      .build_override_card(inputId, override, model, is_general = isTRUE(cat$general))
    })

    do.call(bslib::nav_panel, c(list(title = cat$name), cards))
  })

  # Build the tabset
  tabset <- do.call(bslib::navset_card_tab, nav_panels)

  # Build style attribute
  style <- NULL
  if (!is.null(width)) {
    style <- paste0("width: ", htmltools::validateCssUnit(width), ";")
  }

  # Check if any overrides use formula input type
  has_formula <- any(vapply(categories, function(cat) {
    any(vapply(cat$overrides, function(o) {
      identical(o$input_type, "formula")
    }, logical(1)))
  }, logical(1)))

  # Build dependency list
  deps <- list(override_input_dependency())
  deps <- c(deps, list(override_manager_dependency()))
  if (has_formula) {
    deps <- c(deps, formula_input_dependency())
  }

  # Build manage bar with inline button below tabs
  manage_bar <- htmltools::tags$div(
    class = "override-manage-bar",
    htmltools::tags$button(
      type = "button",
      class = "override-manage-btn",
      `data-input-id` = inputId,
      title = "Manage overrides",
      htmltools::tags$svg(
        xmlns = "http://www.w3.org/2000/svg",
        width = "12",
        height = "12",
        viewBox = "0 0 16 16",
        fill = "currentColor",
        htmltools::tags$path(
          d = "M8 4.754a3.246 3.246 0 1 0 0 6.492 3.246 3.246 0 0 0 0-6.492zM5.754 8a2.246 2.246 0 1 1 4.492 0 2.246 2.246 0 0 1-4.492 0z"
        ),
        htmltools::tags$path(
          d = "M9.796 1.343c-.527-1.79-3.065-1.79-3.592 0l-.094.319a.873.873 0 0 1-1.255.52l-.292-.16c-1.64-.892-3.433.902-2.54 2.541l.159.292a.873.873 0 0 1-.52 1.255l-.319.094c-1.79.527-1.79 3.065 0 3.592l.319.094a.873.873 0 0 1 .52 1.255l-.16.292c-.892 1.64.901 3.434 2.541 2.54l.292-.159a.873.873 0 0 1 1.255.52l.094.319c.527 1.79 3.065 1.79 3.592 0l.094-.319a.873.873 0 0 1 1.255-.52l.292.16c1.64.893 3.434-.902 2.54-2.541l-.159-.292a.873.873 0 0 1 .52-1.255l.319-.094c1.79-.527 1.79-3.065 0-3.592l-.319-.094a.873.873 0 0 1-.52-1.255l.16-.292c.893-1.64-.902-3.433-2.541-2.54l-.292.159a.873.873 0 0 1-1.255-.52l-.094-.319zm-2.633.283c.246-.835 1.428-.835 1.674 0l.094.319a1.873 1.873 0 0 0 2.693 1.115l.291-.16c.764-.415 1.6.42 1.184 1.185l-.159.292a1.873 1.873 0 0 0 1.116 2.692l.318.094c.835.246.835 1.428 0 1.674l-.319.094a1.873 1.873 0 0 0-1.115 2.693l.16.291c.415.764-.42 1.6-1.185 1.184l-.291-.159a1.873 1.873 0 0 0-2.693 1.116l-.094.318c-.246.835-1.428.835-1.674 0l-.094-.319a1.873 1.873 0 0 0-2.692-1.115l-.292.16c-.764.415-1.6-.42-1.184-1.185l.159-.291A1.873 1.873 0 0 0 1.945 8.93l-.319-.094c-.835-.246-.835-1.428 0-1.674l.319-.094A1.873 1.873 0 0 0 3.06 4.377l-.16-.292c-.415-.764.42-1.6 1.185-1.184l.292.159a1.873 1.873 0 0 0 2.692-1.116l.094-.318z"
        )
      ),
      " Manage"
    )
  )

  # Wrap in container div
  container <- htmltools::tags$div(
    id = paste0(inputId, "-container"),
    class = "override-input-container",
    style = style,
    tabset,
    manage_bar
  )

  htmltools::tagList(deps, container)
}

#' Update Override Input
#'
#' Update the value of a specific override within an override input container.
#'
#' @param session The session object passed to the Shiny server function.
#' @param inputId The base input ID used when creating the override input.
#' @param override_name The name of the specific override to update.
#' @param value The new value for the override.
#'
#' @examples
#' \dontrun{
#' # In server function:
#' updateOverrideInput(session, "overrides", "discount_rate", 0.05)
#' }
#'
#' @seealso \code{\link{overrideInput}}
#'
#' @export
updateOverrideInput <- function(session, inputId, override_name, value,
                               strategy = "", group = "") {
  override <- list(name = override_name, strategy = strategy, group = group)
  override_id <- .build_override_id(inputId, override)

  session$sendCustomMessage("override-input-update", list(
    id = override_id,
    value = value
  ))
}

#' Build Override ID
#'
#' Sanitizes an override name, strategy, and group, and combines with the
#' base input ID to produce a unique Shiny input ID.
#'
#' @param base_id The base input ID.
#' @param override The full override list (must have \code{name}, and
#'   optionally \code{strategy} and \code{group}).
#'
#' @return A character string of the form
#'   \code{base_id_name_strategy_group}.
#' @keywords internal
.build_override_id <- function(base_id, override) {
  sanitize <- function(x) gsub("[^a-zA-Z0-9_]", "_", x %||% "")
  parts <- c(
    base_id,
    sanitize(override$name),
    sanitize(override$strategy),
    sanitize(override$group)
  )
  paste(parts, collapse = "_")
}

#' Build Override Card
#'
#' Creates a bslib card for a single override containing header info and
#' the appropriate input widget.
#'
#' @param inputId The base input ID.
#' @param override A list describing the override (name, display_name,
#'   description, input_type, input_config, default_value).
#' @param model The model object.
#' @param is_general Whether this override is in a general category.
#'
#' @return A bslib card element.
#' @keywords internal
.build_override_card <- function(inputId, override, model, is_general = FALSE) {
  override_id <- .build_override_id(inputId, override)

  # Determine CSS class
  card_class <- "override-card"
  if (is_general) {
    card_class <- paste(card_class, "override-card-general")
  }

  # Build the input widget
  input_widget <- .build_override_input(override_id, override, model)

  # Determine default value for reset button data attribute
  default_val <- if (!is.null(override$overridden_expression)) {
    as.character(override$overridden_expression)
  } else if (!is.null(override$default_value)) {
    as.character(override$default_value)
  } else {
    ""
  }

  # Build reset button
  reset_btn <- htmltools::tags$button(
    type = "button",
    class = "btn btn-sm btn-outline-secondary override-reset-btn",
    `data-override-id` = override_id,
    `data-default-value` = default_val,
    `data-input-type` = override$input_type,
    "Reset"
  )

  # Build card header content
  header_content <- htmltools::tags$div(
    class = "override-card-header",
    htmltools::tags$div(
      class = "override-card-title-row",
      htmltools::tags$span(
        class = "override-card-title",
        override$title %||% override$display_name %||% override$name
      ),
      reset_btn
    ),
    if (!is.null(override$description)) {
      htmltools::tags$p(
        class = "override-card-description",
        override$description
      )
    }
  )

  # Build the card
  bslib::card(
    class = card_class,
    bslib::card_header(header_content),
    bslib::card_body(input_widget)
  )
}

#' Build Override Input Widget
#'
#' Creates the appropriate Shiny input widget based on the override's input_type.
#'
#' @param override_id The full input ID for this override.
#' @param override A list describing the override.
#' @param model The model object.
#'
#' @param current_value Optional current value to render instead of the model's
#'   persisted override/default value.
#'
#' @return A Shiny input element.
#' @keywords internal
.build_override_input <- function(override_id, override, model, current_value = NULL) {
  config <- override$input_config %||% list()
  default_val <- current_value %||% override$overridden_expression %||% override$default_value

  switch(override$input_type,
    "numeric" = {
      shiny::numericInput(
        inputId = override_id,
        label = NULL,
        value = default_val,
        min = config$min,
        max = config$max,
        step = config$step %||% config$step_size %||% NA,
        updateOn = "blur"
      )
    },
    "slider" = {
      slider_tag <- shiny::sliderInput(
        inputId = override_id,
        label = NULL,
        value = as.numeric(default_val %||% config$min %||% 0),
        min = config$min %||% 0,
        max = config$max %||% 1,
        step = config$step_size %||% config$step %||% 0.01
      )

      htmltools::tagQuery(slider_tag)$
        find("input.js-range-slider")$
        removeClass("js-range-slider")$
        addClass("override-slider-input")$
        addAttrs(`data-commit-mode` = "finish")$
        allTags()
    },
    "dropdown" = {
      choices <- config$options %||% list()
      if (length(choices) > 0 && is.list(choices[[1]])) {
        values <- vapply(choices, function(x) as.character(x$value), character(1))
        labels <- vapply(choices, function(x) x$label %||% as.character(x$value), character(1))
        choices <- stats::setNames(values, labels)
      }
      .editor_select_input(
        inputId = override_id,
        label = NULL,
        choices = choices,
        selected = default_val
      )
    },
    "formula" = {
      is_oq_model <- inherits(model, c("oq_model", "oq_model_builder"))
      formulaInput(
        inputId = override_id,
        value = default_val %||% "",
        model = if (is_oq_model) model else NULL,
        context = if (is_oq_model) "override" else NULL,
        updateOn = "blur"
      )
    },
    "timeframe" = {
      parsed <- .parse_timeframe_value(default_val %||% "1|year")
      units <- config$units %||% c("day", "week", "month", "year")

      # Build unit options
      unit_options <- lapply(units, function(u) {
        htmltools::tags$option(
          value = u,
          selected = if (u == parsed$unit) "selected" else NULL,
          u
        )
      })

      htmltools::tags$div(
        class = "timeframe-input",
        id = override_id,
        `data-value` = default_val %||% "1|year",
        htmltools::tags$input(
          type = "number",
          class = "form-control timeframe-number",
          value = parsed$number,
          min = config$min %||% 0,
          step = config$step %||% 1
        ),
        do.call(htmltools::tags$select, c(
          list(class = "form-select timeframe-unit"),
          unit_options
        ))
      )
    },
    # Default fallback
    {
      htmltools::tags$div(
        class = "text-muted",
        paste("Unsupported input type:", override$input_type)
      )
    }
  )
}

#' Parse Timeframe Value
#'
#' Splits a timeframe value string in "number|unit" format.
#'
#' @param value A string in "number|unit" format.
#'
#' @return A list with \code{number} and \code{unit} elements.
#' @keywords internal
.parse_timeframe_value <- function(value) {
  parts <- strsplit(as.character(value), "\\|")[[1]]
  if (length(parts) == 2) {
    list(number = parts[1], unit = parts[2])
  } else {
    list(number = parts[1], unit = "year")
  }
}

#' Override Input Dependency
#'
#' Returns the HTML dependency for override input JS and CSS assets.
#'
#' @return An htmltools htmlDependency object.
#' @keywords internal
override_input_dependency <- function() {
  htmltools::htmlDependency(
    name = "override-input",
    version = "1.0.0",
    src = c(file = system.file("www", package = "openqalyshiny")),
    script = "override-input.js",
    stylesheet = "override-input.css",
    all_files = FALSE
  )
}
