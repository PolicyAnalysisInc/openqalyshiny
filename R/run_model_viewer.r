#' Run Model Viewer
#'
#' Launches a standalone Shiny application for viewing and interacting with
#' an openqaly model. Provides a full interface with overrides on the left
#' and results on the right.
#'
#' @param model An openqaly model object. If NULL, a file browser will be
#'   shown to select a model directory.
#' @param model_dir Path to a model directory. Used to load a model via
#'   \code{openqaly::read_model()} if \code{model} is not provided.
#' @param example If TRUE, loads the example Markov model bundled with
#'   the openqaly package. Ignored if \code{model} or \code{model_dir}
#'   is provided.
#'
#' @return A Shiny app object (returned invisibly when run interactively).
#'
#' @examples
#' \dontrun{
#' # Quick start with the example model
#' run_model_viewer(example = TRUE)
#'
#' # With a model object
#' model <- openqaly::read_model("path/to/model")
#' run_model_viewer(model = model)
#'
#' # With a directory path
#' run_model_viewer(model_dir = "path/to/model")
#'
#' # With file browser
#' run_model_viewer()
#' }
#'
#' @export
run_model_viewer <- function(model = NULL, model_dir = NULL, example = FALSE) {

  # Load example model if requested
  if (is.null(model) && is.null(model_dir) && isTRUE(example)) {
    model <- build_example_model()
  }

  # Load model from directory if provided
  if (is.null(model) && !is.null(model_dir)) {
    model <- openqaly::read_model(model_dir)
  }

  ui <- bslib::page_fluid(
    theme = bslib::bs_theme(version = 5),
    shiny::tags$h3("Model Viewer", class = "mb-3 mt-2"),

    # File browser section (only if no model provided)
    if (is.null(model)) {
      shiny::tagList(
        bslib::card(
          bslib::card_body(
            bslib::layout_columns(
              col_widths = c(8, 4),
              shiny::textInput("model_path", "Model Directory", placeholder = "Enter path to model directory..."),
              shiny::actionButton("load_model", "Load Model", class = "btn-primary mt-auto mb-3")
            )
          )
        )
      )
    },

    modelViewerUI("viewer")
  )

  server <- function(input, output, session) {
    loaded_model <- shiny::reactiveVal(model)

    # File browser: load model from text input path
    if (is.null(model)) {
      shiny::observeEvent(input$load_model, {
        path <- input$model_path
        if (is.null(path) || nchar(trimws(path)) == 0) {
          shiny::showNotification("Please enter a model directory path.", type = "warning")
          return()
        }
        tryCatch({
          m <- openqaly::read_model(trimws(path))
          loaded_model(m)
          shiny::showNotification("Model loaded successfully.", type = "message")
        }, error = function(e) {
          shiny::showNotification(
            paste("Failed to load model:", conditionMessage(e)),
            type = "error",
            duration = 10
          )
        })
      })
    }

    modelViewerServer("viewer", model = loaded_model)
  }

  shiny::shinyApp(ui = ui, server = server)
}

#' Build Example Model for Model Viewer
#'
#' Creates a 3-state Markov model with groups, strategy differentiation via
#' treatment relative risk, per-state QALY/cost breakdown, and override
#' categories for demonstrating the model viewer.
#'
#' @return An openqaly model object.
#' @keywords internal
build_example_model <- function() {
  openqaly::define_model("markov") |>
    openqaly::set_settings(
      n_cycles = 50,
      cycle_length = 1,
      cycle_length_unit = "years",
      discount_cost = 3,
      discount_outcomes = 3
    ) |>
    # States
    openqaly::add_state("healthy", initial_prob = 1) |>
    openqaly::add_state("sick", initial_prob = 0) |>
    openqaly::add_state("dead", initial_prob = 0) |>
    # Strategies
    openqaly::add_strategy("standard", "Standard Care") |>
    openqaly::add_strategy("intervention", "Intervention") |>
    # Groups
    openqaly::add_group("young_adults", "Young Adults (30-50)", weight = "0.6") |>
    openqaly::add_group("elderly", "Elderly (65+)", weight = "0.4") |>
    # -- Treatment effect variables --
    # treatment_rr is a shared overridable parameter (default 0.5)
    # rr is strategy-specific: 1 for standard (no effect), treatment_rr for intervention
    openqaly::add_variable("treatment_rr", 0.5) |>
    openqaly::add_variable("rr", 1, strategy = "standard") |>
    openqaly::add_variable("rr", treatment_rr, strategy = "intervention") |>
    # -- Group-specific disease risk (indirection pattern) --
    # Overridable top-level parameters
    openqaly::add_variable("p_disease_young", 0.03) |>
    openqaly::add_variable("p_disease_elderly", 0.08) |>
    # Group-resolved base risk
    openqaly::add_variable("p_disease_base", p_disease_young, group = "young_adults") |>
    openqaly::add_variable("p_disease_base", p_disease_elderly, group = "elderly") |>
    # Effective disease probability (base * treatment effect)
    openqaly::add_variable("p_disease", p_disease_base * rr) |>
    # -- Group-specific death probabilities --
    openqaly::add_variable("p_death_healthy", 0.005, group = "young_adults") |>
    openqaly::add_variable("p_death_healthy", 0.02, group = "elderly") |>
    openqaly::add_variable("p_death_sick", 0.05, group = "young_adults") |>
    openqaly::add_variable("p_death_sick", 0.15, group = "elderly") |>
    # -- Utility variables --
    openqaly::add_variable("utility_healthy", 0.95) |>
    openqaly::add_variable("utility_sick", 0.5) |>
    # -- Cost variables --
    openqaly::add_variable("cost_care", 2000) |>
    openqaly::add_variable("cost_drug_premium", 3000) |>
    openqaly::add_variable("cost_drug", 0, strategy = "standard") |>
    openqaly::add_variable("cost_drug", cost_drug_premium, strategy = "intervention") |>
    # Transitions
    openqaly::add_transition("healthy", "sick", p_disease) |>
    openqaly::add_transition("healthy", "dead", p_death_healthy) |>
    openqaly::add_transition("healthy", "healthy", C) |>
    openqaly::add_transition("sick", "dead", p_death_sick) |>
    openqaly::add_transition("sick", "sick", C) |>
    openqaly::add_transition("dead", "dead", 1) |>
    # Values (per-state breakdown)
    openqaly::add_value(
      "cost_healthy", cost_care + cost_drug,
      state = "healthy", type = "cost",
      display_name = "Cost (Healthy)"
    ) |>
    openqaly::add_value(
      "cost_sick", cost_care,
      state = "sick", type = "cost",
      display_name = "Cost (Sick)"
    ) |>
    openqaly::add_value(
      "qalys_healthy", utility_healthy,
      state = "healthy", type = "outcome",
      display_name = "QALYs (Healthy)"
    ) |>
    openqaly::add_value(
      "qalys_sick", utility_sick,
      state = "sick", type = "outcome",
      display_name = "QALYs (Sick)"
    ) |>
    # Summaries (aggregate per-state values)
    openqaly::add_summary("total_cost", "cost_healthy,cost_sick", type = "cost") |>
    openqaly::add_summary("total_qalys", "qalys_healthy,qalys_sick", type = "outcome") |>
    # Override categories
    openqaly::add_override_category("Clinical Parameters") |>
    openqaly::add_override_category("Economic Parameters") |>
    # Clinical overrides
    openqaly::add_override(
      "Clinical Parameters",
      title = "Treatment Relative Risk",
      name = "treatment_rr",
      input_type = "slider",
      expression = 0.5,
      min = 0.1, max = 1.0, step_size = 0.05
    ) |>
    openqaly::add_override(
      "Clinical Parameters",
      title = "Disease Risk (Young)",
      name = "p_disease_young",
      input_type = "slider",
      expression = 0.03,
      min = 0.005, max = 0.15, step_size = 0.005
    ) |>
    openqaly::add_override(
      "Clinical Parameters",
      title = "Disease Risk (Elderly)",
      name = "p_disease_elderly",
      input_type = "slider",
      expression = 0.08,
      min = 0.01, max = 0.25, step_size = 0.005
    ) |>
    openqaly::add_override(
      "Clinical Parameters",
      title = "Utility (Healthy)",
      name = "utility_healthy",
      input_type = "slider",
      expression = 0.95,
      min = 0.5, max = 1.0, step_size = 0.01
    ) |>
    openqaly::add_override(
      "Clinical Parameters",
      title = "Utility (Sick)",
      name = "utility_sick",
      input_type = "slider",
      expression = 0.5,
      min = 0.1, max = 0.8, step_size = 0.01
    ) |>
    # Economic overrides
    openqaly::add_override(
      "Economic Parameters",
      title = "Annual Care Cost",
      name = "cost_care",
      input_type = "numeric",
      expression = 2000,
      min = 500, max = 10000
    ) |>
    openqaly::add_override(
      "Economic Parameters",
      title = "Intervention Drug Premium",
      name = "cost_drug_premium",
      input_type = "numeric",
      expression = 3000,
      min = 500, max = 15000
    )
}
