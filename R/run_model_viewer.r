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

  # --- UI ---
  ui <- bslib::page_navbar(
    title = "Model Viewer",
    theme = bslib::bs_theme(version = 5),
    sidebar = bslib::sidebar(
      width = "33%",
      # File browser (only if no model provided at launch)
      if (is.null(model)) {
        shiny::tagList(
          bslib::card(
            bslib::card_body(
              shiny::textInput("model_path", "Model Directory",
                placeholder = "Enter path to model directory..."
              ),
              shiny::actionButton("load_model", "Load Model",
                class = "btn-primary mt-2"
              )
            )
          )
        )
      },
      shiny::uiOutput("override_panel")
    ),

    # Base Case page
    bslib::nav_panel(
      "Base Case",
      shiny::conditionalPanel(
        condition = "!output.has_base_results",
        shiny::tags$div(
          class = "text-muted p-3",
          "No model loaded."
        )
      ),
      shiny::conditionalPanel(
        condition = "output.has_base_results",
        bslib::navset_card_tab(
          bslib::nav_panel("Trace", traceResultsUI("trace")),
          bslib::nav_panel("Outcomes", outcomesResultsUI("outcomes")),
          bslib::nav_panel("Costs", costsResultsUI("costs")),
          bslib::nav_panel("NMB", nmbResultsUI("nmb")),
          bslib::nav_panel("Pairwise CE", pairwiseCeResultsUI("pairwise_ce")),
          bslib::nav_panel("Incremental CE", incrementalCeResultsUI("incremental_ce"))
        )
      )
    ),

    # VBP page
    bslib::nav_panel(
      "VBP",
      shiny::conditionalPanel(
        condition = "!output.has_base_results",
        shiny::tags$div(
          class = "text-muted p-3",
          "No model loaded. Load a model to configure VBP analysis."
        )
      ),
      shiny::conditionalPanel(
        condition = "output.has_base_results",
        bslib::navset_card_tab(
          bslib::nav_panel("Configuration",
            shiny::uiOutput("vbp_config"),
            shiny::actionButton("run_vbp", "Run VBP Analysis",
              class = "btn-primary mt-2"
            )
          ),
          bslib::nav_panel("Results", vbpResultsUI("vbp"))
        )
      )
    ),

    # DSA page
    bslib::nav_panel(
      "DSA",
      shiny::conditionalPanel(
        condition = "!output.has_base_results",
        shiny::tags$div(
          class = "text-muted p-3",
          "No model loaded. Load a model to configure DSA analysis."
        )
      ),
      shiny::conditionalPanel(
        condition = "output.has_base_results",
        bslib::navset_card_tab(
          bslib::nav_panel("Inputs",
            shiny::uiOutput("dsa_inputs"),
            shiny::tags$button(
              type = "button",
              class = "btn btn-primary mt-2 w-100 dsa-run-btn",
              "Run DSA Analysis"
            )
          ),
          bslib::nav_panel("Outcomes", dsaResultTabUI("dsa_outcomes")),
          bslib::nav_panel("Costs", dsaResultTabUI("dsa_costs")),
          bslib::nav_panel("NMB", dsaResultTabUI("dsa_nmb")),
          bslib::nav_panel("Cost-Effectiveness", dsaResultTabUI("dsa_ce")),
          bslib::nav_panel("DSA + VBP", dsaResultTabUI("dsa_vbp"))
        )
      )
    ),

    # Model Diff page
    bslib::nav_panel(
      "Model Diff",
      shiny::conditionalPanel(
        condition = "!output.has_base_results",
        shiny::tags$div(
          class = "text-muted p-3",
          "No model loaded."
        )
      ),
      shiny::conditionalPanel(
        condition = "output.has_base_results",
        diffResultsUI("diff")
      )
    )
  )

  # --- Server ---
  server <- function(input, output, session) {

    # Make transition probability violations raise errors instead of warnings
    options(openqaly.error_mode = "checkpoint")

    # Shared state
    model_rv <- shiny::reactiveVal(model)
    original_model_rv <- shiny::reactiveVal(model)

    base_case_rv <- shiny::reactiveValues(
      results = NULL,
      metadata = NULL
    )

    vbp_rv <- shiny::reactiveValues(
      results = NULL
    )

    dsa_rv <- shiny::reactiveValues(
      results = NULL,
      parameters = list()
    )

    # ---- File browser: load model from text input ----
    if (is.null(model)) {
      shiny::observeEvent(input$load_model, {
        path <- input$model_path
        if (is.null(path) || nchar(trimws(path)) == 0) {
          shiny::showNotification("Please enter a model directory path.",
            type = "warning"
          )
          return()
        }
        tryCatch({
          m <- openqaly::read_model(trimws(path))
          model_rv(m)
          original_model_rv(m)
          shiny::showNotification("Model loaded successfully.", type = "message")
        }, error = function(e) {
          shiny::showNotification(
            paste("Failed to load model:", conditionMessage(e)),
            type = "error", duration = 10
          )
        })
      })
    }

    # ---- Override panel ----
    output$override_panel <- shiny::renderUI({
      m <- model_rv()
      if (is.null(m)) return(NULL)
      cats <- openqaly::get_override_categories(m)
      if (length(cats) == 0) {
        return(shiny::tags$div(
          class = "text-muted p-3",
          "This model has no overrides."
        ))
      }
      overrideInput("overrides", m)
    })

    # ---- Override manager module ----
    overrideManagerServer(
      "overrides",
      model = shiny::reactive(model_rv()),
      on_action = function(action) {
        m <- model_rv()
        m <- dispatch_model_action(m, action)
        model_rv(m)
      }
    )

    # ---- Collect override values ----
    override_values <- shiny::reactive({
      m <- model_rv()
      cats <- openqaly::get_override_categories(m)
      if (is.null(m) || length(cats) == 0) return(NULL)

      values <- list()
      for (cat in cats) {
        for (override in cat$overrides) {
          input_id <- .build_override_id("overrides", override)
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

    override_values_debounced <- shiny::debounce(override_values, 1000)

    # ---- Helper: build model with current overrides applied ----
    build_overridden_model <- function() {
      m <- model_rv()
      vals <- override_values_debounced()
      if (is.null(m)) return(NULL)

      if (!is.null(vals) && length(vals) > 0) {
        m <- openqaly::set_override_expressions(m, vals)
      }
      m
    }

    current_model_reactive <- shiny::reactive({
      m <- build_overridden_model()
      if (is.null(m)) return(NULL)
      # Include DSA parameters so they appear in model diff
      params <- dsa_params_reactive()
      if (length(params) > 0) {
        m <- apply_dsa_params(m, params)
      }
      m
    })

    # ---- Initial model run (on load) ----
    shiny::observeEvent(model_rv(), {
      m <- model_rv()
      if (is.null(m)) return()
      base_case_rv$results <- NULL
      base_case_rv$metadata <- NULL
      vbp_rv$results <- NULL
      dsa_rv$results <- NULL

      # Pre-populate DSA parameters from model if defined
      dsa_params <- openqaly::get_dsa_parameters(m)
      if (length(dsa_params) > 0) {
        dsa_rv$parameters <- dsa_params
      } else {
        dsa_rv$parameters <- list()
      }

      tryCatch({
        res <- openqaly::run_model(m)
        base_case_rv$results <- res
        base_case_rv$metadata <- res$metadata
      }, error = function(e) {
        shiny::showNotification(
          paste("Initial model run failed:", conditionMessage(e)),
          type = "error", duration = 10
        )
      })
    })

    # ---- Base case auto-run on override change ----
    shiny::observeEvent(override_values_debounced(), {
      m <- model_rv()
      vals <- override_values_debounced()
      if (is.null(m) || is.null(vals)) return()

      tryCatch({
        updated_model <- build_overridden_model()
        res <- openqaly::run_model(updated_model)
        base_case_rv$results <- res
        base_case_rv$metadata <- res$metadata
      }, error = function(e) {
        base_case_rv$results <- NULL
        shiny::showNotification(
          paste("Model run failed:", conditionMessage(e)),
          type = "error", duration = 10
        )
      })
    }, ignoreNULL = TRUE)

    # ---- VBP config panel ----
    output$vbp_config <- shiny::renderUI({
      meta <- base_case_rv$metadata
      m <- model_rv()
      if (is.null(meta) || is.null(m)) return(NULL)

      var_choices <- get_variable_choices(m)
      strategy_choices <- get_strategy_choices(meta)
      outcome_choices <- get_outcome_summary_choices(meta)
      cost_choices <- get_cost_summary_choices(meta)

      shiny::tagList(
        shiny::selectInput("vbp_price_variable", "Price Variable",
          choices = var_choices,
          selected = if (length(var_choices) > 0) var_choices[1] else NULL
        ),
        shiny::selectInput("vbp_intervention", "Intervention Strategy",
          choices = strategy_choices,
          selected = if (length(strategy_choices) > 1) strategy_choices[2] else
            if (length(strategy_choices) > 0) strategy_choices[1] else NULL
        ),
        shiny::selectInput("vbp_outcome_summary", "Outcome Summary",
          choices = outcome_choices,
          selected = if (length(outcome_choices) > 0) outcome_choices[1] else NULL
        ),
        shiny::selectInput("vbp_cost_summary", "Cost Summary",
          choices = cost_choices,
          selected = if (length(cost_choices) > 0) cost_choices[1] else NULL
        )
      )
    })

    # ---- VBP manual run ----
    shiny::observeEvent(input$run_vbp, {
      shiny::req(
        input$vbp_price_variable,
        input$vbp_intervention,
        input$vbp_outcome_summary,
        input$vbp_cost_summary
      )

      tryCatch({
        updated_model <- build_overridden_model()
        if (is.null(updated_model)) {
          shiny::showNotification("No model loaded.", type = "warning")
          return()
        }

        vbp_res <- openqaly::run_vbp(
          updated_model,
          price_variable = input$vbp_price_variable,
          intervention_strategy = input$vbp_intervention,
          outcome_summary = input$vbp_outcome_summary,
          cost_summary = input$vbp_cost_summary
        )
        vbp_rv$results <- vbp_res

        shiny::showNotification("VBP analysis complete.", type = "message")
      }, error = function(e) {
        vbp_rv$results <- NULL
        shiny::showNotification(
          paste("VBP analysis failed:", conditionMessage(e)),
          type = "error", duration = 10
        )
      })
    })

    # ---- DSA inputs panel ----
    output$dsa_inputs <- shiny::renderUI({
      meta <- base_case_rv$metadata
      m <- model_rv()
      if (is.null(meta) || is.null(m)) return(NULL)

      var_choices <- openqaly::get_variable_names(m)
      var_targeting <- openqaly::get_variable_targeting(m)
      strategy_choices <- get_strategy_choices(meta)
      group_choices <- get_group_choices(meta)
      # Filter out aggregate group options for DSA
      individual_groups <- group_choices[
        !group_choices %in% c("overall", "all", "all_groups")
      ]
      setting_choices <- get_dsa_setting_choices()

      # Serialize current parameters for JS initialization (isolate to break
      # feedback loop: renderUI should not re-fire when dsa_rv$parameters changes)
      params <- shiny::isolate(dsa_rv$parameters)
      initial_json <- if (length(params) > 0) {
        jsonlite::toJSON(params, auto_unbox = TRUE)
      } else {
        "[]"
      }

      # Build the AG Grid container
      param_table <- shiny::tags$div(
        dsa_params_dependency(),
        formula_input_dependency(),
        shiny::tags$div(
          id = "dsa_params_grid",
          class = "dsa-params-container",
          `data-input-id` = "dsa_params",
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
          `data-initial` = initial_json,
          `data-terms` = jsonlite::toJSON(
            get_model_terms(m, "dsa_bound"), auto_unbox = FALSE
          ),
          `data-suggestions` = jsonlite::toJSON(
            get_model_suggestions(m, "dsa_bound"), auto_unbox = FALSE
          )
        ),
        shiny::tags$button(
          type = "button",
          class = "btn btn-sm btn-outline-secondary dsa-add-row-btn",
          "+ Add Parameter"
        )
      )

      # VBP configuration (collapsed)
      vbp_config <- bslib::accordion(
        bslib::accordion_panel(
          "VBP Configuration (for DSA+VBP analysis)",
          shiny::checkboxInput("dsa_include_vbp", "Include VBP Analysis", value = FALSE),
          shiny::conditionalPanel(
            condition = "input.dsa_include_vbp == true",
            shiny::selectInput("dsa_vbp_price_variable", "Price Variable",
              choices = var_choices,
              selected = if (length(var_choices) > 0) var_choices[1] else NULL
            ),
            shiny::selectInput("dsa_vbp_intervention", "Intervention Strategy",
              choices = strategy_choices,
              selected = if (length(strategy_choices) > 1) strategy_choices[2] else
                if (length(strategy_choices) > 0) strategy_choices[1] else NULL
            ),
            shiny::selectInput("dsa_vbp_outcome", "Outcome Summary",
              choices = get_outcome_summary_choices(meta),
              selected = NULL
            ),
            shiny::selectInput("dsa_vbp_cost", "Cost Summary",
              choices = get_cost_summary_choices(meta),
              selected = NULL
            )
          )
        ),
        open = FALSE
      )

      shiny::tagList(param_table, vbp_config)
    })

    # ---- DSA params reactive (direct dependency for model diff) ----
    dsa_params_reactive <- shiny::reactive({
      raw <- input$dsa_params
      js_params <- normalize_dsa_params(raw)
      if (length(js_params) > 0) return(js_params)
      # Before grid renders, use model-loaded defaults
      dsa_rv$parameters
    })

    # ---- DSA run ----
    shiny::observeEvent(input$run_dsa_action, {
      params <- normalize_dsa_params(input$run_dsa_action$params)
      if (length(params) == 0) {
        params <- dsa_params_reactive()
      }
      if (length(params) == 0) {
        shiny::showNotification("Please add at least one parameter.", type = "warning")
        return()
      }

      tryCatch({
        updated_model <- build_overridden_model()
        if (is.null(updated_model)) {
          shiny::showNotification("No model loaded.", type = "warning")
          return()
        }

        # Add DSA parameters to model
        updated_model <- apply_dsa_params(updated_model, params)

        # Build run_dsa args
        dsa_args <- list(updated_model)

        # Add VBP params if enabled
        if (isTRUE(input$dsa_include_vbp)) {
          dsa_args$vbp_price_variable <- input$dsa_vbp_price_variable
          dsa_args$vbp_intervention <- input$dsa_vbp_intervention
          dsa_args$vbp_outcome_summary <- input$dsa_vbp_outcome
          dsa_args$vbp_cost_summary <- input$dsa_vbp_cost
        }

        dsa_res <- do.call(openqaly::run_dsa, dsa_args)
        dsa_rv$results <- dsa_res

        shiny::showNotification("DSA analysis complete.", type = "message")
      }, error = function(e) {
        dsa_rv$results <- NULL
        shiny::showNotification(
          paste("DSA analysis failed:", conditionMessage(e)),
          type = "error", duration = 10
        )
      })
    })

    # ---- Conditional panel flags ----
    output$has_base_results <- shiny::reactive({
      !is.null(base_case_rv$results)
    })
    shiny::outputOptions(output, "has_base_results", suspendWhenHidden = FALSE)

    # ---- Result sub-module servers ----
    results_reactive <- shiny::reactive(base_case_rv$results)
    metadata_reactive <- shiny::reactive(base_case_rv$metadata)
    vbp_results_reactive <- shiny::reactive(vbp_rv$results)

    traceResultsServer("trace", results_reactive, metadata_reactive)
    outcomesResultsServer("outcomes", results_reactive, metadata_reactive)
    costsResultsServer("costs", results_reactive, metadata_reactive)
    nmbResultsServer("nmb", results_reactive, metadata_reactive)
    pairwiseCeResultsServer("pairwise_ce", results_reactive, metadata_reactive)
    incrementalCeResultsServer("incremental_ce", results_reactive, metadata_reactive)
    vbpResultsServer("vbp", vbp_results_reactive, metadata_reactive)
    dsa_results_reactive <- shiny::reactive(dsa_rv$results)
    dsaResultTabServer("dsa_outcomes", "outcomes", dsa_results_reactive, metadata_reactive)
    dsaResultTabServer("dsa_costs", "costs", dsa_results_reactive, metadata_reactive)
    dsaResultTabServer("dsa_nmb", "nmb", dsa_results_reactive, metadata_reactive)
    dsaResultTabServer("dsa_ce", "ce", dsa_results_reactive, metadata_reactive)
    dsaResultTabServer("dsa_vbp", "vbp", dsa_results_reactive, metadata_reactive)
    diffResultsServer("diff", shiny::reactive(original_model_rv()), current_model_reactive)
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
