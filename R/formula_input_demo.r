#' Run Formula Input Demo
#'
#' Launches a Shiny app demonstrating the formula input component with
#' typeahead autocomplete suggestions. The demo includes R functions from
#' base and stats packages, plus example variables, keywords, and tables.
#'
#' @param packages Character vector of package names to include R functions
#'   from. Defaults to c("base", "stats").
#'
#' @return Runs a Shiny app (does not return a value).
#'
#' @examples
#' \dontrun{
#' # Run the demo with default packages
#' run_formula_input_demo()
#'
#' # Run with additional packages
#' run_formula_input_demo(c("base", "stats", "utils"))
#' }
#'
#' @export
run_formula_input_demo <- function(packages = c("base", "stats")) {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Package 'shiny' is required to run the demo. ",
         "Install it with: install.packages('shiny')")
  }


  # Get R function suggestions from specified packages
  r_funcs <- get_r_function_suggestions(packages)

  # Example domain-specific suggestions
  variables <- data.frame(
    name = c("age", "cost", "utility", "discount_rate", "qaly"),
    label = c("age", "cost", "utility", "discount_rate", "qaly"),
    description = c(
      "Patient age in years",
      "Treatment cost in dollars",
      "Health utility score (0-1)",
      "Annual discount rate for future values",
      "Quality-adjusted life years"
    ),
    signature = c(
      "age * mortality_rate",
      "cost + overhead",
      "utility * duration",
      "discount_rate ^ cycle",
      "qaly = utility * life_years"
    ),
    stringsAsFactors = FALSE
  )

  keywords <- data.frame(
    name = c("cycle", "day", "week", "month", "year", "state"),
    label = c("cycle", "day", "week", "month", "year", "state"),
    description = c(
      "Current model cycle number",
      "Time unit: day",
      "Time unit: week",
      "Time unit: month",
      "Time unit: year",
      "Current health state"
    ),
    stringsAsFactors = FALSE
  )

  tables <- data.frame(
    name = c("patients", "costs", "transitions", "mortality"),
    label = c("patients", "costs", "transitions", "mortality"),
    description = c(
      "Patient cohort data frame",
      "Cost lookup table",
      "State transition probability matrix",
      "Age-specific mortality rates"
    ),
    stringsAsFactors = FALSE
  )

  # Combine all suggestions
  suggestions <- list(
    r_function = r_funcs,
    variable = variables,
    keyword = keywords,
    table = tables
  )

  # Also use terms for syntax highlighting

  terms <- list(
    variable = variables$name,
    keyword = keywords$name,
    table = tables$name
  )

  # Define UI

  ui <- shiny::fluidPage(
    shiny::titlePanel("Formula Input Demo"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::h4("Instructions"),
        shiny::tags$ul(
          shiny::tags$li("Type to see autocomplete suggestions"),
          shiny::tags$li("Press Tab to accept a suggestion"),
          shiny::tags$li("Press Enter to submit the formula"),
          shiny::tags$li("Suggestions include R functions and domain terms")
        ),
        shiny::hr(),
        shiny::h4("Suggestion Categories"),
        shiny::tags$ul(
          shiny::tags$li(shiny::strong("r_function:"),
                         paste(nrow(r_funcs), "R functions")),
          shiny::tags$li(shiny::strong("variable:"),
                         paste(variables$name, collapse = ", ")),
          shiny::tags$li(shiny::strong("keyword:"),
                         paste(keywords$name, collapse = ", ")),
          shiny::tags$li(shiny::strong("table:"),
                         paste(tables$name, collapse = ", "))
        ),
        width = 4
      ),
      shiny::mainPanel(
        shiny::h4("Enter Formula"),
        formulaInput(
          inputId = "formula",
          value = "",
          placeholder = "Type a formula (e.g., sum(cost * discount_rate))",
          width = "100%",
          terms = terms,
          suggestions = suggestions
        ),
        shiny::hr(),
        shiny::h4("Formula Value"),
        shiny::verbatimTextOutput("formula_value"),
        shiny::h4("Validation Status"),
        shiny::verbatimTextOutput("formula_valid"),
        width = 8
      )
    )
  )

  # Define server

  server <- function(input, output, session) {
    output$formula_value <- shiny::renderText({
      shiny::req(input$formula)
      input$formula$value
    })

    output$formula_valid <- shiny::renderText({
      shiny::req(input$formula)
      if (input$formula$valid) {
        "Valid (brackets balanced)"
      } else {
        "Invalid (unbalanced brackets)"
      }
    })
  }

  # Run the app

  shiny::shinyApp(ui = ui, server = server)
}

#' Run Model-Aware Formula Input Demo
#'
#' Launches a Shiny app demonstrating the model-aware formula input component.
#' This demo uses a real openqaly model and allows switching between different
#' formula contexts to see how suggestions and syntax highlighting change.
#'
#' @return Runs a Shiny app (does not return a value).
#'
#' @examples
#' \dontrun{
#' # Run the model-aware demo
#' run_model_formula_demo()
#' }
#'
#' @export
run_model_formula_demo <- function() {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Package 'shiny' is required to run the demo. ",
         "Install it with: install.packages('shiny')")
  }

  if (!requireNamespace("openqaly", quietly = TRUE)) {
    stop("Package 'openqaly' is required to run this demo. ",
         "Install it from GitHub: devtools::install_github('openqaly/openqaly')")
  }

  # Build demo model - a simple Markov model with states, variables, tables
  model <- openqaly::define_model("markov") |>
    openqaly::set_settings(n_cycles = 50, cycle_length = "year") |>
    openqaly::add_state("healthy", initial_prob = 1) |>
    openqaly::add_state("sick", initial_prob = 0) |>
    openqaly::add_state("dead", initial_prob = 0) |>
    openqaly::add_strategy("standard") |>
    openqaly::add_strategy("intervention") |>
    openqaly::add_variable("p_disease", 0.1,
      display_name = "Disease Probability",
      description = "Annual probability of transitioning from healthy to sick") |>
    openqaly::add_variable("p_death_healthy", 0.01,
      display_name = "Death Probability (Healthy)",
      description = "Annual probability of death while healthy") |>
    openqaly::add_variable("p_death_sick", 0.05,
      display_name = "Death Probability (Sick)",
      description = "Annual probability of death while sick") |>
    openqaly::add_variable("base_cost", 500,
      display_name = "Base Cost",
      description = "Annual baseline healthcare cost") |>
    openqaly::add_variable("sick_cost", 2000,
      display_name = "Sick State Cost",
      description = "Additional annual cost when in sick state") |>
    openqaly::add_variable("drug_cost", 1000, strategy = "standard",
      display_name = "Drug Cost",
      description = "Annual drug treatment cost") |>
    openqaly::add_variable("drug_cost", 5000, strategy = "intervention",
      display_name = "Drug Cost",
      description = "Annual drug treatment cost") |>
    openqaly::add_variable("utility_healthy", 0.9,
      display_name = "Healthy Utility",
      description = "Quality of life utility score when healthy") |>
    openqaly::add_variable("utility_sick", 0.6,
      display_name = "Sick Utility",
      description = "Quality of life utility score when sick") |>
    openqaly::add_table("mortality", data.frame(
      age = c(50, 60, 70, 80),
      rate = c(0.01, 0.02, 0.04, 0.08)
    )) |>
    openqaly::add_transition("healthy", "sick", p_disease) |>
    openqaly::add_transition("healthy", "dead", p_death_healthy) |>
    openqaly::add_transition("sick", "dead", p_death_sick) |>
    openqaly::add_value("qalys", utility_healthy, state = "healthy",
                        type = "outcome") |>
    openqaly::add_value("qalys", utility_sick, state = "sick",
                        type = "outcome") |>
    openqaly::add_value("total_cost", base_cost + drug_cost, type = "cost")

  # Context descriptions for the info panel
  context_descriptions <- list(
    variable = "Variable formulas define model parameters. Time keywords and model components are available, but NOT the 'bc' keyword.",
    value = "Value formulas define outcomes (QALYs, costs). All time keywords, model variables, tables, and other values are available.",
    transition_markov = "Markov transition formulas define state transition probabilities. Includes state_* time keywords for time spent in current state.",
    initial_probability = "Initial probability formulas set starting state distributions. NO keywords available - only variables and tables.",
    dsa_bound = "DSA bound formulas define sensitivity analysis ranges. Only 'bc' (base case) keyword is available."
  )

  # Define UI
  ui <- shiny::fluidPage(
    shiny::titlePanel("Model-Aware Formula Input Demo"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::selectInput("context", "Formula Context:",
          choices = c(
            "Variable Formula" = "variable",
            "Value Formula" = "value",
            "Markov Transition" = "transition_markov",
            "Initial Probability" = "initial_probability",
            "DSA Bound" = "dsa_bound"
          )
        ),
        shiny::hr(),
        shiny::h4("Context Info"),
        shiny::uiOutput("context_description"),
        shiny::hr(),
        shiny::h4("Available Terms"),
        shiny::uiOutput("context_info"),
        width = 4
      ),
      shiny::mainPanel(
        shiny::h4("Enter Formula"),
        shiny::uiOutput("formula_ui"),
        shiny::hr(),
        shiny::h4("Formula Value"),
        shiny::verbatimTextOutput("formula_value"),
        shiny::h4("Validation Status"),
        shiny::verbatimTextOutput("formula_valid"),
        width = 8
      )
    )
  )

  # Define server
  server <- function(input, output, session) {
    # Reactive formula input that updates when context changes
    output$formula_ui <- shiny::renderUI({
      formulaInput("formula",
        placeholder = "Type formula...",
        width = "100%",
        model = model,
        context = input$context
      )
    })

    # Show context description
    output$context_description <- shiny::renderUI({
      desc <- context_descriptions[[input$context]]
      shiny::tags$p(shiny::tags$em(desc))
    })

    # Show available terms for current context
    output$context_info <- shiny::renderUI({
      terms <- get_model_terms(model, input$context)

      keyword_text <- if (length(terms$keyword) > 0) {
        paste(terms$keyword, collapse = ", ")
      } else {
        "None"
      }

      variable_text <- if (length(terms$variable) > 0) {
        paste(terms$variable, collapse = ", ")
      } else {
        "None"
      }

      table_text <- if (length(terms$table) > 0) {
        paste(terms$table, collapse = ", ")
      } else {
        "None"
      }

      value_text <- if (length(terms$value) > 0) {
        paste(terms$value, collapse = ", ")
      } else {
        "None"
      }

      shiny::tagList(
        shiny::tags$p(shiny::strong("Keywords: "), keyword_text),
        shiny::tags$p(shiny::strong("Variables: "), variable_text),
        shiny::tags$p(shiny::strong("Tables: "), table_text),
        shiny::tags$p(shiny::strong("Values: "), value_text)
      )
    })

    # Display formula value
    output$formula_value <- shiny::renderText({
      shiny::req(input$formula)
      input$formula$value
    })

    # Display validation status
    output$formula_valid <- shiny::renderText({
      shiny::req(input$formula)
      if (input$formula$valid) {
        "Valid (brackets balanced)"
      } else {
        "Invalid (unbalanced brackets)"
      }
    })
  }

  # Run the app
  shiny::shinyApp(ui = ui, server = server)
}
