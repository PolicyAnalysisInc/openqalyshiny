#' Run Override Input Demo
#'
#' Launches a Shiny app demonstrating the override input component with
#' all 5 supported input types: numeric, slider, dropdown, formula, and
#' timeframe. Creates a mock model with override categories to show the
#' tabbed card interface.
#'
#' @return Runs a Shiny app (does not return a value).
#'
#' @examples
#' \dontrun{
#' run_override_input_demo()
#' }
#'
#' @export
run_override_input_demo <- function() {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Package 'shiny' is required to run the demo. ",
         "Install it with: install.packages('shiny')")
  }
  if (!requireNamespace("bslib", quietly = TRUE)) {
    stop("Package 'bslib' is required to run the demo. ",
         "Install it with: install.packages('bslib')")
  }

  # Build a mock model with override_categories
  model <- structure(list(
    override_categories = list(
      list(
        name = "Economic Parameters",
        general = TRUE,
        overrides = list(
          list(
            name = "discount_rate",
            display_name = "Discount Rate",
            description = "Annual discount rate for costs and outcomes",
            input_type = "slider",
            default_value = 0.035,
            input_config = list(min = 0, max = 0.1, step = 0.005)
          ),
          list(
            name = "time_horizon",
            display_name = "Time Horizon",
            description = "Duration of the model simulation",
            input_type = "timeframe",
            default_value = "50|year",
            input_config = list(
              min = 1,
              step = 1,
              units = c("day", "week", "month", "year")
            )
          ),
          list(
            name = "currency",
            display_name = "Currency",
            description = "Currency for cost outputs",
            input_type = "dropdown",
            default_value = "USD",
            input_config = list(
              options = c("USD", "EUR", "GBP", "CAD", "AUD")
            )
          )
        )
      ),
      list(
        name = "Clinical Parameters",
        general = FALSE,
        overrides = list(
          list(
            name = "treatment_efficacy",
            display_name = "Treatment Efficacy",
            description = "Relative risk reduction from treatment",
            input_type = "numeric",
            default_value = 0.75,
            input_config = list(min = 0, max = 1, step = 0.01)
          ),
          list(
            name = "mortality_rate",
            display_name = "Mortality Rate",
            description = "Base annual mortality rate",
            input_type = "slider",
            default_value = 0.02,
            input_config = list(min = 0, max = 0.2, step = 0.001)
          )
        )
      ),
      list(
        name = "Advanced",
        general = FALSE,
        overrides = list(
          list(
            name = "cost_formula",
            display_name = "Cost Formula",
            description = "Custom cost calculation formula",
            input_type = "formula",
            default_value = "base_cost + drug_cost * efficacy"
          ),
          list(
            name = "cycle_length",
            display_name = "Cycle Length",
            description = "Length of each model cycle",
            input_type = "timeframe",
            default_value = "1|month",
            input_config = list(
              min = 1,
              step = 1,
              units = c("day", "week", "month", "year")
            )
          )
        )
      )
    )
  ), class = "oq_model")

  # Define UI
  ui <- shiny::fluidPage(
    theme = bslib::bs_theme(version = 5),
    shiny::titlePanel("Override Input Demo"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::h4("Instructions"),
        shiny::tags$ul(
          shiny::tags$li("Each tab is an override category"),
          shiny::tags$li("Cards show individual overrides"),
          shiny::tags$li("Blue-bordered cards are 'general' overrides"),
          shiny::tags$li("Click 'Reset' to restore default values"),
          shiny::tags$li("All input values are shown on the right")
        ),
        shiny::hr(),
        shiny::h4("Input Types Used"),
        shiny::tags$ul(
          shiny::tags$li(shiny::strong("slider:"), "Discount Rate, Mortality Rate"),
          shiny::tags$li(shiny::strong("timeframe:"), "Time Horizon, Cycle Length"),
          shiny::tags$li(shiny::strong("dropdown:"), "Currency"),
          shiny::tags$li(shiny::strong("numeric:"), "Treatment Efficacy"),
          shiny::tags$li(shiny::strong("formula:"), "Cost Formula")
        ),
        width = 4
      ),
      shiny::mainPanel(
        overrideInput("overrides", model = model, width = "100%"),
        shiny::hr(),
        shiny::h4("Current Input Values"),
        shiny::verbatimTextOutput("all_values"),
        width = 8
      )
    )
  )

  # Define server
  server <- function(input, output, session) {
    output$all_values <- shiny::renderPrint({
      # Collect all override values using the same ID builder as overrideInput
      override_names <- c(
        "discount_rate", "time_horizon", "currency",
        "treatment_efficacy", "mortality_rate",
        "cost_formula", "cycle_length"
      )

      values <- lapply(override_names, function(name) {
        override <- list(name = name, strategy = "", group = "")
        id <- .build_override_id("overrides", override)
        input[[id]]
      })
      names(values) <- override_names

      utils::str(values)
    })
  }

  # Run the app
  shiny::shinyApp(ui = ui, server = server)
}
