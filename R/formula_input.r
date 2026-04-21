#' Formula Input
#'
#' A single-line R code editor input with syntax highlighting. This component
#' provides an interactive code editor for entering R formulas or expressions,
#' with R syntax highlighting, bracket matching validation, and automatic
#' newline prevention. Supports custom term highlighting for domain-specific
#' vocabularies and typeahead autocomplete suggestions.
#'
#' @param inputId The input slot that will be used to access the value.
#' @param value Initial value. Default is an empty string.
#' @param placeholder A character string giving the user a hint as to what
#'   can be entered into the input. Default is NULL (no placeholder).
#' @param width The width of the input, e.g., '400px' or '100%'. Default is
#'   NULL which uses the default width.
#' @param terms A named list where names are token types and values are
#'   character vectors of terms to highlight. Token types can include:
#'   \itemize{
#'     \item \code{keyword}: Domain-specific keywords (e.g., "cycle", "state")
#'     \item \code{variable}: Variable names from the model
#'     \item \code{table}: Table/data frame names
#'     \item \code{value}: Named values or constants
#'     \item \code{tree}: Decision tree names
#'   }
#'   Custom token types are also supported and will be styled with the CSS
#'   class \code{ace_oq-<tokentype>}. Ignored if \code{model} and \code{context}
#'   are provided.
#' @param suggestions A named list for typeahead autocomplete suggestions.
#'   Each element should be a data frame with at least a \code{name} column.
#'   Optional columns include:
#'   \itemize{
#'     \item \code{label}: Display text in dropdown (defaults to name)
#'     \item \code{description}: Shown in details panel
#'     \item \code{signature}: Function signature or formula preview
#'     \item \code{package}: Source/package info
#'   }
#'   Suggestion categories can include:
#'   \itemize{
#'     \item \code{r_function}: R functions (use \code{get_r_function_suggestions()})
#'     \item \code{keyword}: Domain-specific keywords
#'     \item \code{table}: Data frame/table names
#'     \item \code{variable}: Model variables
#'     \item \code{value}: Named values/constants
#'     \item \code{tree}: Decision tree names
#'   }
#'   Ignored if \code{model} and \code{context} are provided.
#' @param model An openqaly model object (\code{oq_model} or
#'   \code{oq_model_builder}). When provided with \code{context}, automatically
#'   extracts context-appropriate terms and suggestions from the model.
#' @param context Character string specifying the formula context when using
#'   model-aware mode. Valid values:
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
#' @param include_r_functions Logical. If TRUE (default), includes R function
#'   suggestions when using model-aware mode.
#' @param r_packages Character vector of package names to include R functions
#'   from when using model-aware mode. If NULL (default), uses
#'   c("base", "stats").
#'
#' @return A Shiny input element that can be included in a UI definition.
#'   The input value is a list with two elements:
#'   \itemize{
#'     \item \code{value}: The text content of the formula
#'     \item \code{valid}: A logical indicating if brackets are balanced
#'   }
#'
#' @examples
#' \dontrun{
#' # Run the interactive demo with one line:
#' run_formula_input_demo()
#'
#' # Model-aware formula input (recommended for openqaly models):
#' formulaInput("formula1", model = my_model, context = "variable")
#'
#' # With specific R packages:
#' formulaInput("formula1", model = my_model, context = "variable",
#'              r_packages = c("base", "stats", "utils"))
#'
#' # Or build your own app with manual suggestions:
#' library(shiny)
#' library(openqalyshiny)
#'
#' # Get R function suggestions from base and stats
#' r_funcs <- get_r_function_suggestions(c("base", "stats"))
#'
#' # Define custom suggestions
#' my_vars <- data.frame(
#'   name = c("age", "cost", "utility"),
#'   description = c("Patient age", "Treatment cost", "Health utility")
#' )
#'
#' ui <- fluidPage(
#'   formulaInput(
#'     "formula1",
#'     value = "age + cost",
#'     placeholder = "Enter R formula",
#'     terms = list(
#'       keyword = c("cycle", "day", "week"),
#'       variable = c("age", "cost", "utility"),
#'       table = c("patients", "costs")
#'     ),
#'     suggestions = list(
#'       r_function = r_funcs,
#'       variable = my_vars
#'     )
#'   ),
#'   verbatimTextOutput("result")
#' )
#'
#' server <- function(input, output, session) {
#'   output$result <- renderText({
#'     req(input$formula1)
#'     paste("Formula:", input$formula1$value, "| Valid:", input$formula1$valid)
#'   })
#' }
#'
#' shinyApp(ui, server)
#' }
#'
#' @seealso \code{\link{run_formula_input_demo}} for an interactive demo,
#'   \code{\link{get_r_function_suggestions}} for auto-discovering R functions,
#'   \code{\link{get_model_terms}} and \code{\link{get_model_suggestions}} for
#'   model-aware extraction
#'
#' @tests
#' result <- formulaInput("test_id", value = "x + y")
#' expect_true(inherits(result, "shiny.tag.list"))
#'
#' @export
#' @importFrom htmltools htmlDependency tags tagList
#' @importFrom shiny restoreInput
#' @importFrom jsonlite toJSON
#' @param updateOn Whether to update on every edit (\code{"change"}) or only
#'   when the user commits the edit via blur/Enter (\code{"blur"}).
formulaInput <- function(inputId, value = "", placeholder = NULL, width = NULL,
                         terms = NULL, suggestions = NULL,
                         model = NULL, context = NULL,
                         include_r_functions = TRUE, r_packages = NULL,
                         updateOn = c("change", "blur")) {
  updateOn <- match.arg(updateOn)

  # Restore value from bookmarked state if available
  value <- shiny::restoreInput(id = inputId, default = value)

  # Model-aware mode: extract terms and suggestions from model if provided
  if (!is.null(model) && !is.null(context)) {
    terms <- get_model_terms(model, context)
    suggestions <- get_model_suggestions(model, context,
                                          include_r_functions = include_r_functions,
                                          r_packages = r_packages)
  } else if (!is.null(model) && is.null(context)) {
    warning("model provided without context - ignoring model parameter")
  } else if (is.null(model) && !is.null(context)) {
    warning("context provided without model - ignoring context parameter")
  }

  # Build style attribute
  style <- NULL
  if (!is.null(width)) {
    style <- paste0("width: ", htmltools::validateCssUnit(width), ";")
  }

  # Serialize terms to JSON if provided
  termsJson <- NULL
  if (!is.null(terms)) {
    if (!is.list(terms)) {
      stop("terms must be a named list")
    }
    if (is.null(names(terms)) || any(names(terms) == "")) {
      stop("terms must be a named list where names are token types")
    }
    termsJson <- jsonlite::toJSON(terms, auto_unbox = FALSE)
  }

  # Serialize suggestions to JSON if provided
  suggestionsJson <- NULL
  if (!is.null(suggestions)) {
    if (!is.list(suggestions)) {
      stop("suggestions must be a named list")
    }
    if (is.null(names(suggestions)) || any(names(suggestions) == "")) {
      stop("suggestions must be a named list where names are category types")
    }
    # Validate each element is a data frame with a name column
    for (cat in names(suggestions)) {
      df <- suggestions[[cat]]
      if (!is.data.frame(df)) {
        stop("Each element of suggestions must be a data frame")
      }
      if (!"name" %in% names(df)) {
        stop("Each suggestions data frame must have a 'name' column")
      }
    }
    suggestionsJson <- jsonlite::toJSON(suggestions, auto_unbox = FALSE)
  }

  # Build the container div
  inputTag <- htmltools::tags$div(
    id = inputId,
    class = "formula-input",
    `data-value` = value,
    `data-update-on` = updateOn,
    `data-autocomplete-parent` = "body",
    `data-placeholder` = if (!is.null(placeholder)) placeholder else "",
    `data-terms` = termsJson,
    `data-suggestions` = suggestionsJson,
    style = style
  )

  # Return the input with dependencies attached
  htmltools::tagList(
    formula_input_dependency(),
    inputTag
  )
}

#' Update Formula Input
#'
#' Change the value, terms, and/or suggestions of a formula input on the client.
#'
#' @param session The session object passed to the Shiny server function.
#' @param inputId The id of the input object.
#' @param value The new value for the input.
#' @param terms A named list of terms to highlight. See \code{\link{formulaInput}}
#'   for details on the format. Set to an empty list \code{list()} to clear
#'   all term highlighting. Use \code{\link{get_model_terms}} to extract from
#'   an openqaly model.
#' @param suggestions A named list of autocomplete suggestions. See
#'   \code{\link{formulaInput}} for details on the format. Set to an empty
#'   list \code{list()} to clear all suggestions. Use
#'   \code{\link{get_model_suggestions}} to extract from an openqaly model.
#'
#' @examples
#' \dontrun{
#' # Reactive update when model or context changes
#' observe({
#'   updateFormulaInput(session, "formula",
#'     terms = get_model_terms(model(), context()),
#'     suggestions = get_model_suggestions(model(), context()))
#' })
#' }
#'
#' @seealso \code{\link{formulaInput}}, \code{\link{get_model_terms}},
#'   \code{\link{get_model_suggestions}}
#'
#' @tests
#' # updateFormulaInput requires a session object, so we test it exists
#' expect_true(is.function(updateFormulaInput))
#'
#' @export
updateFormulaInput <- function(session, inputId, value = NULL, terms = NULL,
                               suggestions = NULL) {
  message <- list()
  if (!is.null(value)) message$value <- value
  if (!is.null(terms)) {
    if (!is.list(terms)) {
      stop("terms must be a named list")
    }
    message$terms <- terms
  }
  if (!is.null(suggestions)) {
    if (!is.list(suggestions)) {
      stop("suggestions must be a named list")
    }
    message$suggestions <- suggestions
  }

  session$sendInputMessage(inputId, message)
}

#' @keywords internal
formula_input_dependency <- function() {
  # Ace Editor from CDN with language_tools extension for autocomplete
  ace_dep <- htmltools::htmlDependency(
    name = "ace-editor",
    version = "1.32.6",
    src = c(href = "https://cdnjs.cloudflare.com/ajax/libs/ace/1.32.6"),
    script = c("ace.min.js", "mode-r.min.js", "theme-chrome.min.js",
               "ext-language_tools.min.js"),
    all_files = FALSE
  )

  # Local formula-input binding with custom mode for term highlighting
  # Note: script order matters - mode and autocomplete must load before main
  binding_dep <- htmltools::htmlDependency(
    name = "formula-input",
    version = "1.2.0",
    src = c(file = system.file("www", package = "openqalyshiny")),
    script = c(
      "formula-input-mode.js",
      "formula-input-autocomplete.js",
      "formula-input.js"
    ),
    stylesheet = "formula-input.css",
    all_files = FALSE
  )

  list(ace_dep, binding_dep)
}
