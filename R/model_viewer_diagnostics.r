# =============================================================================
# Decision Tree Diagnostics Sub-Module
# =============================================================================

#' Get Decision Tree Choices
#'
#' Extract names of evaluated decision trees from model results.
#'
#' @param results Model results object from openqaly::run_model().
#'
#' @return A character vector of tree names, or character(0) if none found.
#' @keywords internal
get_decision_tree_choices <- function(results) {
  segments <- results$segments
  if (is.null(segments) || nrow(segments) == 0) return(character(0))
  if (!"eval_vars" %in% names(segments)) return(character(0))

  eval_vars <- segments[1, ]$eval_vars[[1]]
  if (is.null(eval_vars) || is.null(eval_vars$env)) return(character(0))

  env <- eval_vars$env
  obj_names <- ls(env)
  tree_names <- character(0)
  for (nm in obj_names) {
    obj <- tryCatch(get(nm, envir = env), error = function(e) NULL)
    if (!is.null(obj) && inherits(obj, "eval_decision_tree")) {
      tree_names <- c(tree_names, nm)
    }
  }
  tree_names
}

#' Decision Tree Results UI
#' @param id Module namespace ID.
#' @keywords internal
decisionTreeResultsUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("controls")),
    shiny::plotOutput(ns("result_plot"), height = "600px"),
    shiny::uiOutput(ns("error_display"))
  )
}

#' Decision Tree Results Server
#' @param id Module namespace ID.
#' @param results Reactive containing model results.
#' @param metadata Reactive containing model metadata.
#' @keywords internal
decisionTreeResultsServer <- function(id, results, metadata) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    error_msg <- shiny::reactiveVal(NULL)

    output$controls <- shiny::renderUI({
      res <- results()
      meta <- metadata()
      if (is.null(res) || is.null(meta)) return(NULL)

      tree_choices <- get_decision_tree_choices(res)
      if (length(tree_choices) == 0) {
        return(shiny::tags$div(
          class = "text-muted p-3",
          "No decision trees found in this model."
        ))
      }

      strategy_choices <- get_strategy_choices(meta)
      group_choices <- get_group_choices(meta)
      # Filter out aggregate group options
      individual_groups <- group_choices[
        !group_choices %in% c("overall", "all", "all_groups")
      ]

      bslib::layout_columns(
        col_widths = bslib::breakpoints(sm = 12, md = 4),
        if (length(strategy_choices) > 1) {
          shiny::selectInput(ns("strategy"), "Strategy",
            choices = strategy_choices,
            selected = strategy_choices[1]
          )
        },
        if (length(individual_groups) > 1) {
          shiny::selectInput(ns("group"), "Group",
            choices = individual_groups,
            selected = individual_groups[1]
          )
        },
        if (length(tree_choices) > 1) {
          shiny::selectInput(ns("tree_name"), "Decision Tree",
            choices = tree_choices,
            selected = tree_choices[1]
          )
        }
      )
    })

    output$result_plot <- shiny::renderPlot({
      res <- results()
      shiny::req(res)

      tree_choices <- get_decision_tree_choices(res)
      shiny::req(length(tree_choices) > 0)

      error_msg(NULL)

      args <- list(res)

      if (!is.null(input$strategy)) {
        args$strategy <- input$strategy
      }

      group_choices <- get_group_choices(metadata())
      individual_groups <- group_choices[
        !group_choices %in% c("overall", "all", "all_groups")
      ]
      if (length(individual_groups) > 0 && !is.null(input$group)) {
        args$group <- input$group
      }

      if (length(tree_choices) > 1 && !is.null(input$tree_name)) {
        args$tree_name <- input$tree_name
      }

      tryCatch({
        do.call(openqaly::plot_decision_tree, args)
      }, error = function(e) {
        error_msg(conditionMessage(e))
        NULL
      })
    })

    output$error_display <- shiny::renderUI({
      msg <- error_msg()
      if (is.null(msg)) return(NULL)
      shiny::tags$div(
        class = "alert alert-danger mt-2",
        shiny::tags$strong("Error: "), msg
      )
    })
  })
}

# =============================================================================
# Transition Heatmap Diagnostics Sub-Module
# =============================================================================

#' Transition Heatmap UI
#' @param id Module namespace ID.
#' @keywords internal
transitionHeatmapUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("controls")),
    shiny::plotOutput(ns("result_plot"), height = "600px"),
    shiny::uiOutput(ns("error_display"))
  )
}

#' Transition Heatmap Server
#' @param id Module namespace ID.
#' @param results Reactive containing model results.
#' @param metadata Reactive containing model metadata.
#' @keywords internal
transitionHeatmapServer <- function(id, results, metadata) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    error_msg <- shiny::reactiveVal(NULL)

    output$controls <- shiny::renderUI({
      res <- results()
      meta <- metadata()
      if (is.null(res) || is.null(meta)) return(NULL)

      # Only applicable to Markov models
      model_type <- meta$settings$model_type
      if (is.null(model_type) || model_type != "markov") {
        return(shiny::tags$div(
          class = "text-muted p-3",
          "Transition heatmaps are only available for Markov models."
        ))
      }

      max_cycle <- tryCatch({
        trace <- res$aggregated$collapsed_trace[[1]]
        max(trace$cycle, na.rm = TRUE)
      }, error = function(e) 1)

      strategy_choices <- get_strategy_choices(meta)
      group_choices <- get_group_choices(meta)
      individual_groups <- group_choices[
        !group_choices %in% c("overall", "all", "all_groups")
      ]

      bslib::layout_columns(
        col_widths = bslib::breakpoints(sm = 12, md = 4),
        shiny::sliderInput(ns("cycle"), "Cycle",
          min = 1, max = max_cycle,
          value = 1, step = 1
        ),
        if (length(strategy_choices) > 1) {
          shiny::selectInput(ns("strategy"), "Strategy",
            choices = strategy_choices,
            selected = strategy_choices[1]
          )
        },
        if (length(individual_groups) > 1) {
          shiny::selectInput(ns("group"), "Group",
            choices = individual_groups,
            selected = individual_groups[1]
          )
        },
        shiny::selectInput(ns("view"), "View",
          choices = c("Collapsed" = "TRUE", "Expanded" = "FALSE"),
          selected = "TRUE"
        ),
        shiny::numericInput(ns("decimals"), "Decimals",
          value = 2, min = 0, max = 6, step = 1
        )
      )
    })

    output$result_plot <- shiny::renderPlot({
      res <- results()
      meta <- metadata()
      shiny::req(res, meta)

      model_type <- meta$settings$model_type
      shiny::req(model_type == "markov")
      shiny::req(input$cycle)

      error_msg(NULL)

      args <- list(res, cycle = input$cycle)

      if (!is.null(input$strategy)) {
        args$strategies <- input$strategy
      }

      group_choices <- get_group_choices(meta)
      individual_groups <- group_choices[
        !group_choices %in% c("overall", "all", "all_groups")
      ]
      if (length(individual_groups) > 0 && !is.null(input$group)) {
        args$groups <- input$group
      }

      if (!is.null(input$view)) {
        args$collapsed <- as.logical(input$view)
      }

      if (!is.null(input$decimals)) {
        args$decimals <- input$decimals
      }

      tryCatch({
        do.call(openqaly::transition_plot_heatmap, args)
      }, error = function(e) {
        error_msg(conditionMessage(e))
        NULL
      })
    })

    output$error_display <- shiny::renderUI({
      msg <- error_msg()
      if (is.null(msg)) return(NULL)
      shiny::tags$div(
        class = "alert alert-danger mt-2",
        shiny::tags$strong("Error: "), msg
      )
    })
  })
}
