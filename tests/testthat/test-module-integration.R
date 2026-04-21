make_model_with_overrides <- function() {
  openqaly::define_model("markov") |>
    openqaly::add_strategy("s1", "Strategy 1") |>
    openqaly::add_variable("alpha", 1) |>
    openqaly::add_override_category("Params") |>
    openqaly::add_override("Params",
      title      = "Alpha",
      name       = "alpha",
      input_type = "numeric",
      expression = 1
    )
}

# --- UI function tests ---

test_that("thresholdSummaryUI: returns shiny.tag.list", {
  expect_s3_class(thresholdSummaryUI("ts"), "shiny.tag.list")
})

test_that("thresholdSummaryUI: HTML contains namespaced summary_table placeholder", {
  html <- as.character(thresholdSummaryUI("ts"))
  expect_match(html, "ts-summary_table", fixed = TRUE)
})

test_that("thresholdSummaryUI: HTML contains namespaced error_display placeholder", {
  html <- as.character(thresholdSummaryUI("ts"))
  expect_match(html, "ts-error_display", fixed = TRUE)
})

test_that("thresholdResultTabSidebarUI: returns shiny.tag.list", {
  expect_s3_class(thresholdResultTabSidebarUI("tsbar"), "shiny.tag.list")
})

test_that("thresholdResultTabSidebarUI: HTML contains namespaced controls placeholder", {
  html <- as.character(thresholdResultTabSidebarUI("tsbar"))
  expect_match(html, "tsbar-controls", fixed = TRUE)
})

test_that("thresholdResultTabUI: returns shiny.tag", {
  expect_s3_class(thresholdResultTabUI("trtab"), "shiny.tag")
})

test_that("thresholdResultTabUI: HTML contains results-content-shell wrapper", {
  html <- as.character(thresholdResultTabUI("trtab"))
  expect_match(html, "results-content-shell", fixed = TRUE)
})

test_that("thresholdResultTabUI: HTML contains namespaced result_plot", {
  html <- as.character(thresholdResultTabUI("trtab"))
  expect_match(html, "trtab-result_plot", fixed = TRUE)
})

test_that("thresholdResultTabUI: HTML contains namespaced result_table", {
  html <- as.character(thresholdResultTabUI("trtab"))
  expect_match(html, "trtab-result_table", fixed = TRUE)
})

test_that("thresholdResultTabUI: HTML contains namespaced error_display", {
  html <- as.character(thresholdResultTabUI("trtab"))
  expect_match(html, "trtab-error_display", fixed = TRUE)
})

test_that("thresholdResultTabUI: conditionalPanel conditions reference viz_type input", {
  html <- as.character(thresholdResultTabUI("trtab"))
  expect_match(html, "trtab-viz_type",  fixed = TRUE)
  expect_match(html, "!= &#39;table&#39;", fixed = TRUE)
  expect_match(html, "== &#39;table&#39;", fixed = TRUE)
})

# --- thresholdSummaryServer via testServer ---

test_that("thresholdSummaryServer: starts with null error state", {
  shiny::testServer(
    thresholdSummaryServer,
    args = list(threshold_results = shiny::reactive(NULL)),
    {
      expect_null(error_msg())
    }
  )
})

# --- thresholdResultTabServer via testServer ---

test_that("thresholdResultTabServer: selected_analysis initially NULL", {
  shiny::testServer(
    thresholdResultTabServer,
    args = list(tab_type = "detail", threshold_results = shiny::reactive(NULL)),
    {
      expect_null(selected_analysis())
    }
  )
})

test_that("thresholdResultTabServer: '__all__' maps to NULL", {
  shiny::testServer(
    thresholdResultTabServer,
    args = list(tab_type = "detail", threshold_results = shiny::reactive(NULL)),
    {
      session$setInputs(analysis = "__all__")
      expect_null(selected_analysis())
    }
  )
})

test_that("thresholdResultTabServer: specific analysis name passes through", {
  shiny::testServer(
    thresholdResultTabServer,
    args = list(tab_type = "detail", threshold_results = shiny::reactive(NULL)),
    {
      session$setInputs(analysis = "BaseCase")
      expect_identical(selected_analysis(), "BaseCase")
    }
  )
})

test_that("thresholdResultTabServer: switching back to '__all__' returns NULL", {
  shiny::testServer(
    thresholdResultTabServer,
    args = list(tab_type = "convergence", threshold_results = shiny::reactive(NULL)),
    {
      session$setInputs(analysis = "AnalysisX")
      expect_identical(selected_analysis(), "AnalysisX")
      session$setInputs(analysis = "__all__")
      expect_null(selected_analysis())
    }
  )
})

# --- overrideManagerServer via testServer ---

test_that("overrideManagerServer: starts with modal_open FALSE", {
  m <- make_model_with_overrides()
  shiny::testServer(
    overrideManagerServer,
    args = list(model = shiny::reactive(m), on_action = function(a) NULL),
    {
      expect_false(modal_open())
    }
  )
})

test_that("overrideManagerServer: manager_close sets modal_open to FALSE", {
  m <- make_model_with_overrides()
  shiny::testServer(
    overrideManagerServer,
    args = list(model = shiny::reactive(m), on_action = function(a) NULL),
    {
      session$setInputs(manager_close = 1)
      expect_false(modal_open())
    }
  )
})

test_that("overrideManagerServer: add_category dispatches add_override_category action", {
  m <- make_model_with_overrides()
  actions <- list()
  shiny::testServer(
    overrideManagerServer,
    args = list(
      model     = shiny::reactive(m),
      on_action = function(a) { actions <<- c(actions, list(a)); m }
    ),
    {
      session$setInputs(add_category = list(name = "NewCat", general = FALSE))
    }
  )
  expect_length(actions, 1)
  expect_identical(actions[[1]]$type, "add_override_category")
  expect_identical(actions[[1]]$name, "NewCat")
  expect_false(actions[[1]]$general)
})

test_that("overrideManagerServer: add_category with general=TRUE", {
  m <- make_model_with_overrides()
  actions <- list()
  shiny::testServer(
    overrideManagerServer,
    args = list(
      model     = shiny::reactive(m),
      on_action = function(a) { actions <<- c(actions, list(a)); m }
    ),
    {
      session$setInputs(add_category = list(name = "General", general = TRUE))
    }
  )
  expect_true(actions[[1]]$general)
})

test_that("overrideManagerServer: edit_category dispatches edit_override_category action", {
  m <- make_model_with_overrides()
  actions <- list()
  shiny::testServer(
    overrideManagerServer,
    args = list(
      model     = shiny::reactive(m),
      on_action = function(a) { actions <<- c(actions, list(a)); m }
    ),
    {
      session$setInputs(edit_category = list(
        name = "Params", new_name = "Parameters", general = TRUE
      ))
    }
  )
  expect_identical(actions[[1]]$type,     "edit_override_category")
  expect_identical(actions[[1]]$name,     "Params")
  expect_identical(actions[[1]]$new_name, "Parameters")
  expect_true(actions[[1]]$general)
})

test_that("overrideManagerServer: remove_category dispatches remove_override_category action", {
  m <- make_model_with_overrides()
  actions <- list()
  shiny::testServer(
    overrideManagerServer,
    args = list(
      model     = shiny::reactive(m),
      on_action = function(a) { actions <<- c(actions, list(a)); m }
    ),
    {
      session$setInputs(remove_category = list(name = "Params"))
    }
  )
  expect_identical(actions[[1]]$type, "remove_override_category")
  expect_identical(actions[[1]]$name, "Params")
})

test_that("overrideManagerServer: add_override (slider) coerces min/max/step_size to numeric", {
  m <- make_model_with_overrides()
  actions <- list()
  shiny::testServer(
    overrideManagerServer,
    args = list(
      model     = shiny::reactive(m),
      on_action = function(a) { actions <<- c(actions, list(a)); m }
    ),
    {
      session$setInputs(add_override = list(
        category      = "Params",
        title         = "Alpha Rate",
        name          = "alpha",
        override_type = "variable",
        input_type    = "slider",
        expression    = "0.5",
        strategy      = "s1",
        group         = "",
        min           = "0",
        max           = "1",
        step_size     = "0.1"
      ))
    }
  )
  a <- actions[[1]]
  expect_identical(a$type,       "add_override")
  expect_identical(a$input_type, "slider")
  expect_type(a$min,       "double")
  expect_type(a$max,       "double")
  expect_type(a$step_size, "double")
  expect_equal(a$min,       0,   tolerance = 1e-9)
  expect_equal(a$max,       1,   tolerance = 1e-9)
  expect_equal(a$step_size, 0.1, tolerance = 1e-9)
})

test_that("overrideManagerServer: add_override without numeric config has NULL min/max/step_size", {
  m <- make_model_with_overrides()
  actions <- list()
  shiny::testServer(
    overrideManagerServer,
    args = list(
      model     = shiny::reactive(m),
      on_action = function(a) { actions <<- c(actions, list(a)); m }
    ),
    {
      session$setInputs(add_override = list(
        category      = "Params",
        title         = "Alpha",
        name          = "alpha",
        override_type = "variable",
        input_type    = "numeric",
        expression    = "1"
      ))
    }
  )
  a <- actions[[1]]
  expect_null(a$min)
  expect_null(a$max)
  expect_null(a$step_size)
})

test_that("overrideManagerServer: edit_override dispatches edit_override action", {
  m <- make_model_with_overrides()
  actions <- list()
  shiny::testServer(
    overrideManagerServer,
    args = list(
      model     = shiny::reactive(m),
      on_action = function(a) { actions <<- c(actions, list(a)); m }
    ),
    {
      session$setInputs(edit_override = list(
        category      = "Params",
        override_type = "variable",
        name          = "alpha",
        strategy      = "s1",
        group         = "",
        new_name      = "alpha2",
        title         = "New Alpha",
        expression    = "0.7",
        input_type    = "numeric"
      ))
    }
  )
  a <- actions[[1]]
  expect_identical(a$type,       "edit_override")
  expect_identical(a$name,       "alpha")
  expect_identical(a$new_name,   "alpha2")
  expect_identical(a$title,      "New Alpha")
  expect_identical(a$expression, "0.7")
})

test_that("overrideManagerServer: remove_override dispatches remove_override action", {
  m <- make_model_with_overrides()
  actions <- list()
  shiny::testServer(
    overrideManagerServer,
    args = list(
      model     = shiny::reactive(m),
      on_action = function(a) { actions <<- c(actions, list(a)); m }
    ),
    {
      session$setInputs(remove_override = list(
        category      = "Params",
        override_type = "variable",
        name          = "alpha",
        strategy      = "",
        group         = ""
      ))
    }
  )
  expect_identical(actions[[1]]$type,     "remove_override")
  expect_identical(actions[[1]]$category, "Params")
  expect_identical(actions[[1]]$name,     "alpha")
})

test_that("overrideManagerServer: reorder_overrides dispatches set_override_categories", {
  m <- make_model_with_overrides()
  actions <- list()
  raw_state <- jsonlite::toJSON(list(list(
    name      = "Params",
    general   = FALSE,
    overrides = list(list(
      name          = "alpha",
      title         = "Alpha",
      display_name  = "Alpha",
      type          = "variable",
      strategy      = "",
      group         = "",
      input_type    = "numeric",
      input_config  = list(),
      default_value = "1"
    ))
  )), auto_unbox = TRUE, null = "null")
  shiny::testServer(
    overrideManagerServer,
    args = list(
      model     = shiny::reactive(m),
      on_action = function(a) { actions <<- c(actions, list(a)); m }
    ),
    {
      session$setInputs(reorder_overrides = raw_state)
    }
  )
  expect_length(actions, 1)
  expect_identical(actions[[1]]$type,                    "set_override_categories")
  expect_identical(actions[[1]]$categories[[1]]$name,    "Params")
})

test_that("overrideManagerServer: on_action error does not crash the module", {
  m <- make_model_with_overrides()
  expect_no_error(
    shiny::testServer(
      overrideManagerServer,
      args = list(
        model     = shiny::reactive(m),
        on_action = function(a) stop("Simulated failure")
      ),
      {
        session$setInputs(add_category = list(name = "Cat", general = FALSE))
      }
    )
  )
})

test_that("overrideManagerServer: multiple sequential actions dispatched in order", {
  m <- make_model_with_overrides()
  actions <- list()
  shiny::testServer(
    overrideManagerServer,
    args = list(
      model     = shiny::reactive(m),
      on_action = function(a) { actions <<- c(actions, list(a)); m }
    ),
    {
      session$setInputs(add_category    = list(name = "Cat1", general = FALSE))
      session$setInputs(edit_category   = list(name = "Params", new_name = "P2"))
      session$setInputs(remove_category = list(name = "Cat1"))
    }
  )
  expect_length(actions, 3)
  expect_identical(actions[[1]]$type, "add_override_category")
  expect_identical(actions[[2]]$type, "edit_override_category")
  expect_identical(actions[[3]]$type, "remove_override_category")
})
