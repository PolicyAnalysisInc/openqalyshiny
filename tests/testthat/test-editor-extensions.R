get_ext_registry <- function() getFromNamespace(".editor_extensions", "openqalyshiny")
get_extensions <- getFromNamespace(".get_editor_extensions", "openqalyshiny")
dispatch <- getFromNamespace("dispatch_model_action", "openqalyshiny")

cleanup_ext <- function(tab_id) {
  reg <- get_ext_registry()
  if (exists(tab_id, envir = reg)) rm(list = tab_id, envir = reg)
}

get_editor_html <- function() {
  app <- run_model_editor()
  ui <- environment(app$httpHandler)$ui
  rendered <- htmltools::renderTags(ui)
  paste(c(rendered$head, rendered$html), collapse = "\n")
}

# --- Extension registry ---

test_that("register_editor_extension adds extension to registry", {
  cleanup_ext("test_ext_reg")
  on.exit(cleanup_ext("test_ext_reg"))

  openqalyshiny::register_editor_extension(
    tab_id    = "test_ext_reg",
    label     = "Test Reg",
    ui_fn     = function(tab_id) tags$div("ui"),
    server_fn = function(model, apply_action, input, output, session) NULL
  )

  exts <- get_extensions()
  expect_true("test_ext_reg" %in% names(exts))
  expect_equal(exts[["test_ext_reg"]]$label, "Test Reg")
})

# --- UI rendering ---

test_that("model_inputs_tabs id is present in model editor HTML", {
  html <- get_editor_html()
  expect_match(html, "model_inputs_tabs", fixed = TRUE)
})

test_that("registered extension appears as top-level nav item and page div", {
  cleanup_ext("test_ext_ui")
  on.exit(cleanup_ext("test_ext_ui"))

  openqalyshiny::register_editor_extension(
    tab_id    = "test_ext_ui",
    label     = "Test Extension",
    ui_fn     = function(tab_id) {
      tags$div(id = paste0(tab_id, "_content"), "Extension UI")
    },
    server_fn = function(model, apply_action, input, output, session) NULL
  )

  html <- get_editor_html()

  # Nav item in dropdown
  expect_match(html, "Test Extension", fixed = TRUE)
  expect_match(html, 'data-page="test_ext_ui"', fixed = TRUE)

  # Page div
  expect_match(html, "page_test_ext_ui", fixed = TRUE)
  expect_match(html, "test_ext_ui_content", fixed = TRUE)
})

test_that("extension nav item is NOT inside the model_inputs_tabs navset", {
  cleanup_ext("test_ext_placement")
  on.exit(cleanup_ext("test_ext_placement"))

  openqalyshiny::register_editor_extension(
    tab_id    = "test_ext_placement",
    label     = "Placement Test",
    ui_fn     = function(tab_id) tags$div(id = "placement_marker", "content"),
    server_fn = function(model, apply_action, input, output, session) NULL
  )

  html <- get_editor_html()
  # The extension label should appear in the dropdown, not as a bslib tab value
  # within the navset (which would have role="tab" and data-value attributes)
  expect_false(grepl('data-value="test_ext_placement"', html, fixed = TRUE))
  expect_match(html, "page_test_ext_placement", fixed = TRUE)
})

# --- replace_model dispatch ---

test_that("dispatch_model_action replace_model returns the replacement model", {
  old <- openqaly::define_model("markov")
  new <- openqaly::define_model("psm")
  result <- dispatch(old, list(type = "replace_model", model = new))
  expect_identical(result, new)
})

test_that("dispatch_model_action replace_model rejects non-oq_model", {
  old <- openqaly::define_model("markov")
  expect_error(
    dispatch(old, list(type = "replace_model", model = list())),
    "oq_model"
  )
})
