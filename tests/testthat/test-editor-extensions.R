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

# --- Extension type ---

test_that("default type is 'page' when not specified", {
  cleanup_ext("test_ext_default_type")
  on.exit(cleanup_ext("test_ext_default_type"))

  openqalyshiny::register_editor_extension(
    tab_id    = "test_ext_default_type",
    label     = "Default Type",
    ui_fn     = function(tab_id) tags$div("ui"),
    server_fn = function(model, apply_action, input, output, session) NULL
  )

  exts <- get_extensions()
  expect_equal(exts[["test_ext_default_type"]]$type, "page")
})

test_that("overlay type is stored in registry", {
  cleanup_ext("test_ext_overlay_store")
  on.exit(cleanup_ext("test_ext_overlay_store"))

  openqalyshiny::register_editor_extension(
    tab_id    = "test_ext_overlay_store",
    label     = "Overlay Store",
    ui_fn     = function(tab_id) tags$div("ui"),
    server_fn = function(model, apply_action, input, output, session) NULL,
    type      = "overlay"
  )

  exts <- get_extensions()
  expect_equal(exts[["test_ext_overlay_store"]]$type, "overlay")
})

test_that("overlay extension does NOT appear in dropdown menu", {
  cleanup_ext("test_ext_overlay_no_dropdown")
  on.exit(cleanup_ext("test_ext_overlay_no_dropdown"))

  openqalyshiny::register_editor_extension(
    tab_id    = "test_ext_overlay_no_dropdown",
    label     = "OverlayDropdownLabel_XYZ",
    ui_fn     = function(tab_id) tags$div("ui"),
    server_fn = function(model, apply_action, input, output, session) NULL,
    type      = "overlay"
  )

  html <- get_editor_html()
  expect_false(grepl('data-page="test_ext_overlay_no_dropdown"', html, fixed = TRUE))
  expect_false(grepl('switchAppPage(\'test_ext_overlay_no_dropdown\')', html, fixed = TRUE))
})

test_that("overlay extension does NOT produce an .app-page div", {
  cleanup_ext("test_ext_overlay_no_page")
  on.exit(cleanup_ext("test_ext_overlay_no_page"))

  openqalyshiny::register_editor_extension(
    tab_id    = "test_ext_overlay_no_page",
    label     = "Overlay No Page",
    ui_fn     = function(tab_id) tags$div("ui"),
    server_fn = function(model, apply_action, input, output, session) NULL,
    type      = "overlay"
  )

  html <- get_editor_html()
  expect_false(grepl('id="page_test_ext_overlay_no_page"', html, fixed = TRUE))
})

test_that("overlay extension renders inside an .editor-overlay wrapper", {
  cleanup_ext("test_ext_overlay_render")
  on.exit(cleanup_ext("test_ext_overlay_render"))

  openqalyshiny::register_editor_extension(
    tab_id    = "test_ext_overlay_render",
    label     = "Overlay Render",
    ui_fn     = function(tab_id) tags$div(id = "overlay_marker_xyz", "marker"),
    server_fn = function(model, apply_action, input, output, session) NULL,
    type      = "overlay"
  )

  html <- get_editor_html()
  expect_match(html, 'id="overlay_test_ext_overlay_render"', fixed = TRUE)
  expect_match(html, "editor-overlay", fixed = TRUE)
  expect_match(html, "overlay_marker_xyz", fixed = TRUE)
})

test_that("page and overlay extensions coexist", {
  cleanup_ext("test_ext_mix_page")
  cleanup_ext("test_ext_mix_overlay")
  on.exit({
    cleanup_ext("test_ext_mix_page")
    cleanup_ext("test_ext_mix_overlay")
  })

  openqalyshiny::register_editor_extension(
    tab_id    = "test_ext_mix_page",
    label     = "Mix Page",
    ui_fn     = function(tab_id) tags$div("page-ui"),
    server_fn = function(model, apply_action, input, output, session) NULL
  )
  openqalyshiny::register_editor_extension(
    tab_id    = "test_ext_mix_overlay",
    label     = "Mix Overlay",
    ui_fn     = function(tab_id) tags$div("overlay-ui"),
    server_fn = function(model, apply_action, input, output, session) NULL,
    type      = "overlay"
  )

  html <- get_editor_html()
  expect_match(html, 'data-page="test_ext_mix_page"', fixed = TRUE)
  expect_match(html, 'id="page_test_ext_mix_page"', fixed = TRUE)
  expect_match(html, 'id="overlay_test_ext_mix_overlay"', fixed = TRUE)
  expect_false(grepl('data-page="test_ext_mix_overlay"', html, fixed = TRUE))
  expect_false(grepl('id="page_test_ext_mix_overlay"', html, fixed = TRUE))
})

test_that("reserved tab_id is rejected", {
  expect_error(
    openqalyshiny::register_editor_extension(
      tab_id    = "results",
      label     = "Conflict",
      ui_fn     = function(tab_id) tags$div("ui"),
      server_fn = function(model, apply_action, input, output, session) NULL
    ),
    "reserved"
  )
  expect_error(
    openqalyshiny::register_editor_extension(
      tab_id    = "psa",
      label     = "Conflict",
      ui_fn     = function(tab_id) tags$div("ui"),
      server_fn = function(model, apply_action, input, output, session) NULL,
      type      = "overlay"
    ),
    "reserved"
  )
})

test_that("conflicting type re-registration is rejected", {
  cleanup_ext("test_ext_conflict")
  on.exit(cleanup_ext("test_ext_conflict"))

  openqalyshiny::register_editor_extension(
    tab_id    = "test_ext_conflict",
    label     = "First (page)",
    ui_fn     = function(tab_id) tags$div("ui"),
    server_fn = function(model, apply_action, input, output, session) NULL
  )

  expect_error(
    openqalyshiny::register_editor_extension(
      tab_id    = "test_ext_conflict",
      label     = "Second (overlay)",
      ui_fn     = function(tab_id) tags$div("ui"),
      server_fn = function(model, apply_action, input, output, session) NULL,
      type      = "overlay"
    ),
    "already registered"
  )
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
