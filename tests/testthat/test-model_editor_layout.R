get_editor_html <- function() {
  app <- run_model_editor()
  ui <- environment(app$httpHandler)$ui
  rendered <- htmltools::renderTags(ui)
  paste(c(rendered$head, rendered$html), collapse = "\n")
}

test_that("model editor source includes body-level overlay and sidebar scroll hooks", {
  source_text <- get_editor_html()

  expect_match(source_text, "results-sidebar-tabs", fixed = TRUE)
  expect_match(source_text, "results-sidebar-panel-scroll", fixed = TRUE)
  expect_match(source_text, "overrides", fixed = TRUE)
  expect_match(source_text, ".results-analysis-sidebar > .sidebar-content", fixed = TRUE)
  expect_match(source_text, "event.target.closest('.results-sidebar-panel-scroll')", fixed = TRUE)
  expect_false(grepl(".results-sidebar-tabs > .card > .tab-content > .tab-pane,", source_text, fixed = TRUE))
  expect_false(grepl("window.jQuery.fn.selectize = function(options)", source_text, fixed = TRUE))
  expect_false(grepl("instance.$dropdown.appendTo(document.body)", source_text, fixed = TRUE))
  expect_false(grepl("psa_settings_panel", source_text, fixed = TRUE))
  expect_false(grepl("switchAppPage('overrides')", source_text, fixed = TRUE))
  expect_match(source_text, "univariate_sampling", fixed = TRUE)
  expect_match(source_text, "instance.isOpen", fixed = TRUE)

  config <- .merge_editor_selectize_options(list())
  expect_equal(config$dropdownParent, "body")
  expect_true("auto_position" %in% unlist(config$plugins))
})

test_that("model editor progress snackbar rounds labels to whole percents", {
  source_text <- get_editor_html()

  expect_false(grepl("toFixed(2)", source_text, fixed = TRUE))
  expect_match(source_text, "pct.textContent = Math.round(data.pct) + '%';", fixed = TRUE)
})
