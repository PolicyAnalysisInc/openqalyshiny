test_that("model editor source includes body-level overlay and sidebar scroll hooks", {
  source_text <- paste(
    readLines(test_path("../../R/model_editor.r"), warn = FALSE),
    collapse = "\n"
  )
  utils_source <- paste(
    readLines(test_path("../../R/model_viewer_utils.r"), warn = FALSE),
    collapse = "\n"
  )

  expect_match(source_text, "class = \"results-sidebar-tabs\"", fixed = TRUE)
  expect_match(source_text, "class = \"results-sidebar-panel-scroll\"", fixed = TRUE)
  expect_match(source_text, "\"Overrides\",\n              value = \"overrides\"", fixed = TRUE)
  expect_match(source_text, ".results-analysis-sidebar > .sidebar-content", fixed = TRUE)
  expect_match(source_text, "event.target.closest('.results-sidebar-panel-scroll')", fixed = TRUE)
  expect_false(grepl(".results-sidebar-tabs > .card > .tab-content > .tab-pane,", source_text, fixed = TRUE))
  expect_false(grepl("window.jQuery.fn.selectize = function(options)", source_text, fixed = TRUE))
  expect_false(grepl("instance.$dropdown.appendTo(document.body)", source_text, fixed = TRUE))
  expect_false(grepl("selected = \"settings\"", source_text, fixed = TRUE))
  expect_false(grepl("psa_settings_panel", source_text, fixed = TRUE))
  expect_false(grepl("switchAppPage('overrides')", source_text, fixed = TRUE))
  expect_false(grepl("`data-page` = \"overrides\"", source_text, fixed = TRUE))
  expect_match(source_text, "selected = \"univariate_sampling\"", fixed = TRUE)
  expect_match(source_text, "instance.isOpen", fixed = TRUE)
  expect_match(utils_source, "config$dropdownParent <- \"body\"", fixed = TRUE)
  expect_match(utils_source, "plugin_values <- unique(c(plugin_values, \"auto_position\"))", fixed = TRUE)
})

test_that("model editor progress snackbar rounds labels to whole percents", {
  source_text <- paste(
    readLines(test_path("../../R/model_editor.r"), warn = FALSE),
    collapse = "\n"
  )

  expect_false(grepl("toFixed\\(2\\)", source_text))
  expect_match(source_text, "pct.textContent = Math.round(data.pct) + '%';", fixed = TRUE)
})
