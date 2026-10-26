# The Settings tab shows a default for each setting a model does not set.
# Opening the tab must not change the model, so a displayed default never
# counts as a change.

setting_names <- names(getFromNamespace(".settings_ui_defaults", "openqalyshiny"))
display_value <- getFromNamespace("setting_display_value", "openqalyshiny")
input_changed <- getFromNamespace("setting_input_changed", "openqalyshiny")

bundled_model_settings <- function() {
  dirs <- list.dirs(system.file("models", package = "openqaly"), recursive = FALSE)
  dirs <- dirs[file.exists(file.path(dirs, "model.yaml"))]
  lapply(dirs, function(d) openqaly::get_settings(openqaly::read_model(d)))
}

test_that("displayed defaults are not changes, for bundled and new models", {
  all_settings <- c(
    bundled_model_settings(),
    lapply(c("markov", "psm", "custom_psm"), function(t) {
      openqaly::get_settings(openqaly::define_model(t))
    })
  )
  for (s in all_settings) {
    for (nm in setting_names) {
      expect_false(input_changed(s, nm, display_value(s, nm)), info = nm)
    }
  }
})

test_that("inputs compare by value, not by type", {
  expect_false(input_changed(list(), "country", "US"))
  expect_false(input_changed(list(timeframe = 20), "timeframe", 20L))
  expect_false(input_changed(list(), "number_country", ""))
  expect_false(input_changed(list(reduce_state_cycle = FALSE), "reduce_state_cycle", FALSE))
})

test_that("a real change is detected", {
  expect_true(input_changed(list(), "days_per_year", 365.25))
  expect_true(input_changed(list(country = "GB"), "country", "US"))
  expect_true(input_changed(list(half_cycle_method = "start"), "half_cycle_method", "end"))
})

test_that("days per year defaults to openqaly's default", {
  expect_equal(display_value(list(), "days_per_year"),
               openqaly::define_model("markov")$settings$days_per_year)
})

test_that("decision tree duration unit is shown as days when unset", {
  dt_values <- getFromNamespace("dt_display_values", "openqalyshiny")
  expect_equal(dt_values(list(tree_name = "t", duration = 30))$duration_unit, "days")
  expect_equal(dt_values(list(tree_name = "t", duration = 30))$duration, "30")
  expect_equal(dt_values(NULL)$tree_name, "")
})
