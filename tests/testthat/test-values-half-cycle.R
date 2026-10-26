dispatch <- getFromNamespace("dispatch_model_action", "openqalyshiny")

two_state_model <- function() {
  openqaly::define_model("markov") |>
    openqaly::add_strategy("s") |>
    openqaly::add_state("a", initial_prob = 1) |>
    openqaly::add_state("b", initial_prob = 0)
}

test_that("add_value passes half_cycle_method and a new row takes the value's setting", {
  m <- dispatch(two_state_model(), list(
    type = "add_value", name = "cost", formula = "1", state = "a",
    value_type = "cost", half_cycle_method = "end"
  ))
  expect_equal(m$values$half_cycle_method, "end")

  m <- dispatch(m, list(type = "add_value", name = "cost", formula = "2", state = "b",
                        value_type = "cost", half_cycle_method = ""))
  expect_equal(m$values$half_cycle_method, c("end", "end"))
})

test_that("editing half_cycle_method on one row changes every row; clearing sets NA", {
  m <- two_state_model() |>
    openqaly::add_value("cost", 1, state = "a", type = "cost") |>
    openqaly::add_value("cost", 2, state = "b", type = "cost")

  m <- dispatch(m, list(type = "edit_value", name = "cost", state = "b", destination = "",
                        field = "half_cycle_method", value = "life-table"))
  expect_equal(m$values$half_cycle_method, c("life-table", "life-table"))

  m <- dispatch(m, list(type = "edit_value", name = "cost", state = "a", destination = "",
                        field = "half_cycle_method", value = ""))
  expect_true(all(is.na(m$values$half_cycle_method)))
})

test_that("changing a row's state keeps its half_cycle_method", {
  m <- two_state_model() |>
    openqaly::add_value("cost", 1, state = "a", type = "cost", half_cycle_method = "end")
  m <- dispatch(m, list(type = "edit_value", name = "cost", state = "a", destination = "",
                        field = "state", value = "b"))
  expect_equal(m$values$state, "b")
  expect_equal(m$values$half_cycle_method, "end")
})

test_that("discounting_override suggestions include its own terms and no state time", {
  m <- two_state_model() |> openqaly::add_variable("rate", 0.03)
  terms <- get_model_terms(m, "discounting_override")
  expect_true(all(c("discount_rate", "discount_factors", "discount_time", "trace") %in% terms$keyword))
  expect_false("state_cycle" %in% terms$keyword)
  expect_true("rate" %in% terms$variable)
})

test_that("the half-cycle method shown for a model without one matches openqaly's default", {
  shown <- getFromNamespace(".default_half_cycle_method", "openqalyshiny")
  expect_equal(openqaly::define_model("markov")$settings$half_cycle_method, shown)
  m <- two_state_model()
  m$settings$half_cycle_method <- NULL
  expect_equal(openqaly::set_settings(m, n_cycles = 5)$settings$half_cycle_method, shown)
})
