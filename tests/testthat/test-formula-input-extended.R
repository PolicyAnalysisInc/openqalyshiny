make_mock_session <- function() {
  e <- new.env(parent = emptyenv())
  e$calls <- list()
  e$sendInputMessage <- function(id, msg) {
    e$calls[[length(e$calls) + 1]] <- list(id = id, msg = msg)
  }
  e
}

test_that("updateFormulaInput: sends message with correct inputId", {
  s <- make_mock_session()
  updateFormulaInput(s, "formula1", value = "x + y")
  expect_identical(s$calls[[1]]$id, "formula1")
})

test_that("updateFormulaInput: value-only message contains only 'value' key", {
  s <- make_mock_session()
  updateFormulaInput(s, "f1", value = "abc")
  expect_identical(names(s$calls[[1]]$msg), "value")
})

test_that("updateFormulaInput: value is included in message correctly", {
  s <- make_mock_session()
  updateFormulaInput(s, "f1", value = "abc")
  expect_identical(s$calls[[1]]$msg$value, "abc")
})

test_that("updateFormulaInput: terms-only message excludes 'value' key", {
  s <- make_mock_session()
  updateFormulaInput(s, "f1", terms = list(keyword = c("cycle")))
  expect_false("value" %in% names(s$calls[[1]]$msg))
  expect_true("terms" %in% names(s$calls[[1]]$msg))
})

test_that("updateFormulaInput: suggestions-only message includes 'suggestions' key", {
  s <- make_mock_session()
  updateFormulaInput(s, "f1",
    suggestions = list(keyword = data.frame(name = "cycle", stringsAsFactors = FALSE)))
  expect_true("suggestions" %in% names(s$calls[[1]]$msg))
})

test_that("updateFormulaInput: all three args included together", {
  s <- make_mock_session()
  updateFormulaInput(s, "f1",
    value       = "x",
    terms       = list(a = "b"),
    suggestions = list(c = data.frame(name = "x", stringsAsFactors = FALSE))
  )
  expect_true(all(c("value", "terms", "suggestions") %in% names(s$calls[[1]]$msg)))
})

test_that("updateFormulaInput: no args sends empty message", {
  s <- make_mock_session()
  updateFormulaInput(s, "f1")
  expect_length(s$calls[[1]]$msg, 0)
})

test_that("updateFormulaInput: non-list terms throws error before sending message", {
  s <- make_mock_session()
  expect_error(updateFormulaInput(s, "f1", terms = "bad"))
  expect_length(s$calls, 0)
})

test_that("updateFormulaInput: non-list suggestions throws error before sending message", {
  s <- make_mock_session()
  expect_error(updateFormulaInput(s, "f1", suggestions = "bad"))
  expect_length(s$calls, 0)
})

test_that("updateFormulaInput: sendInputMessage is called exactly once per invocation", {
  s <- make_mock_session()
  updateFormulaInput(s, "f1", value = "x")
  expect_length(s$calls, 1)
})
