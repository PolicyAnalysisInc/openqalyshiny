# Error handling in the result viewers.

test_that("viewer_error_handler re-signals a req() silent error and leaves error_msg NULL", {
  cond <- tryCatch(shiny::req(FALSE), error = function(e) e)
  error_msg <- shiny::reactiveVal(NULL)
  expect_error(viewer_error_handler(cond, error_msg), class = "shiny.silent.error")
  expect_null(shiny::isolate(error_msg()))
})

test_that("viewer_error_handler sets error_msg for an ordinary error", {
  error_msg <- shiny::reactiveVal(NULL)
  viewer_error_handler(simpleError("boom"), error_msg)
  expect_identical(shiny::isolate(error_msg()), "boom")
})
