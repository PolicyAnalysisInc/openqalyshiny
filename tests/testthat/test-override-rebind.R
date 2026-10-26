# The override sidebar rebind message is registered once per session.

make_mock_session <- function() {
  session <- new.env(parent = emptyenv())
  session$userData <- new.env(parent = emptyenv())
  session$flush_callbacks <- list()
  session$onFlushed <- function(fn, once = TRUE) {
    session$flush_callbacks <- c(session$flush_callbacks, list(fn))
    invisible(NULL)
  }
  session$sendCustomMessage <- function(type, message) invisible(NULL)
  class(session) <- "MockShinySession"
  session
}

test_that("register_override_rebind registers one flush callback per session", {
  # session$onFlushed callback, so the rebind message is sent repeatedly.
  session <- make_mock_session()
  for (i in 1:3) register_override_rebind(session)
  expect_length(session$flush_callbacks, 1)
})
