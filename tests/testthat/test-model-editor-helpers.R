test_that(".str: character(1) is returned unchanged", {
  str_fn <- getFromNamespace(".str", "openqalyshiny")
  expect_identical(str_fn("hello"), "hello")
})

test_that(".str: empty string is returned as empty string", {
  str_fn <- getFromNamespace(".str", "openqalyshiny")
  expect_identical(str_fn(""), "")
})

test_that(".str: NULL returns empty string", {
  str_fn <- getFromNamespace(".str", "openqalyshiny")
  expect_identical(str_fn(NULL), "")
})

test_that(".str: empty list returns empty string (key use-case: Shiny JSON coercion)", {
  str_fn <- getFromNamespace(".str", "openqalyshiny")
  expect_identical(str_fn(list()), "")
})

test_that(".str: numeric returns empty string", {
  str_fn <- getFromNamespace(".str", "openqalyshiny")
  expect_identical(str_fn(42), "")
})

test_that(".str: character vector of length > 1 returns empty string", {
  str_fn <- getFromNamespace(".str", "openqalyshiny")
  expect_identical(str_fn(c("a", "b")), "")
})

test_that(".str: logical TRUE returns empty string", {
  str_fn <- getFromNamespace(".str", "openqalyshiny")
  expect_identical(str_fn(TRUE), "")
})

test_that(".str: non-empty list returns empty string", {
  str_fn <- getFromNamespace(".str", "openqalyshiny")
  expect_identical(str_fn(list("a")), "")
})

test_that(".str: NA_character_ passes through (is character, length 1)", {
  str_fn <- getFromNamespace(".str", "openqalyshiny")
  expect_true(is.na(str_fn(NA_character_)))
})

test_that("dispatch_model_action: Markov transition endpoint edits preserve formula", {
  dispatch <- getFromNamespace("dispatch_model_action", "openqalyshiny")
  model <- openqaly::define_model("markov") |>
    openqaly::add_state("a", initial_prob = 1) |>
    openqaly::add_state("b", initial_prob = 0) |>
    openqaly::add_state("c", initial_prob = 0) |>
    openqaly::add_transition("a", "b", 0.2)

  from_edited <- dispatch(model, list(
    type = "edit_transition",
    model_type = "markov",
    from_state = "a",
    to_state = "b",
    field = "from_state",
    value = "c"
  ))

  expect_false(any(from_edited$transitions$from_state == "a" & from_edited$transitions$to_state == "b"))
  expect_true(any(from_edited$transitions$from_state == "c" & from_edited$transitions$to_state == "b"))
  expect_identical(from_edited$transitions$formula[[1]], "0.2")

  to_edited <- dispatch(model, list(
    type = "edit_transition",
    model_type = "markov",
    from_state = "a",
    to_state = "b",
    field = "to_state",
    value = "c"
  ))

  expect_false(any(to_edited$transitions$from_state == "a" & to_edited$transitions$to_state == "b"))
  expect_true(any(to_edited$transitions$from_state == "a" & to_edited$transitions$to_state == "c"))
  expect_identical(to_edited$transitions$formula[[1]], "0.2")
})

test_that("dispatch_model_action: custom PSM transition state edits preserve formula", {
  dispatch <- getFromNamespace("dispatch_model_action", "openqalyshiny")
  model <- openqaly::define_model("custom_psm") |>
    openqaly::add_state("a") |>
    openqaly::add_state("b") |>
    openqaly::add_transition("a", 0.6)

  edited <- dispatch(model, list(
    type = "edit_transition",
    model_type = "custom_psm",
    state = "a",
    field = "state",
    value = "b"
  ))

  expect_false("a" %in% edited$transitions$state)
  expect_true("b" %in% edited$transitions$state)
  expect_identical(edited$transitions$formula[[1]], "0.6")
})
