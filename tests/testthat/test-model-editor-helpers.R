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
