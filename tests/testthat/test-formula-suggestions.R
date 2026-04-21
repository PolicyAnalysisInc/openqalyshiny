test_that("get_r_function_suggestions: 'base' returns data.frame with 5 correct columns", {
  result <- get_r_function_suggestions("base")
  expect_true(is.data.frame(result))
  expect_identical(names(result), c("name", "label", "description", "signature", "package"))
})

test_that("get_r_function_suggestions: 'base' returns non-empty result", {
  result <- get_r_function_suggestions("base")
  expect_gt(nrow(result), 0)
})

test_that("get_r_function_suggestions: 'sum' is present in base package results", {
  result <- get_r_function_suggestions("base")
  expect_true("sum" %in% result$name)
})

test_that("get_r_function_suggestions: label for 'sum' is 'sum()'", {
  result <- get_r_function_suggestions("base")
  expect_identical(result$label[result$name == "sum"], "sum()")
})

test_that("get_r_function_suggestions: package column equals 'base' for all rows", {
  result <- get_r_function_suggestions("base")
  expect_true(all(result$package == "base"))
})

test_that("get_r_function_suggestions: signature for 'sum' is non-empty", {
  result <- get_r_function_suggestions("base")
  sig <- result$signature[result$name == "sum"]
  expect_true(nchar(sig) > 0)
})

test_that("get_r_function_suggestions: include_internal=FALSE excludes dot-prefixed names", {
  result <- get_r_function_suggestions("base", include_internal = FALSE)
  expect_true(all(!grepl("^\\.", result$name)))
})

test_that("get_r_function_suggestions: invalid package returns 0-row data.frame with correct structure", {
  result <- get_r_function_suggestions("nonexistentpkg999xyz")
  expect_equal(nrow(result), 0)
  expect_identical(names(result), c("name", "label", "description", "signature", "package"))
})

test_that("get_r_function_suggestions: caching returns identical result on second call", {
  r1 <- get_r_function_suggestions("stats")
  r2 <- get_r_function_suggestions("stats")
  expect_identical(r1, r2)
})

test_that("get_r_function_suggestions: 'stats' package includes 'lm'", {
  result <- get_r_function_suggestions("stats")
  expect_true("lm" %in% result$name)
})

test_that("get_r_function_suggestions: NULL packages returns data.frame with correct structure", {
  result <- get_r_function_suggestions(NULL)
  expect_true(is.data.frame(result))
  expect_identical(names(result), c("name", "label", "description", "signature", "package"))
  expect_gt(nrow(result), 0)
})

test_that(".build_signature: no-arg function returns 'f()'", {
  build_sig <- getFromNamespace(".build_signature", "openqalyshiny")
  expect_identical(build_sig("f", function() NULL), "f()")
})

test_that(".build_signature: single required arg has no default", {
  build_sig <- getFromNamespace(".build_signature", "openqalyshiny")
  result <- build_sig("f", function(x) x)
  expect_identical(result, "f(x)")
})

test_that(".build_signature: arg with numeric default", {
  build_sig <- getFromNamespace(".build_signature", "openqalyshiny")
  result <- build_sig("f", function(x, y = 10) x + y)
  expect_match(result, "y = 10")
})

test_that(".build_signature: arg with NULL default", {
  build_sig <- getFromNamespace(".build_signature", "openqalyshiny")
  result <- build_sig("f", function(x, z = NULL) x)
  expect_match(result, "z = NULL")
})

test_that(".build_signature: function name is preserved exactly", {
  build_sig <- getFromNamespace(".build_signature", "openqalyshiny")
  result <- build_sig("my_func", function(a) a)
  expect_true(startsWith(result, "my_func("))
})
