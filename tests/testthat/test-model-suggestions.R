make_markov <- function() {
  openqaly::define_model("markov") |>
    openqaly::add_strategy("s1", "Strategy 1") |>
    openqaly::add_variable("alpha", 1)
}

make_psm <- function() {
  openqaly::define_model("psm") |>
    openqaly::add_strategy("s1", "Strategy 1") |>
    openqaly::add_variable("beta", 0.5)
}

# --- .validate_model ---

test_that(".validate_model: accepts oq_model", {
  validate_model <- getFromNamespace(".validate_model", "openqalyshiny")
  expect_no_error(validate_model(make_markov()))
})

test_that(".validate_model: accepts psm model", {
  validate_model <- getFromNamespace(".validate_model", "openqalyshiny")
  expect_no_error(validate_model(make_psm()))
})

test_that(".validate_model: rejects plain string", {
  validate_model <- getFromNamespace(".validate_model", "openqalyshiny")
  expect_error(validate_model("not_a_model"))
})

test_that(".validate_model: rejects plain list", {
  validate_model <- getFromNamespace(".validate_model", "openqalyshiny")
  expect_error(validate_model(list(type = "markov")))
})

test_that(".validate_model: rejects NULL", {
  validate_model <- getFromNamespace(".validate_model", "openqalyshiny")
  expect_error(validate_model(NULL))
})

# --- .validate_context ---

test_that(".validate_context: all 14 valid contexts pass without error", {
  validate_context <- getFromNamespace(".validate_context", "openqalyshiny")
  valid_contexts <- c(
    "variable", "transition_markov", "transition_psm", "transition_custom_psm",
    "value", "initial_probability", "group_weight", "variable_sampling",
    "multivariate_sampling", "dsa_bound", "scenario_override", "twsa_bound",
    "override", "tree_node"
  )
  for (ctx in valid_contexts) {
    expect_no_error(validate_context(ctx))
  }
})

test_that(".validate_context: rejects invalid string", {
  validate_context <- getFromNamespace(".validate_context", "openqalyshiny")
  expect_error(validate_context("bad_context"), "Invalid context")
})

test_that(".validate_context: rejects empty string", {
  validate_context <- getFromNamespace(".validate_context", "openqalyshiny")
  expect_error(validate_context(""))
})

test_that(".validate_context: is case-sensitive", {
  validate_context <- getFromNamespace(".validate_context", "openqalyshiny")
  expect_error(validate_context("Variable"))
})

# --- .get_keywords_for_context ---

test_that(".get_keywords_for_context: 'none' returns character(0) for any model type", {
  fn <- getFromNamespace(".get_keywords_for_context", "openqalyshiny")
  expect_identical(fn("none", "markov"), character(0))
  expect_identical(fn("none", "psm"),   character(0))
})

test_that(".get_keywords_for_context: 'bc_only' returns exactly 'bc'", {
  fn <- getFromNamespace(".get_keywords_for_context", "openqalyshiny")
  expect_identical(fn("bc_only", "markov"), "bc")
  expect_identical(fn("bc_only", "psm"),   "bc")
})

test_that(".get_keywords_for_context: 'oq_vars' + markov includes state_time keywords", {
  fn <- getFromNamespace(".get_keywords_for_context", "openqalyshiny")
  result <- fn("oq_vars", "markov")
  expect_true("state_cycle" %in% result)
  expect_true("state_year"  %in% result)
})

test_that(".get_keywords_for_context: 'oq_vars' + markov includes base keywords", {
  fn <- getFromNamespace(".get_keywords_for_context", "openqalyshiny")
  result <- fn("oq_vars", "markov")
  expect_true("cycle"  %in% result)
  expect_true("group"  %in% result)
})

test_that(".get_keywords_for_context: 'oq_vars' + psm excludes state_time keywords", {
  fn <- getFromNamespace(".get_keywords_for_context", "openqalyshiny")
  result <- fn("oq_vars", "psm")
  expect_false("state_cycle" %in% result)
  expect_false("state_year"  %in% result)
})

test_that(".get_keywords_for_context: 'oq_vars' + psm still has base keywords", {
  fn <- getFromNamespace(".get_keywords_for_context", "openqalyshiny")
  result <- fn("oq_vars", "psm")
  expect_true("cycle" %in% result)
})

test_that(".get_keywords_for_context: 'oq_all' + markov includes state_time keywords", {
  fn <- getFromNamespace(".get_keywords_for_context", "openqalyshiny")
  expect_true("state_cycle" %in% fn("oq_all", "markov"))
})

test_that(".get_keywords_for_context: 'oq_all' + psm excludes state_time keywords", {
  fn <- getFromNamespace(".get_keywords_for_context", "openqalyshiny")
  expect_false("state_cycle" %in% fn("oq_all", "psm"))
})

# --- .build_keyword_suggestions ---

test_that(".build_keyword_suggestions: returns a data.frame", {
  fn <- getFromNamespace(".build_keyword_suggestions", "openqalyshiny")
  expect_true(is.data.frame(fn(c("cycle", "year"))))
})

test_that(".build_keyword_suggestions: has exactly 4 required columns in correct order", {
  fn <- getFromNamespace(".build_keyword_suggestions", "openqalyshiny")
  expect_identical(names(fn(c("cycle"))), c("name", "label", "description", "signature"))
})

test_that(".build_keyword_suggestions: one row per keyword", {
  fn <- getFromNamespace(".build_keyword_suggestions", "openqalyshiny")
  expect_equal(nrow(fn(c("cycle", "year", "bc"))), 3)
})

test_that(".build_keyword_suggestions: name and label equal the keyword", {
  fn <- getFromNamespace(".build_keyword_suggestions", "openqalyshiny")
  result <- fn(c("cycle"))
  expect_identical(result$name[1],  "cycle")
  expect_identical(result$label[1], "cycle")
})

test_that(".build_keyword_suggestions: description populated for known keyword 'cycle'", {
  fn <- getFromNamespace(".build_keyword_suggestions", "openqalyshiny")
  result <- fn(c("cycle"))
  expect_true(nchar(result$description[1]) > 0)
})

test_that(".build_keyword_suggestions: signature is always empty string", {
  fn <- getFromNamespace(".build_keyword_suggestions", "openqalyshiny")
  result <- fn(c("cycle", "year", "bc"))
  expect_true(all(result$signature == ""))
})

test_that(".build_keyword_suggestions: unknown keyword gets empty description without error", {
  fn <- getFromNamespace(".build_keyword_suggestions", "openqalyshiny")
  expect_no_error(result <- fn(c("unknown_kw_xyz")))
  expect_identical(result$description[1], "")
})

# --- .empty_suggestions_df ---

test_that(".empty_suggestions_df: returns a data.frame", {
  fn <- getFromNamespace(".empty_suggestions_df", "openqalyshiny")
  expect_true(is.data.frame(fn()))
})

test_that(".empty_suggestions_df: has zero rows", {
  fn <- getFromNamespace(".empty_suggestions_df", "openqalyshiny")
  expect_equal(nrow(fn()), 0)
})

test_that(".empty_suggestions_df: has exactly 4 columns in correct order", {
  fn <- getFromNamespace(".empty_suggestions_df", "openqalyshiny")
  expect_identical(names(fn()), c("name", "label", "description", "signature"))
})

test_that(".empty_suggestions_df: columns are all character type", {
  fn <- getFromNamespace(".empty_suggestions_df", "openqalyshiny")
  result <- fn()
  expect_true(all(vapply(result, is.character, logical(1))))
})

# --- get_model_terms ---

test_that("get_model_terms: returns a list for 'variable' context", {
  expect_true(is.list(get_model_terms(make_markov(), "variable")))
})

test_that("get_model_terms: 'variable' context has keyword and variable elements", {
  result <- get_model_terms(make_markov(), "variable")
  expect_true("keyword"  %in% names(result))
  expect_true("variable" %in% names(result))
})

test_that("get_model_terms: added variable 'alpha' appears in variable terms", {
  result <- get_model_terms(make_markov(), "variable")
  expect_true("alpha" %in% result$variable)
})

test_that("get_model_terms: markov model has state_cycle keyword in transition_markov context", {
  result <- get_model_terms(make_markov(), "transition_markov")
  expect_true("state_cycle" %in% result$keyword)
})

test_that("get_model_terms: psm model lacks state_cycle in transition_markov context", {
  result <- get_model_terms(make_psm(), "transition_markov")
  expect_false("state_cycle" %in% result$keyword)
})

test_that("get_model_terms: 'initial_probability' context has no keyword element", {
  result <- get_model_terms(make_markov(), "initial_probability")
  expect_false("keyword" %in% names(result))
})

test_that("get_model_terms: 'variable_sampling' context keyword is only 'bc'", {
  result <- get_model_terms(make_markov(), "variable_sampling")
  expect_identical(result$keyword, "bc")
})

test_that("get_model_terms: invalid model causes error", {
  expect_error(get_model_terms("not_a_model", "variable"))
})

test_that("get_model_terms: invalid context causes error", {
  expect_error(get_model_terms(make_markov(), "bad_context"), "Invalid context")
})

# --- get_model_suggestions ---

test_that("get_model_suggestions: returns a named list", {
  result <- get_model_suggestions(make_markov(), "variable", include_r_functions = FALSE)
  expect_true(is.list(result))
  expect_false(is.null(names(result)))
})

test_that("get_model_suggestions: each element is a data.frame", {
  result <- get_model_suggestions(make_markov(), "variable", include_r_functions = FALSE)
  expect_true(all(vapply(result, is.data.frame, logical(1))))
})

test_that("get_model_suggestions: keyword data.frame has 4 required columns", {
  result <- get_model_suggestions(make_markov(), "variable", include_r_functions = FALSE)
  expect_identical(names(result$keyword), c("name", "label", "description", "signature"))
})

test_that("get_model_suggestions: variable 'alpha' appears in variable suggestions", {
  result <- get_model_suggestions(make_markov(), "variable", include_r_functions = FALSE)
  expect_true("alpha" %in% result$variable$name)
})

test_that("get_model_suggestions: include_r_functions=TRUE adds r_function element", {
  result <- get_model_suggestions(make_markov(), "variable",
    include_r_functions = TRUE, r_packages = "base")
  expect_true("r_function" %in% names(result))
})

test_that("get_model_suggestions: r_function data.frame has 'package' column", {
  result <- get_model_suggestions(make_markov(), "variable",
    include_r_functions = TRUE, r_packages = "base")
  expect_true("package" %in% names(result$r_function))
})

test_that("get_model_suggestions: include_r_functions=FALSE has no r_function element", {
  result <- get_model_suggestions(make_markov(), "variable", include_r_functions = FALSE)
  expect_false("r_function" %in% names(result))
})

test_that("get_model_suggestions: 'initial_probability' context has no keyword element", {
  result <- get_model_suggestions(make_markov(), "initial_probability", include_r_functions = FALSE)
  expect_false("keyword" %in% names(result))
})

test_that("get_model_suggestions: invalid model causes error", {
  expect_error(get_model_suggestions(list(), "variable"))
})
