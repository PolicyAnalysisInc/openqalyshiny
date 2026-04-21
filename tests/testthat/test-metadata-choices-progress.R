make_metadata <- function() {
  list(
    summaries  = tibble::tibble(
      name         = c("qaly", "cost"),
      display_name = c("QALYs", "Costs"),
      type         = c("outcome", "cost")
    ),
    strategies = tibble::tibble(
      name         = c("s1", "s2"),
      display_name = c("Strategy 1", "Strategy 2")
    ),
    groups = tibble::tibble(
      name         = c("all", "g1"),
      display_name = c("All", "Group 1")
    )
  )
}

# --- get_outcome_summary_choices ---

test_that("get_outcome_summary_choices: returns named vector filtered to type 'outcome'", {
  result <- get_outcome_summary_choices(make_metadata())
  expect_true("qaly" %in% result)
  expect_false("cost" %in% result)
})

test_that("get_outcome_summary_choices: names are display_name, values are name", {
  result <- get_outcome_summary_choices(make_metadata())
  expect_identical(names(result), "QALYs")
  expect_identical(unname(result), "qaly")
})

test_that("get_outcome_summary_choices: empty metadata returns character(0)", {
  expect_identical(get_outcome_summary_choices(list()), character(0))
})

test_that("get_outcome_summary_choices: null metadata returns character(0)", {
  expect_identical(get_outcome_summary_choices(NULL), character(0))
})

# --- get_cost_summary_choices ---

test_that("get_cost_summary_choices: returns named vector filtered to type 'cost'", {
  result <- get_cost_summary_choices(make_metadata())
  expect_true("cost" %in% result)
  expect_false("qaly" %in% result)
})

test_that("get_cost_summary_choices: empty metadata returns character(0)", {
  expect_identical(get_cost_summary_choices(list()), character(0))
})

# --- get_strategy_choices ---

test_that("get_strategy_choices: returns named vector of strategies", {
  result <- get_strategy_choices(make_metadata())
  expect_true("s1" %in% result)
  expect_true("s2" %in% result)
})

test_that("get_strategy_choices: names are display names", {
  result <- get_strategy_choices(make_metadata())
  expect_true("Strategy 1" %in% names(result))
})

test_that("get_strategy_choices: empty metadata returns character(0)", {
  expect_identical(get_strategy_choices(list()), character(0))
})

# --- get_group_choices ---

test_that("get_group_choices: returns named vector including group values", {
  result <- get_group_choices(make_metadata())
  expect_true("g1" %in% result)
})

test_that("get_group_choices: empty metadata returns character(0)", {
  expect_identical(get_group_choices(list()), character(0))
})

# --- get_dsa_setting_choices ---

test_that("get_dsa_setting_choices: returns named character vector", {
  result <- get_dsa_setting_choices()
  expect_true(is.character(result))
  expect_true(!is.null(names(result)))
})

test_that("get_dsa_setting_choices: returns exactly 10 items", {
  result <- get_dsa_setting_choices()
  expect_length(result, 10)
})

test_that("get_dsa_setting_choices: includes expected setting names", {
  result <- get_dsa_setting_choices()
  expect_true("discount_rate" %in% result)
  expect_true("timeframe"     %in% result)
  expect_true("cycle_length"  %in% result)
})

test_that("get_dsa_setting_choices: pure function returns same result every call", {
  expect_identical(get_dsa_setting_choices(), get_dsa_setting_choices())
})

# --- create_progress_file ---

test_that("create_progress_file: returns a file path", {
  path <- create_progress_file()
  on.exit(unlink(path))
  expect_true(is.character(path))
  expect_length(path, 1)
})

test_that("create_progress_file: file exists after creation", {
  path <- create_progress_file()
  on.exit(unlink(path))
  expect_true(file.exists(path))
})

# --- make_file_progress_callback + read_file_progress ---

test_that("read_file_progress: nonexistent file returns zeros", {
  result <- read_file_progress("/nonexistent/path/file.progress")
  expect_equal(result$total,     0)
  expect_equal(result$completed, 0)
  expect_equal(result$pct,       0)
})

test_that("progress callback + read: setting total is reflected", {
  path <- create_progress_file()
  on.exit(unlink(path))
  cb <- make_file_progress_callback(path)
  cb(total = 5)
  result <- read_file_progress(path)
  expect_equal(result$total, 5)
})

test_that("progress callback + read: completed count increments correctly", {
  path <- create_progress_file()
  on.exit(unlink(path))
  cb <- make_file_progress_callback(path)
  cb(total = 5)
  cb(amount = 1)
  cb(amount = 1)
  cb(amount = 1)
  result <- read_file_progress(path)
  expect_equal(result$completed, 3)
})

test_that("progress callback + read: pct is calculated correctly", {
  path <- create_progress_file()
  on.exit(unlink(path))
  cb <- make_file_progress_callback(path)
  cb(total = 4)
  cb(amount = 1)
  cb(amount = 1)
  result <- read_file_progress(path)
  expect_equal(result$pct, 50, tolerance = 1)
})

test_that("read_file_progress: result has total, completed, and pct fields", {
  path <- create_progress_file()
  on.exit(unlink(path))
  result <- read_file_progress(path)
  expect_true(all(c("total", "completed", "pct") %in% names(result)))
})
