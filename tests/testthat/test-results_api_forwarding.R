test_that("PSA result module forwards normalized openqaly argument names", {
  path <- test_path("..", "..", "R", "model_viewer_psa.r")
  src <- paste(readLines(path, warn = FALSE), collapse = "\n")

  expect_false(grepl("outcome_summary = input\\$outcome", src, fixed = FALSE))
  expect_false(grepl("cost_summary = input\\$cost_outcome", src, fixed = FALSE))
  expect_false(grepl("outcome_summary = input\\$health_outcome", src, fixed = FALSE))

  expect_true(grepl("args <- list\\(res, outcome = input\\$outcome\\)", src, fixed = FALSE))
  expect_true(grepl("health_outcome = input\\$health_outcome", src, fixed = FALSE))
  expect_true(grepl("cost_outcome = input\\$cost_outcome", src, fixed = FALSE))
})

test_that("single-metric DSA, scenario, and TWSA forwarding uses outcome", {
  files <- c(
    test_path("..", "..", "R", "model_viewer_dsa.r"),
    test_path("..", "..", "R", "model_viewer_scenario.r"),
    test_path("..", "..", "R", "model_viewer_twsa.r")
  )

  for (path in files) {
    src <- paste(readLines(path, warn = FALSE), collapse = "\n")
    expect_false(grepl("summary_name = input\\$outcome", src, fixed = FALSE), info = basename(path))
    expect_true(grepl("outcome = input\\$outcome", src, fixed = FALSE), info = basename(path))
  }
})

test_that("DSA outcomes and costs support absolute and differences selectors", {
  path <- test_path("..", "..", "R", "model_viewer_dsa.r")
  src <- paste(readLines(path, warn = FALSE), collapse = "\n")

  expect_true(grepl(
    "if \\(analysis_type %in% c\\(\"outcomes\", \"costs\", \"nmb\", \"ce\"\\)\\)",
    src,
    fixed = FALSE
  ))
  expect_true(grepl(
    "selectInput\\(ns\\(\"analysis_type\"\\), \"Type\"",
    src,
    fixed = FALSE
  ))
  expect_true(grepl(
    "input\\$analysis_type == \"absolute\"",
    src,
    fixed = FALSE
  ))
  expect_true(grepl(
    "args\\$interventions <- input\\$interventions",
    src,
    fixed = FALSE
  ))
  expect_true(grepl(
    "args\\$comparators <- input\\$comparators",
    src,
    fixed = FALSE
  ))
})

test_that("base-case CE result modules use health_outcome and cost_outcome", {
  path <- test_path("..", "..", "R", "model_viewer_results.r")
  src <- paste(readLines(path, warn = FALSE), collapse = "\n")

  expect_false(grepl("input\\$outcome_summary", src, fixed = FALSE))
  expect_false(grepl("input\\$cost_summary", src, fixed = FALSE))

  expect_true(grepl("health_outcome = input\\$health_outcome", src, fixed = FALSE))
  expect_true(grepl("cost_outcome = input\\$cost_outcome", src, fixed = FALSE))
  expect_true(grepl("selectInput\\(ns\\(\"health_outcome\"\\), \"Health Outcome\"", src, fixed = FALSE))
  expect_true(grepl("selectInput\\(ns\\(\"cost_outcome\"\\), \"Cost Outcome\"", src, fixed = FALSE))
})
