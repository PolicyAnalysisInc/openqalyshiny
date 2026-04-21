make_state_json <- function(categories) {
  jsonlite::toJSON(categories, auto_unbox = TRUE, null = "null")
}

make_single_cat_json <- function(input_type = "numeric",
                                  default_value = "0.5",
                                  extra_override_fields = list()) {
  base_override <- list(
    name          = "alpha",
    title         = "Alpha",
    display_name  = "Alpha",
    type          = "variable",
    strategy      = "",
    group         = "",
    input_type    = input_type,
    input_config  = list(min = "0", max = "1", step_size = "0.1"),
    default_value = default_value
  )
  override <- c(base_override, extra_override_fields)
  make_state_json(list(list(
    name      = "Params",
    general   = FALSE,
    overrides = list(override)
  )))
}

# --- .parse_manager_state ---

test_that(".parse_manager_state: returns a list", {
  fn <- getFromNamespace(".parse_manager_state", "openqalyshiny")
  expect_true(is.list(fn(make_single_cat_json())))
})

test_that(".parse_manager_state: list length matches category count", {
  fn <- getFromNamespace(".parse_manager_state", "openqalyshiny")
  expect_length(fn(make_single_cat_json()), 1)
})

test_that(".parse_manager_state: category name is preserved", {
  fn <- getFromNamespace(".parse_manager_state", "openqalyshiny")
  expect_identical(fn(make_single_cat_json())[[1]]$name, "Params")
})

test_that(".parse_manager_state: general=FALSE is preserved", {
  fn <- getFromNamespace(".parse_manager_state", "openqalyshiny")
  expect_false(fn(make_single_cat_json())[[1]]$general)
})

test_that(".parse_manager_state: general=TRUE is preserved", {
  fn <- getFromNamespace(".parse_manager_state", "openqalyshiny")
  json <- make_state_json(list(list(
    name = "Cat", general = TRUE,
    overrides = list(list(
      name = "x", title = "X", display_name = "X", type = "variable",
      strategy = "", group = "", input_type = "numeric",
      input_config = list(), default_value = "1"
    ))
  )))
  expect_true(fn(json)[[1]]$general)
})

test_that(".parse_manager_state: overrides is a list", {
  fn <- getFromNamespace(".parse_manager_state", "openqalyshiny")
  expect_true(is.list(fn(make_single_cat_json())[[1]]$overrides))
})

test_that(".parse_manager_state: override name is preserved", {
  fn <- getFromNamespace(".parse_manager_state", "openqalyshiny")
  expect_identical(fn(make_single_cat_json())[[1]]$overrides[[1]]$name, "alpha")
})

test_that(".parse_manager_state: input_config$min coerced to numeric", {
  fn <- getFromNamespace(".parse_manager_state", "openqalyshiny")
  result <- fn(make_single_cat_json())[[1]]$overrides[[1]]$input_config$min
  expect_true(is.numeric(result))
  expect_equal(result, 0, tolerance = 1e-9)
})

test_that(".parse_manager_state: input_config$max coerced to numeric", {
  fn <- getFromNamespace(".parse_manager_state", "openqalyshiny")
  result <- fn(make_single_cat_json())[[1]]$overrides[[1]]$input_config$max
  expect_true(is.numeric(result))
  expect_equal(result, 1, tolerance = 1e-9)
})

test_that(".parse_manager_state: input_config$step_size coerced to numeric", {
  fn <- getFromNamespace(".parse_manager_state", "openqalyshiny")
  result <- fn(make_single_cat_json())[[1]]$overrides[[1]]$input_config$step_size
  expect_true(is.numeric(result))
  expect_equal(result, 0.1, tolerance = 1e-9)
})

test_that(".parse_manager_state: default_value coerced to numeric for 'numeric' input_type", {
  fn <- getFromNamespace(".parse_manager_state", "openqalyshiny")
  result <- fn(make_single_cat_json("numeric", "0.035"))[[1]]$overrides[[1]]$default_value
  expect_true(is.numeric(result))
  expect_equal(result, 0.035, tolerance = 1e-9)
})

test_that(".parse_manager_state: default_value coerced to numeric for 'slider' input_type", {
  fn <- getFromNamespace(".parse_manager_state", "openqalyshiny")
  result <- fn(make_single_cat_json("slider", "0.7"))[[1]]$overrides[[1]]$default_value
  expect_true(is.numeric(result))
  expect_equal(result, 0.7, tolerance = 1e-9)
})

test_that(".parse_manager_state: default_value stays character for 'formula' input_type", {
  fn <- getFromNamespace(".parse_manager_state", "openqalyshiny")
  result <- fn(make_single_cat_json("formula", "x + y"))[[1]]$overrides[[1]]$default_value
  expect_true(is.character(result))
  expect_identical(result, "x + y")
})

test_that(".parse_manager_state: default_value stays character for 'dropdown' input_type", {
  fn <- getFromNamespace(".parse_manager_state", "openqalyshiny")
  result <- fn(make_single_cat_json("dropdown", "option_a"))[[1]]$overrides[[1]]$default_value
  expect_true(is.character(result))
})

test_that(".parse_manager_state: overridden_expression preserved when present", {
  fn <- getFromNamespace(".parse_manager_state", "openqalyshiny")
  json <- make_single_cat_json(extra_override_fields = list(overridden_expression = "bc * 1.1"))
  result <- fn(json)[[1]]$overrides[[1]]$overridden_expression
  expect_identical(result, "bc * 1.1")
})

test_that(".parse_manager_state: overridden_expression absent when not in JSON", {
  fn <- getFromNamespace(".parse_manager_state", "openqalyshiny")
  result <- fn(make_single_cat_json())[[1]]$overrides[[1]]
  expect_false("overridden_expression" %in% names(result))
})

test_that(".parse_manager_state: strategy and group fields preserved", {
  fn <- getFromNamespace(".parse_manager_state", "openqalyshiny")
  json <- make_state_json(list(list(
    name = "Cat", general = FALSE,
    overrides = list(list(
      name = "x", title = "X", display_name = "X", type = "variable",
      strategy = "s1", group = "g1", input_type = "numeric",
      input_config = list(), default_value = "1"
    ))
  )))
  ov <- fn(json)[[1]]$overrides[[1]]
  expect_identical(ov$strategy, "s1")
  expect_identical(ov$group,    "g1")
})

test_that(".parse_manager_state: empty JSON array returns empty list", {
  fn <- getFromNamespace(".parse_manager_state", "openqalyshiny")
  expect_identical(fn("[]"), list())
})

test_that(".parse_manager_state: multiple categories handled correctly", {
  fn <- getFromNamespace(".parse_manager_state", "openqalyshiny")
  json <- make_state_json(list(
    list(name = "Cat1", general = FALSE, overrides = list()),
    list(name = "Cat2", general = TRUE,  overrides = list())
  ))
  result <- fn(json)
  expect_length(result, 2)
  expect_identical(result[[2]]$name, "Cat2")
})

test_that(".parse_manager_state: empty overrides list is preserved", {
  fn <- getFromNamespace(".parse_manager_state", "openqalyshiny")
  json <- make_state_json(list(list(name = "Cat", general = FALSE, overrides = list())))
  expect_length(fn(json)[[1]]$overrides, 0)
})

# --- .build_manager_card ---

test_that(".build_manager_card: contains 'override-manager-card' class", {
  build_card <- getFromNamespace(".build_manager_card", "openqalyshiny")
  html <- as.character(build_card(list(name = "alpha", title = "Alpha", input_type = "numeric")))
  expect_match(html, "override-manager-card", fixed = TRUE)
})

test_that(".build_manager_card: shows override title", {
  build_card <- getFromNamespace(".build_manager_card", "openqalyshiny")
  html <- as.character(build_card(list(name = "alpha", title = "Alpha Rate", input_type = "slider")))
  expect_match(html, "Alpha Rate", fixed = TRUE)
})

test_that(".build_manager_card: falls back to name when title is absent", {
  build_card <- getFromNamespace(".build_manager_card", "openqalyshiny")
  html <- as.character(build_card(list(name = "alpha", input_type = "numeric")))
  expect_match(html, "alpha", fixed = TRUE)
})

test_that(".build_manager_card: includes data-override JSON attribute", {
  build_card <- getFromNamespace(".build_manager_card", "openqalyshiny")
  html <- as.character(build_card(list(name = "alpha", title = "Alpha", input_type = "numeric")))
  expect_match(html, "data-override=", fixed = TRUE)
})

test_that(".build_manager_card: shows description when present", {
  build_card <- getFromNamespace(".build_manager_card", "openqalyshiny")
  html <- as.character(build_card(list(
    name = "alpha", title = "Alpha", input_type = "numeric",
    description = "The discount rate"
  )))
  expect_match(html, "The discount rate", fixed = TRUE)
})

# --- .build_manager_column ---

test_that(".build_manager_column: contains 'override-manager-column' class", {
  build_col <- getFromNamespace(".build_manager_column", "openqalyshiny")
  html <- as.character(build_col(list(name = "Params", general = FALSE, overrides = list()), 1))
  expect_match(html, "override-manager-column", fixed = TRUE)
})

test_that(".build_manager_column: embeds category index in data attribute", {
  build_col <- getFromNamespace(".build_manager_column", "openqalyshiny")
  html <- as.character(build_col(list(name = "Params", general = FALSE, overrides = list()), 2))
  expect_match(html, 'data-category-index="2"', fixed = TRUE)
})

test_that(".build_manager_column: shows category name", {
  build_col <- getFromNamespace(".build_manager_column", "openqalyshiny")
  html <- as.character(build_col(list(name = "My Category", general = FALSE, overrides = list()), 1))
  expect_match(html, "My Category", fixed = TRUE)
})

test_that(".build_manager_column: contains add-override button", {
  build_col <- getFromNamespace(".build_manager_column", "openqalyshiny")
  html <- as.character(build_col(list(name = "Params", general = FALSE, overrides = list()), 1))
  expect_match(html, "override-manager-add-btn", fixed = TRUE)
})
