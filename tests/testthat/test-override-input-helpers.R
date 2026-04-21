test_that(".build_override_id: name only with empty strategy/group produces trailing underscores", {
  build_id <- getFromNamespace(".build_override_id", "openqalyshiny")
  result <- build_id("base", list(name = "discount_rate", strategy = "", group = ""))
  expect_identical(result, "base_discount_rate__")
})

test_that(".build_override_id: with strategy and empty group", {
  build_id <- getFromNamespace(".build_override_id", "openqalyshiny")
  result <- build_id("ov", list(name = "alpha", strategy = "s1", group = ""))
  expect_identical(result, "ov_alpha_s1_")
})

test_that(".build_override_id: with group and empty strategy", {
  build_id <- getFromNamespace(".build_override_id", "openqalyshiny")
  result <- build_id("ov", list(name = "alpha", strategy = "", group = "g1"))
  expect_identical(result, "ov_alpha__g1")
})

test_that(".build_override_id: with both strategy and group", {
  build_id <- getFromNamespace(".build_override_id", "openqalyshiny")
  result <- build_id("ov", list(name = "alpha", strategy = "s1", group = "g1"))
  expect_identical(result, "ov_alpha_s1_g1")
})

test_that(".build_override_id: spaces in name are replaced with underscore", {
  build_id <- getFromNamespace(".build_override_id", "openqalyshiny")
  result <- build_id("base", list(name = "my param", strategy = "", group = ""))
  expect_identical(result, "base_my_param__")
})

test_that(".build_override_id: hyphens in name are replaced with underscore", {
  build_id <- getFromNamespace(".build_override_id", "openqalyshiny")
  result <- build_id("base", list(name = "my-param", strategy = "", group = ""))
  expect_identical(result, "base_my_param__")
})

test_that(".build_override_id: hyphens in strategy are sanitized", {
  build_id <- getFromNamespace(".build_override_id", "openqalyshiny")
  result <- build_id("base", list(name = "x", strategy = "s-1", group = ""))
  expect_identical(result, "base_x_s_1_")
})

test_that(".build_override_id: NULL name treated as empty string", {
  build_id <- getFromNamespace(".build_override_id", "openqalyshiny")
  result <- build_id("base", list(name = NULL, strategy = "", group = ""))
  expect_identical(result, "base___")
})

test_that(".parse_timeframe_value: '5|year' parses to number='5', unit='year'", {
  parse_tf <- getFromNamespace(".parse_timeframe_value", "openqalyshiny")
  result <- parse_tf("5|year")
  expect_identical(result$number, "5")
  expect_identical(result$unit,   "year")
})

test_that(".parse_timeframe_value: '10|month' parses correctly", {
  parse_tf <- getFromNamespace(".parse_timeframe_value", "openqalyshiny")
  result <- parse_tf("10|month")
  expect_identical(result$number, "10")
  expect_identical(result$unit,   "month")
})

test_that(".parse_timeframe_value: '1|week' parses correctly", {
  parse_tf <- getFromNamespace(".parse_timeframe_value", "openqalyshiny")
  result <- parse_tf("1|week")
  expect_identical(result$number, "1")
  expect_identical(result$unit,   "week")
})

test_that(".parse_timeframe_value: value without pipe defaults unit to 'year'", {
  parse_tf <- getFromNamespace(".parse_timeframe_value", "openqalyshiny")
  result <- parse_tf("10")
  expect_identical(result$number, "10")
  expect_identical(result$unit,   "year")
})

test_that(".parse_timeframe_value: empty string defaults unit to 'year'", {
  parse_tf <- getFromNamespace(".parse_timeframe_value", "openqalyshiny")
  result <- parse_tf("")
  expect_identical(result$unit, "year")
})

test_that(".parse_timeframe_value: NULL input causes an error", {
  parse_tf <- getFromNamespace(".parse_timeframe_value", "openqalyshiny")
  expect_error(parse_tf(NULL))
})

test_that(".parse_timeframe_value: result is a named list with 'number' and 'unit'", {
  parse_tf <- getFromNamespace(".parse_timeframe_value", "openqalyshiny")
  result <- parse_tf("5|year")
  expect_true(is.list(result))
  expect_identical(names(result), c("number", "unit"))
})

test_that(".parse_timeframe_value: number is returned as character, not numeric", {
  parse_tf <- getFromNamespace(".parse_timeframe_value", "openqalyshiny")
  result <- parse_tf("5|year")
  expect_true(is.character(result$number))
})
