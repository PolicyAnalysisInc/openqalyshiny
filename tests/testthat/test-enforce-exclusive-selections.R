test_that("enforce_exclusive_groups: adding 'all' makes it the only selection", {
  result <- enforce_exclusive_groups(c("s1", "all"), c("s1"))
  expect_identical(result, "all")
})

test_that("enforce_exclusive_groups: adding 'all_groups' makes it the only selection", {
  result <- enforce_exclusive_groups(c("s1", "all_groups"), c("s1"))
  expect_identical(result, "all_groups")
})

test_that("enforce_exclusive_groups: adding non-exclusive when 'all' is selected removes 'all'", {
  result <- enforce_exclusive_groups(c("all", "s1"), c("all"))
  expect_identical(result, "s1")
})

test_that("enforce_exclusive_groups: adding non-exclusive when 'all_groups' is selected removes 'all_groups'", {
  result <- enforce_exclusive_groups(c("all_groups", "s1"), c("all_groups"))
  expect_identical(result, "s1")
})

test_that("enforce_exclusive_groups: no exclusive involved returns new_val unchanged", {
  result <- enforce_exclusive_groups(c("s1", "s2"), c("s1"))
  expect_identical(result, c("s1", "s2"))
})

test_that("enforce_exclusive_groups: empty new_val returns empty", {
  result <- enforce_exclusive_groups(character(0), c("s1"))
  expect_identical(result, character(0))
})

test_that("enforce_exclusive_groups: adding exclusive when another exclusive already selected", {
  result <- enforce_exclusive_groups(c("all", "all_groups"), c("all"))
  expect_identical(result, "all_groups")
})

test_that("enforce_exclusive_strategies: empty changed_new reverts to prev state", {
  result <- enforce_exclusive_strategies(
    changed_new  = character(0),
    other_current = c("s2"),
    changed_prev  = c("s1")
  )
  expect_identical(result$changed, c("s1"))
  expect_identical(result$other,   c("s2"))
})

test_that("enforce_exclusive_strategies: conflict removed and other still has items", {
  result <- enforce_exclusive_strategies(
    changed_new   = c("s1", "s2"),
    other_current = c("s2", "s3"),
    changed_prev  = c("s1")
  )
  expect_identical(result$changed, c("s1", "s2"))
  expect_identical(result$other,   "s3")
})

test_that("enforce_exclusive_strategies: auto-swap moves pre-existing from changed to other", {
  result <- enforce_exclusive_strategies(
    changed_new   = c("s1", "s2"),
    other_current = c("s2"),
    changed_prev  = c("s1")
  )
  # s2 conflicts with other_current; removing s2 leaves other empty
  # s1 is pre-existing (in changed_prev), so it gets moved to other
  expect_identical(result$changed, "s2")
  expect_identical(result$other,   "s1")
})

test_that("enforce_exclusive_strategies: fallback uses freed strategy when no pre-existing available", {
  result <- enforce_exclusive_strategies(
    changed_new   = c("s3"),
    other_current = c("s3"),
    changed_prev  = c("s1")
  )
  # s3 conflicts; removing leaves other empty; no pre-existing (s3 was newly added)
  # freed: s1 was in changed_prev but not changed_new
  expect_identical(result$other, "s1")
})

test_that("enforce_exclusive_strategies: no conflict is simple pass-through", {
  result <- enforce_exclusive_strategies(
    changed_new   = c("s1", "s2"),
    other_current = c("s3"),
    changed_prev  = c("s1")
  )
  expect_identical(result$changed, c("s1", "s2"))
  expect_identical(result$other,   "s3")
})

test_that("enforce_exclusive_strategies: returns list with 'changed' and 'other' elements", {
  result <- enforce_exclusive_strategies(c("s1"), c("s2"), c("s1"))
  expect_true(all(c("changed", "other") %in% names(result)))
})

test_that("lookup_model_display_name: returns display_name when match found", {
  df <- data.frame(name = c("s1", "s2"), display_name = c("Strategy 1", "Strategy 2"), stringsAsFactors = FALSE)
  expect_identical(lookup_model_display_name(df, "s1"), "Strategy 1")
})

test_that("lookup_model_display_name: returns item_name when display_name is NA", {
  df <- data.frame(name = "s1", display_name = NA_character_, stringsAsFactors = FALSE)
  expect_identical(lookup_model_display_name(df, "s1"), "s1")
})

test_that("lookup_model_display_name: returns item_name when display_name is empty string", {
  df <- data.frame(name = "s1", display_name = "", stringsAsFactors = FALSE)
  expect_identical(lookup_model_display_name(df, "s1"), "s1")
})

test_that("lookup_model_display_name: returns item_name when no match", {
  df <- data.frame(name = "s2", display_name = "S2", stringsAsFactors = FALSE)
  expect_identical(lookup_model_display_name(df, "s1"), "s1")
})

test_that("lookup_model_display_name: returns item_name when df is empty", {
  df <- data.frame(name = character(0), display_name = character(0), stringsAsFactors = FALSE)
  expect_identical(lookup_model_display_name(df, "s1"), "s1")
})

test_that("lookup_model_display_name: handles null df gracefully", {
  expect_identical(lookup_model_display_name(NULL, "s1"), "s1")
})
