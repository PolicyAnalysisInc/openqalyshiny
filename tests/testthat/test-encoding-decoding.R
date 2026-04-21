test_that("encode_vbp_variable_choice: encodes all args as pipe-delimited string", {
  result <- encode_vbp_variable_choice("alpha", "s1", "g1", 3)
  expect_identical(result, "3|||alpha|||s1|||g1")
})

test_that("encode_vbp_variable_choice: NULL strategy and group become empty strings", {
  result <- encode_vbp_variable_choice("alpha", NULL, NULL, 1)
  expect_identical(result, "1|||alpha||||||")
})

test_that("encode_vbp_variable_choice: round-trip with decode preserves variable/strategy/group", {
  encoded <- encode_vbp_variable_choice("beta", "s2", "g1", 2)
  decoded <- decode_vbp_variable_choice(encoded)
  expect_identical(decoded$variable[1], "beta")
  expect_identical(decoded$strategy[1], "s2")
  expect_identical(decoded$group[1],    "g1")
})

test_that("normalize_vbp_variable_choice: returns the variable name from encoded value", {
  encoded <- encode_vbp_variable_choice("gamma", "s1", "g1", 1)
  expect_identical(normalize_vbp_variable_choice(encoded), "gamma")
})

test_that("normalize_vbp_variable_choice: returns NULL for NULL input", {
  expect_null(normalize_vbp_variable_choice(NULL))
})

test_that("normalize_vbp_variable_choice: returns NULL for empty character input", {
  expect_null(normalize_vbp_variable_choice(character(0)))
})

test_that("encode_psa_parameter_choice: encodes variable/strategy/group with ||| separator", {
  result <- encode_psa_parameter_choice("p_response", "s1", "g1")
  expect_identical(result, "p_response|||s1|||g1")
})

test_that("encode_psa_parameter_choice: NULL strategy and group produce empty segments", {
  result <- encode_psa_parameter_choice("p_response", NULL, NULL)
  expect_identical(result, "p_response||||||")
})

test_that("encode_psa_parameter_choice: round-trip with decode_psa_parameter_choices", {
  encoded <- encode_psa_parameter_choice("alpha", "s1", "g2")
  decoded <- decode_psa_parameter_choices(encoded)
  expect_identical(decoded$variable[1], "alpha")
  expect_identical(decoded$strategy[1], "s1")
  expect_identical(decoded$group[1],    "g2")
})

test_that("map_psa_strategy_name: returns display_name when strategy found in metadata", {
  meta <- list(strategies = tibble::tibble(name = "s1", display_name = "Strategy 1"))
  expect_identical(map_psa_strategy_name("s1", meta), "Strategy 1")
})

test_that("map_psa_strategy_name: returns original name when strategy not in metadata", {
  meta <- list(strategies = tibble::tibble(name = "s2", display_name = "S2"))
  expect_identical(map_psa_strategy_name("s1", meta), "s1")
})

test_that("map_psa_strategy_name: returns original name when strategies is NULL", {
  expect_identical(map_psa_strategy_name("s1", list()), "s1")
})

test_that("map_psa_strategy_name: returns empty string for empty strategy_name", {
  meta <- list(strategies = tibble::tibble(name = "s1", display_name = "S1"))
  expect_identical(map_psa_strategy_name("", meta), "")
})

test_that("map_psa_strategy_name: returns original when display_name is NA", {
  meta <- list(strategies = tibble::tibble(name = "s1", display_name = NA_character_))
  expect_identical(map_psa_strategy_name("s1", meta), "s1")
})

test_that("map_psa_group_name: returns display_name when group found in metadata", {
  meta <- list(groups = tibble::tibble(name = "g1", display_name = "Group 1"))
  expect_identical(map_psa_group_name("g1", meta), "Group 1")
})

test_that("map_psa_group_name: returns original name when not in metadata", {
  meta <- list(groups = tibble::tibble(name = "g2", display_name = "G2"))
  expect_identical(map_psa_group_name("g1", meta), "g1")
})

test_that("map_psa_group_name: returns original when groups is NULL", {
  expect_identical(map_psa_group_name("g1", list()), "g1")
})

# Note: resolve_param_group and build_psa_parameter_args are nested closures
# defined inside psaResultTabServer — they cannot be accessed outside that scope.
