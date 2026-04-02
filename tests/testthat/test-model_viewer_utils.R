test_that("get_variable_choices returns one entry per variable row", {
  model <- openqaly::define_model("markov") |>
    openqaly::add_strategy("s1", "Strategy 1") |>
    openqaly::add_strategy("s2", "Strategy 2") |>
    openqaly::add_group("g1", "Group 1") |>
    openqaly::add_variable("alpha", 1, display_name = "Alpha Rate", strategy = "s1") |>
    openqaly::add_variable("alpha", 2, display_name = "Alpha Rate", strategy = "s2") |>
    openqaly::add_variable("beta", 3, display_name = "Beta Rate", group = "g1") |>
    openqaly::add_variable("gamma", 4)

  choices <- get_variable_choices(model)

  expect_identical(
    unname(names(choices)),
    c("Alpha Rate [Strategy 1]", "Alpha Rate [Strategy 2]", "Beta Rate", "gamma")
  )
  expect_length(unique(unname(choices)), 4)

  decoded <- decode_vbp_variable_choice(unname(choices))
  expect_identical(decoded$variable, c("alpha", "alpha", "beta", "gamma"))
  expect_identical(decoded$strategy, c("s1", "s2", NA_character_, NA_character_))
  expect_identical(decoded$group, c(NA_character_, NA_character_, "g1", NA_character_))
})

test_that("resolve_vbp_variable_choice prefers matching intervention strategy", {
  model <- openqaly::define_model("markov") |>
    openqaly::add_strategy("standard", "Standard Chemotherapy") |>
    openqaly::add_strategy("targeted", "Targeted Therapy") |>
    openqaly::add_variable("c_drug", 1000, display_name = "Monthly drug cost", strategy = "standard") |>
    openqaly::add_variable("c_drug", 2000, display_name = "Monthly drug cost", strategy = "targeted")

  selected <- resolve_vbp_variable_choice(model, "c_drug", intervention_strategy = "targeted")
  decoded <- decode_vbp_variable_choice(selected)

  expect_identical(decoded$variable, "c_drug")
  expect_identical(decoded$strategy, "targeted")
})

test_that("get_vbp_group_specific_variable_groups returns display names", {
  model <- openqaly::define_model("markov") |>
    openqaly::add_group("young", "Young Adults") |>
    openqaly::add_group("old", "Older Adults") |>
    openqaly::add_variable("cost_tx", 1000, group = "young") |>
    openqaly::add_variable("cost_tx", 2000, group = "old") |>
    openqaly::add_variable("cost_base", 500)

  expect_identical(
    get_vbp_group_specific_variable_groups(model, "cost_tx"),
    c("Young Adults", "Older Adults")
  )
  expect_identical(
    get_vbp_group_specific_variable_groups(model, "cost_base"),
    character(0)
  )
})
