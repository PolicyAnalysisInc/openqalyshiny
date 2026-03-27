test_that("get_psa_parameter_choices keeps strategy-specific tuples separate", {
  results <- list(
    segments = tibble::tibble(
      strategy = c("efgartigimod_ct", "ct_only", "efgartigimod_ct"),
      group = c("all_patients", "all_patients", "all_patients"),
      parameter_overrides = list(
        c(p_response = 0.73),
        c(p_response = 0.38),
        numeric(0)
      )
    )
  )
  metadata <- list(
    variables = tibble::tibble(
      name = c("p_response", "p_response"),
      strategy = c("efgartigimod_ct", "ct_only"),
      group = c("all_patients", "all_patients"),
      display_name = c("Response Probability (EF+CT)", "Response Probability (CT)")
    ),
    strategies = tibble::tibble(
      name = c("efgartigimod_ct", "ct_only"),
      display_name = c("Efgartigimod + CT", "CT-Only")
    ),
    groups = tibble::tibble(
      name = "all_patients",
      display_name = "All Patients"
    )
  )

  choices <- get_psa_parameter_choices(results, metadata)
  tuples <- decode_psa_parameter_choices(unname(choices))

  expect_identical(
    unname(names(choices)),
    c("Response Probability (EF+CT)", "Response Probability (CT)")
  )
  expect_identical(
    tuples$variable,
    c("p_response", "p_response")
  )
  expect_identical(
    tuples$strategy,
    c("efgartigimod_ct", "ct_only")
  )
  expect_identical(
    tuples$group,
    c("all_patients", "all_patients")
  )
})


test_that("get_psa_parameter_choices disambiguates duplicate labels", {
  results <- list(
    segments = tibble::tibble(
      strategy = c("s1", "s2"),
      group = c("g1", "g1"),
      parameter_overrides = list(
        c(alpha = 0.1),
        c(alpha = 0.2)
      )
    )
  )
  metadata <- list(
    variables = tibble::tibble(
      name = c("alpha", "alpha"),
      strategy = c("s1", "s2"),
      group = c("g1", "g1"),
      display_name = c("Alpha Rate", "Alpha Rate")
    ),
    strategies = tibble::tibble(
      name = c("s1", "s2"),
      display_name = c("Strategy 1", "Strategy 2")
    )
  )

  choices <- get_psa_parameter_choices(results, metadata)

  expect_identical(
    unname(names(choices)),
    c("Alpha Rate [Strategy 1 / g1]", "Alpha Rate [Strategy 2 / g1]")
  )
})


test_that("get_psa_parameter_choices respects specific group filters", {
  results <- list(
    segments = tibble::tibble(
      strategy = c("s1", "s1"),
      group = c("g1", "g2"),
      parameter_overrides = list(
        c(alpha = 0.1),
        c(alpha = 0.2)
      )
    )
  )
  metadata <- list(
    variables = tibble::tibble(
      name = c("alpha", "alpha"),
      strategy = c("s1", "s1"),
      group = c("g1", "g2"),
      display_name = c("Alpha G1", "Alpha G2")
    )
  )

  choices <- get_psa_parameter_choices(results, metadata, groups_input = "g2")
  tuples <- decode_psa_parameter_choices(unname(choices))

  expect_identical(unname(names(choices)), "Alpha G2")
  expect_identical(tuples$group, "g2")
})


test_that("get_psa_parameter_choices returns empty when overrides are absent", {
  expect_identical(get_psa_parameter_choices(list()), character(0))
  expect_identical(
    get_psa_parameter_choices(list(segments = tibble::tibble(simulation = 1))),
    character(0)
  )
})
