test_that("get_psa_parameter_choices returns one entry per variable row", {
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

  choices <- get_psa_parameter_choices(metadata)
  tuples <- decode_psa_parameter_choices(unname(choices))

  expect_identical(
    unname(names(choices)),
    c("Response Probability (EF+CT)", "Response Probability (CT)")
  )
  expect_identical(tuples$variable, c("p_response", "p_response"))
  expect_identical(tuples$strategy, c("efgartigimod_ct", "ct_only"))
})


test_that("get_psa_parameter_choices disambiguates duplicate labels", {
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

  choices <- get_psa_parameter_choices(metadata)

  expect_identical(
    unname(names(choices)),
    c("Alpha Rate [Strategy 1 / g1]", "Alpha Rate [Strategy 2 / g1]")
  )
})


test_that("get_psa_parameter_choices returns empty when variables are absent", {
  expect_identical(get_psa_parameter_choices(list()), character(0))
  expect_identical(
    get_psa_parameter_choices(list(variables = tibble::tibble())),
    character(0)
  )
})
