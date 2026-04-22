test_that("formulaInput exposes commit timing via data-update-on", {
  default_html <- as.character(formulaInput("formula_default"))
  blur_html    <- as.character(formulaInput("formula_blur", updateOn = "blur"))

  expect_match(default_html, 'data-update-on="change"', fixed = TRUE)
  expect_match(blur_html, 'data-update-on="blur"', fixed = TRUE)
  expect_match(default_html, 'data-autocomplete-parent="body"', fixed = TRUE)
})

test_that("override numeric inputs commit on blur", {
  build_override_input <- getFromNamespace(".build_override_input", "openqalyshiny")
  tag <- build_override_input(
    "override_numeric",
    list(
      input_type = "numeric",
      default_value = 0.5,
      input_config = list(min = 0, max = 1, step = 0.1)
    ),
    model = NULL
  )

  html <- as.character(tag)

  expect_match(html, 'data-update-on="blur"', fixed = TRUE)
})

test_that("override sliders use the finish-only binding", {
  build_override_input <- getFromNamespace(".build_override_input", "openqalyshiny")
  tag <- build_override_input(
    "override_slider",
    list(
      input_type = "slider",
      default_value = 0.5,
      input_config = list(min = 0, max = 1, step = 0.1)
    ),
    model = NULL
  )

  html <- as.character(tag)

  expect_match(html, 'class="override-slider-input"', fixed = TRUE)
  expect_match(html, 'data-commit-mode="finish"', fixed = TRUE)
  expect_false(grepl("\\bjs-range-slider\\b", html))
})

test_that("override formula inputs use blur-based commit timing", {
  build_override_input <- getFromNamespace(".build_override_input", "openqalyshiny")
  tag <- build_override_input(
    "override_formula",
    list(
      input_type = "formula",
      default_value = "x + y"
    ),
    model = NULL
  )

  html <- as.character(tag)

  expect_match(html, 'data-update-on="blur"', fixed = TRUE)
})
