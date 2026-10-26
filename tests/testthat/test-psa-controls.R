# The PSA parameters controls must not re-render when the variable selection
# changes (re-rendering would reset the selector while it is in use).

test_that("PSA parameter controls keep their state when variables are selected", {
  psa <- suppressMessages(openqaly::run_psa(
    openqaly::read_model(system.file("models", "example_markov", package = "openqaly")),
    n_sim = 10, seed = 1
  ))
  shiny::testServer(psaResultTabServer, args = list(
    analysis_type = "parameters",
    psa_results = shiny::reactive(psa),
    metadata = shiny::reactive(psa$metadata)
  ), {
    before <- output$controls$html
    choices <- unname(get_psa_parameter_choices(psa$metadata))
    session$setInputs(variables = choices[1:2])
    expect_identical(output$controls$html, before)
  })
})
