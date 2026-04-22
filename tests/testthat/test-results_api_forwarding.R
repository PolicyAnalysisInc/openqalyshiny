mock_meta <- list(
  summaries = data.frame(
    name         = c("qalys", "total_cost"),
    display_name = c("Total QALYs", "Total Cost"),
    type         = c("outcome", "cost"),
    stringsAsFactors = FALSE
  ),
  strategies = data.frame(name = character(0), display_name = character(0), stringsAsFactors = FALSE),
  groups     = data.frame(name = character(0), display_name = character(0), stringsAsFactors = FALSE)
)

test_that("PSA result module forwards normalized openqaly argument names", {
  psaResultTabServer <- getFromNamespace("psaResultTabServer", "openqalyshiny")

  shiny::testServer(
    psaResultTabServer,
    args = list(
      analysis_type = "outcomes",
      psa_results   = shiny::reactive(list()),
      metadata      = shiny::reactive(mock_meta)
    ),
    {
      session$flushReact()
      html <- output$controls$html
      expect_match(html, "Outcome Summary", fixed = TRUE)
      expect_false(grepl("outcome_summary", html, fixed = TRUE))
    }
  )

  shiny::testServer(
    psaResultTabServer,
    args = list(
      analysis_type = "nmb",
      psa_results   = shiny::reactive(list()),
      metadata      = shiny::reactive(mock_meta)
    ),
    {
      session$flushReact()
      html <- output$controls$html
      expect_match(html, "Health Outcome", fixed = TRUE)
      expect_match(html, "Cost Outcome", fixed = TRUE)
      expect_false(grepl("cost_summary", html, fixed = TRUE))
    }
  )
})

test_that("single-metric DSA, scenario, and TWSA forwarding uses outcome", {
  dsaResultTabServer <- getFromNamespace("dsaResultTabServer", "openqalyshiny")
  shiny::testServer(
    dsaResultTabServer,
    args = list(
      analysis_type = "outcomes",
      dsa_results   = shiny::reactive(list()),
      metadata      = shiny::reactive(mock_meta)
    ),
    {
      session$flushReact()
      html <- output$controls$html
      expect_match(html, "Outcome Summary", fixed = TRUE)
      expect_false(grepl("summary_name", html, fixed = TRUE))
    }
  )

  scenarioResultTabServer <- getFromNamespace("scenarioResultTabServer", "openqalyshiny")
  shiny::testServer(
    scenarioResultTabServer,
    args = list(
      analysis_type    = "outcomes",
      scenario_results = shiny::reactive(list()),
      metadata         = shiny::reactive(mock_meta)
    ),
    {
      session$flushReact()
      html <- output$controls$html
      expect_match(html, "Outcome Summary", fixed = TRUE)
      expect_false(grepl("summary_name", html, fixed = TRUE))
    }
  )

  twsaResultTabServer <- getFromNamespace("twsaResultTabServer", "openqalyshiny")
  shiny::testServer(
    twsaResultTabServer,
    args = list(
      analysis_type = "outcomes",
      twsa_results  = shiny::reactive(list()),
      metadata      = shiny::reactive(mock_meta)
    ),
    {
      session$flushReact()
      html <- output$controls$html
      expect_match(html, "Outcome Summary", fixed = TRUE)
      expect_false(grepl("summary_name", html, fixed = TRUE))
    }
  )
})

test_that("DSA outcomes and costs support absolute and differences selectors", {
  dsaResultTabServer <- getFromNamespace("dsaResultTabServer", "openqalyshiny")

  shiny::testServer(
    dsaResultTabServer,
    args = list(
      analysis_type = "outcomes",
      dsa_results   = shiny::reactive(list()),
      metadata      = shiny::reactive(mock_meta)
    ),
    {
      session$flushReact()
      html <- output$controls$html
      expect_match(html, "Type", fixed = TRUE)
      expect_match(html, "Absolute", fixed = TRUE)
      expect_match(html, "Differences", fixed = TRUE)
    }
  )
})

test_that("base-case CE result modules use health_outcome and cost_outcome", {
  pairwiseCeResultsServer <- getFromNamespace("pairwiseCeResultsServer", "openqalyshiny")
  shiny::testServer(
    pairwiseCeResultsServer,
    args = list(
      results  = shiny::reactive(list()),
      metadata = shiny::reactive(mock_meta)
    ),
    {
      session$flushReact()
      html <- output$controls$html
      expect_match(html, "Health Outcome", fixed = TRUE)
      expect_match(html, "Cost Outcome", fixed = TRUE)
      expect_false(grepl("outcome_summary", html, fixed = TRUE))
      expect_false(grepl("cost_summary", html, fixed = TRUE))
    }
  )

  incrementalCeResultsServer <- getFromNamespace("incrementalCeResultsServer", "openqalyshiny")
  shiny::testServer(
    incrementalCeResultsServer,
    args = list(
      results  = shiny::reactive(list()),
      metadata = shiny::reactive(mock_meta)
    ),
    {
      session$flushReact()
      html <- output$controls$html
      expect_match(html, "Health Outcome", fixed = TRUE)
      expect_match(html, "Cost Outcome", fixed = TRUE)
      expect_false(grepl("outcome_summary", html, fixed = TRUE))
      expect_false(grepl("cost_summary", html, fixed = TRUE))
    }
  )
})
