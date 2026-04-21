test_that("JavaScript unit tests pass", {
  skip_on_cran()
  skip_if(!nzchar(Sys.which("node")), "Node.js not available")
  pkg_root <- rprojroot::find_package_root_file()
  status <- system2(
    "npm", "test",
    stdout = FALSE, stderr = FALSE,
    env = c(Sys.getenv(), "CI=true"),
    wd = pkg_root
  )
  expect_equal(status, 0L)
})
