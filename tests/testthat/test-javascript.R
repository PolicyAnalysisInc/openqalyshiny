test_that("JavaScript unit tests pass", {
  skip_on_cran()
  skip_if(!nzchar(Sys.which("node")), "Node.js not available")
  pkg_root <- rprojroot::find_package_root_file()
  old_wd <- setwd(pkg_root)
  on.exit(setwd(old_wd), add = TRUE)
  old_ci <- Sys.getenv("CI", unset = NA_character_)
  Sys.setenv(CI = "true")
  on.exit({
    if (is.na(old_ci)) Sys.unsetenv("CI") else Sys.setenv(CI = old_ci)
  }, add = TRUE)
  status <- system2("npm", "test", stdout = FALSE, stderr = FALSE)
  expect_equal(status, 0L)
})
