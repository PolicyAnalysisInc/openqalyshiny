test_that("JavaScript unit tests pass", {
  skip_on_cran()
  skip_if(!nzchar(Sys.which("node")), "Node.js not available")
  # The JS tests and package.json are excluded from the built package, so they
  # can only run from the source tree (not under R CMD check)
  pkg_root <- tryCatch(rprojroot::find_package_root_file(), error = function(e) NULL)
  skip_if(is.null(pkg_root) || !file.exists(file.path(pkg_root, "package.json")) ||
            !dir.exists(file.path(pkg_root, "tests", "js")),
          "JavaScript test sources not available (built package)")
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
