# Declared package dependencies.

desc_fields <- function(field) {
  desc <- read.dcf(system.file("DESCRIPTION", package = "openqalyshiny"))
  raw <- desc[1, field]
  if (is.na(raw)) return(character())
  parts <- trimws(strsplit(gsub("\n", " ", raw), ",")[[1]])
  trimws(sub("\\s*\\(.*$", "", parts[nzchar(parts)]))
}

test_that("htmlwidgets is in Imports and openqalyregimen is in Suggests", {
  # via library() but not declared.
  expect_true("htmlwidgets" %in% desc_fields("Imports"))
  expect_true("openqalyregimen" %in% desc_fields("Suggests"))
})
