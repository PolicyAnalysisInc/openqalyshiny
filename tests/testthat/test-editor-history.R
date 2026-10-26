# Saving and restoring model versions, and undo history in the editor.

# A small markov model whose state names identify it (s200, s300, s400, ...).
make_tagged_model <- function(tag) {
  openqaly::define_model("markov") |>
    openqaly::add_state(paste0("s", tag), initial_prob = 1) |>
    openqaly::add_state("other", initial_prob = 0) |>
    openqaly::add_transition(paste0("s", tag), "other", 0.2)
}

# Model text exactly as save_model_with_history() stores it in a history entry
# (the file read back with readLines and joined by newlines).
model_file_text <- function(m) {
  tf <- tempfile(fileext = ".json")
  openqaly::write_model(m, tf, format = "json")
  paste(readLines(tf, warn = FALSE), collapse = "\n")
}

history_path <- function(fpath) {
  paste0(tools::file_path_sans_ext(fpath), "_history.json")
}

# Same JSON layout that write_history_file() writes: a current_message and a
# newest-first list of versions, each with timestamp, message and content.
history_version <- function(timestamp, content, message = "") {
  list(timestamp = timestamp, message = message, content = content)
}

write_history <- function(fpath, versions) {
  hpath <- history_path(fpath)
  history <- list(current_message = "", versions = versions)
  writeLines(jsonlite::toJSON(history, auto_unbox = TRUE, pretty = TRUE), hpath)
  hpath
}

# A model file on disk (state s300) with a history holding versions 200 and 100.
history_fixture <- function() {
  dir <- tempfile("model-history-")
  dir.create(dir)
  fpath <- file.path(dir, "model.json")
  openqaly::write_model(make_tagged_model("300"), fpath, format = "json")
  ts_200 <- "2026-10-20 10:00:00"
  versions <- list(
    history_version(ts_200, model_file_text(make_tagged_model("200"))),
    history_version("2026-10-19 10:00:00", model_file_text(make_tagged_model("100")))
  )
  write_history(fpath, versions)
  list(dir = dir, fpath = fpath, selected_ts = ts_200)
}

test_that("restore_history_version restores the selected version, not the file on disk", {
  fx <- history_fixture()
  restore_history_version(
    fx$fpath,
    selected = fx$selected_ts,
    unsaved = make_tagged_model("400")
  )
  restored_text <- paste(readLines(fx$fpath, warn = FALSE), collapse = "\n")
  expect_true(grepl("s200", restored_text, fixed = TRUE))
  expect_false(grepl("s300", restored_text, fixed = TRUE))
})

test_that("after restore the history contains the unsaved model as a version", {
  # the restore overwrites the file.
  fx <- history_fixture()
  restore_history_version(
    fx$fpath,
    selected = fx$selected_ts,
    unsaved = make_tagged_model("400")
  )
  h <- jsonlite::fromJSON(history_path(fx$fpath), simplifyVector = FALSE)
  contents <- vapply(h$versions, function(v) v$content, character(1))
  expect_true(any(grepl("s400", contents, fixed = TRUE)))
})

test_that("save_model_with_history never overwrites a damaged history file", {
  dir <- tempfile("damaged-history-")
  dir.create(dir)
  fpath <- file.path(dir, "model.json")
  openqaly::write_model(make_tagged_model("300"), fpath, format = "json")
  hpath <- history_path(fpath)
  damaged <- charToRaw('{"current_message": "x", "versions": [{"timestamp": "2026-10-2')
  writeBin(damaged, hpath)
  original <- readBin(hpath, "raw", n = file.size(hpath))
  files_before <- list.files(dir, full.names = TRUE)

  err <- NULL
  warn <- NULL
  withCallingHandlers(
    tryCatch(
      save_model_with_history(make_tagged_model("500"), fpath, message = "edit"),
      error = function(e) {
        # A missing helper must surface as a test error, not as a caught save error.
        if (grepl("could not find function", conditionMessage(e))) stop(e)
        err <<- e
      }
    ),
    warning = function(w) {
      warn <<- w
      invokeRestart("muffleWarning")
    }
  )

  expect_true(!is.null(err) || !is.null(warn))
  expect_identical(readBin(hpath, "raw", n = file.size(hpath)), original)

  # A timestamped backup (name containing a run of 8+ digits, e.g. a date)
  # with the original bytes is written alongside.
  new_files <- setdiff(list.files(dir, full.names = TRUE), files_before)
  backups <- new_files[grepl("[0-9]{8,}", basename(new_files))]
  expect_length(backups, 1)
  expect_identical(readBin(backups[1], "raw", n = file.size(backups[1])), original)
})

test_that("discard_last drops the pushed entry, leaves no redo, and returns the model", {
  # without creating a redo entry.
  history <- create_history_manager(max_size = 5)
  old_model <- list(name = "old")
  restored <- shiny::isolate({
    history$push(old_model)
    history$discard_last()
  })
  expect_false(shiny::isolate(history$can_redo()))
  expect_false(shiny::isolate(history$can_undo()))
  expect_identical(restored, old_model)
})
