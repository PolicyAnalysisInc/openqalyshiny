# Model files and their version history
#
# Each model file has a "<name>_history.json" file next to it with the
# previous versions, newest first. Saving a model first stores the content
# currently on disk as a version, then writes the new model.

#' Path of a Model File's History File
#' @param fpath Path to the model file.
#' @return Path to the history JSON file.
#' @keywords internal
history_file_path <- function(fpath) {
  paste0(tools::file_path_sans_ext(fpath), "_history.json")
}

#' Read a Model File's Version History
#'
#' @param fpath Path to the model file.
#' @param strict If `TRUE`, a history file that cannot be parsed is an error
#'   of class `oq_history_parse_error`; if `FALSE` it reads as empty.
#' @return List with `current_message` and `versions` (newest first).
#' @keywords internal
read_history_file <- function(fpath, strict = FALSE) {
  hpath <- history_file_path(fpath)
  if (!file.exists(hpath)) return(list(current_message = "", versions = list()))
  h <- tryCatch(
    jsonlite::fromJSON(hpath, simplifyVector = FALSE),
    error = function(e) {
      if (!strict) return(NULL)
      stop(structure(
        class = c("oq_history_parse_error", "error", "condition"),
        list(message = paste0("Version history file '", basename(hpath),
                              "' could not be read: ", conditionMessage(e)),
             call = NULL)
      ))
    }
  )
  if (is.null(h)) return(list(current_message = "", versions = list()))
  if (is.null(h$versions)) h$versions <- list()
  if (is.null(h$current_message)) h$current_message <- ""
  h
}

#' Write a Model File's Version History
#' @param fpath Path to the model file.
#' @param history List with `current_message` and `versions`.
#' @keywords internal
write_history_file <- function(fpath, history) {
  hpath <- history_file_path(fpath)
  writeLines(jsonlite::toJSON(history, auto_unbox = TRUE, pretty = TRUE), hpath)
}

#' Save a Model, Keeping the Previous Content as a Version
#'
#' If the model file exists, its current content is added to the version
#' history before the model is written. A history file that cannot be read is
#' never overwritten: it is copied to a timestamped backup and the save stops
#' with an error, so no versions are lost.
#'
#' @param m The model.
#' @param fpath Path to the model file (`.json` or YAML).
#' @param message Message stored with the new current version.
#' @return `fpath`, invisibly.
#' @keywords internal
save_model_with_history <- function(m, fpath, message = "") {
  ext <- tolower(tools::file_ext(fpath))
  format <- if (ext == "json") "json" else "yaml"
  if (file.exists(fpath)) {
    history <- tryCatch(
      read_history_file(fpath, strict = TRUE),
      oq_history_parse_error = function(e) {
        backup <- backup_history_file(fpath)
        stop("The version history file could not be read, so nothing was saved. ",
             "It was copied to '", basename(backup), "'. Fix or remove '",
             basename(history_file_path(fpath)), "' and save again.", call. = FALSE)
      }
    )
    old_content <- paste(readLines(fpath, warn = FALSE), collapse = "\n")
    prev_message <- if (nzchar(history$current_message)) history$current_message else "Initial version"
    new_entry <- list(
      timestamp = as.character(Sys.time()),
      message = prev_message,
      content = old_content
    )
    history$versions <- c(list(new_entry), history$versions)
    history$current_message <- message
    write_history_file(fpath, history)
  }
  openqaly::write_model(m, fpath, format = format)
  invisible(fpath)
}

# Copy the history file to "<name>_damaged_<YYYYmmddHHMMSS>[_n].json" and
# return the backup path.
backup_history_file <- function(fpath) {
  hpath <- history_file_path(fpath)
  stem <- paste0(tools::file_path_sans_ext(hpath), "_damaged_", format(Sys.time(), "%Y%m%d%H%M%S"))
  backup <- paste0(stem, ".json")
  n <- 1
  while (file.exists(backup)) {
    backup <- paste0(stem, "_", n, ".json")
    n <- n + 1
  }
  file.copy(hpath, backup, overwrite = FALSE)
  backup
}

# A model from a history entry's content (JSON or YAML text)
parse_history_content <- function(content) {
  if (grepl("^\\s*\\{", content)) {
    openqaly::read_model_json(text = content)
  } else {
    openqaly::read_model_yaml(text = content)
  }
}

#' Load a Version From a Model File's History
#' @param fpath Path to the model file.
#' @param version_id Position of the version in the history (1 = newest).
#' @return The model.
#' @keywords internal
load_version_model <- function(fpath, version_id) {
  history <- read_history_file(fpath)
  idx <- as.integer(version_id)
  if (is.na(idx) || idx < 1 || idx > length(history$versions)) {
    stop("Invalid version ID: ", version_id)
  }
  parse_history_content(history$versions[[idx]]$content)
}

#' Restore a Version of a Model File
#'
#' Finds the version saved at `selected` and captures its content before
#' anything is written. If `unsaved` is given, it is saved first (so it is kept
#' as a version); then the selected version is written as the current model,
#' with the content it replaces kept in the history.
#'
#' @param fpath Path to the model file.
#' @param selected Timestamp of the version to restore.
#' @param unsaved Optional model with unsaved changes to keep as a version.
#' @return The restored model, invisibly.
#' @keywords internal
restore_history_version <- function(fpath, selected, unsaved = NULL) {
  history <- read_history_file(fpath, strict = TRUE)
  ts <- vapply(history$versions, function(v) as.character(v$timestamp), character(1))
  idx <- match(as.character(selected), ts)
  if (is.na(idx)) stop("Version not found: ", selected, call. = FALSE)
  restored <- parse_history_content(history$versions[[idx]]$content)
  if (!is.null(unsaved)) {
    save_model_with_history(unsaved, fpath, message = "Auto-save before restore")
  }
  save_model_with_history(restored, fpath, message = paste("Restored version from", selected))
  invisible(restored)
}
