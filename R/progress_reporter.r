#' Create a temporary progress file for cross-process progress reporting
#'
#' @return Path to the created temp file
#' @keywords internal
create_progress_file <- function() {
  tempfile(fileext = ".progress")
}

#' Create a file-based progress callback for openqaly's progress parameter
#'
#' Returns a closure that writes progress ticks to a file. Designed to be
#' called inside a future worker process. Uses only base R functions so the
#' closure serializes cleanly.
#'
#' @param progress_file Path to the progress file
#' @return A function compatible with openqaly's progress callback API
#' @keywords internal
make_file_progress_callback <- function(progress_file) {
  function(total = NULL, amount = NULL) {
    if (!is.null(total)) {
      writeLines(as.character(total), progress_file)
    } else if (!is.null(amount)) {
      cat("1\n", file = progress_file, append = TRUE)
    }
  }
}

#' Read progress from a progress file
#'
#' @param progress_file Path to the progress file
#' @return A list with total, completed, and pct (0-1)
#' @keywords internal
read_file_progress <- function(progress_file) {
  lines <- tryCatch(
    readLines(progress_file, warn = FALSE),
    error = function(e) character(0)
  )
  if (length(lines) == 0) {
    return(list(total = 0L, completed = 0L, pct = 0))
  }
  total <- as.integer(lines[1])
  if (is.na(total) || total == 0L) {
    return(list(total = 0L, completed = 0L, pct = 0))
  }
  completed <- length(lines) - 1L
  list(total = total, completed = completed, pct = min(completed / total, 1))
}
