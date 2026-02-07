# Cache environment for memoization
.suggestion_cache <- new.env(parent = emptyenv())

#' Get R Function Suggestions
#'
#' Extracts function information from loaded packages for autocomplete.
#' Results are memoized per session for performance.
#'
#' @param packages Character vector of package names. If NULL, uses all loaded
#'   packages.
#' @param include_internal Include non-exported functions (starting with .).
#'   Default is FALSE.
#'
#' @return A data frame with columns: name, label, description, signature,
#'   package
#'
#' @examples
#' \dontrun{
#' # Run the interactive demo with one line:
#' run_formula_input_demo()
#'
#' # Or get functions from specific packages:
#' r_funcs <- get_r_function_suggestions(c("base", "stats"))
#'
#' # Use in formulaInput
#' formulaInput("formula1", suggestions = list(r_function = r_funcs))
#' }
#'
#' @seealso \code{\link{run_formula_input_demo}} for an interactive demo,
#'   \code{\link{formulaInput}} for the input component
#'
#' @export
get_r_function_suggestions <- function(packages = NULL, include_internal = FALSE) {

  if (is.null(packages)) {
    packages <- sub("^package:", "", grep("^package:", search(), value = TRUE))
  }

  # Build cache key from sorted packages and include_internal flag
  cache_key <- paste0(
    paste(sort(packages), collapse = ","),
    "|internal=", include_internal
  )

  # Return cached result if available
  if (exists(cache_key, envir = .suggestion_cache, inherits = FALSE)) {
    return(get(cache_key, envir = .suggestion_cache, inherits = FALSE))
  }

  results <- lapply(packages, function(pkg) {
    tryCatch({
      ns <- getNamespace(pkg)
      fn_names <- getNamespaceExports(ns)
      if (!include_internal) {
        fn_names <- fn_names[!grepl("^\\.", fn_names)]
      }

      # Filter to functions only
      fn_names <- fn_names[vapply(fn_names, function(n) {
        is.function(tryCatch(get(n, envir = ns), error = function(e) NULL))
      }, logical(1))]

      if (length(fn_names) == 0) {
        return(NULL)
      }

      do.call(rbind, lapply(fn_names, function(fn_name) {
        fn <- get(fn_name, envir = ns)
        data.frame(
          name = fn_name,
          label = paste0(fn_name, "()"),
          description = .extract_help_description(fn_name, pkg),
          signature = .build_signature(fn_name, fn),
          package = pkg,
          stringsAsFactors = FALSE
        )
      }))
    }, error = function(e) NULL)
  })

  result <- do.call(rbind, results)
  if (is.null(result)) {
    # Return empty data frame with correct structure
    result <- data.frame(
      name = character(0),
      label = character(0),
      description = character(0),
      signature = character(0),
      package = character(0),
      stringsAsFactors = FALSE
    )
  }

  # Cache the result
  assign(cache_key, result, envir = .suggestion_cache)

  result
}

#' Build function signature from formals
#'
#' @param fn_name Function name
#' @param fn Function object
#'
#' @return Character string of function signature
#' @keywords internal
.build_signature <- function(fn_name, fn) {
  args_list <- formals(fn)
  if (is.null(args_list) || length(args_list) == 0) {
    return(paste0(fn_name, "()"))
  }

  arg_strs <- mapply(function(arg, val) {
    if (is.symbol(val) && nchar(as.character(val)) == 0) {
      arg
    } else {
      paste0(arg, " = ", deparse(val, width.cutoff = 30)[1])
    }
  }, names(args_list), args_list, SIMPLIFY = TRUE, USE.NAMES = FALSE)

  paste0(fn_name, "(", paste(arg_strs, collapse = ", "), ")")
}

#' Extract description from help file
#'
#' @param fn_name Function name
#' @param pkg Package name
#'
#' @return Character string of function description or NA_character_
#' @keywords internal
.extract_help_description <- function(fn_name, pkg) {
  tryCatch({
    help_file <- utils::help(fn_name, package = (pkg))
    if (length(help_file) == 0) {
      return(NA_character_)
    }

    # Use utils::.getHelpFile to parse the Rd file
    Rd <- utils:::.getHelpFile(as.character(help_file))
    desc_tags <- Rd[vapply(Rd, function(x) {
      attr(x, "Rd_tag") == "\\description"
    }, logical(1))]

    if (length(desc_tags) > 0) {
      txt <- paste(unlist(desc_tags[[1]]), collapse = " ")
      txt <- gsub("\\s+", " ", trimws(txt))
      if (nchar(txt) > 200) {
        txt <- paste0(substr(txt, 1, 197), "...")
      }
      return(txt)
    }
    NA_character_
  }, error = function(e) NA_character_)
}
