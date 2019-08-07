#' Merge multiple label lexicas into one
#'
#' This takes multiple [LabelLexicon] class objects and merges them together into
#' a single [LabelLexicon] class object.
#' In case some class objects have entries with the same name, the 
#' class objects passed in later overwrite the class objects passed in first 
#' (e.g. in \code{merge_lexicas(x, y, z)}: The lexicon \code{z} overwrites
#' \code{x} and \code{y}. The lexicon \code{y} overwrites \code{x}).
#' @param ... Multiple [LabelLexicon] class objects arguments
#' @rdname LabelLexicon-class
#' @return The merged [LabelLexicon] class object
#' @include label_lexicon.R
merge_lexicas <- function(...) {
  UseMethod("merge_lexicas")
}

#' Merge multiple label lexicas into one
#'
#' This takes multiple [LabelLexicon] class objects and merges them together into
#' a single [LabelLexicon] class object.
#' In case some class objects have entries with the same name, the 
#' class objects passed in later overwrite the class objects passed in first 
#' (e.g. in \code{merge_lexicas(x, y, z)}: The lexicon \code{z} overwrites
#' \code{x} and \code{y}. The lexicon \code{y} overwrites \code{x}).
#' @param ... Multiple [LabelLexicon] class objects arguments
#' @param show_wanings A logical flag that defines, whether warnings should be shown (\code{TRUE}) or not (\code{FALSE})
#' @rdname LabelLexicon-class
#' @return The merged [LabelLexicon] class object
#' @export
merge_lexicas <- function(..., show_warnings = TRUE) {
  err_handler <- composerr("Error while calling 'merge_lexicas'")
  args <- list(...)
  if (length(args) < 2)
    err_handler("There must be at least two LabelLexicon objects passed as arguments.")
  for (i in seq_len(length(args))) {
    if (class(args[[i]]) != "LabelLexicon") 
      err_handler(paste0("The argument at position '", i, "' is not a LabelLexicon class object."))
  }
  if (show_warnings) {
    overwritten_names <- c()
    all_names <- c()
    lapply(args, function(x) {
      overwritten_names <<- unique(c(
        overwritten_names,
        intersect(all_names, names(x))
      ))
      all_names <<- unique(c(all_names, names(x)))
    })
    if (length(overwritten_names) > 0)
      warning(paste(
        "The following LabelLexicon entries will be overwritten:",
        paste0(overwritten_names, collapse = ", ")
      ))
  }
  # Merge the 1st with the 2nd LabelLexicon object
  x <- args[[1]]
  lapply(names(args[[2]]), function(name) x[[name]] <<- args[[2]][[name]])
  # Merge the resulting LabelLexicon with the rest
  if (length(args) > 2) {
    args[[1]] <- x
    args[[2]] <- NULL
    args$show_warnings <- FALSE
    x <- do.call(merge_lexicas, args)
  }
  x
}
