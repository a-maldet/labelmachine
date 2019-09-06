#' Merge multiple label lexicas into one
#'
#' This function takes multiple [lama_dictionary][new_dictionary()] class
#' objects and merges them together into
#' a single [lama_dictionary][new_dictionary()] class object.
#' In case some class objects have entries with the same name, the 
#' class objects passed in later overwrite the class objects passed in first 
#' (e.g. in \code{lama_merge(x, y, z)}: The lexicon \code{z} overwrites
#' \code{x} and \code{y}. The lexicon \code{y} overwrites \code{x}).
#' @param ... Two or more [lama_dictionary][new_dictionary()]
#' class objects, which should be merged together.
#' @param show_warnings A logical flag that defines, whether warnings should be
#' shown (\code{TRUE}) or not (\code{FALSE}).
#' @return The merged [lama_dictionary][new_dictionary()] class object
#' @seealso [lama_translate()], [new_dictionary()], [lama_rename()], [lama_select()],
#' [lama_mutate()], [lama_read()], [lama_write()]
#' @rdname lama_merge
#' @export
#' @include lama_dictionary.R
lama_merge <- function(..., show_warnings = TRUE) {
  UseMethod("lama_merge")
}

#' @rdname lama_merge
#' @export
lama_merge.lama_dictionary <- function(..., show_warnings = TRUE) {
  err_handler <- composerr("Error while calling 'lama_merge'")
  args <- list(...)
  if (length(args) < 2)
    err_handler(paste0("There must be at least two lama_dictionary class object passed into."))
  lapply(seq_len(length(args)), function(i) {
    if (!is.dictionary(args[[i]]))
      err_handler(paste0(
        "Invalid argument at position ",
        stringify(i),
        ": Object is not a lama_dictionary class object."
      ))
  })
  if (!is.logical(show_warnings) || is.na(show_warnings) || length(show_warnings) != 1)
    err_handler("The value passed into 'show_warnings' must be either 'TRUE' or 'FALSE'.")
  if (show_warnings) {
    overwritten_names <- c()
    all_names <- c()
    lapply(as.list(args), function(x) {
      overwritten_names <<- unique(c(
        overwritten_names,
        intersect(all_names, names(x))
      ))
      all_names <<- unique(c(all_names, names(x)))
    })
    if (length(overwritten_names) > 0)
      warning(paste0(
        "The following lama_dictionary entries will be overwritten: ",
       stringify(overwritten_names),
       "."
      ))
  }
  # Merge the 1st with the 2nd lama_dictionary object
  x <- args[[1]]
  y <- args[[2]]
  lapply(names(y), function(name) x[[name]] <<- y[[name]])
  # Merge the resulting lama_dictionary with the rest
  if (length(args) > 2) {
    return(do.call(
      lama_merge, 
      as.list(c(list(x), args[3:length(args)], show_warnings = FALSE))
    ))
  }
  x
}
