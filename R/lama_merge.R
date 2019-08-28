#' Merge multiple label lexicas into one
#'
#' This function takes multiple [LabelDictionary][new_dictionary()] class
#' objects and merges them together into
#' a single [LabelDictionary][new_dictionary()] class object.
#' In case some class objects have entries with the same name, the 
#' class objects passed in later overwrite the class objects passed in first 
#' (e.g. in \code{lama_merge(x, y, z)}: The lexicon \code{z} overwrites
#' \code{x} and \code{y}. The lexicon \code{y} overwrites \code{x}).
#' @param x A [LabelDictionary][new_dictionary()] class object
#' @param y A [LabelDictionary][new_dictionary()] class object
#' @param ... Optional additional multiple [LabelDictionary][new_dictionary()]
#' class objects
#' @param show_wanings A logical flag that defines, whether warnings should be
#' shown (\code{TRUE}) or not (\code{FALSE})
#' @return The merged [LabelDictionary][new_dictionary()] class object
#' @seealso [translate()], [new_dictionary()], [lama_rename()], [tran_select()],
#' [lama_mutate()], [lama_read()], [lama_write()]
#' @rdname lama_merge
#' @export
#' @include dictionary.R
lama_merge <- function(.data, x, y, ..., show_warnings = TRUE) {
  UseMethod("lama_merge")
}

#' @rdname lama_merge
#' @export
lama_merge.LabelDictionary <- function(x, y, ..., show_warnings = TRUE) {
  err_handler <- composerr("Error while calling 'lama_merge'")
  if (!is.dictionary(y))
    err_handler(paste0("The argument at position '", 2, "' is not a LabelDictionary class object."))
  args <- list(...)
  if (length(args) > 0)
    lapply(seq_len(length(args)), function(i) {
      if (!is.dictionary(ars[[i]]))
        err_handler(paste0("The argument at position '", i + 2, "' is not a LabelDictionary class object."))
    })
  if (show_warnings) {
    overwritten_names <- c()
    all_names <- c()
    lapply(as.list(c(x, y, as.list(args))), function(x) {
      overwritten_names <<- unique(c(
        overwritten_names,
        intersect(all_names, names(x))
      ))
      all_names <<- unique(c(all_names, names(x)))
    })
    if (length(overwritten_names) > 0)
      warning(paste0(
        "The following LabelDictionary entries will be overwritten: ",
       stringify(overwritten_names),
       "."
      ))
  }
  # Merge the 1st with the 2nd LabelDictionary object
  lapply(names(y), function(name) x[[name]] <<- y[[name]])
  # Merge the resulting LabelDictionary with the rest
  if (length(args) > 0) {
    args$show_warnings <- FALSE
    x <- do.call(merge, as.list(c(x = x, args, show_warnings = FALSE)))
  }
  x
}
