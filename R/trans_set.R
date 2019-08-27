#' Change or append a variable translation to an existing [LabelDictionary][new_dictionary()] object
#'
#' The functions [trans_set()] and [trans_set_()] alter a 
#' [LabelDictionary][new_dictionary()] object. Tjey either alter
#' or append a translation to a [LabelDictionary][new_dictionary()] object.
#' The function [trans_set()] uses named arguments to assign the translations
#' to the new names (similar to [dplyr:mutate()]), whereas the function
#' [trans_set_()] is takes a character string \code{key} holding the
#' name to which the translation should be assigned and a named character
#' vector \code{translation} holding the actual translation mapping.
#' @param .data A [LabelDictionary][new_dictionary()] object
#' @param ... One or more unquoted expressions separated by commas. Use named
#' arguments, e.g. ‘new_transation_name = c(a = "A", b = "B")’, to set
#' translations (named character vectors) to new translation names.
#' @return An updated [LabelDictionary][new_dictionary()] class object.
#' @seealso [translate()], [new_dictionary()], [trans_rename()], [tran_select()],
#' [trans_merge()], [read_dictionary()], [write_dictionary()]
#' @rdname trans_set
#' @export
trans_set <- function(.data, ...) {
  UseMethod("trans_set")
}

#' @rdname trans_set
#' @export
trans_set.LabelDictionary <- function(.data, ...) {
  args <- list(...)
  err_handler <- composerr("Error while calling 'trans_set'")
  if (!is.dictionary(.data))
    err_handler("The object given in the argument '.data' must be a LabelDictionary class object.")
  if (length(args) == 0)
    err_handler("Translation assignments are missing.")
  new <- names(args)
  if (is.null(new) || any(new == ""))
    err_handler("Translation assignment is invalid. Use named arguments, e.g. ‘new_name = X’, where 'X' is a named character vector and 'new_name' is the name to which the translation should be assigned.")
  duplicates <- table(new)
  duplicates <- names(duplicates[duplicates > 1])
  if (length(duplicates) > 0)
    err_handler(paste0(
      "The following translation names are used more than once: ",
      stringify(duplicates),
      "."
    ))
  invalid <- !is.syntactic(new)
  if (any(invalid))
    err_handler(paste0(
      "The following translation names are invalid: ",
      stringify(new[invalid]),
      "."
    ))
  lapply(names(args), function(x) {
    translation <- args[[x]]
    if (
      (!is.list(translation) && !is.character(translation)) || 
      is.null(names(translation)) || length(translation) == 0 ||
      (is.list(translation) && (!all(sapply(translation, is.character))) || (!all(sapply(translation, length) == 1)))
    )
      err_handler(paste0(
        "The object assigned to the translation name '",
        x,
        "' must either be a named character vector or a named list ",
        "holding character strings."
      ))
    if (is.list(translation))
      translation <- unlist(translation)
    .data[[x]] <<- translation 
  })
  .data
}

#' @param key The name of the variable translation that should be altered.
#' It can also be variable translation name that does not exist yet.
#' @param translation A named character vector holding the new variable
#' translation that should be assigned to the name given in argument \code{key}.
#' The names of the character vector \code{translation} correspond to the
#' original variable values that should be replaced by the new labels.
#' The values in the character vector \code{translations} are the labels
#' that should be assigned to the original values.
#' @rdname trans_set
#' @export
trans_set_ <- function(.data, key, translation) {
  UseMethod("trans_set_")
}

#' @rdname trans_set
#' @method trans_set LabelDictionary
#' @export
trans_set_.LabelDictionary <- function(.data, key, translation) {
  err_handler <- composerr("Error while calling 'trans_set_'")
  if (!is.dictionary(.data))
    err_handler("The object given in the argument '.data' must be a LabelDictionary class object.")
  if (!is.character(key) || length(key) != 1)
    err_handler("The object given in the argument 'key' must be a character string.")
  if (
    (!is.list(translation) && !is.character(translation)) || 
    is.null(names(translation)) || length(translation) == 0 ||
    (is.list(translation) && (!all(sapply(translation, is.character))) || (!all(sapply(translation, length) == 1)))
  )
    err_handler("The object given in the argument 'translation' must either be a named character vector or a named list holding character strings.")
  if (is.list(translation))
    translation <- unlist(translation)
  .data[[key]] <- translation
  .data
}
