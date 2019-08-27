#' Rename multiple variable translations in a [LabelDictionary][new_dictionary()] object
#'
#' The functions [trans_rename()] and [trans_rename_()]
#' are used to rename one or more variable translations inside of a 
#' [LabelDictionary][new_dictionary()] class object.
#' The function [trans_rename()] uses non-standard evaluation,
#' whereas [trans_rename_()] is the standard evaluation alternativ.
#' @param .data A [LabelDictionary][new_dictionary()] object, holding the
#' variable translations
#' @param ... One or more unquoted expressions separated by commas. Use named
#' arguments, e.g. ‘new_name = old_name’, to rename selected variables.
#' @return The updated [LabelDictionary][new_dictionary()] class object.
#' @seealso [translate()], [new_dictionary()], [trans_select()],
#' [trans_mutate()], [trans_merge()], [read_dictionary()], [write_dictionary()]
#' @rdname trans_rename
#' @export
trans_rename <- function(.data, ...) {
  UseMethod("trans_rename")
}

#' @export
trans_rename.LabelDictionary <- function(.data, ...) {
  args <- rlang::quos(...)
  err_handler <- composerr("Error while calling 'rename'")
  if (!is.dictionary(.data))
    err_handler("The object given in the argument '.data' must be a LabelDictionary class object.")
  if (length(args) == 0)
    err_handler("Name assignments are missing.")
  new <- names(args)
  if (is.null(new) || any(new == ""))
    err_handler("Name assignment is invalid. Use named arguments, e.g. ‘new_name = old_name’, to rename translations.")
  duplicates <- table(new)
  duplicates <- names(duplicates[duplicates > 1])
  if (length(duplicates) > 0)
    err_handler(paste(
      "The following names are used more than once:",
      stringify(duplicates)
    ))
  invalid <- !is.syntactic(new)
  if (any(invalid))
    err_handler(paste(
      "The following variable names are invalid:",
      stringify(new[invalid])
    ))
  old <- as.character(sapply(args, function(x) {
    if (!rlang::quo_is_symbolic(x)) {
      x_name <- rlang::quo_get_expr(x)
    } else {
      x_name <- rlang::quo_name(x)
    }
    x_name
  }))
  invalid <- !is.syntactic(old) 
  if (any(invalid)) {
    err_handler(paste(
      "Some passed in arguments are invalid. The old variable names could not", 
      "be parsed, which should be to be assigned to the following variables:",
      stringify(new[invalid])
    ))
  }
  invalid <- !old %in% names(.data)
  if (any(invalid))
    err_handler(paste(
        "The following old variable names could not be",
        "found in the LabelDictionary object:", 
        stringify(old[invalid]),
        "\nOnly the following variable translations exist:",
        stringify(names(.data))
      ))
  rename_translation(.data, old, new)
}

check_rename <- function(.data, old, new, fn_name) {
  err_handler <- paste0("Error while calling '", fn_name, "'") %>%
    composerr
  if (!is.dictionary(.data))
    err_handler("The object given in the argument '.data' must be a LabelDictionary class object.")
  if (!is.character(old) || length(old) == 0)
    err_handler("The object given in the argument 'old' must be a character vector.")
  if (!is.character(new) || length(new) != length(old))
    err_handler("The object given in the argument 'new' must be a character vector of the same length as the one given in argument 'old'.")
  wrong_variable <- old[!old %in% names(.data)]
  if (length(wrong_variable) != 0)
    err_handler(paste(
        "The following names given in the argument 'old' could not be",
        "found in the LabelDictionary object:", 
        stringify(wrong_variable),
        "\nOnly the following variable translations exist:",
        stringify(names(.data))
      ))
}

#' @rdname trans_rename
#' @param old A character vector holding the names of the variable translations,
#' that should be renamed.
#' @param new A character vector holding the new names of the variable
#' translations.
#' @export
trans_rename_ <- function(.data, old, new) {
  UseMethod("trans_rename_")
}

#' @rdname trans_rename 
#' @export
trans_rename_.LabelDictionary <- function(.data, old, new) {
  rename_translation(.data, old, new)
}


#' Function that actually performs the renaming of the translations
#' @param .data A [LabelDictionary][new_dictionary()] object, holding the
#' variable translations
#' @param old A character vector holding the names of the variable
#' translations, that should be renamed.
#' @param new A character vector holding the new names of the variable
#' translations.
#' @return The updated [LabelDictionary][new_dictionary()] class object.
rename_translation <- function(.data, old, new) {
  names(.data)[match(old, names(.data))] <- new
  .data
}
