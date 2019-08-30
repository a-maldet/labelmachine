#' Rename multiple variable translations in a [LabelDictionary][new_dictionary()] object
#'
#' The functions [lama_rename()] and [lama_rename_()]
#' are used to rename one or more variable translations inside of a 
#' [LabelDictionary][new_dictionary()] class object.
#' The function [lama_rename()] uses non-standard evaluation,
#' whereas [lama_rename_()] is the standard evaluation alternativ.
#' @param .data A [LabelDictionary][new_dictionary()] object, holding the variable translations
#' @param ... One or more unquoted expressions separated by commas. Use named arguments, e.g. ‘new_name = old_name’, to rename selected variables.
#' @return The updated [LabelDictionary][new_dictionary()] class object.
#' @seealso [translate()], [new_dictionary()], [lama_select()], [lama_set()],
#' [lama_merge()], [lama_read()], [lama_write()]
#' @rdname lama_rename
#' @export
lama_rename <- function(.data, ...) {
  UseMethod("lama_rename")
}

#' @export
lama_rename.LabelDictionary <- function(.data, ...) {
  args <- rlang::quos(...)
  err_handler <- composerr("Error while calling 'lama_rename'")
  err_handler_argument <- composerr(
    err_prior = err_handler,
    text_2 = "Use named arguments, e.g. ‘new_name = old_name’, to rename translations.",
    sep_2 = " "
  )
  if (length(args) == 0)
    err_handler_argument("Name assignments are missing.")
  new <- names(args)
  if (is.null(new) || any(new == ""))
    err_handler_argument("Name assignment is invalid.")
  old <- as.character(sapply(args, function(x) {
    if (!rlang::quo_is_symbolic(x)) {
      x_name <- rlang::quo_get_expr(x)
    } else {
      x_name <- rlang::quo_name(x)
    }
    x_name
  }))
  check_rename(.data, old, new, err_handler)
  rename_translation(.data, old, new)
}


#' @rdname lama_rename
#' @param old A character vector holding the names of the variable translations,
#' that should be renamed.
#' @param new A character vector holding the new names of the variable
#' translations.
#' @export
lama_rename_ <- function(.data, old, new) {
  UseMethod("lama_rename_")
}

#' @rdname lama_rename 
#' @export
lama_rename_.LabelDictionary <- function(.data, old, new) {
  err_handler <- composerr("Error while calling 'lama_rename_'")
  check_rename(.data, old, new, err_handler)
  rename_translation(.data, old, new)
}


#' Function that actually performs the renaming of the translations
#'
#' @param .data A [LabelDictionary][new_dictionary()] object, holding the
#' variable translations
#' @param old A character vector holding the names of the variable
#' translations, that should be renamed.
#' @param new A character vector holding the new names of the variable
#' translations.
#' @return The updated [LabelDictionary][new_dictionary()] class object.
#' @include lama_select.R
rename_translation <- function(.data, old, new) {
  .data <- lama_select_(.data, setdiff(names(.data), setdiff(new, old)))
  id_rename <- match(old, names(.data))
  names(.data)[id_rename] <- new
  .data
}

#' Function that checks the passed in arguments for [lama_rename()] and [lama_rename_()]
#' 
#' @param .data A [LabelDictionary][new_dictionary()] object, holding the
#' variable translations
#' @param old A character vector holding the names of the variable
#' translations, that should be renamed.
#' @param new A character vector holding the new names of the variable
#' translations.
#' @param err_handler A error handling function
check_rename <- function(.data, old, new, err_handler) {
  # check .data
  if (!is.dictionary(.data))
    err_handler("The object given in the argument '.data' must be a LabelDictionary class object.")
  # check old
  if (!is.character(old) || length(old) == 0)
    err_handler("The object given in the argument 'old' must be a character vector.")
  wrong_variable <- old[!old %in% names(.data)]
  if (length(wrong_variable) != 0)
    err_handler(paste0(
        "The following old translation names could not be ",
        "found in the LabelDictionary object: ", 
        stringify(wrong_variable),
        ".\nOnly the following variable translations exist: ",
        stringify(names(.data)),
        "."
      ))
  duplicates <- table(old)
  duplicates <- names(duplicates[duplicates > 1])
  if (length(duplicates) > 0)
    err_handler(paste0(
      "The following old translation names are used more than once: ",
      stringify(duplicates),
      "."
    ))
  # check new
  if (!is.character(new) || length(new) != length(old))
    err_handler("The object given in the argument 'new' must be a character vector of the same length as the one given in argument 'old'.")
  invalid <- !is.syntactic(new)
  if (any(invalid))
    err_handler(paste0(
      "The following translation names are invalid: ",
      stringify(new[invalid]),
      "."
    ))
  duplicates <- table(new)
  duplicates <- names(duplicates[duplicates > 1])
  if (length(duplicates) > 0)
    err_handler(paste0(
      "The following translation names are used more than once: ",
      stringify(duplicates),
      "."
    ))
}
