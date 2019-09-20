#' Rename multiple variable translations in a [lama_dictionary][new_lama_dictionary()] object
#'
#' The functions [lama_rename()] and [lama_rename_()]
#' are used to rename one or more variable translations inside of a 
#' [lama_dictionary][new_lama_dictionary()] class object.
#' The function [lama_rename()] uses non-standard evaluation,
#' whereas [lama_rename_()] is the standard evaluation alternative.
#' @param .data A [lama_dictionary][new_lama_dictionary()] object, holding the variable translations
#' @param ... One or more unquoted expressions separated by commas. Use named arguments, e.g. `new_name = old_name`, to rename selected variables.
#' @return The updated [lama_dictionary][new_lama_dictionary()] class object.
#' @seealso [lama_translate()], [lama_translate_all()], [new_lama_dictionary()],
#'   [as.lama_dictionary()], [lama_select()], [lama_mutate()],
#'   [lama_merge()], [lama_read()], [lama_write()]
#' @rdname lama_rename
#' @export
lama_rename <- function(.data, ...) {
  UseMethod("lama_rename")
}

#' @rdname lama_rename
#' @export
#' @examples
#'   # initialize lama_dictinoary
#'   dict <- new_lama_dictionary(
#'     country = c(uk = "United Kingdom", fr = "France", NA_ = "other countries"),
#'     language = c(en = "English", fr = "French"),
#'     result = c("1" = "Very good", "2" = "Good", "3" = "Not so good")
#'   )
#' 
#'   ## Example-1: Usage of 'lama_rename'
#'   # rename translations 'result' and 'language' to 'res' and 'lang'
#'   dict_new <- lama_rename(dict, res = result, lang = language)
#'   dict_new
#'   
lama_rename.lama_dictionary <- function(.data, ...) {
  args <- rlang::quos(...)
  err_handler <- composerr("Error while calling 'lama_rename'")
  err_handler_argument <- composerr(
    err_prior = err_handler,
    text_2 = "Use named arguments, e.g. `new_name = old_name`, to rename translations.",
    sep_2 = " "
  )
  if (length(args) == 0)
    err_handler_argument("Name assignments are missing.")
  new <- names(args)
  if (is.null(new) || any(new == ""))
    err_handler_argument("Name assignment is invalid.")
  old <- as.character(sapply(seq_len(length(new)), function(i) {
    err_handler <- composerr_parent(
      paste(
        "Invalid argument at position",
        stringify(i + 1)
      ),
      err_handler
    )
    obj <- args[[i]]
    if (!rlang::quo_is_symbol(obj)) {
      obj <- rlang::quo_get_expr(obj)
      if (!is.character(obj))
        err_handler(paste(
          "The expression",
          stringify(paste(
            c(new[i], rlang::quo_name(args[[i]])),
            collapse = " = "
          )),
          "could not be parsed."
        ))
    } else {
      obj <- rlang::quo_name(obj)
    }
    obj
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
#' @examples
#'   ## Example-2: Usage of 'lama_rename_'
#'   # rename translations 'result' and 'language' to 'res' and 'lang'
#'   dict_new <- lama_rename_(dict, c("result", "language"), c("res", "lang"))
#'   dict_new
#' @export
lama_rename_.lama_dictionary <- function(.data, old, new) {
  err_handler <- composerr("Error while calling 'lama_rename_'")
  check_rename(.data, old, new, err_handler)
  rename_translation(.data, old, new)
}


#' Function that actually performs the renaming of the translations
#'
#' @param .data A [lama_dictionary][new_lama_dictionary()] object, holding the
#' variable translations
#' @param old A character vector holding the names of the variable
#' translations, that should be renamed.
#' @param new A character vector holding the new names of the variable
#' translations.
#' @return The updated [lama_dictionary][new_lama_dictionary()] class object.
#' @include lama_select.R
rename_translation <- function(.data, old, new) {
  .data <- lama_select_(.data, setdiff(names(.data), setdiff(new, old)))
  id_rename <- match(old, names(.data))
  names(.data)[id_rename] <- new
  .data
}

#' Function that checks the passed in arguments for [lama_rename()] and [lama_rename_()]
#' 
#' @param .data A [lama_dictionary][new_lama_dictionary()] object, holding the
#' variable translations
#' @param old A character vector holding the names of the variable
#' translations, that should be renamed.
#' @param new A character vector holding the new names of the variable
#' translations.
#' @param err_handler A error handling function
check_rename <- function(.data, old, new, err_handler) {
  # check .data
  if (!is.dictionary(.data))
    err_handler("The object given in the argument '.data' must be a lama_dictionary class object.")
  # check old
  if (!is.character(old) || length(old) == 0)
    err_handler("The object given in the argument 'old' must be a character vector.")
  wrong_variable <- old[!old %in% names(.data)]
  if (length(wrong_variable) != 0)
    err_handler(paste0(
        "The following old translation names could not be ",
        "found in the lama_dictionary object: ", 
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
    err_handler(paste("The object given in the argument 'new' must be",
      "a character vector of the same length as the one given in",
      "argument 'old'."
    ))
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
