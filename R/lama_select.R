#' Select multiple variable translations and create a new [lama_dictionary][new_lama_dictionary()] object
#'
#' The functions [lama_select()] and [lama_select_()] pick one or more 
#' variable translations from a [lama_dictionary][new_lama_dictionary()] class object
#' and create a new [lama_dictionary][new_lama_dictionary()] class object.
#' The function [lama_select()] uses non-standard evaluation, whereas 
#' [lama_select_()] is the standard evaluation alternative.
#' @param .data A [lama_dictionary][new_lama_dictionary()] object, holding the variable translations
#' @param ... One or more unquoted translation names separated by commas.
#' @return A new [lama_dictionary][new_lama_dictionary()] class object, holding the picked variable translations.
#' @seealso [lama_translate()], [lama_to_factor()], [lama_translate_all()],
#'   [lama_to_factor_all()], [new_lama_dictionary()],
#'   [as.lama_dictionary()], [lama_rename()], [lama_mutate()],
#'   [lama_merge()], [lama_read()], [lama_write()]
#' @rdname lama_select
#' @export
lama_select <- function(.data, ...) {
  UseMethod("lama_select")
}

#' @rdname lama_select
#' @examples
#'   # initialize lama_dictinoary
#'   dict <- new_lama_dictionary(
#'     country = c(uk = "United Kingdom", fr = "France", NA_ = "other countries"),
#'     language = c(en = "English", fr = "French"),
#'     result = c("1" = "Very good", "2" = "Good", "3" = "Not so good")
#'   )
#'
#'   ## Example-1: Usage of 'lama_select'
#'   # pick the translations 'result' and 'language'
#'   # and add them to a new lama_dictionary
#'   dict_sub <- lama_select(dict, result, language)
#'   dict_sub
#'   
#' @export
lama_select.lama_dictionary <- function(.data, ...) {
  args <- rlang::quos(...)
  err_handler <- composerr(
    text_1 = "Error while calling 'lama_select'",
    text_2 = "Use unquoted arguments e.g. 'lama_select(.data, x, y, z)'.",
    sep_2 = " "
  )
  if (length(args) == 0)
    err_handler("Selected translation names are missing.")
  key <- as.character(sapply(seq_len(length(args)), function(i) {
    err_handler <- composerr_parent(
      paste(
        "Invalid argument at position",
        stringify(i + 1)
      ),
      err_handler
    )
    x <- args[[i]]
    if (!rlang::quo_is_symbol(x))
      err_handler(paste(
        "The expression",
        stringify(rlang::quo_name(x)),
        "could not be parsed."
      ))
    rlang::quo_name(x)
  }))
  check_select(.data, key, err_handler)
  new_lama_dictionary(.data[key])
}

#' @param key A character vector holding the names of the variable translations that
#' should be picked.
#' @rdname lama_select
#' @export
lama_select_ <- function(.data, key) {
  UseMethod("lama_select_")
}

#' @rdname lama_select
#' @examples
#'   ## Example-2: Usage of 'lama_select_'
#'   # pick the translations 'result' and 'language'
#'   # and add them to a new lama_dictionary
#'   dict_sub <- lama_select_(dict, c("result", "language"))
#'   dict_sub
#' @export
lama_select_.lama_dictionary <- function(.data, key) {
  err_handler <- composerr("Error while calling 'lama_select_'")
  if (!is.character(key) || length(key) == 0)
    err_handler("The object given in the argument 'key' must be a character vector.")
  check_select(.data, key, err_handler)
  new_lama_dictionary(.data[key])
}

#' Function that checks the passed in arguments for [lama_select()] and [lama_select_()]
#' 
#' @param .data A [lama_dictionary][new_lama_dictionary()] object, holding the
#' variable translations
#' @param key A character vector holding the names of the variable
#' translations, that should be renamed.
#' @param err_handler A error handling function
check_select <- function(.data, key, err_handler) {
  if (!is.lama_dictionary(.data))
    err_handler("The object given in the argument '.data' must be a lama_dictionary class object.")
  invalid <- !key %in% names(.data)
  if (any(invalid))
    err_handler(paste0(
        "The following translation names could not be ",
        "found in the lama_dictionary object: ", 
        stringify(key[invalid]),
        ".\nOnly the following variable translations exist: ",
        stringify(names(.data)),
        "."
      ))
  duplicates <- table(key)
  duplicates <- names(duplicates[duplicates > 1])
  if (length(duplicates) > 0)
    err_handler(paste0(
      "The following translation names are used more than once: ",
      stringify(duplicates),
      "."
    ))
}
