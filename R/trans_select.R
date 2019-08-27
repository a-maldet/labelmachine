#' Select multiple variable translations and create a new [LabelDictionary][new_dictionary()] object
#'
#' The functions [trans_select()] and [trans_select_()] pick one or more 
#' variable translations from a [LabelDictionary][new_dictionary()] class object
#' and create a new [LabelDictionary][new_dictionary()] class object.
#' The function [trans_select()] uses non-standard evaluation, whereas 
#' [trans_select_()] is the standard evaluation alternative.
#' @param .data A [LabelDictionary][new_dictionary()] object, holding the variable translations
#' @param ... One or more unquoted translation names separated by commas.
#' @return A new [LabelDictionary][new_dictionary()] class object, holding the picked variable translations.
#' @seealso [translate()], [new_dictionary()], [trans_rename()], [trans_set()],
#' [trans_merge()], [read_dictionary()], [write_dictionary()]
#' @rdname trans_select
#' @export
trans_select <- function(.data, ...) {
  UseMethod("trans_select")
}

#' @export
trans_select.LabelDictionary <- function(.data, ...) {
  args <- rlang::quos(...)
  err_handler <- composerr("Error while calling 'trans_select'")
  if (!is.dictionary(.data))
    err_handler("The object given in the argument '.data' must be a LabelDictionary class object.")
  if (length(args) == 0)
    err_handler("Selected translation names are missing.")
  variable <- as.character(sapply(args, function(x) {
    if (!rlang::quo_is_symbolic(x)) {
      x_name <- rlang::quo_get_expr(x)
    } else {
      x_name <- rlang::quo_name(x)
    }
    x_name
  }))
  invalid <- !is.syntactic(variable) 
  if (any(invalid)) {
    err_handler(paste(
      "Some passed in arguments are invalid. The passed in translation names could not", 
      "be parsed."
    ))
  }
  invalid <- !variable %in% names(.data)
  if (any(invalid))
    err_handler(paste0(
        "The following translation names could not be ",
        "found in the LabelDictionary object: ", 
        stringify(variable[invalid]),
        ".\nOnly the following variable translations exist: ",
        stringify(names(.data)),
        "."
      ))
  new_dictionary(.data[variable])
}

#' @param variable A character vector holding the names of the variable translations that
#' should be picked.
#' @rdname trans_select
#' @export
trans_select_ <- function(.data, variable) {
  UseMethod("trans_select_")
}

#' @rdname trans_select
#' @export
trans_select_.LabelDictionary <- function(.data, variable) {
  err_handler <- composerr("Error while calling 'trans_select_'")
  if (!is.dictionary(.data))
    err_handler("The object given in the argument '.data' must be a LabelDictionary class object.")
  if (!is.character(variable) || length(variable) == 0)
    err_handler("The object given in the argument 'variable' must be a character vector.")
  wrong_variable <- variable[!variable %in% names(.data)]
  if (length(wrong_variable) != 0)
    err_handler(paste0(
        "The following names given in the argument 'variable' could not be ",
        "found in the LabelDictionary object: ", 
        stringify(wrong_variable),
        ".\nOnly the following translation names exist: ",
        stringify(names(.data)),
        "."
      ))
  new_dictionary(.data[variable])
}
