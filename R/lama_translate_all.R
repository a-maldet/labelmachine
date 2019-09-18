#' Assign new labels to all variables of a data.frame
#'
#' Converts all variables (which have a translation in the given lama-dictionary)
#' of a data frame `.data`
#' into factor variables with new labels. 
#' This function a special version of the function [lama_translate()].
#' The difference to [lama_translate()] is, that when using [lama_translate_all()]
#' the used translations in `dictionary` must have the exact
#' same names as the corresponding columns in the data frame `.data`.
#' @param .data Either a data frame, a factor or a vector.
#' @param dictionary A [lama_dictionary][new_lama_dictionary()] object,
#'   holding the translations for various variables.
#' @param prefix A character string, which is used as prefix for the new 
#'   column names.
#' @param suffix A character string, which is used as suffix for the new 
#'   column names.
#' @param fn_colname A function, which transforms character string into a new
#'   character string. This function will be used to transform the old column
#'   names into new column names under which the labelled variables will then
#'   be stored.
#' @param keep_order A logical of length one, defining if the original order
#'   (factor order or alphanumerical order) of the data frame variables should
#'   be preserved.
#' @return An extended data.frame, that has a factor variable holding the
#'   assigned labels.
#' @rdname lama_translate_all
#' @include lama_translate.R lama_dictionary.R
#' @export
lama_translate_all <- function(
  .data,
  dictionary,
  prefix = "",
  suffix = "",
  fn_colname = function(x) x,
  keep_order = FALSE
) {
  UseMethod("lama_translate_all")
}

#' @rdname lama_translate_all
#' @examples
#'   ## initialize lama_dictinoary
#'   dict <- new_lama_dictionary(
#'     subject = c(en = "English", ma = "Mathematics"),
#'     result = c("1" = "Very good", "2" = "Good", "3" = "Not so good")
#'   )
#'   ## data frame which should be translated
#'   df <- data.frame(
#'     pupil = c(1, 1, 2, 2, 3),
#'     subject = c("en", "ma", "ma", "en", "en"),
#'     result = c(1, 2, 3, 2, 2)
#'   )
#'   
#'   ## Example-1: Use 'prefix'
#'   # (apply translation 'subject' to column 'subject' and save it to column 'new_subject')
#'   # (apply translation 'result' to column 'result' and save it to column 'new_result')
#'   df_new <- lama_translate_all(df, dict, prefix = "new_")
#'   str(df_new)
#'
#'   # Example-2: Use 'suffix'
#'   # (apply translation 'subject' to column 'subject' and save it to column 'subject_new')
#'   # (apply translation 'result' to column 'result' and save it to column 'result_new')
#'   df_new <- lama_translate_all(df, dict, suffix = "_new")
#'   str(df_new)
#' 
#'   # Example-3: Use 'fn_colname'
#'   # (apply translation 'subject' to column 'subject' and save it to column 'SUBJECT')
#'   # (apply translation 'result' to column 'result' and save it to column 'RESULT')
#'   df_new <- lama_translate_all(df, dict, fn_colname = toupper)
#'   str(df_new)
#' @export
lama_translate_all.data.frame <- function(
  .data,
  dictionary,
  prefix = "",
  suffix = "",
  fn_colname = function(x) x,
  keep_order = FALSE
) {
  err_handler <- composerr("Error while calling 'lama_translate_all'")
  if (!is.dictionary(dictionary))
    err_handler("The argument 'dictionary' must be a lama_dictionary class object.")
  if (!is.character(prefix) || length(prefix) != 1 || is.na(prefix))
    err_handler("The argument 'prefix' must be a character string.")
  if (!is.syntactic(paste0(prefix, "a")))
    err_handler("The argument 'prefix' must be a valid object name prefix.")
  if (!is.character(suffix) || length(suffix) != 1 || is.na(suffix))
    err_handler("The argument 'suffix' must be a character string.")
  if (!is.syntactic(paste0("a", suffix)))
    err_handler("The argument 'suffix' must be a valid object name suffix.")
  if (!is.function(fn_colname))
    err_handler(paste(
      "The argument 'fn_colname' must be a function taking a character string",
      "and returning a character string."
    ))
  if (!is.logical(keep_order) || length(keep_order) != 1 || is.na(keep_order))
    err_handler(paste(
      "The argument 'keep_order' must be a logical."
    ))
  translation <- intersect(names(dictionary), colnames(.data))
  if (length(translation) == 0)
    return(.data)
  col_new <- sapply(translation, fn_colname)
  if (!is.character(col_new) || length(col_new) != length(translation) ||
      any(is.na(col_new))
  )
    err_handler(paste(
      "The function given in argument 'fn_colname' does not produce valid",
      "column names:",
      "It must take character strings and return character strings."
    ))
  col_new <- paste0(prefix, col_new, suffix)
  invalid <- !is.syntactic(col_new)
  if (any(invalid)) 
    err_handler(paste0(
      "The function given in argument 'fn_colname' does not produce valid ",
      "column names: The following produced column names are invalid: ",
      stringify(col_new[invalid]),
      "."
    ))
  translate_df(
    .data = .data, 
    dictionary = dictionary,
    translation = translation,
    col = translation,
    col_new = col_new,
    keep_order = rep(keep_order, length(translation)),
    err_handler = err_handler
  )
}
