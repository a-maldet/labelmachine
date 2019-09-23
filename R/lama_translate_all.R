#' Assign new labels to all variables of a data.frame
#'
#' The functions [lama_translate_all()] and [lama_to_factor_all()]
#' converts all variables (which have a translation in the given lama-dictionary)
#' of a data frame `.data`
#' into factor variables with new labels.
#' These functions are special versions of the functions [lama_translate()]
#' and [lama_to_factor()].
#' The difference to [lama_translate()] and [lama_to_factor()] is,
#' that when using [lama_translate_all()] and [lama_to_factor_all()]
#' the used translations in `dictionary` must have the exact
#' same names as the corresponding columns in the data frame `.data`.
#' 
#' The difference between [lama_translate_all()] and [lama_to_factor_all()]
#' is the following:
#' * [lama_translate_all()]: Assign new labels to the variables
#'   and turn them into factor variables with the order given in the corresponding
#'   translations (`keep_order = FALSE`) or in the same order as the original
#'   variable (`keep_order = TRUE`).
#' * [lama_to_factor_all()]: The variariables are character
#'   vectors or factors already holding the right label strings. The variables
#'   are turned into a factor variables with the order given in the corresponding
#'   translation (`keep_order = FALSE`) or in the same order as the original
#'   variable (`keep_order = TRUE`).
#' @param .data Either a data frame, a factor or a vector.
#' @param dictionary A [lama_dictionary][new_lama_dictionary()] object,
#'   holding the translations for various variables.
#' @param prefix A character string, which is used as prefix for the new 
#'   column names.
#' @param suffix A character string, which is used as suffix for the new 
#'   column names.
#' @param fn_colname A function, which transforms character string into a new
#'   character string. This function will be used to transform the old column
#'   names into new column names under which the labeled variables will then
#'   be stored.
#' @param keep_order A logical of length one, defining if the original order
#'   (factor order or alphanumerical order) of the data frame variables should
#'   be preserved.
#' @param to_factor A logical of length one, defining if the resulting labeled
#'   varibles should be factor variables (`to_factor = TRUE`) or plain
#'   character vectors (`to_factor = FALSE`).
#' @return An extended data.frame, that has a factor variable holding the
#'   assigned labels.
#' @rdname lama_translate_all
#' @include lama_translate.R lama_dictionary.R
#' @seealso [lama_translate()], [lama_to_factor()], [new_lama_dictionary()],
#'   [as.lama_dictionary()], [lama_rename()], [lama_select()], [lama_mutate()],
#'   [lama_merge()], [lama_read()], [lama_write()]
#' @export
lama_translate_all <- function(
  .data,
  dictionary,
  prefix = "",
  suffix = "",
  fn_colname = function(x) x,
  keep_order = FALSE,
  to_factor = TRUE
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
#'   ## Example-1: 'lama_translate_all''
#'   df_new <- lama_translate_all(
#'     df,
#'     dict,
#'     prefix = "pre_",
#'     fn_colname = toupper,
#'     suffix = "_suf"
#'   )
#'   str(df_new)
#' 
#'   ## Example-2: 'lama_translate_all' with 'to_factor = FALSE'
#'   # The resulting variables are plain character vectors
#'   df_new <- lama_translate_all(df, dict, suffix = "_new", to_factor = TRUE)
#'   str(df_new)
#'
#' @export
lama_translate_all.data.frame <- function(
  .data,
  dictionary,
  prefix = "",
  suffix = "",
  fn_colname = function(x) x,
  keep_order = FALSE,
  to_factor = TRUE
) {
  err_handler <- composerr("Error while calling 'lama_translate_all'")
  check_and_translate_all(
    .data = .data, 
    dictionary = dictionary,
    prefix = prefix,
    suffix = suffix,
    fn_colname = fn_colname,
    keep_order = keep_order,
    to_factor = to_factor,
    is_translated = FALSE,
    err_handler = err_handler
  )
}

#' @rdname lama_translate_all
#' @export
lama_to_factor_all <- function(
  .data,
  dictionary,
  prefix = "",
  suffix = "",
  fn_colname = function(x) x,
  keep_order = FALSE
) {
  UseMethod("lama_to_factor_all")
}

#' @rdname lama_translate_all
#' @examples
#'   ## Example-3: 'lama_to_factor_all'
#'   # The variables 'subject' and 'result' are turned into factor variables
#'   # The ordering is taken from the translations 'subject' and 'result'
#'   df_2 <- data.frame(
#'     pupil = c(1, 1, 2, 2, 3),
#'     subject = c("English", "Mathematics", "Mathematics", "English", "English"),
#'     result = c("Very good", "Good", "Good", "Very good", "Good")
#'   )
#'   df_2_new <- lama_to_factor_all(
#'     df_2, dict,
#'     prefix = "pre_",
#'     fn_colname = toupper,
#'     suffix = "_suf"
#'   )
#'   str(df_new)
#'
#' @export
lama_to_factor_all.data.frame <- function(
  .data,
  dictionary,
  prefix = "",
  suffix = "",
  fn_colname = function(x) x,
  keep_order = FALSE
) {
  err_handler <- composerr("Error while calling 'lama_to_factor_all'")
  check_and_translate_all(
    .data = .data, 
    dictionary = dictionary,
    prefix = prefix,
    suffix = suffix,
    fn_colname = fn_colname,
    keep_order = keep_order,
    to_factor = TRUE,
    is_translated = TRUE,
    err_handler = err_handler
  )
}

#' Check and translate function used by `lama_translate_all()` and `lama_to_factor_all()`
#'
#' @inheritParams lama_translate_all
#' @inheritParams translate_df
check_and_translate_all <- function(
  .data,
  dictionary,
  prefix,
  suffix,
  fn_colname,
  keep_order,
  to_factor,
  is_translated,
  err_handler
) {
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
  if (!is.logical(to_factor) || length(to_factor) != 1 || is.na(to_factor))
    err_handler(paste(
      "The argument 'to_factor' must be a logical."
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
    keep_order = keep_order,
    to_factor = to_factor,
    is_translated = is_translated,
    err_handler = err_handler
  )
}
