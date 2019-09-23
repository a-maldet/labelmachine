#' Assign new labels to a variable of a data.frame
#'
#' The functions [lama_translate()] and [lama_translate_()] take a factor,
#' a vector or a data.frame
#' and convert one or more of its categorical variables
#' (not necessarily a factor variable) into factor variables with new labels. 
#' The function [lama_translate()] uses non-standard evaluation, whereas 
#' [lama_translate_()] is the standard evaluation alternative.
#' The functions [lama_to_factor()] and [lama_to_factor_()] are very similar
#' to the functions [lama_translate()] and [lama_translate_()], but instead
#' of assigning new label strings to values, it is assumed that the variables
#' are character vectors or factors, but need to be turned into factors
#' with the order given in the translations:
#' * [lama_translate()] and [lama_translate_()]: Assign new labels to a variable
#'   and turn it into a factor variable with the order given in the corresponding
#'   translation (`keep_order = FALSE`) or in the same order as the original
#'   variable (`keep_order = TRUE`).
#' * [lama_to_factor()] and [lama_to_factor_()]: The variariable is a character
#'   vector or a factor already holding the right label strings. The variables
#'   are turned into a factor variable with the order given in the corresponding
#'   translation (`keep_order = FALSE`) or in the same order as the original
#'   variable (`keep_order = TRUE`).
#'
#' The functions [lama_translate()], [lama_translate_()], [lama_to_factor()]
#' and [lama_to_factor_()] require different
#' arguments, depending on the data type passed into argument `.data`.
#' If `.data` is of type character, logical, numeric or factor, then
#' the arguments `col` and `col_new` are omitted, since those are only
#' necessary in the case of data frames.
#' @param .data Either a data frame, a factor or an atomic vector.
#' @param dictionary A [lama_dictionary][new_lama_dictionary()] object, holding the translations for various
#'   variables.
#' @param ... Only used by [lama_translate()] and [lama_to_factor()].
#'   Each argument in `...` is an unquoted expression and defines a translation.
#'   Use unquoted
#'   arguments that tell which translation should be applied to which column and
#'   which column name the relabeled variable should be assigned to. E.g.
#'   `lama_translate(.data, dict, Y1 = TRANS1(X1), Y2 = TRANS2(Y2))` and
#'   `lama_to_factor(.data, dict, Y1 = TRANS1(X1), Y2 = TRANS2(Y2))` and
#'   to apply the translations `TRANS1` and `TRANS2` to the data.frame
#'   columns `X1` and `X2` and save the new labeled variables under
#'   the column names `Y1` and `Y2`.
#'   There are also two abbreviation mechanisms available:
#'   The argument assignment `FOO(X)` is the same as `X = FOO(X)` and
#'   `FOO` is an abbreviation for `FOO = FOO(FOO)`.
#'   In case, `.data` is not a data frame but a plain factor or an atomic vector, then
#'   the argument `...` must be a single unquoted translation name
#'   (e.g. `lama_translate(x, dict, TRANS1)`, where `x` is a factor or an atomic vector
#'   and `TRANS1` is the name of the translation, which should be used to assign
#'   the labels to the values of `x`.)
#' @param keep_order A boolean vector of length one or the same length as the
#'   number of translations. If the vector has length one, then the same 
#'   configuration is applied to all variable translations. If the vector has 
#'   the same length as the number of arguments in `...`, then the 
#'   to each variable translation there is a corresponding boolean configuration.
#'   If a translated variable in the data.frame is a factor variable,
#'   and the corresponding boolean configuration is set to `TRUE`, then the
#'   the order of the original factor variable will be preserved.
#' @param to_factor A boolean vector of length one or the same length as the
#'   number of translations. If the vector has length one, then the same 
#'   configuration is applied to all variable translations. If the vector has 
#'   the same length as the number of arguments in `...`, then the 
#'   to each variable translation there is a corresponding boolean configuration.
#'   If `to_factor` is `TRUE`, then the resulting labeled
#'   variable will be a factor. If `to_factor` is set to `FALSE`, then
#'   the resulting labeled variable will be a plain character vector.
#' @return An extended data.frame, that has a factor variable holding the
#'   assigned labels.
#' @seealso [lama_translate_all()], [lama_to_factor_all()], [new_lama_dictionary()],
#'   [as.lama_dictionary()], [lama_rename()], [lama_select()], [lama_mutate()],
#'   [lama_merge()], [lama_read()], [lama_write()]
#' @rdname lama_translate
#' @include lama_dictionary.R
#' @export
lama_translate <- function(
  .data,
  dictionary,
  ...,
  keep_order = FALSE,
  to_factor = TRUE
) {
  UseMethod("lama_translate")
}

#' @rdname lama_translate
#' @examples
#'   # initialize lama_dictinoary
#'   dict <- new_lama_dictionary(
#'     subject = c(en = "English", ma = "Mathematics"),
#'     result = c("1" = "Very good", "2" = "Good", "3" = "Not so good")
#'   )
#'   # the data frame which should be translated
#'   df <- data.frame(
#'     pupil = c(1, 1, 2, 2, 3),
#'     subject = c("en", "ma", "ma", "en", "en"),
#'     res = c(1, 2, 3, 2, 2)
#'   )
#'
#'   ## Example-1: Usage of 'lama_translate' for data frames
#'   ##            Full length assignment
#'   # (apply translation 'subject' to column 'subject' and save it to column 'subject_new')
#'   # (apply translation 'result' to column 'res' and save it to column 'res_new')
#'   df_new <- lama_translate(
#'     df,
#'     dict,
#'     sub_new = subject(subject),
#'     res_new = result(res)
#'   )
#'   str(df_new)
#'
#'   ## Example-2: Usage of 'lama_translate' for data frames
#'   ##            Abbreviation overwriting original columns
#'   # (apply translation 'subject' to column 'subject' and save it to column 'subject')
#'   # (apply translation 'result' to column 'res' and save it to column 'res')
#'   df_new_overwritten <- lama_translate(
#'     df,
#'     dict,
#'     subject(subject),
#'     result(res)
#'   )
#'   str(df_new_overwritten)
#'
#'   ## Example-3: Usage of 'lama_translate' for data frames
#'   ##            Abbreviation if `translation_name == column_name`
#'   # (apply translation 'subject' to column 'subject' and save it to column 'subject_new')
#'   # (apply translation 'result' to column 'res' and save it to column 'res_new')
#'   df_new_overwritten <- lama_translate(
#'     df, 
#'     dict,
#'     subject_new = subject,
#'     res_new = result(res)
#'   )
#'   str(df_new_overwritten)
#'   
#'   ## Example-4: Usage of 'lama_translate' for data frames labeling as character vectors
#'   # (apply translation 'subject' to column 'subject' and
#'   # save it as a character vector to column 'subject_new')
#'   df_new_overwritten <- lama_translate(
#'     df, 
#'     dict,
#'     subject_new = subject,
#'     to_factor = TRUE
#'   )
#'   str(df_new_overwritten)
#'   
#' @export
lama_translate.data.frame <- function(
  .data,
  dictionary,
  ...,
  keep_order = FALSE,
  to_factor = TRUE
) {
  check_and_translate_df(
    .data = .data,
    dictionary = dictionary,
    args = rlang::quos(...),
    keep_order = keep_order,
    to_factor = to_factor,
    is_translated = FALSE,
    err_handler = composerr("Error while calling 'lama_translate'")
  )
}

#' @rdname lama_translate
#' @examples
#'   ## Example-5: Usage of 'lama_translate' for atomic vectors
#'   sub <- c("ma", "en", "ma")
#'   sub_new <- df_new_overwritten <- lama_translate(
#'     sub,
#'     dict,
#'     subject
#'   )
#'   str(sub_new)
#' 
#'   ## Example-6: Usage of 'lama_translate' for factors
#'   sub <- factor(c("ma", "en", "ma"), levels = c("ma", "en"))
#'   sub_new <- df_new_overwritten <- lama_translate(
#'     sub,
#'     dict,
#'     subject,
#'     keep_order = TRUE
#'   )
#'   str(sub_new)
#'   
#' @export
lama_translate.default <- function(
  .data,
  dictionary,
  ...,
  keep_order = FALSE,
  to_factor = TRUE
) {
  args <- rlang::quos(...)
  if (length(args) > 1) {
      warning(paste(
        "Warning while calling `lama_translate`:",
        "If the first element is a factor or an atomic vector,",
        "then only the arguments 'dictionary', a single argument for '...'",
        "(the unquoted translation name)",
        "and the arguments 'keep_order' and 'to_factor' are",
        "used and all extra arguments",
        "will be ignored."
      ))
  }
  check_and_translate_vector(
    .data = .data,
    dictionary = dictionary,
    args = args,
    keep_order = keep_order,
    to_factor = to_factor,
    is_translated = FALSE,
    err_handler = composerr("Error while calling 'lama_translate'")
  )
}

#' @param translation A character vector holding the names of the variable 
#'   translations which
#'   should be used for assigning new labels to the variable. This names must be
#'   a subset of the translation names returned by `names(dictionary)`.
#' @param col Only used if `.data` is a data frame. The argument `col` must be
#'   a character vector of the same length as `translation` holding
#'   the names of the data.frame columns that
#'   should be relabeled. If omitted, then it will be assumed that the column
#'   names are the same as the given translation names in the argument `translation`.
#' @param col_new Only used if `.data` is a data frame. The argument `col` must be
#'   a character vector of the same length as `translation` holding
#'   the names under which the relabeled variables should be stored in
#'   the data.frame. If omitted, then it will be assumed that the new column
#'   names are the same as the column names of the original variables.
#' @rdname lama_translate
#' @export
lama_translate_ <- function(
  .data,
  dictionary,
  translation,
  col = translation,
  col_new = col,
  keep_order = FALSE,
  to_factor = TRUE,
  ...
) {
  UseMethod("lama_translate_")
}

#' @rdname lama_translate
#' @examples
#'   ## Example-7: Usage of 'lama_translate_' for data frames
#'   # (apply translation 'subject' to column 'subject' and save it to column 'subject_new')
#'   # (apply translation 'result' to column 'res' and save it to column 'res_new')
#'   df_new <- lama_translate_(
#'     df, 
#'     dict,
#'     translation = c("subject", "result"),
#'     col = c("subject", "res"),
#'     col_new = c("subject_new", "res_new")
#'   )
#'   str(df_new)
#'   
#'   ## Example-8: Usage of 'lama_translate_' for data frames and store as character vector
#'   # (apply translation 'subject' to column 'subject' and save it to column 'subject_new')
#'   # (apply translation 'result' to column 'res' and save it to column 'res_new')
#'   df_new <- lama_translate_(
#'     df, 
#'     dict,
#'     translation = c("subject", "result"),
#'     col = c("subject", "res"),
#'     col_new = c("subject_new", "res_new"),
#'     to_factor = c(FALSE, FALSE)
#'   )
#'   str(df_new)
#'   
#' @export
lama_translate_.data.frame <- function(
  .data,
  dictionary,
  translation,
  col = translation,
  col_new = col,
  keep_order = FALSE,
  to_factor = TRUE,
  ...
) {
  # --- Check arguments ---
  if (length(rlang::quos(...)) > 0) {
      warning(paste(
        "Warning while calling 'lama_translate_':",
        "If the first element is a data frame,",
        "then only the arguments 'dictionary', 'translation',",
        "'col', 'col_new',",
        "'keep_order' and 'to_factor' are used and all extra arguments",
        "will be ignored."
      ))
  }
  check_and_translate_df_(
    .data = .data,
    dictionary = dictionary,
    translation = translation,
    col = col,
    col_new = col_new,
    keep_order = keep_order,
    to_factor = to_factor,
    is_translated = FALSE,
    err_handler = composerr("Error while calling 'lama_translate_'")
  )
}

#' @rdname lama_translate
#' @examples
#'   ## Example-9: Usage of 'lama_translate_' for atomic vectors
#'   res <- c(1, 2, 1, 3, 1, 2)
#'   res_new <- df_new_overwritten <- lama_translate_(
#'     res,
#'     dict,
#'     "result"
#'   )
#'   str(res_new)
#' 
#'   ## Example-10: Usage of 'lama_translate_' for factors
#'   sub <- factor(c("ma", "en", "ma"), levels = c("ma", "en"))
#'   sub_new <- df_new_overwritten <- lama_translate_(
#'     sub,
#'     dict,
#'     "subject",
#'     keep_order = TRUE
#'   )
#'   str(sub_new)
#' @export
lama_translate_.default <- function(
  .data,
  dictionary,
  translation,
  ...,
  keep_order = FALSE,
  to_factor = TRUE
) {
  if (length(rlang::quos(...)) > 0) {
      warning(paste(
        "Warning while calling 'lama_translate_':",
        "If the first element is a factor or an atomic vector,",
        "then only the arguments 'dictionary', 'translation',",
        "'keep_order' and 'to_factor' are used and all extra arguments",
        "will be ignored."
      ))
  }
  check_and_translate_vector_(
    .data = .data,
    dictionary = dictionary,
    translation = translation,
    keep_order = keep_order,
    to_factor = to_factor,
    is_translated = FALSE,
    err_handler = composerr("Error while calling 'lama_translate_'")
  )
}

#' @rdname lama_translate
#' @export
lama_to_factor <- function(
  .data,
  dictionary,
  ...,
  keep_order = FALSE
) {
  UseMethod("lama_to_factor")
}

#' @rdname lama_translate
#' @examples
#'   # the data frame which holds the right labels, but no factors
#'   df_translated <- data.frame(
#'     pupil = c(1, 1, 2, 2, 3),
#'     subject = c("English", "Mathematics", "Mathematics", "English", "English"),
#'     res = c("Very good", "Good", "Not so good", "Good", "Good")
#'   )
#'  
#'   ## Example-11: Usage of 'lama_to_factor' for data frames
#'   ##            Full length assignment
#'   # (apply order of translation 'subject' to column 'subject' and save it to column 'subject_new')
#'   # (apply order of translation 'result' to column 'res' and save it to column 'res_new')
#'   df_new <- lama_to_factor(
#'     df_translated,
#'     dict,
#'     sub_new = subject(subject),
#'     res_new = result(res)
#'   )
#'   str(df_new)
#'
#'   ## Example-12: Usage of 'lama_to_factor' for data frames
#'   ##            Abbreviation overwriting original columns
#'   # (apply order of translation 'subject' to column 'subject' and save it to column 'subject')
#'   # (apply order of translation 'result' to column 'res' and save it to column 'res')
#'   df_new_overwritten <- lama_to_factor(
#'     df_translated,
#'     dict,
#'     subject(subject),
#'     result(res)
#'   )
#'   str(df_new_overwritten)
#'
#'   ## Example-13: Usage of 'lama_to_factor' for data frames
#'   ##            Abbreviation if `translation_name == column_name`
#'   # (apply order of translation 'subject' to column 'subject' and save it to column 'subject_new')
#'   # (apply order of translation 'result' to column 'res' and save it to column 'res_new')
#'   df_new_overwritten <- lama_to_factor(
#'     df_translated, 
#'     dict,
#'     subject_new = subject,
#'     res_new = result(res)
#'   )
#'   str(df_new_overwritten)
#'   
#' @export
lama_to_factor.data.frame <- function(
  .data,
  dictionary,
  ...,
  keep_order = FALSE
) {
  check_and_translate_df(
    .data = .data,
    dictionary = dictionary,
    args = rlang::quos(...),
    keep_order = keep_order,
    to_factor = TRUE,
    is_translated = TRUE,
    err_handler = composerr("Error while calling 'lama_to_factor'")
  )
}

#' @rdname lama_translate
#' @examples
#'   ## Example-14: Usage of 'lama_translate' for atomic vectors
#'   var <- c("Mathematics", "English", "Mathematics")
#'   var_new <- lama_to_factor(
#'     var,
#'     dict,
#'     subject
#'   )
#'   str(var_new)
#'   
#' @export
lama_to_factor.default <- function(
  .data,
  dictionary,
  ...,
  keep_order = FALSE
) {
  args <- rlang::quos(...)
  if (length(args) > 1) {
      warning(paste(
        "Warning while calling `lama_to_factor`:",
        "If the first element is a factor or an atomic vector,",
        "then only the arguments 'dictionary', a single argument for '...'",
        "(the unquoted translation name)",
        "and the argument 'keep_order' are",
        "used and all extra arguments",
        "will be ignored."
      ))
  }
  check_and_translate_vector(
    .data = .data,
    dictionary = dictionary,
    args = args,
    keep_order = keep_order,
    to_factor = TRUE,
    is_translated = TRUE,
    err_handler = composerr("Error while calling 'lama_to_factor'")
  )
}

#' @rdname lama_translate
#' @export
lama_to_factor_ <- function(
  .data,
  dictionary,
  translation,
  col = translation,
  col_new = col,
  keep_order = FALSE,
  ...
) {
  UseMethod("lama_to_factor_")
}

#' @rdname lama_translate
#' @examples
#'   ## Example-15: Usage of 'lama_to_factor_' for data frames
#'   # (apply order of translation 'subject' to column 'subject' and save it to column 'subject_new')
#'   # (apply order of translation 'result' to column 'res' and save it to column 'res_new')
#'   df_new <- lama_to_factor_(
#'     df_translated, 
#'     dict,
#'     translation = c("subject", "result"),
#'     col = c("subject", "res"),
#'     col_new = c("subject_new", "res_new")
#'   )
#'   str(df_new)
#'   
#' @export
lama_to_factor_.data.frame <- function(
  .data,
  dictionary,
  translation,
  col = translation,
  col_new = col,
  keep_order = FALSE,
  ...
) {
  # --- Check arguments ---
  if (length(rlang::quos(...)) > 0) {
      warning(paste(
        "Warning while calling 'lama_to_factor_':",
        "If the first element is a data frame,",
        "then only the arguments 'dictionary', 'translation',",
        "'col', 'col_new' and",
        "'keep_order' are used and all extra arguments",
        "will be ignored."
      ))
  }
  check_and_translate_df_(
    .data = .data,
    dictionary = dictionary,
    translation = translation,
    col = col,
    col_new = col_new,
    keep_order = keep_order,
    to_factor = TRUE,
    is_translated = TRUE,
    err_handler = composerr("Error while calling 'lama_to_factor_'")
  )
}

#' @rdname lama_translate
#' @examples
#'   ## Example-16: Usage of 'lama_to_factor_' for atomic vectors
#'   var <- c("Very good", "Good", "Good")
#'   var_new <- lama_to_factor_(
#'     var,
#'     dict,
#'     "result"
#'   )
#'   str(var_new)
#' @export
lama_to_factor_.default <- function(
  .data,
  dictionary,
  translation,
  ...,
  keep_order = FALSE
) {
  if (length(rlang::quos(...)) > 0) {
      warning(paste(
        "Warning while calling 'lama_to_factor_':",
        "If the first element is a factor or an atomic vector,",
        "then only the arguments 'dictionary', 'translation' and",
        "'keep_order' are used and all extra arguments",
        "will be ignored."
      ))
  }
  check_and_translate_vector_(
    .data = .data,
    dictionary = dictionary,
    translation = translation,
    keep_order = keep_order,
    to_factor = TRUE,
    is_translated = TRUE,
    err_handler = composerr("Error while calling 'lama_to_factor_'")
  )
}


#' This function relabels several variables in a data.frame
#'
#' @inheritParams lama_translate
#' @inheritParams lama_translate_
#' @param is_translated A boolean vector of length one or the same length as the
#'   number of translations. If the vector has length one, then the same 
#'   configuration is applied to all variable translations. 
#'   If `is_translated = TRUE`, then the original variable is a character
#'   vector holding the right labels (character strings). 
#'   In this case, the labels are left unchanged, but the variables are turned
#'   into factors with order given in the selected translations.
#' @param err_handler An error handling function
#' @return An factor vector holding the assigned labels.
translate_df <- function(
  .data,
  dictionary,
  translation,
  col,
  col_new,
  keep_order,
  to_factor,
  is_translated,
  err_handler
) {
  if (length(keep_order) == 1)
    keep_order <- rep(keep_order, length(col_new))
  if (length(to_factor) == 1)
    to_factor <- rep(to_factor, length(col_new))
  if (length(is_translated) == 1)
    is_translated <- rep(is_translated, length(col_new))
  .data[col_new] <- lapply(
    seq_len(length(translation)),
    function(i) {
      translate_vector(
        val = .data[[col[i]]],
        translation = dictionary[[translation[i]]],
        keep_order = keep_order[i],
        to_factor = to_factor[i],
        is_translated = is_translated[i],
        err_handler = composerr_parent(
          paste0(
            "Translation '", translation[i], "' could not be applied to ",
            "column '", col[i], "'"
          ),
          err_handler
        )
      )
    }
  )
  .data
}

#' This function relabels a vector
#'
#' @param val The vector that should be relabeled. Allowed are all vector types (also factor).
#' @param translation Named character vector holding the label assignments.
#' @param keep_order A logical flag. If the vector in `val`
#' is a factor variable and `keep_order` is set to `TRUE`, then
#' the order of the original factor variable is preserved.
#' @param to_factor A logical flag. If set to `TRUE`, the the resulting
#'   labeled variable will be a factor and a plain character vector
#'   otherwise.
#' @param is_translated A logical flag. If `is_translated = TRUE`, then
#' `val` must be a character vector holding the right labels (character strings)
#' and will be turned into a factor with ordering given in the translation
#' (except for the case when `keep_order = TRUE`).
#' @param err_handler An error handling function
#' @return A factor vector holding the assigned labels
translate_vector <- function(
  val,
  translation,
  keep_order,
  to_factor,
  is_translated,
  err_handler
) {
  old_labels <- names(translation)
  val_char <- as.character(val)
  if (is_translated) {
    # The variable is already translated and should just be turned into a factor
    missing_labels <- unique(val_char[!is.na(val_char)])
    missing_labels <- missing_labels[!missing_labels %in% translation]
    if (length(missing_labels) > 0)
      err_handler(paste0(
        "The following values in the column variable could not be found ",
        "in the translation: ",
        stringify(missing_labels),
        "."
      ))
    if (keep_order) {
      if (is.factor(val)) {
        col_levels <- intersect(as.character(levels(val)), translation)
      } else {
        col_levels <- unique(val_char)
        col_levels <- col_levels[!is.na(col_levels)]
        col_levels <- col_levels[order(col_levels)] 
      }
      col_levels <- c(col_levels, setdiff(translation, col_levels))
    } else {
      col_levels <- unique(translation)
    }
    return(factor(val_char, levels = col_levels))
  }
  flag_na_escape <- any(is.na(val_char)) && contains_na_escape(old_labels)
  if (flag_na_escape)
    val_char <- na_to_escape(val_char) 
  # Check that all old labels can be found in the labeling dictionary
  missing_labels <- unique(val_char[!is.na(val_char)])
  missing_labels <- missing_labels[!missing_labels %in% old_labels]
  if (length(missing_labels) > 0)
    err_handler(paste0(
      "The following values in the column variable could not be found ",
      "in the translation: ",
      stringify(missing_labels),
      "."
    ))
  # --- Relabel val ---
  labeling_map <- data.frame(old = old_labels, new = translation, stringsAsFactors = FALSE)
  if (keep_order) {
    # if the old order should be kept, then reorder the labeling map
    if (is.factor(val)) {
      col_levels <- intersect(as.character(levels(val)), old_labels)
    } else {
      col_levels <- unique(val)
      col_levels <- col_levels[!is.na(col_levels)]
      col_levels <- as.character(col_levels[order(col_levels)])
    }
    labeling_map <- labeling_map[
      match(c(col_levels, setdiff(old_labels, col_levels)), old_labels),
    ]
  }
  if (to_factor) {
    # set new labels as factor
    labeling_map$new <- factor(
      labeling_map$new,
      levels = unique(labeling_map$new)
    )
  } else {
    # set new labels as character vector
    labeling_map$new <- labeling_map$new
  }
  # Merge labels
  temp <- merge(
    data.frame(ord = seq_len(length(val_char)), old = val_char),
    labeling_map,
    by = "old",
    all.x = TRUE
  )
  temp <- temp[order(temp$ord),]
  temp$new
}

#' Function that applies some general checks to the arguments of [lama_translate()] and [lama_translate_()]
#'
#' @inheritParams translate_df
#' @param err_handler An error handling function
check_arguments <- function(
  .data,
  dictionary,
  col_new,
  keep_order,
  to_factor,
  err_handler
) {
  if (!is.data.frame(.data))
    err_handler("The argument '.data' must be a data.frame.")
  if (!is.dictionary(dictionary))
    err_handler("The argument 'dictionary' must be a lama_dictionary class object.")
  if (!is.logical(keep_order) || !length(keep_order) %in% c(1, length(col_new)))
    err_handler(paste(
      "The argument 'keep_order' must be a logical",
      "vector of length one or length equal to the number of applied translations."
    ))
  if (!is.logical(to_factor) || !length(to_factor) %in% c(1, length(to_factor)))
    err_handler(paste(
      "The argument 'to_factor' must be a logical",
      "vector of length one or length equal to the number of applied translations."
    ))
}

#' Checks arguments and translate a data.frame
#'
#' @inheritParams translate_df
#' @param args The list of arguments given in ... when calling `lama_translate()` or `lama_to_factor()`
check_and_translate_df <- function(
  .data,
  dictionary,
  args,
  keep_order,
  to_factor,
  is_translated,
  err_handler
) {
  if (length(args) == 0)
    err_handler("Translation assignments are missing. Use unquoted arguments, e.g. 'Y = TRANS(X)', to relabel variable 'X' with the translation 'TRANS' and assign the result to the variable 'Y'.")
  new_cols <- names(args)
  if (is.null(new_cols))
    new_cols <- rep("", length(args))
  check_arguments(.data, dictionary, col_new = new_cols, keep_order, to_factor, err_handler)
  translation_list <- lapply(
    seq_len(length(args)), 
    function(i) {
      col_new <- new_cols[i]
      x <- args[[i]]
      arg_str <- rlang::as_label(rlang::quo_get_expr(x))
      err_handler <- composerr_parent(paste(
          "Invalid argument at position",
          stringify(i + 2)
        ),
        err_handler
      )
      err_handler_parse <- composerr(
        paste(
          "The expression",
          stringify(ifelse(col_new == "", arg_str, paste(col_new, "=", arg_str))),
          "could not be parsed. Use unquoted function calls with a single",
          "argument, where the function name is the translation that should be used",
          "and the function argument is the unquoted name of the column,",
          "which should be translated",
          "(e.g. 'lama_translate(df, dict, y = foo(x))' or 'lama_to_factor(df, dict, y = foo(x))',",
          "where 'y' is the new column name, 'x' is the name of the column",
          "holding the original data and 'foo' is the name of the translation",
          " stored in 'dict'). It is also possible to just pass a translation",
          "name. In this case it will be assumed that the column name matches",
          "the translation name",
          "(e.g. 'lama_translate(df, dict, y = foo)' is the same as",
          "'lama_translate(df, dict, y = foo(foo))' and",
          "'lama_to_factor(df, dict, y = foo)' is the same as",
          "'lama_to_factor(df, dict, y = foo(foo))')."
        ),
        err_handler
      )
      if (rlang::is_call(rlang::quo_get_expr(x), n = 1, name = names(dictionary))) {
        tryCatch(
          {
            translation <- rlang::call_name(x)
            col <- deparse(rlang::call_args(x)[[1]])
          },
          error = function(e) err_handler_parse(e),
          warning = function(w) err_handler_parse(w)
        ) 
      } else {
        if (!rlang::quo_is_symbol(x))
          err_handler_parse()
        translation <- rlang::quo_name(x)
        col <- translation
      }
      if (col_new == "")
        col_new <- col
      if (!col %in% names(.data))
        err_handler(paste0(
          "The variable name '", col, "' could not be found in the data.frame."
        ))
      if (!translation %in% names(dictionary))
        err_handler(paste0(
          "The translation name '", translation, "' could not be found in the lama_dictionary."
        ))
      list(
        col_new = col_new,
        col = col,
        translation = translation
      )
    }
  )
  col_new <- sapply(translation_list, function(x) x$col_new)
  duplicates <- table(col_new)
  duplicates <- names(duplicates[duplicates > 1])
  if (length(duplicates) > 0)
    err_handler(paste0(
      "More than one relabeled variable was assigned to the same column ",
      "name. The following column names have multiple assignments: ",
      stringify(duplicates),
      "."
    ))
  translate_df(
    .data = .data, 
    dictionary = dictionary,
    translation = sapply(translation_list, function(x) x$translation),
    col = sapply(translation_list, function(x) x$col),
    col_new = col_new,
    keep_order = keep_order,
    to_factor = to_factor,
    is_translated = is_translated,
    err_handler = err_handler
  )
}

#' Checks arguments and translate a vector
#'
#' @inheritParams check_and_translate_df
check_and_translate_vector <- function(
  .data,
  dictionary,
  args,
  keep_order,
  to_factor,
  is_translated,
  err_handler
) {
  if (!is.atomic(.data))
    err_handler(paste(
      "The argument '.data' must either be a data frame, a factor or an atomic vector."
    ))
  if (length(args) == 0)
    err_handler(paste(
      "The name of the used translation is missing",
      "(e.g. 'lama_translate(x, dict, my_trans)')."
    ))
  if (!is.dictionary(dictionary))
    err_handler("The argument 'dictionary' must be a lama_dictionary class object.")
  if (!is.logical(keep_order) || length(keep_order) != 1 || is.na(keep_order))
    err_handler(paste(
      "The argument 'keep_order' must be a logical."
    ))
  if (!is.logical(to_factor) || length(to_factor) != 1 || is.na(to_factor))
    err_handler(paste(
      "The argument 'to_factor' must be a logical."
    ))
  x <- args[[1]]
  x_str <- rlang::as_label(rlang::quo_get_expr(x))
  translation <- rlang::quo_name(x)
  if (!translation %in% names(dictionary))
    err_handler(paste0(
      "The translation name '", translation, "' could not be found in the lama_dictionary."
    ))
  translate_vector(
    val = .data,
    translation = dictionary[[translation]],
    keep_order = keep_order,
    to_factor = to_factor,
    is_translated = is_translated,
    err_handler = err_handler
  )
}

#' Checks arguments and translate a data.frame (standard eval)
#'
#' @inheritParams lama_translate_
#' @inheritParams translate_df
check_and_translate_df_ <- function(
  .data,
  dictionary,
  translation,
  col,
  col_new,
  keep_order,
  to_factor,
  is_translated,
  err_handler
) {
  # --- Check arguments ---
  check_arguments(.data, dictionary, col_new = col_new, keep_order, to_factor, err_handler)
  # specific checks for 'lama_translate_'
  if (!is.character(translation) || length(translation) == 0)
    err_handler("The argument 'translation' must be a character vector.")
  if (any(is.na(translation)))
    err_handler("The argument 'translation' contains missing values ('NA').")
  if (!is.character(col) || length(col) != length(translation))
    err_handler("The argument 'col' must be a character vector of the same length as 'translation'.")
  if (any(is.na(col)))
    err_handler("The argument 'col' contains missing values ('NA').")
  if (!is.character(col_new) || length(col_new) != length(translation))
    err_handler("The argument 'col_new' must be a character vector of the same length as 'translation'.")
  if (any(is.na(col_new)))
    err_handler("The argument 'col_new' contains missing values ('NA').")
  duplicates <- table(col_new)
  duplicates <- names(duplicates[duplicates > 1])
  if (length(duplicates) > 0)
    err_handler(paste0(
      "The argument 'col_new' is invalid: More than one relabeled variable ",
      "was assigned to the same column name. ",
      "The following column names have multiple assignments: ",
      stringify(duplicates),
      "."
    ))
  invalid <- !translation %in% names(dictionary)
  if (any(invalid)) 
    err_handler(paste0(
      "The following values of the argument 'translation' could not be found ",
      "in the lama_dictionary object given in argument 'dictionary': ",
      stringify(translation[invalid]),
      ".\nUse a subset of the following variable names: ",
      stringify(names(dictionary)),
      "."
    ))
  invalid <- !col %in% names(.data)
  if (any(invalid))
    err_handler(paste0(
      "The following values of the argument 'col' are no column names of the data.frame ",
      "given in argument '.data': ",
      stringify(col[invalid]),
      ".\nConsider using ",
      "The argument 'col' in order to deal with column names differing ",
      "from the variable names defined in the passed in dictionary."
    ))
  invalid <- !is.syntactic(col_new)
  if (any(invalid))
    err_handler(paste0(
      "The following values of 'col_new' are invalid column names: ",
      stringify(col_new[invalid]),
      "."
    ))
  translate_df(
    .data = .data,
    dictionary = dictionary,
    translation = translation,
    col = col,
    col_new = col_new,
    keep_order = keep_order,
    to_factor = to_factor,
    is_translated = is_translated,
    err_handler = err_handler
  )
}

#' Checks arguments and translate a character vector (standard eval)
#'
#' @inheritParams lama_translate_
#' @inheritParams translate_df
check_and_translate_vector_ <- function(
  .data,
  dictionary,
  translation,
  keep_order,
  to_factor,
  is_translated,
  err_handler
) {
  if (!is.atomic(.data))
    err_handler(paste(
      "The argument '.data' must either be a data frame, a factor or an atomic vector."
    ))
  if (!is.dictionary(dictionary))
    err_handler("The argument 'dictionary' must be a lama_dictionary class object.")
  if (!is.logical(keep_order) || length(keep_order) != 1 || is.na(keep_order))
    err_handler(paste(
      "The argument 'keep_order' must be a logical."
    ))
  if (!is.logical(to_factor) || length(to_factor) != 1 || is.na(to_factor))
    err_handler(paste(
      "The argument 'to_factor' must be a logical."
    ))
  if (!is.character(translation) || length(translation) != 1 || is.na(translation))
    err_handler("The argument 'translation' must be a character string.")
  if (!translation %in% names(dictionary))
    err_handler(paste0(
      "The translation name '", translation, "' could not be found in the lama_dictionary."
    ))
  translate_vector(
    val = .data,
    translation = dictionary[[translation]],
    keep_order = keep_order,
    to_factor = to_factor,
    is_translated = is_translated,
    err_handler = err_handler
  )
}
