#' Assign new labels to a variable of a data.frame
#'
#' The functions [lama_translate()] and [lama_translate_()] take a data.frame
#' and convert one or more of its categorical variables
#' (not necessarely a factor variable) into factor variables with new labels. 
#' The function [lama_translate()] uses non-standard evaluation, whereas 
#' [lama_translate_()] is the standard evaluation alternative.
#' @param .data The data.frame object which contains the variable that should be relabelled
#' @param dictionary A [LabelDictionary][new_dictionary()] object, holding the translations for various
#' variables.
#' @param ... One or more unquoted expressions separated by commas. Use unquoted
#' arguments that tell which translation should be applied to which column and
#' which column name the relabelled variable should be assigned to. E.g.
#' \code{lama_trans(.data, dict, Y1 = TRANS1(X1), Y2 = TRANS2(Y2))}
#' to apply the translations \code{TRANS1} and \code{TRANS2} to the data.frame
#' variables \code{X1} and \code{X2} and save the new labelled variables under
#' the names \code{Y1} and \code{Y2}.
#' There are also two abbreviation mechanisms available:
#' The argument assignement \code{FOO(X)} is the same as \code{X = FOO(X)} and
#' \code{FOO} is an abbreviation for \code{FOO = FOO(FOO)}.
#' @param keep_order A boolean vector of length one or the same length as the
#' number of arguments in \code{...}. If the vector has length one, then the same 
#' configuration is applied to all variable translations. If the vector has 
#' the same length as the number of arguments in \code{...}, then the 
#' to each variable translation there is a corresponding boolean configuration.
#' If a translated variable in the data.frame is a factor variable,
#' and the corresponding boolean configuration is set to \code{TRUE}, then the
#' the order of the original factor variable will be preserved.
#' @return An extended data.frame, that has a factor variable holding the
#' assigned labels.
#' @rdname lama_translate
#' @include dictionary.R
#' @export
lama_translate <- function(.data, dictionary, ..., keep_order = FALSE) {
  UseMethod("lama_translate")
}

#' @rdname lama_translate
#' @method lama_translate data.frame
#' @export
lama_translate.data.frame <- function(.data, dictionary, ..., keep_order = FALSE) {
  args <- rlang::quos(...)
  err_handler <- composerr("Error while calling 'lama_translate'")
  if (length(args) == 0)
    err_handler("Translation assignments are missing. Use unquoted arguments, e.g. 'Y = TRANS(X)', to relabel variable 'X' with the translation 'TRANS' and assign the result to the variable 'Y'.")
  new_cols <- names(args)
  if (is.null(new_cols))
    new_cols <- rep("", length(args))
  check_translate_general(.data, dictionary, col_new = new_cols, keep_order, err_handler)
  if (length(keep_order) == 1)
    keep_order <- rep(keep_order, length(new_cols))
  translation_list <- lapply(
    seq_len(length(args)), 
    function(i) {
      col_new <- new_cols[i]
      arg_str <- rlang::as_label(rlang::quo_get_expr(args[[i]]))
      err_handler <- composerr_parent(paste0(
        "Argument in position '", i, "' could not be parsed: ",
        stringify(ifelse(col_new == "", arg_str, paste(col_new, "=", arg_str))),
        "."
      ), err_handler)
      p_long <- "^([^\\s\\(\\)\\[\\]\\{\\}=\\+-]+)\\(([^\\s\\(\\)\\[\\]\\{\\}=\\+-]+)\\)$"
      p_short <- "^([^\\s\\(\\)\\[\\]\\{\\}=\\+-]+)$"
      if (grepl(p_long, arg_str, perl = TRUE)) {
        translation <- gsub(p_long, "\\1", arg_str, per = TRUE)
        col <- gsub(p_long, "\\2", arg_str, perl = TRUE)
      } else if (grepl(p_short, arg_str, perl = TRUE)) {
        translation <- gsub(p_short, "\\1", arg_str, perl = TRUE)
        col <- translation
      } else {
        err_handler() 
      }
      if (col_new == "")
        col_new <- col
      if (!is.syntactic(translation) || !is.syntactic(col))
        err_handler()
      if (!col %in% names(.data))
        err_handler()
      if (!col %in% names(.data))
        err_handler(paste0(
          "The variable name '", col, "' could not be found in the data.frame."
        ))
      if (!translation %in% names(dictionary))
        err_handler(paste0(
          "The translation name '", translation, "' could not be found in the LabelDictionary."
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
      "More than one relabelled variable was assigned to the same column ",
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
    err_handler = err_handler
  )
}

#' @param translation A character vector holding the names of the variable 
#' translations which
#' should be used for assigning new labels to the variable. This names must be
#' a subset of the translation names returned by \code{names(dictionary)}.
#' @param col A character vector of the same length as \code{translation} holding
#' the names of the data.frame columns that
#' should be relabelled. If omitted, then it will be assumed that the column
#' names are the same as the given translation names in the argument \code{translation}.
#' @param col_new A character vector of the same length as \code{translation} holding
#' the names under which the relabelled variables should be stored in
#' the data.frame. If omitted, then it will be assumed that the new column
#' names are the same as the column names of the original variables.
#' @rdname lama_translate
#' @export
lama_translate_ <- function(.data, dictionary, translation, col = translation, col_new = col, keep_order = FALSE) {
  UseMethod("lama_translate_")
}

#' @rdname lama_translate
#' @export
lama_translate_.data.frame <- function(.data, dictionary, translation, col = translation, col_new = col, keep_order = FALSE) {
  # --- Check arguments ---
  err_handler <- composerr("Error while calling 'lama_translate_'")
  check_translate_general(.data, dictionary, col_new = col_new, keep_order, err_handler)
  if (length(keep_order) == 1)
    keep_order <- rep(keep_order, length(col_new))
  # specific checks for 'lama_translate_'
  if (!is.character(translation) || length(translation) == 0 || any(is.na(translation)))
    err_handler("The argument 'translation' must be a character vector.")
  if (!is.character(col) || length(col) != length(translation) || any(is.na(col)))
    err_handler("The argument 'col' must be a character vector of the same length as 'translation'.")
  if (!is.character(col_new) || length(col_new) != length(translation) || any(is.na(col_new)))
    err_handler("The argument 'col_new' must be a character vector of the same length as 'translation'.")
  if (length(col_new) != length(unique(col_new)))
    err_handler("The argument 'col_new' must be a character vector without duplicates.")
  invalid <- !translation %in% names(dictionary)
  if (any(invalid)) 
    err_handler(paste0(
      "The following values of the argument 'translation' could not be found ",
      "in the LabelDictionary object given in argument 'dictionary': ",
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
  translate_df(.data, dictionary, translation, col, col_new, keep_order, err_handler)
}

#' This function relabels several variables in a data.frame
#'
#' @inheritParams lama_translate
#' @inheritParams lama_translate_
#' @param err_handler An error handling function
#' @return An factor vector holding the assigned labels.
translate_df <- function(
  .data,
  dictionary,
  translation,
  col,
  col_new,
  keep_order,
  err_handler
) {
  .data[col_new] <- lapply(
    seq_len(length(translation)),
    function(i) {
      translate_variable(
        val = .data[[col[i]]],
        translation = dictionary[[translation[i]]],
        keep_order = keep_order[i],
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
#' @param val The vector that should be relabelled. Allowed are all vector types (also factor).
#' @param translation Named character vector holding the label assignements.
#' @param keep_order A logical flag. If the vector in \code{val}
#' is a factor variable and \code{keep_order} is set to \code{TRUE}, then
#' the order of the original factor variable is preserved.
#' @param err_handler An error handling function
#' @return A factor vector holding the assigned labels
translate_variable <- function(val, translation, keep_order, err_handler) {
  val_char <- as.character(val)
  # Check that all old labels can be found in the labelling dictionary
  old_labels <- names(translation)
  missing_labels <- unique(val_char)
  missing_labels <- missing_labels[!missing_labels %in% old_labels]
  if (length(missing_labels) > 0)
    err_handler(paste0(
      "The following values in the column variable could not be found ",
      "in the translation: ",
      stringify(missing_labels),
      "."
    ))
  # --- Relabel val ---
  labelling_map <- data.frame(old = old_labels, new = translation)
  if (is.factor(val) && keep_order) {
    # if the old order should be kept, then reorder the labelling map
    col_levels <- intersect(as.character(levels(val)), old_labels)
    labelling_map <- labelling_map[
        match(c(col_levels, setdiff(old_labels, col_levels)), old_labels),
      ]
  }
  # set new labels as factor
  labelling_map$new = factor(
      labelling_map$new,
      levels = unique(labelling_map$new)
    )
  # Merge labels
  temp <- merge(
    data.frame(ord = seq_len(length(val_char)), old = val_char),
    labelling_map
  )
  temp <- temp[order(temp$ord),]
  temp$new
}

#' Function that applies some general checks to the arguments of [lama_translate()] and [lama_translate_()]
#'
#' @inheritParams lama_translate_
#' @param err_handler An error handling function
check_translate_general <- function(.data, dictionary, col_new, keep_order, err_handler) {
  if (!is.data.frame(.data))
    err_handler("The argument '.data' must be a data.frame.")
  if (!is.dictionary(dictionary))
    err_handler("The argument 'dictionary' must be a LabelDictionary class object.")
  if (!is.logical(keep_order) || !length(keep_order) %in% c(1, length(col_new)))
    err_handler(paste(
      "The argument 'keep_order' must be a character string or a character",
      "vector with length equal to the number of applied translations."
    ))
}
