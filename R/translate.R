#' Assign new labels to a variable in a data.frame
#'
#' This function takes a data.frame and converts one or more of its
#' categorical variables
#' (not necessarely a factor variable) into factor variables with new labels. 
#' The label assignment is given by the variable translations, which are 
#' defined by the function arguments \code{dictionary} and \code{variable}. 
#' @param ... Various arguents
#' @include dictionary.R
#' @rdname translate
#' @export
translate <- function(...) {
  UseMethod("translate")
}

#' @param df The data.frame object which contains the variable that should be labelled
#' @param dictionary A [LabelDictionary][new_dictionary()] object, holding the translations for various
#' variables.
#' @param variable A character vector holding the names of the variable 
#' translations which
#' should be used for assigning new labels to the variable. This names must be
#' a subset of the translation names returned by \code{names(dictionary)}.
#' @param col A character vector of the same length as \code{variable} holding
#' the names of the data.frame columns that
#' should be relabelled. If omitted, then it will be assumed that the column
#' names are the same as the given translation names in the argument \code{variable}.
#' @param col_new A character vector of the same length as \code{variable} holding
#' the names under which the relabelled variables should be stored in
#' the data.frame. If omitted, then it will be assumed that the new column
#' names are the same as the column names of the original variables.
#' @param keep_order A logical flag. If the original data.frame variable
#' as a factor variable and \code{keep_order} is set to \code{TRUE}, then
#' the order of the original factor variable is preserved.
#' return An extended data.frame, that has a factor variable holding the
#' assigned labels.
#' @rdname translate
#' @method translate data.frame
#' @export
translate.data.frame <- function(df, dictionary, variable, col = variable, col_new = col, keep_order = FALSE) {
  # --- Check arguments ---
  err_handler <- composerr("Error while calling 'translate'")
  if (!is.data.frame(df))
    err_handler("The argument 'df' must be a data.frame.")
  if (!is.dictionary(dictionary))
    err_handler("The argument 'dictionary' must be a LabelDictionary class object.")
  if (!is.character(variable) || length(variable) == 0)
    err_handler("The argument 'variable' must be a character vector.")
  if (!is.character(col) || length(col) != length(variable))
    err_handler("The argument 'col' must be a character vector of the same length as 'variable'.")
  if (!is.character(col_new) || length(col_new) != length(variable))
    err_handler("The argument 'col_new' must be a character vector of the same length as 'variable'.")
  if (length(col_new) != length(unique(col_new)))
    err_handler("The argument 'col_new' must be a character vector without duplicates.")
  if (!is.logical(keep_order) || length(keep_order) != 1)
    err_handler("The argument 'keep_order' must be a character string.")
  if (any(!variable %in% names(dictionary))) 
    err_handler(paste0(
        "The following values of the argument 'variable' could not be found ",
        "in the LabelDictionary object given in argument 'dictionary': ",
        paste(variable[!variable %in% names(dictionary)], collapse = ", "),
        "\nUse a subset of the following variable names: ",
        paste0(names(dictionary), collapse = c(", "))
      ))
  if (any(!col %in% names(df)))
    err_handler(paste0(
      "The following values of 'col' are no column names of the data.frame given in argument 'df': ",
      paste(col[!col %in% names(df)], collapse = ", "),
      "\nConsider using ",
      "The argument 'col' in order to deal with column names differing ",
      "from the variable names defined in the passed in dictionary."
    ))
  if (any(!is.syntactic(col_new)))
    err_handler(paste(
      "The following values of 'col_new' are invalid column names:",
      paste(col_new[!is.syntactic(col_new)], collapse = ", ")
    ))
  for (i in seq_len(length(variable))) 
    df[[col_new[i]]] <- create_labelled_vector(
      col[i], df[[col[i]]], dictionary[[variable[i]]], keep_order, err_handler
    )
  df
}


create_labelled_vector <- function(col, val, translation, keep_order, err_handler) {
  val_char <- as.character(val)
  # Check that all old labels can be found in the labelling dictionary
  old_labels <- names(translation)
  missing_labels <- unique(val_char)
  missing_labels <- missing_labels[!missing_labels %in% old_labels]
  if (length(missing_labels) > 0)
    err_handler(paste0(
        "The following variable levels in 'df$", col,"' have no corresponding ",
        "label in the LabelDictionary object given in argument 'dictionary': ",
        paste(missing_labels, collapse = ", ")
      ))
  # --- Relabel val ---
  labelling_map <- data.frame(old = old_labels, new = translation)
  if (is.factor(val) && keep_order) {
    # if the old order should be kept, then reorder the labelling map
    col_levels <- intersect(as.character(levels(val)), old_labels)
    labelling_map <- labelling_map[
        match(old_labels, c(col_levels, setdiff(old_labels, col_levels))),
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

