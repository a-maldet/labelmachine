#' Relabel a variable in a [data.frame]
#'
#' This function takes a data.frame and converts one of its categorical variables
#' (not necessarely a factor variable) to a factor variable with new labels. 
#' The label assignment is given by a lexicon entry (defined by the arguments
#' \code{lexicon} and \code{translation}). 
#' @param ... Various arguents
#' @return An extended data.frame, that has a factor variable holding the
#' @include label_lexicon.R
translate <- function(...) {
  UseMethod("translate")
}

#' Label a variable in a [data.frame]
#'
#' This function takes a data set and converts one of its categorical variables
#' (not necessarely a factor variable) to a factor variable with new labels. 
#' The label assignment is given by a label translation (defined by the arguments
#' \code{lexicon} and \code{translation}). 
#' @param df The data.frame object which contains the variable that should be labelled
#' @param lexicon A [LabelLexicon] object, holding various label translations
#' @param translation A character string holding the name of the label translation that
#' should be used for labelling the variable. This name must be one of the translation names
#' returned by \code{names(lexicon)}.
#' @return An extended data.frame, that has a factor variable holding the
#' assigned labels.
translate.data.frame <- function(df, lexicon, translation, col = translation, col_new = col, keep_ordering = FALSE) {
  # --- Check arguments ---
  err_handler <- composerr("Error while calling 'translate'")
  if (!is.data.frame(df))
    err_handler("The argument 'df' must be a data.frame.")
  if (class(lexicon) != "LabelLexicon")
    err_handler("The argument 'lexicon' must be a LabelLexicon class object.")
  if (!is.character(translation) || length(translation) != 1)
    err_handler("The argument 'translation' must be a character string.")
  if (!is.character(col) || length(col) != 1)
    err_handler("The argument 'col' must be a character string.")
  if (!is.character(col_new) || length(col_new) != 1)
    err_handler("The argument 'col_new' must be a character string.")
  if (!is.logical(keep_ordering) || length(keep_ordering) != 1)
    err_handler("The argument 'keep_ordering' must be a character string.")
  if (!translation %in% names(lexicon)) 
    err_handler(paste0(
        "Error in argument 'translation': The translation '",
        translation,
        "' could not be found in the LabelLexicon object given in argument 'lexicon': ",
        "Use one of the following translation names: ",
        paste0(names(lexicon), collapse = c(", "))
      ))
  if (!col %in% names(df))
    err_handler(paste0(
        "Error in argument 'col': The variable '",
        col,
        "' is not a column of the data.frame given in argument 'df'. Consider using ",
        "the argument 'col' in order to deal with column names differing ",
        "from the variable name defined in the underlying theme."
      ))
  if (!is.character(col_new) || length(col_new) != 1 || !is.syntactic(col_new))
    err_handler("The argument 'col_new' must be a valid column name.")
  # Check that all old labels can be found in the labelling theme
  old_labels <- names(lexicon[[translation]])
  missing_labels <- unique(as.character(df[[col]]))
  missing_labels <- missing_labels[!missing_labels %in% old_labels]
  if (length(missing_labels) > 0)
    err_handler(paste0(
        "The following variable levels in 'df$", col,"' have no corresponding ",
        "label in the LabelLexicon object given in argument 'lexicon': ",
        paste(missing_labels, collapse = ", ")
      ))
  # --- Relabel df[[col]] and save to df[[col_new]] ---
  labelling_map <- data.frame(old = old_labels, new = lexicon[[translation]])
  if (is.factor(df[[col]]) && keep_ordering) {
    # if the old order should be kept, then reorder the labelling map
    col_levels <- intersect(as.character(levels(df[[col]])), old_labels)
    labelling_map <- labelling_map[
        match(old_labels, c(col_levels, setdiff(old_labels, col_levels))),
      ]
  }
  # set new labels as factor
  labelling_map$new = factor(
      labelling_map$new,
      levels = unique(labelling_map$new)
    )
  # Merge labels and set new labels to df[[col_new]]
  temp <- merge(
    data.frame(ord = seq_len(nrow(df)), old = as.character(df[[col]])),
    labelling_map
  )
  temp <- temp[order(temp$ord),]
  df[[col_new]] <- temp$new
  df
}
