default_theme_name <- "default"

#' Class LabelMachine
#'
#' An \code{S4 class} which holds the labels of various variables and various
#' themes.
#' @name LabelMachine-class
#' @rdname LabelMachine-class
#' @exportClass LabelMachine
setClass(
  "LabelMachine",
  representation(
    themes = "list"
  )
)

#' Constructor method of the [LabelMachine] class object
#'
#' @name LabelMachine
#' @rdname LabelMachine-Class
#' @aliases initialize,LabelMachine-method
#' @param .Object A [LabelMachine] class object
#' @param themes A named list object with the following structure:
#' \code{themes (named list) > variables (named list) > label mappings (named character vector)}
setMethod("initialize", signature(.Object = "StyledCell"),
  function(.Object, themes) {
    # Check the 'themes' argument
    err_handler <- paste(
        "Error while initializing the LabelMachine:",
        "The 'themes' argument is invalid"
      ) %>%
      composerr
    if (!is.list(themes) || is.null(names(themes)) || any(names(themes) == ""))
      err_handler("The 'themes' argument must be a named list.")
    lapply(names(themes), function(thm_name) {
      err_handler <-  composerr_parent(
        paste0("Error in list child element '", thm_name, "'"),
        err_handler
      )
      thm <- themes[[thm_name]]
      if (!is.list(thm) || is.null(names(thm)) || any(names(thm) == ""))
         err_handler("The element must be a named list itself")
      lapply(names(thm), function(var_name) {
        err_handler <-  composerr_parent(
          paste0("Error in the grand child element '", var_name, "'"),
          err_handler
        )
        mapping <- thm[[var_name]]
        if (!is.character(mapping) || is.null(names(mapping)) || any(names(mapping) == ""))
         err_handler("The element must be a named character vector")
      })
    })
  .Object@themes <- themes
  .Object
  }
)

#' Read in a \code{yaml} file holding labels and themes
#'
#' @param yaml_path Path to yaml file holding the labels and themes
read_label_file <- function(yaml_path) {
  new("LabelMachine", themes = yaml_to_class(yaml::read_yaml(file_path)))
}




#TODO: merge_label_machines

#' Merge two value label structures (old fields will be overwritten by the new ones)
#'
#' @param value_labels_old The old value label structure (can be NULL)
#' @param value_labels The new value label structure (overwrites fields in the old value structure)
#' @return The merged value label structure
mergeValueLabels <- function(value_labels_old, value_labels) {
  if (is.null(value_labels_old))
    return(value_labels)
  # check if label definitions will be overwritten
  overwritten_names <- lapply(
      intersect(names(value_labels), names(value_labels_old)),
      function(nam) {
        overwritten <- intersect(names(value_labels[[nam]]), names(value_labels_old[[nam]]))
        if (length(overwritten) == 0)
          return(NULL)
        paste0(nam, ":", overwritten)
      }
    ) %>%
    unlist
  if (length(overwritten_names) > 0)
    paste(
        "The following label definitions will be overwritten:",
        paste0(overwritten_names, col = ", ")
      ) %>%
      warning
  # overwrite old label definitions by new ones
  lapply(
      names(value_labels), 
      function(var_name) 
        lapply(
          names(value_labels[[var_name]]), 
          function(x)
            value_labels_old[[var_name]][[x]] <<- value_labels[[var_name]][[x]]
        )
    )
  value_labels_old
}


#' Create a labelling function
#'
#' @param lm A [LabelMachine] object, holding the label themes
#' @param theme (optional) A character string with the name of the required
#' labelling theme. If omitted, then the first labelling theme in the yaml file
#' is used.
#' @return A function that applies the picked theme to variables.
create_labelling_function <- function(lm, theme = NULL) {
  function(df, var, ...)
    label_variable(lm, df, var, theme, ...)
}

label_variable <- function(lm, df, var, theme = default_theme_name, col = var, col_new = col, keep_ordering = FALSE) {
  # --- Check arguments ---
  err_handler <- function(msg)
    paste0(
        "Error while labelling variable '", 
        var,
        "': ",
        msg
      ) %>%
      stop
  if (!is.character(var) || length(var) != 1)
    stop("Error while calling the labelling function: The argument 'var' must be a character string.")
  if (!is.data.frame(df))
    err_handler("The argument 'df' must be a data.frame.")
  if (!is.character(col) || length(col) != 1)
    err_handler("The argument 'col' must be a character string.")
  if (!is.null(theme) && (!is.character(col_new) || length(col_new) != 1))
    err_handler("The argument 'theme' must be a character string.")
  if (!theme %in% names(lm@themes))
    paste0(
        "The label machine 'lm' does not contain any theme with the name '",
        theme,
        "'."
      ) %>%
      err_handler
  thm <- lm@themes[[theme]]
  if (!var %in% names(thm)) 
    paste0(
        "The theme '",
        theme,
        "' does not contain any variable with the name '",
        var,
        "'. Please, check passed name in the argument 'var'."
      ) %>%
      err_handler
  if (!col %in% names(df))
    paste0(
        "The variable name '",
        var,
        "' is not a column name of the data.frame 'df'. Consider using ",
        "the argument 'col' in order to deal with column names differing ",
        "from the variable name defined in the underlying theme."
      ) %>%
      err_handler
  if (!is.character(col_new) || length(col_new) != 1 || !is.syntactic(col_new))
    err_handler("The argument 'col_new' must be a valid column name.")
  # Check that all old labels can be found in the labelling theme
  old_labels <- names(thm[[var]])
  missing_labels <- unique(as.character(df[[col]]))
  missing_labels <- missing_labels[!missing_labels %in% old_labels]
  if (length(missing_labels) > 0)
    paste0(
        "No label information found for the following variable levels: ",
        paste(missing_labels, collapse = ", ")
      ) %>%
      err_handler
  # --- Relabel df[[col]] and save to df[[col_new]] ---
  labelling_map <- data.frame(OLD_ = old_labels, NEW_ = thm[[var]])
  if (is.factor(df[[col]]) && keep_ordering) {
    # if the old order should be kept, then reorder the labelling map
    col_levels <- intersect(as.character(levels(df[[col]])), old_labels)
    labelling_map <- labelling_map[
        match(old_labels, c(col_levels, setdiff(old_labels, col_levels))),
      ]
  }
  # set new labels as factor
  labelling_map$NEW_ = factor(
      labelling_map$NEW_,
      levels = unique(labelling_map$NEW_)
    )
  df$OLD_ <- as.character(df[[col]])
  df <- merge(df, labelling_map, by = "OLD_")
  df$OLD_ <- NULL
  df[[col_new]] <- df$NEW_
  df$NEW_ <- NULL
  df
}



