#' Constructor function of the [LabelDictionary] class
#'
#' An \code{S3 class} which holds the variable translations.
#' @param translation_list A named list object holding your 
#' variable translations. Each
#' list item must be a named character vector, which represent the translations
#' replacing the original values of the variables by new labels.
#' The item names of the character vectors correspond to the original variable
#' values which should be relabelled.
#' The values of the character vectors are the new labels that should be used.
#' The order of the 
#' character vectors will be used as the order of the new labels.
#' (If the original variable is of the type \code{factor}, you can also
#' preserve its order by passing \code{keep_order = TRUE} to the
#' \code{translate} function.)
#' @return A [LabelDictionary] class object
#' @rdname LabelDictionary-class
#' @export
new_dictionary <- function(translation_list = NULL) {
  if (is.null(translation_list) || (is.list(translation_list) && length(translation_list) == 0)) {
    translation_list <- list()
  } else {
    # Check the 'translation_list' argument
    err_handler <- composerr(paste(
        "Error while initializing the LabelDictionary class object:",
        "The 'translation_list' argument is invalid"
      ))
    if (!is.list(translation_list) || is.null(names(translation_list)) || any(names(translation_list) == ""))
      err_handler("The 'translation_list' argument must be a named list.")
    lapply(names(translation_list), function(var_name) {
      err_handler <-  composerr_parent(
        paste0("Error in list child element '", var_name, "'"),
        err_handler
      )
      translation <- translation_list[[var_name]]
      if (!is.character(translation) || is.null(names(translation)) || any(names(translation) == ""))
        err_handler("The element must be a named character vector holding the label translation_list.")
    })
  }
  structure(translation_list, class = "LabelDictionary")
}

#' Pick multiple variable translations and create a new [LabelDictionary] object
#'
#' This function picks one or more variable translations from a [LabelDictionary] class object
#' and creates a new [LabelDictionary] class object.
#' @param .data A [LabelDictionary] object, holding the variable translations
#' @param ... Various arguments
#' @rdname select
select <- function(.data, ...) {
  UseMethod("select")
}

#' @param variable A character vector holding the names of the variable translations that
#' should be picked.
#' @return A new [LabelDictionary] class object, holding the picked variable translations.
#' @rdname select
#' @method select LabelDictionary
#' @export
select.LabelDictionary <- function(.data, variable) {
  err_handler <- composerr("Error while calling 'select'")
  if (class(.data) != "LabelDictionary")
    err_handler("The object given in the argument '.data' must be a LabelDictionary class object.")
  if (!is.character(variable) || length(variable) == 0)
    err_handler("The object given in the argument 'variable' must be a character vector.")
  wrong_variable <- variable[!variable %in% names(.data)]
  if (length(wrong_variable) != 0)
    err_handler(paste0(
        "The following names given in the argument 'variable' could not be ",
        "found in the LabelDictionary object: ", 
        paste(wrong_variable, collapse = ", "),
        "\nOnly the following variable names exist: ",
        paste0(names(.data), collapse = c(", "))
      ))
  new_dictionary(.data[variable])
}

#' Rename multiple variable translations in a [LabelDictionary] object
#'
#' This function renamess one or more variable translations inside of a [LabelDictionary] class object.
#' @param .data A [LabelDictionary] object, holding the variable translations
#' @param ... Various arguments
#' @rdname rename
#' @export
rename <- function(.data, ...) {
  UseMethod("rename")
}

#' @param old A character vector holding the names of the variable translations, that should be renamed.
#' @param new A character vector holding the new names of the variable translations.
#' @return The updated [LabelDictionary] class object.
#' @rdname rename 
#' @method rename LabelDictionary
#' @export
rename.LabelDictionary <- function(.data, old, new) {
  err_handler <- composerr("Error while calling 'rename'")
  if (class(.data) != "LabelDictionary")
    err_handler("The object given in the argument '.data' must be a LabelDictionary class object.")
  if (!is.character(old) || length(old) == 0)
    err_handler("The object given in the argument 'old' must be a character vector.")
  if (!is.character(new) || length(new) != length(old))
    err_handler("The object given in the argument 'new' must be a character vector of the same length as the one given in argument 'old'.")
  wrong_variable <- old[!old %in% names(.data)]
  if (length(wrong_variable) != 0)
    err_handler(paste0(
        "The following names given in the argument 'old' could not be ",
        "found in the LabelDictionary object: ", 
        paste(wrong_variable, collapse = ", "),
        "\nOnly the following variable translations exist: ",
        paste0(names(.data), collapse = c(", "))
      ))
  names(.data)[match(old, names(.data))] <- new
  .data
}

#' Change a variable translation or append a variable translation to an existing [LabelDictionary] object
#'
#' This function alters a [LabelDictionary] object. It can be used for altering
#' or appending a translation to a [LabelDictionary] object.
#' @param .data A [LabelDictionary] object
#' @param ... Various arguments
#' @return An updated [LabelDictionary] class object.
#' @rdname mutate
#' @export
mutate <- function(.data, ...) {
  UseMethod("mutate")
}

#' @param variable The name of the variable translation that should be altered.
#' If can also be variable translation name that does not exist yet.
#' @param translation A named character vector holding the new variable
#' translation that should be assigned to the name given in argument \code{variable}.
#' The names of the character vector \code{translation} correspond to the
#' original variable values that should be replaced by the new labels.
#' The values in the character vector \code{translations} are the labels
#' that should be assigned to the original values.
#' @rdname mutate
#' @method mutate LabelDictionary
#' @export
mutate.LabelDictionary <- function(.data, variable, translation) {
  err_handler <- composerr("Error while calling 'mutate'")
  if (class(.data) != "LabelDictionary")
    err_handler("The object given in the argument '.data' must be a LabelDictionary class object.")
  if (!is.character(variable) || length(variable) != 1)
    err_handler("The object given in the argument 'variable' must be a character string.")
  if (
    (!is.list(translation) && !is.character(translation)) || 
    is.null(names(translation)) || length(translation) == 0 ||
    (is.list(translation) && (!all(sapply(translation, is.character))) || (!all(sapply(translation, length) == 1)))
  )
    err_handler("The object given in the argument 'translation' must either be a named character vector or a named list holding character strings.")
  if (is.list(translation))
    translation <- unlist(translation)
  .data[[variable]] <- translation
  .data
}

#' Print a [LabelDictionary] class object
#'
#' @param x The [LabelDictionary] class object that should be printed.
#' @rdname print
#' @method print LabelDictionary
#' @export
print.LabelDictionary <- function(x) {
  cat("\n--- LabelDictionary ---\n")
  for (name in names(x)) {
    cat(paste0("Variable '", name, "':\n"))
    print(x[[name]])
    cat("\n")
  }
}
