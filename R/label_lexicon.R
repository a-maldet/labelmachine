#' Constructor function of the [LabelLexicon] class
#'
#' An \code{S3 class} which holds the label translations of various variables.
#' @param translations A named list object holding your label translations. Each
#' list item must be a named character vector, which represent the translations replacing
#' the old labels of the variables by new labels.
#' The item names of the character vectors correspond to the old variable
#' labels which should be relabed and
#' the values are the new labels that should be used. The ordering of the 
#' character vectors will be used as the ordering of the new labels.
#' @rdname LabelLexicon-class
#' @export
new_label_lexicon <- function(translations = NULL) {
  if (is.null(translations) || (is.list(translations) && length(translations) == 0)) {
    translations <- list()
  } else {
    # Check the 'translations' argument
    err_handler <- composerr(paste(
        "Error while initializing the LabelLexicon class object:",
        "The 'translations' argument is invalid"
      ))
    if (!is.list(translations) || is.null(names(translations)) || any(names(translations) == ""))
      err_handler("The 'translations' argument must be a named list.")
    lapply(names(translations), function(translation_name) {
      err_handler <-  composerr_parent(
        paste0("Error in list child element '", translation_name, "'"),
        err_handler
      )
      translation <- translations[[translation_name]]
      if (!is.character(translation) || is.null(names(translation)) || any(names(translation) == ""))
        err_handler("The element must be a named character vector holding the label translations.")
    })
  }
  structure(translations, class = "LabelLexicon")
}

# #' Return a specific label translation from a [LabelLexicon] object
# #'
# #' @param x A [LabelLexicon] object, holding the label translations
# #' @param translation A character string holding the name of the translation that
# #' should be returned.
# #' @return A named character vector holding the new label translation
# #' information. The names of the character vector correspond to the old variabel levels that should be replaced.
# #' The values in the character values are the new labels that should be assigned.
# #' @rdname LabelLexicon-class
# #' @export
# `[[.LabelLexicon` <- function(x, translation) {
#   err_handler <- composerr("Error while calling 'get_translations'")
#   if (class(x) != "LabelLexicon")
#     err_handler("The object given in the argument 'x' must be a LabelLexicon class object.")
#   if (!is.character(translation) || length(translation) != 1)
#     err_handler("The object given in the argument 'translation' must be a character string.")
#   if (!translation %in% names(x)) 
#     err_handler(paste0(
#         "Error in argument 'translation': The translation '",
#         translation,
#         "' could not be found in the LabelLexicon object given in argument 'x': ",
#         "Use one of the following translation names: ",
#         paste0(names(x), collapse = c(", "))
#       ))
#   x[[translation]]
# }

#' Pick multiple translations and create a new [LabelLexicon] object
#'
#' This function picks one or more translations from a [LabelLexicon] class object
#' and creates a new [LabelLexicon] class object.
#' @param .data A [LabelLexicon] object, holding the label translations
#' @param ... Various arguments
#' @rdname LabelLexicon-class
#' @return A new [LabelLexicon] class object, holding the picked label translations.
select <- function(.data, ...) {
  UseMethod("select")
}

#' @param .data A [LabelLexicon] object, holding the label translations
#' @param translation A character vector holding the names of the translations that
#' should be picked.
#' @return A new [LabelLexicon] class object, holding the picked label translations.
#' @rdname LabelLexicon-class
#' @export
select.LabelLexicon <- function(.data, translation) {
  err_handler <- composerr("Error while calling 'select'")
  if (class(.data) != "LabelLexicon")
    err_handler("The object given in the argument '.data' must be a LabelLexicon class object.")
  if (!is.character(translation) || length(translation) == 0)
    err_handler("The object given in the argument 'translation' must be a character vector.")
  wrong_translation <- translation[!translation %in% names(.data)]
  if (length(wrong_translation) != 0)
    err_handler(paste0(
        "The following names given in the argument 'translation' could not be ",
        "found in the LabelLexicon object: ", 
        paste(wrong_translation, collapse = ", "),
        "\nOnly the following translation names exist: ",
        paste0(names(.data), collapse = c(", "))
      ))
  new_label_lexicon(.data[translation])
}

#' Rename multiple translations in a [LabelLexicon] object
#'
#' This function renamess one or more translations inside of a [LabelLexicon] class object.
#' @param .data A [LabelLexicon] object, holding the label translations
#' @param ... Various arguments old A character vector holding the names of the translations that
#' should be picked.
#' @return A new [LabelLexicon] class object, holding the picked label translations.
#' @rdname LabelLexicon-class
#' @export
rename <- function(.data, ...) {
  UseMethod("rename")
}

#' Rename multiple translations in a [LabelLexicon] object
#'
#' This function renamess one or more translations inside of a [LabelLexicon] class object.
#' @param .data A [LabelLexicon] object, holding the label translations
#' @param old A character vector holding the names of the translations, that should be renamed.
#' @param new A character vector holding the new names of the translations.
#' @return The updated [LabelLexicon] class object.
#' @rdname LabelLexicon-class
#' @export
rename.LabelLexicon <- function(.data, old, new) {
  err_handler <- composerr("Error while calling 'rename'")
  if (class(.data) != "LabelLexicon")
    err_handler("The object given in the argument '.data' must be a LabelLexicon class object.")
  if (!is.character(old) || length(old) == 0)
    err_handler("The object given in the argument 'old' must be a character vector.")
  if (!is.character(new) || length(new) != length(old))
    err_handler("The object given in the argument 'new' must be a character vector of the same length as the one given in argument 'old'.")
  wrong_translations <- old[!old %in% names(.data)]
  if (length(wrong_translations) != 0)
    err_handler(paste0(
        "The following names given in the argument 'old' could not be ",
        "found in the LabelLexicon object: ", 
        paste(wrong_translations, collapse = ", "),
        "\nOnly the following translation names exist: ",
        paste0(names(.data), collapse = c(", "))
      ))
  names(.data)[match(old, names(.data))] <- new
  .data
}

#' Change a translation or append a translation to an existing [LabelLexicon] object
#'
#' This function alters a [LabelLexicon] object. It can be used for altering
#' or appending a translation to a [LabelLexicon] object.
#' @param .data A [LabelLexicon] object
#' @param ... Various arguments old A character vector holding the names of the translations that
#' @return An updated [LabelLexicon] class object.
#' @rdname LabelLexicon-class
#' @export
mutate <- function(.data, ...) {
  UseMethod("mutate")
}

#' Change a translation or append a translation to an existing [LabelLexicon] object
#'
#' This function alters a [LabelLexicon] object. It can be used for altering
#' or appending a translation to a [LabelLexicon] object.
#' @param .data A [LabelLexicon] object
#' @param translation The name of the translation that should be altered.
#' It can also be translation name that does not exist yet.
#' @param value A named character vector holding the new label translation
#' information. The names of the character vector correspond to the old variabel levels that should be replaced.
#' The values in the character values are the new labels that should be assigned.
#' @return An updated [LabelLexicon] class object.
#' @rdname LabelLexicon-class
#' @export
mutate.LabelLexicon <- function(.data, translation, value) {
  err_handler <- composerr("Error while calling 'mutate'")
  if (class(.data) != "LabelLexicon")
    err_handler("The object given in the argument '.data' must be a LabelLexicon class object.")
  if (!is.character(translation) || length(translation) != 1)
    err_handler("The object given in the argument 'translation' must be a character string.")
  if (
    (!is.list(value) && !is.character(value)) || 
    is.null(names(value)) || length(value) == 0 ||
    (is.list(value) && (!all(sapply(value, is.character))) || (!all(sapply(value, length) == 1)))
  )
    err_handler("The object given in the argument 'value' must either be a named character vector or a named list holding character strings.")
  if (is.list(value))
    value <- unlist(value)
  .data[[translation]] <- value
  .data
}

#' Print a [LabelLexicon] class object
#'
#' @param x The [LabelLexicon] class object that should be printed.
#' @rdname LabelLexicon-class
#' @export
print.LabelLexicon <- function(x) {
  cat("\n--- LabelLexicon ---\n")
  for (name in names(x)) {
    cat(paste0("Variable '", name, "':\n"))
    print(x[[name]])
    cat("\n")
  }
}
