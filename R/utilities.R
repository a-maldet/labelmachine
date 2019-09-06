#' Check if a variable name is syntactically valid
#'
#' This function was suggested by Hadley Wickham in a forum
#' @references \url{http://r.789695.n4.nabble.com/Syntactically-valid-names-td3636819.html}
#' @param x A character string that should be checked, if it conatains a valid object name.
#' @return `TRUE` if valid, `FALSE` else.
is.syntactic <- function(x) x == make.names(x)

#' Transform data structure from yaml format to the [lama_dictionary][new_lama_dictionary()] class input format
#'
#' When a yaml file is read in, the data has the structure
#' vars (named list) > translations (named list)
#' This structure is transformed to the [lama_dictionary][new_lama_dictionary()] class input structure
#' vars (named list) >  translations (named character vector)
#' @param data An object similar to a pre-dictionary object, but each translation is not a named character vector, but a named list holding character strings.
#' @return A pre-dictionary object.
yaml_to_dictionary <- function(data) {
  lapply(data, unlist)
}

#' Transform data structure from [lama_dictionary][new_lama_dictionary()] class input format to the yaml format
#'
#' In the [lama_dictionary][new_lama_dictionary()] class object the data has the structure
#' vars (named list) > translations (named character vector)
#' This structure is transformed to the yaml file structure
#' vars (named list) > translations (named list)
#' @param data A pre-dictionary object.
#' @return An object similar to a pre-dictionary object, but each translation is not a named character vector, but a named list holding character strings.
dictionary_to_yaml <- function(data) {
  lapply(data, as.list)
}

#' Check if a character vector contains NA replacement strings
#'
#' @param x A character vector that should be checked.
#' @return `TRUE` if the vector contains NA replacement strings. `FALSE` else.
contains_na_escape <- function(x) {
  if (!is.character(x))
    stop("Error while calling 'na_to_escape': Argument 'x' is not a character.")
  any(x[!is.na(x)] == NA_lama_)
}

#' Replace `NA` by `"NA_"`
#'
#' @param x A character vector that should be modified.
#' @return A character vector, where the `NA`s are replaced.
na_to_escape <- function(x) {
  if (!is.character(x))
    stop("Error while calling 'na_to_escape': Argument 'x' is not a character.")
  x[is.na(x)] <- NA_lama_
  x
}

#' Replace `"NA_"` by `NA`
#'
#' @param x A character vector that should be modified.
#' @return A character vector, where the NA replacement strings are replaced by `NA`s.
escape_to_na <- function(x) {
  x_char <- as.character(x)
  x[!is.na(x_char) & x_char == NA_lama_] <- NA
  x
}


#' Coerce a vector into a character string (`'x1', 'x2', ...`)
#'
#' @param x A vector that should be coerced. 
#' @return A character string holding the collapsed vector.
stringify <- function(x) {
  paste0(paste0("'", x, "'"), collapse = ", ")
}

#' Create a named list
#'
#' @param .names A character vector holding the names of the list
#' @param obj A vector or list object of the same length
#' @return A named list
#' @include lappli.R
named_list <- function(.names, obj) {
  err_handler <- composerr("Error while calling 'named_list'")
  if (!is.character(.names) || any(is.na(.names)))
    err_handler("Argument '.names' must be a character vector holding the list entry names.")
  if (length(.names) != length(obj))
    err_handler("The object given in argument 'obj' must be of the same length as '.names'.")
  mapply(function(x, y) y, .names, obj, SIMPLIFY = FALSE, USE.NAMES = TRUE)
}

#' Create a named list with lapply from a character vector
#'
#' @param .names A character vector holding the names of the list
#' @inheritParams lapplI
#' @return A named list
named_lapply <- function(.names, FUN, ...) {
  err_handler <- composerr("Error while calling 'named_lapply'")
  if (!is.character(.names) || any(is.na(.names)))
    err_handler("Argument '.names' must be a character vector holding the list entry names.")
  if (!is.function(FUN))
    err_handler("Argument 'FUN' must be a function.")
  Y <- lapplI(
    .names,
    FUN,
    ...
  )
  names(Y) <- .names
  Y
}
