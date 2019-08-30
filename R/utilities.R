# Function that checks if a variable name is syntactically valid
# This function was suggested by Hadley Wickham see [http://r.789695.n4.nabble.com/Syntactically-valid-names-td3636819.html]
is.syntactic <- function(x) x == make.names(x)

#' Transform data structure from yaml format to the [LabelDictionary][new_dictionary()] class input format
#'
#' When a yaml file is read in, the data has the structure
#' vars (named list) > translations (named list)
#' This structure is transformed to the [LabelDictionary][new_dictionary()] class input structure
#' vars (named list) >  translations (named character vector)
yaml_to_dictionary <- function(data) {
  lapply(data, unlist)
}

#' Transform data structure from [LabelDictionary][new_dictionary()] class input format to the yaml format
#'
#' In the [LabelDictionary][new_dictionary()] class object the data has the structure
#' vars (named list) > translations (named character vector)
#' This structure is transformed to the yaml file structure
#' vars (named list) > translations (named list)
dictionary_to_yaml <- function(data) {
  lapply(data, as.list)
}

#' Replace `NA` by `"NA_"`
na_to_escpape <- function(x) {
  if (!is.character(x))
    stop("Error while calling 'na_to_escape': Argument 'x' is not a character.")
  x[is.na(x)] <- NA_lama_
  x
}

#' Replace `"NA_"` by `NA`
escape_to_na <- function(x) {
  x_char <- as.character(x)
  x[!is.na(x_char) & x_char == NA_lama_] <- NA
  x
}


#' Stringify a vector into a single character string (`'x1', 'x2', ...`)
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
