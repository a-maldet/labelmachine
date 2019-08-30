#' Check if an object has the right length
#'
#' @param obj An object that should be checked if it has the right length
#' @param comp_obj A object that should be used for comparison
#' @param comp_len A numeric vector holding valid length values
#' @param err_handler An error handling function, which should be used
lama_check_length <- function(obj, comp_obj = NULL, comp_len = NULL, err_handler = composerr("Error in 'lama_check_length")) {
  obj_name <- deparse(substitute(obj))
  use_comp <- !is.null(comp_obj)
  use_len <- !is.null(comp_len)
  if (use_comp)
    comp_name <- deparse(substitute(comp_obj))
  if ((!use_comp || length(obj) != length(comp_obj)) && (!use_len || !length(obj) %in% comp_len)) {
    msg <- paste("Object", stringify(obj_name), "must")
    if (use_comp)
      msg <- paste(
        msg,
        "be of the same length as object",
        stringify(comp_name)
      )
    if (use_comp && use_len)
      msg <- paste(msg, "or it must")
    if (use_len) {
      if (length(comp_len) == 1) {
        msg <- paste(msg, "have length", stringify(comp_len))
      } else {
        msg <- paste(msg, "have one of the following lengths:", stringify(comp_len))
      }
    }
    msg <- paste0(msg, ".")
    err_handler(msg)
  }
}

#' Check if an object is a character
#'
#' @param obj An object that should be checked if it has the right length
#' @param len_1 A flag that tells if `obj` should be of length 1
#' @param allow_null A flag that tells if `NULL` is allowed.
#' @param allow_empty A flag that tells if `""` is allowed.
#' @param allow_na A flag that tells if `NA` is allowed.
#' @param err_handler An error handling function, which should be used
lama_check_character <- function(
  obj,
  len_1 = TRUE,
  allow_null = FALSE,
  allow_empty = FALSE,
  allow_na = FALSE,
  err_handler = composerr("Error in 'lama_check_character")
) {
  obj_name <- deparse(substitute(obj))
  msg <- ""
  append_to_msg <- function(x, sep = " ")
    msg <<- paste(msg, x, sep = sep)
 if (
    (!allow_null && is.null(obj)) || 
      (!is.null(obj) && !is.character(obj)) ||
      (!allow_null && length(obj) == 0) ||
      (!is.null(obj) && !allow_na && any(is.na(obj))) ||
      (is.character(obj) && !allow_empty && any(obj[!is.na(obj)] == "")) ||
      (!is.null(obj) && len_1 && length(obj) > 1)
  ) {
    msg <- paste("Object", stringify(obj_name), "must be")
    if (!allow_null && is.null(obj)) {
      append_to_msg("a character, but is 'NULL'.")
      err_handler(msg)
    }
    if (!is.null(obj) && !is.character(obj)) {
      append_to_msg("a character")
      if (allow_null)
        append_to_msg("(or 'NULL')")
      append_to_msg(paste0(", but is of type ", stringify(typeof(obj)), "."), sep = "")
      err_handler(msg)
    }
    if (!allow_null && length(obj) == 0) {
      append_to_msg("a character vector of non-zero length, but has length zero.")
      err_handler(msg)
    }
    if (!is.null(obj) && !allow_na && any(is.na(obj))) {
      append_to_msg("a character without 'NA' values")
      if (allow_null)
        append_to_msg("(or 'NULL')")
      append_to_msg(", but has one or more 'NA' values.", sep = "")
      err_handler(msg)
    }
    if (is.character(obj) && !allow_empty && any(obj[!is.na(obj)] == "")) {
      append_to_msg("a character without empty string values")
      if (allow_null)
        append_to_msg("(or 'NULL')")
      append_to_msg(", but has one or more empty string values.", sep = "")
      err_handler(msg)
    }
    if (!is.null(obj) && len_1 && length(obj) > 1) {
      append_to_msg("a character vector of length '1'")
      if (allow_null)
        append_to_msg("(or 'NULL')")
      append_to_msg(paste0(", but has length ", stringify(length(obj)), "."), sep = "")
      err_handler(msg)
    }
    err_handler(msg)
  }
}

lama_check_in_dictionary <- function(x, dict) {
  x_name <- deparse(substitute(x))
  dict_name <- deparse(substitute(dict))
  wrong_variable <- x[!x %in% names(dict)]
  if (length(wrong_variable) != 0)
    err_handler(paste0(
        "Object ", stringify(x_name), " must be a character vector holding ",
        "translation names: ",
        "The following old translation names could not be ",
        "found in the LabelDictionary object ", dict_name, "': ", 
        stringify(wrong_variable),
        ".\nOnly the following variable translations are available: ",
        stringify(names(dict)),
        "."
      ))
}
