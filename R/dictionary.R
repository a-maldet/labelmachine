#' Constructor function of the LabelDictionary class
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
#' @return A LabelDictionary class object
#' @seealso [is.dictionary()], [translate()], [read_directory()], [write_directory()], [select()], [rename()], [mutate()], [merge()]
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

#' @rdname LabelDictionary-class
#' @export
as.dictionary <- function(translation_list = NULL) {
  new_dictionary(translation_list)
}

#' Check if an object is a [LabelDictionary][new_dictionary()] class object
#'
#' @param x The object in question
#' @return \code{TRUE} if the object is a [LabelDictionary][new_dictionary()] class object, \code{FALSE} otherwise.
#' @rdname is_dictionary
#' @seealso [as.dictionary()], [new_dictionary()], [translate()], [read_directory()], [write_directory()], [select()], [rename()], [mutate()], [merge()]
#' @export
is.dictionary <- function(x) {
  inherits(x, "LabelDictionary")
}

#' Print a [LabelDictionary][new_dictionary()] class object
#'
#' @param x The [LabelDictionary][new_dictionary()] class object that should be printed.
#' @rdname print
#' @seealso [translate()], [new_dictionary()], [select()], [rename()], [mutate()], [merge()], [read_directory()], [write_directory()]
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
