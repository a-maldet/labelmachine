#' Change or append a variable translation to an existing [lama_dictionary][new_lama_dictionary()] object
#'
#' The functions [lama_mutate()] and [lama_mutate_()] alter a 
#' [lama_dictionary][new_lama_dictionary()] object. They can be used to alter,
#' delete or append a translations to a
#' [lama_dictionary][new_lama_dictionary()] object.
#' The function [lama_mutate()] uses named arguments to assign the translations
#' to the new names (similar to \code{dplyr::mutate}), whereas the function
#' [lama_mutate_()] is takes a character string \code{key} holding the
#' name to which the translation should be assigned and a named character
#' vector \code{translation} holding the actual translation mapping.
#' @param .data A [lama_dictionary][new_lama_dictionary()] object
#' @param ... One or more unquoted expressions separated by commas. Use named
#'   arguments, e.g. `new_transation_name = c(a = "A", b = "B")`, to set
#'   translations (named character vectors) to new translation names.
#'   If you want to delete an existing translation assign the value `NULL`
#'   (e.g. `old_translation = NULL`). It is also
#'   possible use complex expressions as long as the resulting object is a valid
#'   translation object (named character vector).
#'   Furthermore, it is possible to use translation names that are already
#'   existing in the dictionary, in order to modify them
#'   (e.g. `new_translation = c(v = "V", w = "W", old_translation, z = "Z")`, where 
#'   `old_translation = c(x = "X", y = "Y")`).
#' @return An updated [lama_dictionary][new_lama_dictionary()] class object.
#' @seealso [lama_translate()], [lama_translate_all()], [new_lama_dictionary()],
#'   [as.lama_dictionary()], [lama_rename()], [lama_select()],
#'   [lama_merge()], [lama_read()], [lama_write()]
#' @rdname lama_mutate
#' @export
lama_mutate <- function(.data, ...) {
  UseMethod("lama_mutate")
}

#' @rdname lama_mutate
#' @examples
#'   # initialize lama_dictinoary
#'   dict <- new_lama_dictionary(
#'     subject = c(en = "English", ma = "Mathematics"),
#'     result = c("1" = "Very good", "2" = "Good", "3" = "Not so good")
#'   )
#'  
#'   ## Example-1: mutate and append with 'lama_mutate'
#'   # add a few subjects and a few grades
#'   dict_new <- lama_mutate(
#'     dict, 
#'     subject = c(bio = "Biology", subject, sp = "Sports"),
#'     result = c("0" = "Beyond expectations", result, "4" = "Failed", NA_ = "Missed")
#'   )
#'   # the subjects "Biology" and "Sports" were added
#'   # and the results "Beyond expectations", "Failed" and "Missed"
#'   dict_new
#'
#'   ## Example-2: delete with 'lama_mutate'
#'   dict_new <- lama_mutate(
#'     dict, 
#'     subject = NULL
#'   )
#'   dict_new
#'
#' @export
lama_mutate.lama_dictionary <- function(.data, ...) {
  args <- rlang::quos(...)
  err_handler <- composerr("Error while calling 'lama_mutate'")
  if (!is.dictionary(.data))
    err_handler("The object given in the argument '.data' must be a lama_dictionary class object.")
  if (length(args) == 0)
    err_handler(paste(
      "Translation assignments are missing.",
      "Use named arguments, e.g. 'new_name = X', where 'X' is a",
      "named character vector and 'new_name' is the name to which",
      "the translation should be assigned."
    ))
  key <- names(args)
  if (is.null(key) || any(key == ""))
    err_handler(paste(
      "Translation assignments are invalid.",
      "Use named arguments, e.g. 'new_name = X', where 'X' is a",
      "named character vector and 'new_name' is the name to which",
      "the translation should be assigned."
    ))
  duplicates <- table(key)
  duplicates <- names(duplicates[duplicates > 1])
  if (length(duplicates) > 0)
    err_handler(paste0(
      "The following translation names are used more than once: ",
      stringify(duplicates),
      "."
    ))
  lapply(seq_len(length(args)), function(i) {
    err_handler <- composerr_parent(
      paste(
        "Invalid argument at position",
        stringify(i + 1)
      ),
      err_handler
    )
    obj <- args[[i]]
    err_handler_parse <- composerr(
      paste(
        "The expression",
        stringify(paste(
          c(key[i], rlang::quo_name(obj)),
          collapse = " = "
        )),
        "could not be parsed."
      ),
      err_handler
    )
    translation <- tryCatch(
      rlang::eval_tidy(
        rlang::quo_get_expr(obj),
        data = .data
      ),
      error = function(e) err_handler_parse(e),
      warning = function(w) err_handler_parse(w)
    )
    if (is.list(translation))
      translation <- unlist(translation)
    if (!is.null(translation))
      translation <- validate_translation(translation, err_handler)
    .data[[key[i]]] <<- translation
  })
  .data
}

#' @param key The name of the variable translation that should be altered.
#'   It can also be variable translation name that does not exist yet.
#' @param translation A named character vector holding the new variable
#'   translation that should be assigned to the name given in argument \code{key}.
#'   The names of the character vector \code{translation} correspond to the
#'   original variable values that should be replaced by the new labels.
#'   The values in the character vector \code{translations} are the labels
#'   that should be assigned to the original values.
#' @rdname lama_mutate
#' @export
lama_mutate_ <- function(.data, key, translation) {
  UseMethod("lama_mutate_")
}

#' @rdname lama_mutate
#' @examples
#'   ## Example-3: Alter and append with 'lama_mutate_'
#'   # generate the new translation (character string)
#'   subj <- c(
#'     bio = "Biology",
#'     lama_get(dict, subject),
#'     sp = "Sports"
#'   )
#'   # save the translation under the name "subject"
#'   dict_new <- lama_mutate_(
#'     dict,
#'     key = "subject",
#'     translation = subj
#'   )
#'   # the translation "subject" now also contains
#'   # the subjects "Biology" and "Sports"
#'   dict_new
#'
#'   ## Example-4: Delete with 'lama_mutate_'
#'   # save the translation under the name "subject"
#'   dict_new <- lama_mutate_(
#'     dict,
#'     key = "subject",
#'     translation = NULL
#'   )
#'   # the translation "subject" was deleted
#'   dict_new
#'   
#' @export
lama_mutate_.lama_dictionary <- function(.data, key, translation) {
  err_handler <- composerr("Error while calling 'lama_mutate_'")
  if (!is.dictionary(.data))
    err_handler("The object given in the argument '.data' must be a lama_dictionary class object.")
  if (!is.character(key) || length(key) != 1)
    err_handler("The object given in the argument 'key' must be a character string.")
  if (!is.syntactic(key))
    err_handler(paste0(
      "The following translation name given in argument 'key' is not a valid ",
      "object name: ",
      stringify(key),
      "."
    ))
  if (is.list(translation))
    translation <- unlist(translation)
  if (!is.null(translation))
    translation <- validate_translation(
      translation,
      composerr(
        "The object given in the argument 'translation' is invalid",
        err_handler
      )
    )
  # transform 'translation' into a mapping (named character vector)
  .data[[key]] <- translation
  .data
}
