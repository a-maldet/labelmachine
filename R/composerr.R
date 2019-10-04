#' Compose error handlers (concatenate error messages)
#'
#' The functions [composerr()], [composerr_()] and [composerr_parent()]
#' modify error handlers by
#' appending character strings to the error messages of the error handling
#' functions:
#' * [composerr()] uses non-standard evaluation.
#' * [composerr_()] is the standard evaluation alternative of [composerr()].
#' * [composerr_parent()] is a wrapper of [composerr()], defining the parent 
#'   environment as the lookup environment of the `err_handler`.
#'   This function looks up the prior error handling function in the parent
#'   environment of the current environment and allows you to store
#'   the modified error handling function under the same name as the
#'   error handling function from the parent environment without running into
#'   recursion issues.
#'   This is especially useful when doing error handling
#'   in nested environments (e.g. checking nested list objects) and you don not
#'   want to use different names for the  error handling functions in the
#'   nested levels.
#'   If you don't have a nested environment situation, better use
#'   [composerr()] or [composerr_()].
#'
#' @param text_1 A character string, which will be appended
#'   at the beginning of the error message. The argument `sep_1` will be used
#'   as text separator.
#' @param err_prior There are three valid types:
#'   * `err_prior` is omitted: A new error handling message will be returned.
#'   * `composerr_` is the calling function: `err_prio` must be a character
#'     string holding the name of the error handling function
#'     to which the message part should be appended.
#'   * `composerr` is the calling function: `err_prio` must be the error
#'     handling function
#'     to which the message part should be appended.
#' @param text_2 A character string, which will be appended
#'   at the end of the error message. The argument `sep_2` will be used
#'   as text separator.
#' @param env_prior An environment where the error handling function given in
#'   \code{err_prior} can be found. If no environment is given, then
#'   the \code{err_prior} will be looked up in the current environment.
#'   In the situation of nested scopes, you may change the lookup environment
#'   to the parent environment in order to be able to recursively override
#'   the name of the error handling function. In order to keep it simple,
#'   the function [composerr_parent()] can be used instead. 
#' @param sep_1 A character string that is used as separator for the
#'   concatenation of `text_1` at the beginning of the error message.
#' @param sep_2 A character string that is used as separator for the
#'   concatenation of `text_2` at the end of the error message.
#' @return A new error handling function that has an extended error message.
#' @rdname composerr
#' @examples 
#' \dontrun{
#' # ------     composerr_ in flat situation      ----------
#' # -- create a modified error handler in the same scope --
#' # check if variable 'obj' exists and holds value TRUE
#' obj <- FALSE
#' # original error handler
#' err_h <- composerr_("Something is wrong with obj")
#' if (!exists("obj"))
#'   err_h("obj does not exist")
#' # create more precise error handler (same scope)
#' err_h2 <- composerr_("obj has wrong value", "err_h")
#' if (!obj)
#'   err_h2("Value is FALSE")
#' #--- resulting error ---
#' # "Something is wrong with obj: obj has wrong value: Value is FALSE"}
composerr_ <- function(
  text_1 = NULL,
  err_prior = NULL,
  text_2 = NULL,
  sep_1 = ": ",
  sep_2 = ": ",
  env_prior = parent.frame()
) {
  if (!is.null(text_1)) {
    if (!is.character(text_1))
      stop("Error while calling 'composerr_': argument 'text_1' must be a character string.", call. = FALSE)
    if (!is.character(sep_1) || length(sep_1) != 1)
      stop("Error while calling 'composerr_': argument 'sep_1' must be a character string.", call. = FALSE)
  }
  if (!is.null(text_2)) {
    if (!is.character(text_2))
      stop("Error while calling 'composerr_': argument 'text_2' must be a character string.", call. = FALSE)
    if (!is.character(sep_2) || length(sep_2) != 1)
      stop("Error while calling 'composerr_': argument 'sep_2' must be a character string.", call. = FALSE)
  }
  append_to_msg <- function(msg = NULL)
    paste(
      c(
        paste(c(text_1, msg), collapse = sep_1),
        text_2
      ),
      collapse = sep_2
    )
  # If no parent error handler is given, then setup a new error handler from scratch
  if (is.null(err_prior))
    return(
      function(msg = NULL) 
        stop(append_to_msg(msg), call. = FALSE)
    )
  if (!is.character(err_prior) || length(err_prior) != 1)
    stop("Error while calling 'composerr_': argument 'err_prior' must be a character string.", call. = FALSE)
  if (!is.environment(env_prior))
    stop("Error while calling 'composerr_': argument 'env_prior' must be an environment.", call. = FALSE)
  # If a parent error handler is given, then compose the error messages together
  err_handler_composerr <- function(msg)
    stop(
      paste0(
        "Error while calling 'composerr_': The error handler '",
        err_prior,
        "' could not be found in the lookup environment, consider using argument 'env_prior': ",
        msg
      ),
      call. = FALSE
    )
  tryCatch({
      err_handler <- get(err_prior, env_prior)
    },
    error = function(e) err_handler_composerr(e)
  )
  if (!is.function(err_handler))
    err_handler_composerr("The found object is not a function.")
  function(msg = NULL) 
    err_handler(append_to_msg(msg))
}

#' @rdname composerr
#' @export
#' @examples 
#' \dontrun{
#' # ------      composerr in flat situation      ----------
#' # -- create a modified error handler in the same scope --
#' # check if variable 'obj' exists and holds value TRUE
#' obj <- FALSE
#' # original error handler
#' err_h <- composerr("Something is wrong with obj")
#' if (!exists("obj"))
#'   err_h("obj does not exist")
#' # create more precise error handler (same scope)
#' err_h2 <- composerr("obj has wrong value", err_h)
#' if (!obj)
#'   err_h2("Value is FALSE")
#' #--- resulting error ---
#' # "Something is wrong with obj: obj has wrong value: Value is FALSE"}
composerr <- function(
  text_1 = NULL,
  err_prior = NULL,
  text_2 = NULL,
  sep_1 = ": ",
  sep_2 = ": ",
  env_prior = parent.frame()
) {
  if (!is.null(text_1)) {
    if (!is.character(text_1))
      stop("Error while calling 'composerr': argument 'text_1' must be a character string.", call. = FALSE)
    if (!is.character(sep_1) || length(sep_1) != 1)
      stop("Error while calling 'composerr': argument 'sep_1' must be a character string.", call. = FALSE)
  }
  if (!is.null(text_2)) {
    if (!is.character(text_2))
      stop("Error while calling 'composerr': argument 'text_2' must be a character string.", call. = FALSE)
    if (!is.character(sep_2) || length(sep_2) != 1)
      stop("Error while calling 'composerr': argument 'sep_2' must be a character string.", call. = FALSE)
  }
  # If no parent error handler is given, then setup a new error handler from scratch
  if (missing(err_prior)) {
    err_prior <- NULL
  } else {
    err_prior <- deparse(substitute(err_prior))
  }
  composerr_(text_1, err_prior, text_2, sep_1, sep_2, env_prior)
}

#' @rdname composerr
#' @export
#' @examples
#' \dontrun{
#' # ------  composerr_parent in nested situation   --------
#' # -- overwrite error handler in the deeper level scope --
#' # check if all entries of the list object 'obj' are TRUE
#' obj <- list(x = TRUE, y = TRUE, z = FALSE)
#' # original error handler
#' err_h <- composerr("obj is invalid")
#' # check each list element 
#' sapply(names(obj), function(name) {
#'   # modify error handler to nested sitation
#'   err_h <- composerr_parent(paste("Error in", name), err_h)
#'   # check element and throw error FALSE
#'   if (!obj[[name]])
#'     err_h("Value is FALSE")
#' })
#' #--- resulting error ---
#' # "obj is invalid: Error in z: Value is FALSE"}
composerr_parent <- function(
  text_1 = NULL,
  err_prior = NULL,
  text_2 = NULL,
  sep_1 = ": ",
  sep_2 = ": ",
  env_prior = parent.frame()
) {
  if (!is.null(text_1)) {
    if (!is.character(text_1))
      stop("Error while calling 'composerr_parent': argument 'text_1' must be a character string.", call. = FALSE)
    if (!is.character(sep_1) || length(sep_1) != 1)
      stop("Error while calling 'composerr_parent': argument 'sep_1' must be a character string.", call. = FALSE)
  }
  if (!is.null(text_2)) {
    if (!is.character(text_2))
      stop("Error while calling 'composerr_parent': argument 'text_2' must be a character string.", call. = FALSE)
    if (!is.character(sep_2) || length(sep_2) != 1)
      stop("Error while calling 'composerr_parent': argument 'sep_2' must be a character string.", call. = FALSE)
  }
  if (missing(err_prior)) {
    err_prior <- NULL
  } else {
    err_prior <- deparse(substitute(err_prior))
  }
  composerr_(text_1, err_prior, text_2, sep_1, sep_2, env_prior)
}
