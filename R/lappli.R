#' Improve `lapply` and `sapply` with index
#'
#' Improve [base::lapply()] and [base::sapply()] functions by allowing
#' an extra index argument `.I` to be passed into the function given in `FUN`.
#' If the function given in `FUN` has an argument `.I` then, for each entry
#' of `X` passed into `FUN` the corresponding index is passed into
#' argument `.I`. If the function given in `FUN` has no argument `.I`, 
#' then `lapplI` and  `sapplI` are exactly the same as
#' [base::lapply()] and [base::sapply()].
#' Besides this extra feature, there is no difference to [base::lapply()] and
#' [base::sapply()].
#' @param FUN Here comes the great difference to [base::lapply()] and
#'   [base::sapply()]. When using `lapplI` and `sapplI`, the function
#'   passed into `FUN` may also have an extra argument `.I`. If it does, then
#'   for each item of `X` the current item index
#'   is passed into argument `.I` of `FUN`.
#'   Besides this extra feature, there is no difference to [base::lapply()] and
#'   [base::sapply()].
#' @inheritParams base::lapply
#' @rdname lapplI
#' @export
lapplI <- function(X, FUN, ...) {
  if (!is.function(FUN)) 
    stop("Unexpected argument in 'lapplI'. Argument 'FUN' must be a function.")
  use_i <- ".I" %in% rlang::fn_fmls_names(FUN)
  if (use_i) {
    if (!typeof(X) %in% c("logical", "integer", "double", "complex", "character")) {
      X <- as.list(X)
      get_element <- function(i) X[[i]]
    } else {
      get_element <- function(i) X[i]
    }
    Y <- seq_len(length(X))
    Y <- lapply(
      Y,
      function(i, ...) FUN(get_element(i), ..., .I = i),
      ...
    )
    if (!is.null(names(X)))
      names(Y) <- names(X)
    Y
  } else {
    lapply(X, FUN, ...)
  }
}

#' @inheritParams base::sapply
#' @rdname lapplI
#' @export
sapplI <- function(X, FUN, ..., simplify = TRUE, USE.NAMES = TRUE) {
  if (!is.function(FUN)) 
    stop("Unexpected argument in 'lapplI'. Argument 'FUN' must be a function.")
  if (!is.logical(USE.NAMES)) 
    stop("Unexpected argument in 'lapplI'. Argument 'USE.NAMES' must be a 'TRUE' or 'FALSE'")
  use_i <- ".I" %in% rlang::fn_fmls_names(FUN)
  if (use_i) {
    if (!typeof(X) %in% c("logical", "integer", "double", "complex", "character")) {
      X <- as.list(X)
      get_element <- function(i) X[[i]]
    } else {
      get_element <- function(i) X[i]
    }
    Y <- seq_len(length(X))
    Y <- sapply(
      Y,
      function(i, ...) FUN(get_element(i), ..., .I = i),
      ...,
      simplify = simplify,
      USE.NAMES = USE.NAMES
    )
    if (!is.null(names(X))) {
      names(Y) <- names(X)
    } else if (USE.NAMES & is.character(X)) {
      names(Y) <- X
    }
    Y
  } else {
    sapply(X, FUN, ..., simplify = simplify, USE.NAMES = USE.NAMES)
  }
}
