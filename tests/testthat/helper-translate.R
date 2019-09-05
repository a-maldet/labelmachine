#' Check that a translated data.frame has the right column names
#'
#' @param object A data.frame
#' @param cols A character vector holding the expected column names 
expect_column_names <- function(object, cols) {
  # 1. Capture object and label
  act <- quasi_label(rlang::enquo(object), arg = "object")
  # 2. Call expect()
  expect_s3_class(object, "data.frame")
  missing <- !cols %in% names(act$val)
  expect(
    all(!missing),
    sprintf(
      "The data.frame '%s' does not contain the following columns: %s.",
      act$lab, 
      stringify(cols[missing])
    )
  )
  too_much <- !names(act$val) %in% cols
  expect(
    all(!too_much),
    sprintf(
      "The data.frame '%s' contains too many columns: %s.",
      act$lab,
      stringify(names(act$val)[too_much])
    )
  )
  # 3. Invisibly return the value
  invisible(act$val)
}

#' Check that a factor variable has the right levels
#'
#' @param object A factor variable
#' @param vals A character vector holding the expected factor levels
expect_factor_levels <- function(object, vals) {
  # 1. Capture object and label
  act <- quasi_label(rlang::enquo(object), arg = "object")
  # 2. Call expect()
  expect(
    is.factor(act$val),
    sprintf("The passed in variable '%s' is not a factor variable.", act$lab)
  )
  expect_identical(
    levels(act$val),
    vals,
    sprintf("The passed in variable '%s' has the wrong factor levels.", act$lab)
  )
  # 3. Invisibly return the value
  invisible(act$val)
}
