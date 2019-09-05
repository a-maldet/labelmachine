library(magrittr)
library(labelmachine)

#' Check that an object is a [LamaDictionary][new_dictionary()] class object
#'
#' @param object The object that should be checked
expect_dictionary <- function(object) {
  # 1. Capture object and label
  act <- quasi_label(rlang::enquo(object), arg = "object")
  # 2. Call expect()
  expect(
    inherits(act$val, "LamaDictionary"),
    sprintf("The object '%s' is not a 'LamaDictionary'.", act$lab)
  )
  # 3. Invisibly return the value
  invisible(act$val)
}

#' Check that a dictionary contains the right translations
#'
#' Check that the translation names of the dictionary are just the names given
#' in \code{translation}.
#' @param object A [LamaDictionary][new_dictionary()]
#' @param translation A character vector holding the translation names
expect_translation_names <- function(object, translation) {
  # 1. Capture object and label
  act <- quasi_label(rlang::enquo(object), arg = "object")
  # 2. Call expect()
  expect_dictionary(object)
  missing <- !translation %in% names(act$val)
  expect(
    all(!missing),
    sprintf(
      "The dictionary '%s' does not contain the following translations: %s.",
      act$lab, 
      stringify(translation[missing])
    )
  )
  too_much <- !names(act$val) %in% translation
  expect(
    all(!too_much),
    sprintf(
      "The dictionary '%s' contains too many translations: %s.",
      act$lab,
      stringify(names(act$val)[too_much])
    )
  )
  # 3. Invisibly return the value
  invisible(act$val)
}

#' Check that a translation is identical to a given mapping
#'
#' @param object A [LamaDictionary][new_dictionary()]
#' @param key A character string holding the name of the translation in question
#' @param translation A translation (named character vector) to which the
#' translation in question should be identical.
expect_translation_identical <- function(object, key, translation) {
  # 1. Capture object and label
  act <- quasi_label(rlang::enquo(object), arg = "object")
  # 2. Call expect()
  missing <- !key %in% names(act$val)
  expect_dictionary(object)
  expect(
    all(!missing),
    sprintf(
      "The dictionary '%s' does not contain the following translation: %s.",
      act$lab, 
      stringify(translation[!missing])
    )
  )
  expect_identical(
    act$val[[key]], 
    translation,
    sprintf(
      "Translation '%s' in dictionary '%s' looks like:\n%s\nBut should be:\n%s",
      key,
      act$lab,
      deparse(act$val[[key]]),
      deparse(translation)
    )
  )
  # 3. Invisibly return the values
  invisible(act$val)
}

#' Check that a dictionary contains the right translations
#'
#' Check that the translation names of the dictionary are just the names given
#' in \code{translation}.
#' @param object A [LamaDictionary][new_dictionary()]
#' @param translation A character vector holding the translation names
expect_translation_names <- function(object, translation) {
  # 1. Capture object and label
  act <- quasi_label(rlang::enquo(object), arg = "object")
  # 2. Call expect()
  expect_dictionary(object)
  missing <- !translation %in% names(act$val)
  expect(
    all(!missing),
    sprintf(
      "The dictionary '%s' does not contain the following translations: %s.",
      act$lab, 
      stringify(translation[missing])
    )
  )
  too_much <- !names(act$val) %in% translation
  expect(
    all(!too_much),
    sprintf(
      "The dictionary '%s' contains too many translations: %s.",
      act$lab,
      stringify(names(act$val)[too_much])
    )
  )
  # 3. Invisibly return the value
  invisible(act$val)
}
