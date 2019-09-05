context("lama_dictionary")
test_that("list argument is allowed",{
  dict <- new_dictionary(
    list(a = list(a = "A", b = NA, NA_ = NA), x = c(x = "X", NA_ = "Y"))
  )
  expect_dictionary(dict)
  expect_translation_names(dict, c("a", "x"))
  expect_translation_identical(dict, "a", c(a = "A", b = NA, NA_ = NA))
  expect_translation_identical(dict, "x", c(x = "X", NA_ = "Y"))
})

test_that("list case throws the right errors", {
  expect_warning(
    new_dictionary(
      list(a = list(a = "A", b = "B")),
      x = c(x = "X")
    ),
    "If the first element is a list object, then no more arguments are needed. '1' extra arguments will be ignored.",
    fixed = TRUE
  )
  expect_error(
    new_dictionary(
      list(c(a = "A"), x = c(x = "X", y = "Y"))
    ),
    paste(
      "The passed in translation definitions are invalid:",
      "The names of the translations are missing."
    ),
    fixed = TRUE
  )
  expect_error(
    new_dictionary(
      list(a = list(a = "A"), x = c(x = "X", x = "Y"))
    ),
    paste(
      "Invalid translation with name 'x':",
      "The object has duplicate names.",
      "The following original values are defined more than once: 'x'."
    ),
    fixed = TRUE
  )
  expect_error(
    new_dictionary(
      list(a = list(a = "A"), x = c(x = 3))
    ),
    paste(
      "Invalid translation with name 'x':",
      "The object is not a valid named character vector."
    ),
    fixed = TRUE
  )
  expect_error(
    new_dictionary(
      list(a = list(a = "A"), x = c("X", "Y"))
    ),
    paste(
      "Invalid translation with name 'x':",
      "The object is not a valid named character vector."
    ),
    fixed = TRUE
  )
})

test_that("named arguments are allowed",{
  dict <- new_dictionary(
    a = list(a = "A", b = NA, NA_ = NA),
    x = c(x = "X", NA_ = "Y")
  )
  expect_dictionary(dict)
  expect_translation_names(dict, c("a", "x"))
  expect_translation_identical(dict, "a", c(a = "A", b = NA, NA_ = NA))
  expect_translation_identical(dict, "x", c(x = "X", NA_ = "Y"))
})
test_that("named arguments case throws the right errors", {
  expect_error(
    new_dictionary(
      a = c(a = "a", b = "b"),
      c(x = "X", y = "Y")
    ),
    paste(
      "The passed in translation definitions are invalid:",
      "The names of the translations are missing."
    ),
    fixed = TRUE
  )
  expect_error(
    new_dictionary(
      a = c(a = "a", a = "b"),
      x = c(x = "X", y = "Y")
    ),
    paste(
      "Invalid translation with name 'a':",
      "The object has duplicate names.",
      "The following original values are defined more than once: 'a'."
    ),
    fixed = TRUE
  )
  expect_error(
    new_dictionary(
      a = list(a = "a", a = "b"),
      x = c(x = "X", y = "Y")
    ),
    paste(
      "Invalid translation with name 'a':",
      "The object has duplicate names.",
      "The following original values are defined more than once: 'a'."
    ),
    fixed = TRUE
  )
  expect_error(
    new_dictionary(
      a = c(a = 1, b = 2),
      x = c(x = "X", y = "Y")
    ),
    paste(
      "Invalid translation with name 'a':",
      "The object is not a valid named character vector."
    ),
    fixed = TRUE
  )
})
