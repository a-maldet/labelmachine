context("utility functions")

test_that("'contains_na_escape' works", {
  expect_identical(
    contains_na_escape(c("asdf", NA_lama_, "asdf")),
    TRUE
  )
  expect_identical(
    contains_na_escape(c("asdf", "asdf", "asdf")),
    FALSE
  )
})
test_that("'contains_na_escape' throws the right error", {
  expect_error(
    contains_na_escape(list(a = 3)),
    "Argument 'x' is not a character."
  )
})

test_that("'named_lapply' works", {
  expect_identical(
    named_lapply(.names = c("a", "b"), function(x, .I) 2 * .I),
    list(a = 2, b = 4)
  )
})
test_that("'named_lapply' throws the right error", {
  expect_error(
    named_lapply(.names = list("a", "b"), function(x, .I) 2 * .I),
    "Argument '.names' must be a character vector holding the list entry names."
  )
  expect_error(
    named_lapply(.names = c("a", "b"), 4),
    "Argument 'FUN' must be a function."
  )
})
