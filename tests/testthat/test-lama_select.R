context("lama_select")
dict_a = new_lama_dictionary(
  x = c(x = "a"), 
  y = c(y = "a"),
  z = c(z = "a")
)
# lama_select
test_that("'lama_select' works unqoted", {
  dict_new <- lama_select(dict_a, y, x)
  expect_dictionary(dict_new)
  expect_translation_names(dict_new, c("x", "y"))
  expect_translation_identical(dict_new, "x", c(x = "a"))
})
test_that("'lama_select' throws the right errors", {
  expect_error(
    lama_select(dict_a, x, "y"),
    "Invalid argument at position '3': The expression 'y' could not be parsed.",
    fixed = TRUE
  )
  expect_error(
    lama_select(dict_a, c(x, y)),
    "Invalid argument at position '2': The expression 'c(x, y)' could not be parsed.",
    fixed = TRUE
  )
  expect_error(
    lama_select(dict_a, x, y, x),
    "The following translation names are used more than once: 'x'.",
    fixed = TRUE
  )
  expect_error(
    lama_select(dict_a, X, y, Y),
    "The following translation names could not be found in the lama_dictionary object: 'X', 'Y'.",
    fixed = TRUE
  )
})

# lama_select_
test_that("'lama_select_' works", {
  dict_new <- lama_select_(dict_a, c("x", "y"))
  expect_dictionary(dict_new)
  expect_translation_names(dict_new, c("x", "y"))
  expect_translation_identical(dict_new, "x", c(x = "a"))
})
test_that("'lama_select_' throws the right errors", {
  expect_error(
    lama_select_(dict_a, c("X", "y", "Y")),
    "The following translation names could not be found in the lama_dictionary object: 'X', 'Y'.",
    fixed = TRUE
  )
  expect_error(
    lama_select_(dict_a, c("x", "y", "x")),
    "The following translation names are used more than once: 'x'.",
    fixed = TRUE
  )
})
