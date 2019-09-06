context("lama_rename")
dict_a = new_lama_dictionary(
  x = c(x = "a"), 
  y = c(y = "a"),
  z = c(z = "a")
)
# lama_rename
test_that("'lama_rename' works unquoted", {
  dict_new <- lama_rename(dict_a, x1_. = x, y1 = y)
  expect_dictionary(dict_new)
  expect_translation_names(dict_new, c("x1_.", "y1", "z"))
  expect_translation_identical(dict_new, "y1", c("y" = "a"))
  dict_new_2 <- lama_rename(dict_a, y = x, x = y, z = z)
  expect_translation_names(dict_new_2, c("x", "y", "z"))
  expect_translation_identical(dict_new_2, "x", c(y = "a"))
  expect_translation_identical(dict_new_2, "y", c(x = "a"))
  expect_translation_identical(dict_new_2, "z", c(z = "a"))
})
test_that("'lama_rename' throws the right errors", {
  expect_error(
    lama_rename(dict_a, x = x, x = y),
    "Error while calling 'lama_rename': The following translation names are used more than once: 'x'.",
    fixed = TRUE
  )
  expect_error(
    lama_rename(dict_a, x = x, y = 35),
    "Invalid argument at position '3': The expression 'y = 35' could not be parsed.",
    fixed = TRUE
  )
  expect_error(
    lama_rename(dict_a, x = x, y = y(1)),
    "Invalid argument at position '3': The expression 'y = y(1)' could not be parsed.",
    fixed = TRUE
  )
  expect_error(
    lama_rename(dict_a, x = x, y = x),
    "The following old translation names are used more than once: 'x'.",
    fixed = TRUE
  )
  expect_error(
    lama_rename(dict_a, x = X, y = Y),
    "The following old translation names could not be found in the lama_dictionary object: 'X', 'Y'.",
    fixed = TRUE
  )
})

# lama_rename_
test_that("lama_rename_ works", {
  dict_new <- lama_rename_(dict_a, c("x", "y"), c("y", "x1_."))
  expect_dictionary(dict_new)
  expect_translation_names(dict_new, c("x1_.", "y", "z"))
  expect_translation_identical(dict_new, "x1_.", c(y = "a"))
  expect_translation_identical(dict_new, "y", c(x = "a"))
  expect_translation_identical(dict_new, "z", c(z = "a"))
})
test_that("'lama_rename_' throws the right errors", {
  expect_error(
    lama_rename_(dict_a, c("x", "y"), c("x", "x")),
    "The following translation names are used more than once: 'x'.",
    fixed = TRUE
  )
  expect_error(
    lama_rename_(dict_a, c("x", "x"), c("x", "y")),
    "The following old translation names are used more than once: 'x'.",
    fixed = TRUE
  )
  expect_error(
    lama_rename_(dict_a, c("X", "Y"), c("x", "y")),
    "The following old translation names could not be found in the lama_dictionary object: 'X', 'Y'.",
    fixed = TRUE
  )
})
