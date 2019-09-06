context("lama_mutate")
dict_a = new_lama_dictionary(
  x = c(x = "a"), 
  y = c(y = "a"),
  z = c(z = "a")
)
# lama_mutate
test_that("'lama_mutate' works unquoted", {
  dict_new <- lama_mutate(dict_a, x = c(x = "new"), y1_. = list(y = "new", y2 = NA))
  expect_dictionary(dict_new)
  expect_translation_names(dict_new, c("x", "y", "z", "y1_."))
  expect_translation_identical(dict_new, "x", c(x = "new"))
  expect_translation_identical(dict_new, "y", c(y = "a"))
  expect_translation_identical(dict_new, "z", c(z = "a"))
  expect_translation_identical(dict_new, "y1_.", c(y = "new", y2 = NA))
})
test_that("'lama_mutate' throws the right errors", {
  expect_error(
    lama_mutate(dict_a, x),
    "Translation assignments are invalid.",
    fixed = TRUE
  )
  expect_error(
    lama_mutate(dict_a, x = c(x = "new"), x = c(x = "new2")),
    "The following translation names are used more than once: 'x'.",
    fixed = TRUE
  )
  expect_error(
    lama_mutate(dict_a),
    "Translation assignments are missing.",
    fixed = TRUE
  )
  expect_error(
    lama_mutate(dict_a, x = c(x = "new"), y = "new"),
    "Invalid argument at position '3': The object is not a valid named character vector.",
    fixed = TRUE
  )
  expect_error(
    lama_mutate(dict_a, x = c(a = "A"), y = NA),
    "Invalid argument at position '3': The object is not a valid named character vector.",
    fixed = TRUE
  )
  expect_error(
    lama_mutate(dict_a, x = c(a = "A"), y = NULL),
    "Invalid argument at position '3': The object is not a valid named character vector.",
    fixed = TRUE
  )
  expect_error(
    lama_mutate(dict_a, x = c(a = "A"), y = list("ba")),
    "Invalid argument at position '3': The object is not a valid named character vector.",
    fixed = TRUE
  )
})

# lama_mutate_
test_that("'lama_mutate_' works", {
  dict_new <- lama_mutate_(dict_a, "x", c(x = "new"))
  dict_new <- lama_mutate_(dict_new, "y1_.", c(y = "new"))
  expect_dictionary(dict_new)
  expect_translation_names(dict_new, c("x", "y", "z", "y1_."))
  expect_translation_identical(dict_new, "x", c(x = "new"))
  expect_translation_identical(dict_new, "y", c(y = "a"))
  expect_translation_identical(dict_new, "z", c(z = "a"))
  expect_translation_identical(dict_new, "y1_.", c(y = "new"))
})
test_that("'lama_mutate_' throws the right errors", {
  expect_error(
    lama_mutate_(dict_a, "x"),
    "argument \"translation\" is missing",
    fixed = TRUE
  )
  expect_error(
    lama_mutate_(dict_a, "1x", c(x = "new")),
    "The following translation name given in argument 'key' is not a valid object name: '1x'.",
    fixed = TRUE
  )
  expect_error(
    lama_mutate_(dict_a, "x", "bla"),
    "The object given in the argument 'translation' is invalid: The object is not a valid named character vector.",
    fixed = TRUE
  )
  expect_error(
    lama_mutate_(dict_a, "x", NA),
    "The object given in the argument 'translation' is invalid: The object is not a valid named character vector.",
    fixed = TRUE
  )
  expect_error(
    lama_mutate_(dict_a, "x", NULL),
    "The object given in the argument 'translation' is invalid: The object is not a valid named character vector.",
    fixed = TRUE
  )
  expect_error(
    lama_mutate_(dict_a, "x", c()),
    "The object given in the argument 'translation' is invalid: The object is not a valid named character vector.",
    fixed = TRUE
  )
})
