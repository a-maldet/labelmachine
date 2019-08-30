context("lama_mutate")
dict_a = new_dictionary(
  x = c(x = "a"), 
  y = c(y = "a"),
  z = c(z = "a")
)
# lama_mutate
test_that("'lama_mutate' works unquoted", {
  dict_new <- lama_mutate(dict_a, x = c(x = "mut"), y1_. = list(y = "mut"))
  expect_dictionary(dict_new)
  expect_translation_names(dict_new, c("x", "y", "z", "y1_."))
  expect_translation_identical(dict_new, "x", c(x = "mut"))
  expect_translation_identical(dict_new, "y", c(y = "a"))
  expect_translation_identical(dict_new, "z", c(z = "a"))
  expect_translation_identical(dict_new, "y1_.", c(y = "mut"))
})
test_that("'lama_mutate' throws the right errors", {
  expect_error(
    lama_mutate(dict_a, x),
    "object 'x' not found",
    fixed = TRUE
  )
  expect_error(
    lama_mutate(dict_a, x = c(x = "mut"), x = c(x = "mut2")),
    "The following translation names are used more than once: 'x'.",
    fixed = TRUE
  )
  expect_error(
    lama_mutate(dict_a),
    "Translation assignments are missing.",
    fixed = TRUE
  )
  expect_error(
    lama_mutate(dict_a, x = c(x = "mut"), y = "mut"),
    "The object assigned to the translation name 'y' must either be a named character vector or a named list holding character strings",
    fixed = TRUE
  )
  expect_error(
    lama_mutate(dict_a, x = c(a = "A"), y = NA),
    "The object assigned to the translation name 'y' must either be a named character vector or a named list holding character strings",
    fixed = TRUE
  )
  expect_error(
    lama_mutate(dict_a, x = c(a = "A"), y = NULL),
    "The object assigned to the translation name 'y' must either be a named character vector or a named list holding character strings",
    fixed = TRUE
  )
  expect_error(
    lama_mutate(dict_a, x = c(a = "A"), y = list("ba")),
    "The object assigned to the translation name 'y' must either be a named character vector or a named list holding character strings",
    fixed = TRUE
  )
})

# lama_mutate_
test_that("'lama_mutate_' works", {
  dict_new <- lama_mutate_(dict_a, "x", c(x = "mut"))
  dict_new <- lama_mutate_(dict_new, "y1_.", c(y = "mut"))
  expect_dictionary(dict_new)
  expect_translation_names(dict_new, c("x", "y", "z", "y1_."))
  expect_translation_identical(dict_new, "x", c(x = "mut"))
  expect_translation_identical(dict_new, "y", c(y = "a"))
  expect_translation_identical(dict_new, "z", c(z = "a"))
  expect_translation_identical(dict_new, "y1_.", c(y = "mut"))
})
test_that("'lama_mutate_' throws the right errors", {
  expect_error(
    lama_mutate_(dict_a, "x"),
    "argument \"translation\" is missing",
    fixed = TRUE
  )
  expect_error(
    lama_mutate_(dict_a, "1x", c(x = "mut")),
    "The following translation name given in argument 'key' is not a valid object name: '1x'.",
    fixed = TRUE
  )
  expect_error(
    lama_mutate_(dict_a, "x", "bla"),
    "The object given in the argument 'translation' must either be a named character vector or a named list holding character strings.",
    fixed = TRUE
  )
  expect_error(
    lama_mutate_(dict_a, "x", NA),
    "The object given in the argument 'translation' must either be a named character vector or a named list holding character strings.",
    fixed = TRUE
  )
  expect_error(
    lama_mutate_(dict_a, "x", NULL),
    "The object given in the argument 'translation' must either be a named character vector or a named list holding character strings.",
    fixed = TRUE
  )
  expect_error(
    lama_mutate_(dict_a, "x", c()),
    "The object given in the argument 'translation' must either be a named character vector or a named list holding character strings.",
    fixed = TRUE
  )
})
