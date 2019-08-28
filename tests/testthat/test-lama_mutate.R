context("lama_mutate")
# lama_mutate
test_that("'lama_mutate' works unquoted", {
  expect_dictionary(dict_2)
  dict_new <- lama_mutate(dict_2, x = c(a = "A", b = "B"), y = list(x = "X", y = "Y"))
  expect_dictionary(dict_new)
  expect_translation_names(
    dict_new,
    c("dose", "dose_long", "supp", "supp_long", "x", "y")
  )
  expect_translation_identical(
    dict_new,
    "x",
    c(a = "A", b = "B")
  )
  expect_translation_identical(
    dict_new,
    "y",
    c(x = "X", y = "Y")
  )
})
test_that("'lama_mutate' throws the right errors", {
  expect_error(
    lama_mutate(dict_2, x),
    "object 'x' not found",
    fixed = TRUE
  )
  expect_error(
    lama_mutate(dict_2, x = c(a = "A", b = "B"), x = c(x = "X", y = "Y")),
    "The following translation names are used more than once: 'x'.",
    fixed = TRUE
  )
  expect_error(
    lama_mutate(dict_2),
    "Translation assignments are missing.",
    fixed = TRUE
  )
  expect_error(
    lama_mutate(dict_2, x = c(a = "A"), y = "ba"),
    "The object assigned to the translation name 'y' must either be a named character vector or a named list holding character strings",
    fixed = TRUE
  )
  expect_error(
    lama_mutate(dict_2, x = c(a = "A"), y = c()),
    "The object assigned to the translation name 'y' must either be a named character vector or a named list holding character strings",
    fixed = TRUE
  )
  expect_error(
    lama_mutate(dict_2, x = c(a = "A"), y = list("ba")),
    "The object assigned to the translation name 'y' must either be a named character vector or a named list holding character strings",
    fixed = TRUE
  )
})

# lama_mutate_
test_that("'lama_mutate_' works", {
  expect_dictionary(dict_2)
  dict_new <- lama_mutate_(dict_2, "x", c(a = "A", b = "B"))
  expect_dictionary(dict_new)
  expect_translation_names(
    dict_new,
    c("dose", "dose_long", "supp", "supp_long", "x")
  )
  expect_translation_identical(
    dict_new,
    "x",
    c(a = "A", b = "B")
  )
  dict_new_2 <- lama_mutate_(dict_2, "supp", list(a = "A", b = "B"))
  expect_dictionary(dict_new)
  expect_translation_names(
    dict_new_2,
    c("dose", "dose_long", "supp", "supp_long")
  )
  expect_translation_identical(
    dict_new_2,
    "supp",
    c(a = "A", b = "B")
  )
})
test_that("'lama_mutate_' throws the right errors", {
  expect_error(
    lama_mutate_(dict_2, "x"),
    "argument \"translation\" is missing",
    fixed = TRUE
  )
  expect_error(
    lama_mutate_(dict_2, "1x", c(a = "A", b = "B")),
    "The following translation name given in argument 'key' is not a valid object name: '1x'.",
    fixed = TRUE
  )
  expect_error(
    lama_mutate_(dict_2, "x", "bla"),
    "The object given in the argument 'translation' must either be a named character vector or a named list holding character strings.",
    fixed = TRUE
  )
  expect_error(
    lama_mutate(dict_2, x = c(a = "A"), y = "ba"),
    "The object assigned to the translation name 'y' must either be a named character vector or a named list holding character strings",
    fixed = TRUE
  )
  expect_error(
    lama_mutate(dict_2, x = c(a = "A"), y = c()),
    "The object assigned to the translation name 'y' must either be a named character vector or a named list holding character strings",
    fixed = TRUE
  )
  expect_error(
    lama_mutate(dict_2, x = c(a = "A"), y = list("ba")),
    "The object assigned to the translation name 'y' must either be a named character vector or a named list holding character strings",
    fixed = TRUE
  )
})
