context("lama_rename")
# lama_rename
test_that("'lama_rename' works unquoted", {
  expect_dictionary(dict_2)
  dict_new <- lama_rename(dict_2, x = dose_long, y = dose)
  expect_dictionary(dict_new)
  expect_translation_names(
    dict_new,
    c("supp", "supp_long", "x", "y"))
  expect_translation_identical(
    dict_new,
    "y",
    c("0" = "Low", "1" = "Medium", "2" = "High")
  )
  dict_new_2 <- lama_rename(dict_2, supp = dose_long, y = dose)
  expect_translation_identical(
    dict_new_2,
    "supp",
    c("0" = "Low dosage", "1" = "Medium dosage", "2" = "High dosage")
  )
  expect_translation_names(
    dict_new_2,
    c("y", "supp", "supp_long")
  )
})
test_that("'lama_rename' throws the right errors", {
  expect_error(
    lama_rename(dict_2, x = dose, x = supp),
    "Error while calling 'lama_rename': The following translation names are used more than once: 'x'.",
    fixed = TRUE
  )
  expect_error(
    lama_rename(dict_2, x = dose, y = dose),
    "The following old translation names are used more than once: 'dose'.",
    fixed = TRUE
  )
  expect_error(
    lama_rename(dict_2, x = DOSE, y = SUPP),
    "The following old translation names could not be found in the LabelDictionary object: 'DOSE', 'SUPP'.",
    fixed = TRUE
  )
})

# lama_rename_
test_that("lama_rename_ works", {
  dict_new <- lama_rename_(dict_2, c("dose_long", "dose"), c("x", "y"))
  expect_dictionary(dict_2)
  expect_dictionary(dict_new)
  expect_translation_names(
    dict_new,
    c("supp", "supp_long", "x", "y"))
  expect_translation_identical(
    dict_new,
    "y",
    c("0" = "Low", "1" = "Medium", "2" = "High")
  )
  dict_new_2 <- lama_rename_(dict_2, c("dose_long", "dose"), c("supp", "y"))
  expect_translation_identical(
    dict_new_2,
    "supp",
    c("0" = "Low dosage", "1" = "Medium dosage", "2" = "High dosage")
  )
  expect_translation_names(
    dict_new_2,
    c("y", "supp", "supp_long")
  )
})
test_that("'lama_rename_' throws the right errors", {
  expect_error(
    lama_rename_(dict_2, c("dose", "supp"), c("x", "x")),
    "The following translation names are used more than once: 'x'.",
    fixed = TRUE
  )
  expect_error(
    lama_rename_(dict_2, c("dose", "dose"), c("x", "y")),
    "The following old translation names are used more than once: 'dose'.",
    fixed = TRUE
  )
  expect_error(
    lama_rename_(dict_2, c("DOSE", "SUPP"), c("x", "y")),
    "The following old translation names could not be found in the LabelDictionary object: 'DOSE', 'SUPP'.",
    fixed = TRUE
  )
})
