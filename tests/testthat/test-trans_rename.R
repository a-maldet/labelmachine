context("trans_rename")
# trans_rename
test_that("'trans_rename' works unquoted", {
  expect_dictionary(dict_2)
  dict_new <- trans_rename(dict_2, x = dose_long, y = dose)
  expect_dictionary(dict_new)
  expect_has_exact_translation(
    dict_new,
    c("supp", "supp_long", "x", "y"))
  expect_translation_identical(
    dict_new,
    "y",
    c("0" = "Low", "1" = "Medium", "2" = "High")
  )
})
test_that("'trans_rename' throws the right errors", {
  expect_error(
    trans_rename(dict_2, x = DOSE, y = SUPP),
    "The following old variable names could not be found in the LabelDictionary object: 'DOSE', 'SUPP'.",
    fixed = TRUE
  )
})

# trans_rename_
test_that("trans_rename_ works", {
  dict_new <- trans_rename_(dict_2, c("dose_long", "dose"), c("x", "y"))
  expect_dictionary(dict_2)
  expect_dictionary(dict_new)
  expect_has_exact_translation(
    dict_new,
    c("supp", "supp_long", "x", "y"))
  expect_translation_identical(
    dict_new,
    "y",
    c("0" = "Low", "1" = "Medium", "2" = "High")
  )
})
test_that("'trans_rename_' throws the right errors", {
  expect_error(
    trans_rename_(dict_2, c("DOSE", "SUPP"), c("x", "y")),
    "The following names given in the argument 'old' could not be found in the LabelDictionary object: 'DOSE', 'SUPP'.",
    fixed = TRUE
  )
})
