context("trans_select")
# trans_select
test_that("'trans_select' works unqoted", {
  expect_dictionary(dict_2)
  dict_new <- trans_select(dict_2, dose_long, dose, supp_long)
  expect_dictionary(dict_new)
  expect_has_exact_translation(
    dict_new,
    c("dose_long", "dose", "supp_long"))
})
test_that("'trans_select' works qoted", {
  expect_dictionary(dict_2)
  expect_has_exact_translation(
    trans_select(dict_2, dose_long, dose, supp_long),
    c("dose_long", "dose", "supp_long"))
  expect_has_exact_translation(
    trans_select(dict_2, "dose_long", "dose", "supp_long"),
    c("dose_long", "dose", "supp_long"))
})
test_that("'trans_select' throws the right errors", {
  expect_dictionary(dict_2)
  expect_error(
    trans_select(dict_2, c("dose", "supp_long")),
    "Some passed in arguments are invalid.",
    fixed = TRUE
  )
  expect_error(
    trans_select(dict_2, DOSE, SUPP),
    "The following translation names could not be found in the LabelDictionary object: 'DOSE', 'SUPP'.",
    fixed = TRUE
  )
})

# trans_select_
test_that("'trans_select_' works", {
  expect_dictionary(dict_2)
  dict_new <- trans_select_(dict_2, c("dose_long", "dose", "supp_long"))
  expect_dictionary(dict_new)
  expect_has_exact_translation(
    dict_new,
    c("dose_long", "dose", "supp_long")
  )
})
test_that("'trans_select_' throws the right errors", {
  expect_error(
    trans_select_(dict_2, c("DOSE", "SUPP")),
    "The following names given in the argument 'variable' could not be found in the LabelDictionary object: 'DOSE', 'SUPP'.",
    fixed = TRUE
  )
})
