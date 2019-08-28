context("lama_select")
# lama_select
test_that("'lama_select' works unqoted", {
  expect_dictionary(dict_2)
  dict_new <- lama_select(dict_2, dose_long, dose, supp_long)
  expect_dictionary(dict_new)
  expect_translation_names(
    dict_new,
    c("dose_long", "dose", "supp_long")
  )
})
test_that("'lama_select' works qoted", {
  expect_dictionary(dict_2)
  expect_translation_names(
    lama_select(dict_2, dose_long, dose, supp_long),
    c("dose_long", "dose", "supp_long"))
  expect_translation_names(
    lama_select(dict_2, "dose_long", "dose", "supp_long"),
    c("dose_long", "dose", "supp_long"))
})
test_that("'lama_select' throws the right errors", {
  expect_dictionary(dict_2)
  expect_error(
    lama_select(dict_2, c("dose", "supp_long")),
    "Some passed in arguments are invalid.",
    fixed = TRUE
  )
  expect_error(
    lama_select(dict_2, dose, dose),
    "The following translation names are used more than once: 'dose'.",
    fixed = TRUE
  )
  expect_error(
    lama_select(dict_2, DOSE, SUPP),
    "The following translation names could not be found in the LabelDictionary object: 'DOSE', 'SUPP'.",
    fixed = TRUE
  )
  expect_error(
    lama_select(dict_2, "dose", "dose"),
    "The following translation names are used more than once: 'dose'.",
    fixed = TRUE
  )
})

# lama_select_
test_that("'lama_select_' works", {
  expect_dictionary(dict_2)
  dict_new <- lama_select_(dict_2, c("dose_long", "dose", "supp_long"))
  expect_dictionary(dict_new)
  expect_translation_names(
    dict_new,
    c("dose_long", "dose", "supp_long")
  )
})
test_that("'lama_select_' throws the right errors", {
  expect_error(
    lama_select_(dict_2, c("DOSE", "SUPP")),
    "The following translation names could not be found in the LabelDictionary object: 'DOSE', 'SUPP'.",
    fixed = TRUE
  )
  expect_error(
    lama_select_(dict_2, c("dose", "dose")),
    "The following translation names are used more than once: 'dose'.",
    fixed = TRUE
  )
})
