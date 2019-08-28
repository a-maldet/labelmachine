context("lama_translate")
# lama_translate
test_that("'lama_translate' works unquoted", {
  expect_dictionary(dict_2)
  dict_new <- lama_mutate(dict_2, .DOSE_LONG1 = c("0.5" = "A", "1" = "B", "1.5" ="C", "2" = "D"))
  expect_dictionary(dict_new)
  df_new <- lama_translate(df, dict_new, .DOSE_LONG2 = .DOSE_LONG1(dose), .DOSE_LONG1(dose), supp2 =supp, supp, SUPP_LONG = supp_long(supp))
  expect_s3_class(df_new, "data.frame")
  expect_column_names(df_new, c("len", "dose", "supp", ".DOSE_LONG2", "supp2", "SUPP_LONG"))
  expect_factor_levels(df_new$.DOSE_LONG2, c("A", "B", "C", "D"))
  expect_factor_levels(df_new$dose, c("A", "B", "C", "D"))
  expect_factor_levels(df_new$supp2, c("Asc. acid", "O. juice"))
})
test_that("'lama_translate' respects keep_order", {
  expect_dictionary(dict_2)
  dict_new <- lama_mutate(dict_2, dose = c("0.5" = "A", "1" = "B", "1.5" ="C", "2" = "D"))
  expect_dictionary(dict_new)
  df$dose <- factor(df$dose, levels = c(0, 2, 0.5, 3, 1))
  df_new <- lama_translate(
    df,
    dict_new,
    dose,
    supp,
    keep_order = TRUE
  )
  expect_s3_class(df_new, "data.frame")
  expect_column_names(df_new, c("len", "dose", "supp"))
  expect_factor_levels(df_new$dose, c("D", "A", "B", "C"))
  expect_factor_levels(df_new$supp, c("O. juice", "Asc. acid"))
  df_new_2 <- lama_translate(
    df,
    dict_new,
    dose,
    supp,
    keep_order = c(TRUE, FALSE)
  )
  expect_s3_class(df_new_2, "data.frame")
  expect_column_names(df_new_2, c("len", "dose", "supp"))
  expect_factor_levels(df_new_2$dose, c("D", "A", "B", "C"))
  expect_factor_levels(df_new_2$supp, c("Asc. acid", "O. juice"))
})
# lama_translate errors
test_that("'lama_translate' throws the right errors", {
  expect_dictionary(dict_2)
  dict_new <- lama_rename(dict_2, .DOSE_LONG1 = dose_long)
  expect_dictionary(dict_new)
  expect_error(
    lama_translate(df, dict_new),
    "Translation assignments are missing."
  )
  expect_error(
    lama_translate(df, dict_new, X = supp(supp), X = supp(supp)),
    "More than one relabelled variable was assigned to the same column name. The following column names have multiple assignments: 'X'."
  )
  expect_error(
    lama_translate(df, dict_new, X = (supp)),
    "Argument in position '1' could not be parsed: 'X = \\(supp\\)'."
  )
  expect_error(
    lama_translate(df, dict_new, X = asdf(supp)),
    "The translation name 'asdf' could not be found in the LabelDictionary."
  )
  expect_error(
    lama_translate(df, dict_new, supp(asdf)),
    "Argument in position '1' could not be parsed: 'supp\\(asdf\\)'."
  )
  expect_error(
    lama_translate(df, dict_new, supp(dose)),
    "The following variable levels in '.data\\$dose' have no corresponding label in the LabelDictionary object given in argument 'dictionary': '0.5', '1', '2'."
  )
})

# lama_translate_
test_that("'lama_translate_' works", {
  expect_dictionary(dict_2)
  dict_new <- lama_mutate(dict_2, .DOSE_LONG1 = c("0.5" = "A", "1" = "B", "1.5" ="C", "2" = "D"))
  expect_dictionary(dict_new)
  df_new <- lama_translate_(
    df,
    dict_new,
    c(".DOSE_LONG1", "dose", "supp"),
    c("dose", "dose", "supp"),
    c(".DOSE_LONG1", "dose", "supp")
  )
  expect_s3_class(df_new, "data.frame")
  expect_column_names(df_new, c("len", "dose", "supp", ".DOSE_LONG1"))
  expect_factor_levels(df_new$.DOSE_LONG1, c("A", "B", "C", "D"))
  expect_factor_levels(df_new$dose, c("Low", "Medium", "High"))
  expect_factor_levels(df_new$supp, c("Asc. acid", "O. juice"))
})
test_that("'lama_translate_' has the right defaults", {
  expect_dictionary(dict_2)
  df_new <- lama_translate_(
    df,
    dict_2,
    translation = c("dose", "supp")
  )
  expect_s3_class(df_new, "data.frame")
  expect_column_names(df_new, c("len", "dose", "supp"))
  expect_factor_levels(df_new$dose, c("Low", "Medium", "High"))
  expect_factor_levels(df_new$supp, c("Asc. acid", "O. juice"))
  df_new_2 <- lama_translate_(
    df,
    dict_2,
    translation = c("dose_long", "supp_long"),
    col = c("dose", "supp")
  )
  expect_s3_class(df_new_2, "data.frame")
  expect_column_names(df_new_2, c("len", "dose", "supp"))
  expect_factor_levels(df_new_2$dose, c("Low dosage", "Medium dosage", "High dosage"))
  expect_factor_levels(df_new_2$supp, c("Ascorbic acid", "Orange juice"))
  df_new_3 <- lama_translate_(
    df,
    dict_2,
    translation = c("dose", "supp"),
    col_new = c("dose_2", "supp_2")
  )
  expect_s3_class(df_new_3, "data.frame")
  expect_column_names(df_new_3, c("len", "dose", "supp", "dose_2", "supp_2"))
  expect_factor_levels(df_new_3$dose_2, c("Low", "Medium", "High"))
  expect_factor_levels(df_new_3$supp_2, c("Asc. acid", "O. juice"))
})
test_that("'lama_translate_' respects keep_order", {
  expect_dictionary(dict_2)
  dict_new <- lama_mutate(dict_2, dose = c("0.5" = "A", "1" = "B", "1.5" ="C", "2" = "D"))
  expect_dictionary(dict_new)
  df$dose <- factor(df$dose, levels = c(0, 2, 0.5, 3, 1))
  df_new <- lama_translate_(
    df,
    dict_new,
    translation = c("dose", "supp"),
    keep_order = TRUE
  )
  expect_s3_class(df_new, "data.frame")
  expect_column_names(df_new, c("len", "dose", "supp"))
  expect_factor_levels(df_new$dose, c("D", "A", "B", "C"))
  expect_factor_levels(df_new$supp, c("O. juice", "Asc. acid"))
})
# lama_translate errors
test_that("'lama_translate_' throws the right errors", {
  expect_dictionary(dict_2)
  dict_new <- lama_rename(dict_2, .DOSE_LONG1 = dose_long)
  expect_dictionary(dict_new)
  expect_error(
    lama_translate_(df, dict_new),
    "argument \"translation\" is missing"
  )
  expect_error(
    lama_translate_(
      df,
      dict_new,
      translation = c("supp", "supp"),
      col_new = c("X", "X")
    ),
    "The argument 'col_new' must be a character vector without duplicates."
  )
  expect_error(
    lama_translate_(
      df,
      dict_new,
      translation = c("asdf", "blu"),
      col = c("dose", "dose"),
      col_new = c("X", "Y")
    ),
    paste(
      "The following values of the argument 'translation' could not be",
      "found in the LabelDictionary object given in argument 'dictionary':",
      "'asdf', 'blu'."
    )
  )
  expect_error(
    lama_translate_(
      df,
      dict_new,
      translation = c("dose", "supp"),
      col = c("x", "y"),
      col_new = c("X", "Y")
    ),
    paste(
      "The following values of the argument 'col' are no column names",
      "of the data.frame given in argument '.data': 'x', 'y'."
    )
  )
  expect_error(
    lama_translate_(
      df,
      dict_new,
      translation = "supp",
      col = "dose"
    ),
    "The following variable levels in '.data\\$dose' have no corresponding label in the LabelDictionary object given in argument 'dictionary': '0.5', '1', '2'."
  )
})
