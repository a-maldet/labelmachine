context("lama_translate_all")
dict = new_lama_dictionary(
  a = c(a = "A", b = "B", c = "X", d = "D", e = "X", f = "F"), 
  x = c(x = "X", y = NA, "NA_" = NA, z = "Z"),
  u = c(u = NA, v = "V", w = NA, "NA_" = "NAnew"),
  xx = c(x = "X", y = NA, "NA_" = NA, z = "Z")
)
df <- data.frame(
  a = factor(
    c("c", "d", "d", "a", "a", "f", NA, "e"),
    levels = c("f", "e", "X1", "d", "c", "a", "X2")
  ),
  a_labeled = c("X", "D", "D", "A", "A", "F", NA, "X"),
  x = factor(
    c("y", "y", NA, "x", "x", "y", "y", "x"),
    levels = c("X1", "y", "X2", "x", "X3")
  ),
  x_labeled = c(NA, NA, NA, "X", "X", NA, NA, "X"),
  u = c("u", "v", NA, "v", "u", "v", "u", "v"),
  u_labeled = c(NA, "V", "NAnew", "V", NA, "V", NA, "V"),
  stringsAsFactors = FALSE
)
# lama_translate
test_that("'lama_translate_all' works", {
  df_new <- lama_translate_all(
    df,
    dict,
    prefix = "new_",
    suffix = "_new",
    fn_colname = toupper,
    keep_order = TRUE
  )
  expect_s3_class(df_new, "data.frame")
  expect_column_names(
    df_new, 
    c(
      "a", "a_labeled", "x", "x_labeled", "u", "u_labeled",
      "new_A_new", "new_X_new", "new_U_new"
    )
  )
  expect_factor_levels(df_new$new_A_new, c("F", "X", "D", "A", "B"))
  expect_factor_levels(df_new$new_X_new, c("X", "Z"))
  expect_factor_levels(df_new$new_U_new, c("V", "NAnew"))
  expect_identical(as.character(df_new$new_A_new), df_new$a_labeled)
  expect_identical(as.character(df_new$new_X_new), df_new$x_labeled)
  expect_identical(as.character(df_new$new_U_new), df_new$u_labeled)
})
# lama_translate errors
test_that("'lama_translate_all' throws the right errors", {
  expect_error(
    lama_translate_all(df, df),
    paste(
      "The argument 'dictionary' must be a lama_dictionary class object."
    )
  )
  expect_error(
    lama_translate_all(df, dict, prefix = NA_character_),
    paste(
      "The argument 'prefix' must be a character string."
    )
  )
  expect_error(
    lama_translate_all(df, dict, prefix = c("x", "y")),
    paste(
      "The argument 'prefix' must be a character string."
    )
  )
  expect_error(
    lama_translate_all(df, dict, prefix = "_"),
    paste(
      "The argument 'prefix' must be a valid object name prefix."
    )
  )
  expect_error(
    lama_translate_all(df, dict, suffix = NA_character_),
    paste(
      "The argument 'suffix' must be a character string."
    )
  )
  expect_error(
    lama_translate_all(df, dict, suffix = c("x", "y")),
    paste(
      "The argument 'suffix' must be a character string."
    )
  )
  expect_error(
    lama_translate_all(df, dict, suffix = " "),
    paste(
      "The argument 'suffix' must be a valid object name suffix."
    )
  )
  expect_error(
    lama_translate_all(df, dict, keep_order = NA),
    paste(
      "The argument 'keep_order' must be a logical."
    )
  )
  expect_error(
    lama_translate_all(df, dict, fn_colname = 3),
    paste(
      "The argument 'fn_colname' must be a function taking a character string and",
      "returning a character string."
    )
  )
  expect_error(
    lama_translate_all(df, dict, fn_colname = function(x) 4),
    paste(
      "The function given in argument 'fn_colname' does not produce valid",
      "column names: It must take character strings and return character",
      "strings."
    )
  )
  expect_error(
    lama_translate_all(df, dict, fn_colname = function(x) ifelse(x == "x", NA, toupper(x))),
    paste(
      "The function given in argument 'fn_colname' does not produce valid",
      "column names: It must take character strings and return character",
      "strings."
    )
  )
  expect_error(
    lama_translate_all(df, dict, fn_colname = function(x) sprintf("%s ", x)),
    paste(
      "The function given in argument 'fn_colname' does not produce valid",
      "column names: The following produced column names are",
      "invalid: 'a ', 'x ', 'u '."
    )
  )
})
