context("lama_translate")
dict = new_dictionary(
  r._ = c(a = "A", b = "B", c = "X", d = "D", e = "X", f = "F"), 
  s = c(x = "X", y = "Y", z = "Z"),
  var_t = c(u = "U", v = "V", w ="W")
)
df <- data.frame(
  var_r = factor(
    c("c", "d", "d", "a", "a", "f", "e", "e"),
    levels = c("f", "e", "X1", "d", "c", "a", "X2")
  ),
  var_r_labelled = c("X", "D", "D", "A", "A", "F", "X", "X"),
  var_s = factor(
    c("y", "y", "x", "x", "x", "y", "y", "x"),
    levels = c("X1", "y", "X2", "x", "X3")
  ),
  var_s_labelled = c("Y", "Y", "X", "X", "X", "Y", "Y", "X"),
  var_t = c("u", "v", "u", "v", "u", "v", "u", "v"),
  var_t_labelled = c("U", "V", "U", "V", "U", "V", "U", "V"),
  var_t_false = c("u", "v", "X", "u", "v", "u", "v", "u"),
  var_r_false = factor(
    c("c", "d", "d", "a", "a", "f", "e", "X2"),
    levels = c("f", "e", "X1", "d", "c", "a", "X2")
  ),
  stringsAsFactors = FALSE
)
# lama_translate
test_that("'lama_translate' works unquoted", {
  df_new <- lama_translate(
    df,
    dict,
    var_r_new = r._(var_r),
    r._(var_r),
    var_t_new = var_t,
    var_t,
    keep_order = c(TRUE, FALSE, TRUE, FALSE)
  )
  expect_s3_class(df_new, "data.frame")
  expect_column_names(
    df_new, 
    c(
      "var_r", "var_r_labelled", "var_s", "var_s_labelled", "var_t",
      "var_t_labelled", "var_t_false", "var_r_false",
      "var_r_new", "var_t_new"
    )
  )
  expect_factor_levels(df_new$var_r_new, c("F", "X", "D", "A", "B"))
  expect_factor_levels(df_new$var_r, c("A", "B", "X", "D", "F"))
  expect_factor_levels(df_new$var_t_new, c("U", "V", "W"))
  expect_factor_levels(df_new$var_t, c("U", "V", "W"))
  expect_identical(as.character(df_new$var_r), df_new$var_r_labelled)
  expect_identical(as.character(df_new$var_r_new), df_new$var_r_labelled)
  expect_identical(as.character(df_new$var_t_new), df_new$var_t_labelled)
  expect_identical(as.character(df_new$var_t), df_new$var_t_labelled)
  df_new_2 <- lama_translate(
    df,
    dict,
    r._(var_r),
    s(var_s),
    keep_order = TRUE
  )
  expect_s3_class(df_new_2, "data.frame")
  expect_column_names(
    df_new_2, 
    c(
      "var_r", "var_r_labelled", "var_s", "var_s_labelled", "var_t",
      "var_t_labelled", "var_t_false", "var_r_false"
    )
  )
  expect_factor_levels(df_new_2$var_r, c("F", "X", "D", "A", "B"))
  expect_factor_levels(df_new_2$var_s, c("Y", "X", "Z"))
  expect_identical(as.character(df_new_2$var_r), df_new_2$var_r_labelled)
  expect_identical(as.character(df_new_2$var_s), df_new_2$var_s_labelled)
})
# lama_translate errors
test_that("'lama_translate' throws the right errors", {
  expect_error(
    lama_translate(df, dict),
    "Translation assignments are missing."
  )
  expect_error(
    lama_translate(df, dict, x = s(var_s), y = r._(var_r), x = s(var_s)),
    "More than one relabelled variable was assigned to the same column name. The following column names have multiple assignments: 'x'."
  )
  expect_error(
    lama_translate(df, dict, x = (var_t)),
    "Argument in position '1' could not be parsed: 'x = \\(var_t\\)'."
  )
  expect_error(
    lama_translate(df, dict, x = asdf(var_t)),
    "The translation name 'asdf' could not be found in the LabelDictionary."
  )
  expect_error(
    lama_translate(df, dict, s(asdf)),
    "Argument in position '1' could not be parsed: 's\\(asdf\\)'."
  )
  expect_error(
    lama_translate(df, dict, r._(var_r_false)),
    paste(
      "Translation 'r._' could not be applied to column 'var_r_false': The",
      "following values in the column variable could not be found in the",
      "translation: 'X2'."
    )
  )
})

# lama_translate_
test_that("'lama_translate_' works", {
  df_new <- lama_translate_(
    df,
    dict,
    translation = c("r._", "r._", "var_t", "var_t"),
    col = c("var_r", "var_r", "var_t", "var_t"),
    col_new = c("var_r_new", "var_r", "var_t_new", "var_t"),
    keep_order = c(TRUE, FALSE, TRUE, FALSE)
  )
  expect_s3_class(df_new, "data.frame")
  expect_column_names(
    df_new, 
    c(
      "var_r", "var_r_labelled", "var_s", "var_s_labelled", "var_t",
      "var_t_labelled", "var_t_false", "var_r_false",
      "var_r_new", "var_t_new"
    )
  )
  expect_factor_levels(df_new$var_r_new, c("F", "X", "D", "A", "B"))
  expect_factor_levels(df_new$var_r, c("A", "B", "X", "D", "F"))
  expect_factor_levels(df_new$var_t_new, c("U", "V", "W"))
  expect_factor_levels(df_new$var_t, c("U", "V", "W"))
  expect_identical(as.character(df_new$var_r), df_new$var_r_labelled)
  expect_identical(as.character(df_new$var_r_new), df_new$var_r_labelled)
  expect_identical(as.character(df_new$var_t_new), df_new$var_t_labelled)
  expect_identical(as.character(df_new$var_t), df_new$var_t_labelled)
  df_new_2 <- lama_translate_(
    df,
    dict,
    translation = c("r._", "s"),
    col = c("var_r", "var_s"),
    keep_order = TRUE
  )
  expect_s3_class(df_new_2, "data.frame")
  expect_column_names(
    df_new_2, 
    c(
      "var_r", "var_r_labelled", "var_s", "var_s_labelled", "var_t",
      "var_t_labelled", "var_t_false", "var_r_false"
    )
  )
  expect_factor_levels(df_new_2$var_r, c("F", "X", "D", "A", "B"))
  expect_factor_levels(df_new_2$var_s, c("Y", "X", "Z"))
  expect_identical(as.character(df_new_2$var_r), df_new_2$var_r_labelled)
  expect_identical(as.character(df_new_2$var_s), df_new_2$var_s_labelled)
  df_new_3 <- lama_translate_(
    df,
    dict,
    translation = "var_t",
    col_new = "var_t_new"
  )
  expect_s3_class(df_new_3, "data.frame")
  expect_column_names(
    df_new_3, 
    c(
      "var_r", "var_r_labelled", "var_s", "var_s_labelled", "var_t",
      "var_t_labelled", "var_t_false", "var_r_false", "var_t_new"
    )
  )
  expect_factor_levels(df_new_3$var_t_new, c("U", "V", "W"))
})
# lama_translate errors
test_that("'lama_translate_' throws the right errors", {
  expect_error(
    lama_translate_(df, dict),
    "argument \"translation\" is missing"
  )
  expect_error(
    lama_translate_(
      df,
      dict,
      translation = c("var_t", "var_t", "var_t"),
      col_new = c("X", "x2", "X")
    ),
    "The argument 'col_new' must be a character vector without duplicates."
  )
  expect_error(
    lama_translate_(
      df,
      dict,
      translation = c("asdf", "var_t", "blu"),
      col = c("dose", "var_t", "dose"),
      col_new = c("X", "Y", "Z")
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
      dict,
      translation = c("var_t", "var_t", "var_t"),
      col = c("x", "var_t", "y"),
      col_new = c("X", "Y", "Z")
    ),
    paste(
      "The following values of the argument 'col' are no column names",
      "of the data.frame given in argument '.data': 'x', 'y'."
    )
  )
  expect_error(
    lama_translate_(
      df,
      dict,
      translation = c("var_t", "var_t"),
      col = c("var_t", "var_t", "y")
    ),
    paste(
      "The argument 'col' must be a character vector of the same length as",
      "'translation'."
    )
  )
  expect_error(
    lama_translate_(
      df,
      dict,
      translation = c("var_t", "var_t"),
      col = c("var_t", "var_t"),
      col_new = c("var_t", "var_t", "y")
    ),
    paste(
      "The argument 'col_new' must be a character vector of the same length as",
      "'translation'."
    )
  )
  expect_error(
    lama_translate_(
      df,
      dict,
      translation = "var_t",
      col = "var_t_false"
    ),
    paste(
      "Translation 'var_t' could not be applied to column 'var_t_false': The",
      "following values in the column variable could not be found in the",
      "translation: 'X'."
    )
  )
})
