context("lama_to_factor")
dict = new_lama_dictionary(
  r._ = c(a = "A", b = "B", c = "X", d = "D", e = "X", f = "F"), 
  s = c(x = "X", y = NA, "NA_" = NA, z = "Z"),
  var_t = c(u = NA, v = "V", w = NA, "NA_" = "NAnew")
)
df <- data.frame(
  var_r = factor(
    c("X", "D", "D", "A", "A", "F", NA, "X"),
    levels = c("F", "X", "X1", "D", "C", "A", "X2")
  ),
  var_r_labeled = c("X", "D", "D", "A", "A", "F", NA, "X"),
  var_s = factor(
    c(NA, NA, NA, "X", "X", NA, NA, "X"),
    levels = c("X1", "X", "X3")
  ),
  var_s_labeled = c(NA, NA, NA, "X", "X", NA, NA, "X"),
  var_t = c(NA, "V", NA, "V", NA, "V", NA, "V"),
  var_t_labeled = c(NA, "V", NA, "V", NA, "V", NA, "V"),
  var_r_false = c("X2", "D", "D", "A", "A", "F", NA, "X2"),
  var_t_false = c(NA, "V", "X", "V", NA, "V", NA, "V"),
  stringsAsFactors = FALSE
)
# lama_to_factor
test_that("'lama_to_factor' works unquoted", {
  df_new <- lama_to_factor(
    df,
    dict,
    var_r_new = r._(var_r),
    r._(var_r),
    var_s_new = s(var_s),
    var_t_new = var_t,
    var_t,
    keep_order = c(TRUE, FALSE, TRUE, TRUE, FALSE)
  )
  expect_s3_class(df_new, "data.frame")
  expect_column_names(
    df_new, 
    c(
      "var_r", "var_r_labeled", "var_s", "var_s_labeled", "var_t",
      "var_t_labeled", "var_r_false", "var_t_false",
      "var_r_new", "var_s_new", "var_t_new"
    )
  )
  expect_factor_levels(df_new$var_r_new, c("F", "X", "D", "A", "B"))
  expect_factor_levels(df_new$var_r, c("A", "B", "X", "D", "F"))
  expect_factor_levels(df_new$var_s_new, c("X", "Z"))
  expect_factor_levels(df_new$var_t_new, c("V", "NAnew"))
  expect_factor_levels(df_new$var_t, c("V", "NAnew"))
  expect_identical(as.character(df_new$var_r), df_new$var_r_labeled)
  expect_identical(as.character(df_new$var_r_new), df_new$var_r_labeled)
  expect_identical(as.character(df_new$var_t_new), df_new$var_t_labeled)
  expect_identical(as.character(df_new$var_t), df_new$var_t_labeled)
  df_new_2 <- lama_to_factor(
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
      "var_r", "var_r_labeled", "var_s", "var_s_labeled", "var_t",
      "var_t_labeled", "var_r_false", "var_t_false"
    )
  )
  expect_factor_levels(df_new_2$var_r, c("F", "X", "D", "A", "B"))
  expect_factor_levels(df_new_2$var_s, c("X", "Z"))
  expect_identical(as.character(df_new_2$var_r), df_new_2$var_r_labeled)
  expect_identical(as.character(df_new_2$var_s), df_new_2$var_s_labeled)
})
# lama_to_factor errors
test_that("'lama_to_factor' throws the right errors", {
  expect_error(
    lama_to_factor(df, dict),
    "Translation assignments are missing."
  )
  expect_error(
    lama_to_factor(df, dict, x = s(var_s), y = r._(var_r), x = s(var_s)),
    "More than one relabeled variable was assigned to the same column name. The following column names have multiple assignments: 'x'."
  )
  expect_error(
    lama_to_factor(df, dict, y = s(var_s), x = (var_t)),
    "Invalid argument at position '4': The expression 'x = \\(var_t\\)' could not be parsed."
  )
  expect_error(
    lama_to_factor(df, dict, y = s(var_s), x = asdf(var_t)),
    "Invalid argument at position '4': The expression 'x = asdf\\(var_t\\)' could not be parsed."
  )
  expect_error(
    lama_to_factor(df, dict, y = s(var_s), s(asdf)),
    "Invalid argument at position '4': The variable name 'asdf' could not be found in the data.frame."
  )
  expect_error(
    lama_to_factor(df, dict, r._(var_r_false)),
    paste(
      "Translation 'r._' could not be applied to column 'var_r_false': The",
      "following values in the column variable could not be found in the",
      "translation: 'X2'."
    )
  )
})

# lama_to_factor_
test_that("'lama_to_factor_' works", {
  df_new <- lama_to_factor_(
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
      "var_r", "var_r_labeled", "var_s", "var_s_labeled", "var_t",
      "var_t_labeled", "var_t_false", "var_r_false",
      "var_r_new", "var_t_new"
    )
  )
  expect_factor_levels(df_new$var_r_new, c("F", "X", "D", "A", "B"))
  expect_factor_levels(df_new$var_r, c("A", "B", "X", "D", "F"))
  expect_factor_levels(df_new$var_t_new, c("V", "NAnew"))
  expect_factor_levels(df_new$var_t, c("V", "NAnew"))
  expect_identical(as.character(df_new$var_r), df_new$var_r_labeled)
  expect_identical(as.character(df_new$var_r_new), df_new$var_r_labeled)
  expect_identical(as.character(df_new$var_t_new), df_new$var_t_labeled)
  expect_identical(as.character(df_new$var_t), df_new$var_t_labeled)
  df_new_2 <- lama_to_factor_(
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
      "var_r", "var_r_labeled", "var_s", "var_s_labeled", "var_t",
      "var_t_labeled", "var_t_false", "var_r_false"
    )
  )
  expect_factor_levels(df_new_2$var_r, c("F", "X", "D", "A", "B"))
  expect_factor_levels(df_new_2$var_s, c("X", "Z"))
  expect_identical(as.character(df_new_2$var_r), df_new_2$var_r_labeled)
  expect_identical(as.character(df_new_2$var_s), df_new_2$var_s_labeled)
  df_new_3 <- lama_to_factor_(
    df,
    dict,
    translation = "var_t",
    col_new = "var_t_new"
  )
  expect_s3_class(df_new_3, "data.frame")
  expect_column_names(
    df_new_3, 
    c(
      "var_r", "var_r_labeled", "var_s", "var_s_labeled", "var_t",
      "var_t_labeled", "var_t_false", "var_r_false", "var_t_new"
    )
  )
  expect_factor_levels(df_new_3$var_t_new, c("V", "NAnew"))
})
# lama_to_factor errors
test_that("'lama_to_factor_' throws the right errors", {
  expect_error(
    lama_to_factor_(df, dict),
    "argument \"translation\" is missing"
  )
  expect_error(
    lama_to_factor_(
      df,
      dict,
      translation = c("var_t", "var_t", "var_t"),
      col_new = c("X", "x2", "X")
    ),
    "The argument 'col_new' is invalid: More than one relabeled variable was assigned to the same column name. The following column names have multiple assignments: 'X'."
  )
  expect_error(
    lama_to_factor_(
      df,
      dict,
      translation = c("asdf", "var_t", "blu"),
      col = c("dose", "var_t", "dose"),
      col_new = c("X", "Y", "Z")
    ),
    paste(
      "The following values of the argument 'translation' could not be",
      "found in the lama_dictionary object given in argument 'dictionary':",
      "'asdf', 'blu'."
    )
  )
  expect_error(
    lama_to_factor_(
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
    lama_to_factor_(
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
    lama_to_factor_(
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
    lama_to_factor_(
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
