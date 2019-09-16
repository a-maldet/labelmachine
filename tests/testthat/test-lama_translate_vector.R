context("lama_translate for factors and vectors")
dict = new_lama_dictionary(
  a = c(a = "A", b = "B", c = "X", d = "D", e = "X", f = "F"), 
  x = c(x = "X", y = NA, "NA_" = NA, z = "Z")
)
var_a = factor(
  c("c", "d", "d", "a", "a", "f", NA, "e"),
  levels = c("f", "e", "X1", "d", "c", "a", "X2")
)
var_a_labelled = c("X", "D", "D", "A", "A", "F", NA, "X")
var_x = factor(
  c("y", "y", NA, "x", "x", "y", "y", "x"),
  levels = c("X1", "y", "X2", "x", "X3")
)
var_x_labelled = c(NA, NA, NA, "X", "X", NA, NA, "X")
# lama_translate
test_that("'lama_translate' works unquoted", {
  a_new <- lama_translate(
    var_a,
    dict,
    a,
    keep_order = TRUE
  )
  x_new <- lama_translate(
    var_x,
    dict,
    x
  )
  expect_s3_class(a_new, "factor")
  expect_factor_levels(a_new, c("F", "X", "D", "A", "B"))
  expect_identical(as.character(a_new), var_a_labelled)
  expect_s3_class(x_new, "factor")
  expect_factor_levels(x_new, c("X", "Z"))
  expect_identical(as.character(x_new), var_x_labelled)
})
# lama_translate errors
test_that("'lama_translate' throws the right errors", {
  expect_error(
    lama_translate(list(a = 1), dict, a),
    "The argument '.data' must either be a data frame, a factor or an atomic vector."
  )
  expect_error(
    lama_translate(var_a, var_a, a),
    "The argument 'dictionary' must be a lama_dictionary class object."
  )
  expect_error(
    lama_translate(var_a, dict),
    paste(
      "The name of the used translation is missing",
      "\\(e.g. 'lama_translate\\(x, dict, my_trans\\)'\\)."
    )
  )
  expect_error(
    lama_translate(var_a, dict, a, keep_order = NA),
    paste(
      "The argument 'keep_order' must be a logical."
    )
  )
  expect_error(
    lama_translate(var_a, dict, a, keep_order = c(TRUE, TRUE)),
    paste(
      "The argument 'keep_order' must be a logical."
    )
  )
  expect_warning(
    lama_translate(var_a, dict, a, x),
    paste(
      "If the first element is a factor or an",
      "atomic vector, then only the arguments 'dictionary', a single argument",
      "for '...' \\(the unquoted translation name\\) and the argument",
      "'keep_order' are used and all extra arguments will be ignored."
    )
  )
})

# lama_translate_
test_that("'lama_translate_' works quoted", {
  a_new <- lama_translate_(
    var_a,
    dict,
    "a",
    keep_order = TRUE
  )
  x_new <- lama_translate_(
    var_x,
    dict,
    "x"
  )
  expect_s3_class(a_new, "factor")
  expect_factor_levels(a_new, c("F", "X", "D", "A", "B"))
  expect_identical(as.character(a_new), var_a_labelled)
  expect_s3_class(x_new, "factor")
  expect_factor_levels(x_new, c("X", "Z"))
  expect_identical(as.character(x_new), var_x_labelled)
})
# lama_translate errors
test_that("'lama_translate_' throws the right errors", {
  expect_error(
    lama_translate_(list(a = 1), dict, "a"),
    "The argument '.data' must either be a data frame, a factor or an atomic vector."
  )
  expect_error(
    lama_translate_(var_a, var_a, "a"),
    "The argument 'dictionary' must be a lama_dictionary class object."
  )
  expect_error(
    lama_translate_(var_a, dict, "a", keep_order = NA),
    paste(
      "The argument 'keep_order' must be a logical."
    )
  )
  expect_error(
    lama_translate_(var_a, dict, "a", keep_order = c(TRUE, TRUE)),
    paste(
      "The argument 'keep_order' must be a logical."
    )
  )
  expect_error(
    lama_translate_(var_a, dict, c("a", "x")),
    paste(
      "The argument 'translation' must be a character string."
    )
  )
})
