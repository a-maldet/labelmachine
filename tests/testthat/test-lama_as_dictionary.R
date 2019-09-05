context("as.dictionary")
# as.dictionary.LamaDictionary
test_that("as.dictionary takes LamaDictionary objects", {
  dict <- as.dictionary(new_dictionary(a = c(a = "A", b = NA, NA_ = "C"), x = list(x = "X", NA_ = NA)))
  expect_dictionary(dict)
  expect_translation_names(dict, c("a", "x"))
  expect_translation_identical(dict, "a", c(a = "A", b = NA, NA_ = "C"))
  expect_translation_identical(dict, "x", c(x = "X", NA_ = NA))
})
# as_dictionary.list
test_that("as.dictionary takes list objects", {
  dict <- as.dictionary(list(a = c(a = "A", b = NA, NA_ = "C"), x = list(x = "X", NA_ = NA)))
  expect_dictionary(dict)
  expect_translation_names(dict, c("a", "x"))
  expect_translation_identical(dict, "a", c(a = "A", b = NA, NA_ = "C"))
  expect_translation_identical(dict, "x", c(x = "X", NA_ = NA))
})
test_that("throws the right errors", {
  expect_error(
    as.dictionary(list(c(a = "A"), x = c(x = "X"))),
    paste(
      "The passed in translation definitions are invalid:",
      "The names of the translations are missing."
    ),
    fixed = TRUE
  )
  expect_error(
    as.dictionary(list(x = c(a = "A"), x = c(x = "X"))),
    paste(
      "The passed in translation definitions are invalid:",
      "The following translation names are used more than once: 'x'."
    ),
    fixed = TRUE
  )
  expect_error(
    as.dictionary(list(a = "A", x = c(x = "X"))),
    paste(
      "The passed in translation definitions are invalid:",
      "Invalid translation with name 'a':",
      "The object is not a valid named character vector."
    ),
    fixed = TRUE
  )
  expect_error(
    as.dictionary(list(a = c(a = "A"), x = list("X"))),
    paste(
      "The passed in translation definitions are invalid:",
      "Invalid translation with name 'x':",
      "The object must be a named character vector holding the variable translations."
    ),
    fixed = TRUE
  )
  expect_error(
    as.dictionary(list(a = 3, x = c(x = "X"))),
    paste(
      "The passed in translation definitions are invalid:",
      "Invalid translation with name 'a':",
      "The object is not a valid named character vector."
    ),
    fixed = TRUE
  )
  expect_error(
    as.dictionary(list(a = 3, list = c("X"))),
    paste(
      "The passed in translation definitions are invalid:",
      "Invalid translation with name 'a':",
      "The object is not a valid named character vector."
    ),
    fixed = TRUE
  )
})

# as_dictionary.list
test_that("LamaDictionary objects are allowed", {
  dict <- as.dictionary(new_dictionary(a = c(a = "A", b = NA, NA_ = "c"), x = list(x = "X", NA_ = NA)))
  expect_dictionary(dict)
  expect_translation_names(dict, c("a", "x"))
  expect_translation_identical(dict, "a", c(a = "A", b = NA, NA_ = "c"))
  expect_translation_identical(dict, "x", c(x = "X", NA_ = NA))
})
test_that("throws the right errors", {
  expect_error(
    as.dictionary(list(c(a = "A"), x = c(x = "X"))),
    paste(
      "The passed in translation definitions are invalid:",
      "The names of the translations are missing."
    ),
    fixed = TRUE
  )
  expect_error(
    as.dictionary(list(x = c(a = "A"), x = c(x = "X"))),
    paste(
      "The passed in translation definitions are invalid:",
      "The following translation names are used more than once: 'x'."
    ),
    fixed = TRUE
  )
  expect_error(
    as.dictionary(list(a = "A", x = c(x = "X"))),
    paste(
      "The passed in translation definitions are invalid:",
      "Invalid translation with name 'a':",
      "The object is not a valid named character vector."
    ),
    fixed = TRUE
  )
  expect_error(
    as.dictionary(list(a = c(a = "A"), x = list("X"))),
    paste(
      "The passed in translation definitions are invalid:",
      "Invalid translation with name 'x':",
      "The object must be a named character vector holding the variable translations."
    ),
    fixed = TRUE
  )
  expect_error(
    as.dictionary(list(a = 3, x = c(x = "X"))),
    paste(
      "The passed in translation definitions are invalid:",
      "Invalid translation with name 'a':",
      "The object is not a valid named character vector."
    ),
    fixed = TRUE
  )
  expect_error(
    as.dictionary(list(a = 3, list = c("X"))),
    paste(
      "The passed in translation definitions are invalid:",
      "Invalid translation with name 'a':",
      "The object is not a valid named character vector."
    ),
    fixed = TRUE
  )
})

# as_dictionary.data.frame
test_that("as.dictionary takes data.frames", {
  translations <- c("aR", "aO", "aN", "xR", "xO", "xN", "rR", "rO", "rN")
  df <- data.frame(
    a_o = c("b", "a", "NA_", "d"),
    a_n = c("D", "E", "NAnew", "D"),
    x_o = factor(c(NA,  "x", "z", "y"), levels = c("y", "z", "x")),
    x_n = factor(c(NA,  "X", "Z", NA), levels = c("Z", "X")),
    r_o = factor(c("r", "s", "t", NA), levels = c("s", "t", "r")),
    r_n = factor(c("R", "S", "T", "NAnew"), levels = c("T", "NAnew", "R", "S")),
    stringsAsFactors = FALSE
  )
  dict <- as.dictionary(
    df,
    translation = translations,
    col_old = rep(c("a_o", "x_o", "r_o"), each = 3),
    col_new = rep(c("a_n", "x_n", "r_n"), each = 3),
    ordering = rep(c("row", "old", "new"), 3)
  )
  expect_dictionary(dict)
  expect_translation_names(dict, translations)
  ### Caution alphanumerical sorting 'base::order' depends on C_LOCALE. Better do not use fixed compare values
  ### The result of base::order differs from OS to OS
  temp <- df$a_n
  names(temp) <- df$a_o
  expect_translation_identical(dict, "aR", temp)
  temp <- df$a_n[order(df$a_o)]
  names(temp) <- df$a_o[order(df$a_o)]
  expect_translation_identical(dict, "aO", temp)
  temp <- df$a_n[order(df$a_n)]
  names(temp) <- df$a_o[order(df$a_n)]
  expect_translation_identical(dict, "aN", temp)
  expect_translation_identical(dict, "xR", c(x = "X", z = "Z", y = NA))
  expect_translation_identical(dict, "xO", c(y = NA,  z = "Z", x = "X"))
  expect_translation_identical(dict, "xN", c(z = "Z", x = "X", y = NA))
  expect_translation_identical(dict, "rR", c(r = "R", s = "S", t = "T", NA_ = "NAnew"))
  expect_translation_identical(dict, "rO", c(s = "S", t = "T", r = "R", NA_ = "NAnew"))
  expect_translation_identical(dict, "rN", c(t = "T", NA_ = "NAnew", r = "R", s = "S"))
  dict_2 <- as.dictionary(
    data.frame(
      o = c(NA, "a", "b"),
      n = 1:3
    ),
    translation = "a",
    col_old = "o",
    col_new = "n"
  )
  expect_dictionary(dict_2)
  expect_translation_names(dict_2, "a")
  expect_translation_identical(dict_2, "a", c(NA_ = "1", a = "2", "b" = "3"))
})

test_that("throws the right errors", {
  expect_error(
    as.dictionary(
      data.frame(
        o = c("a", "b"),
        n = c("A", "B")
      )
    ),
    paste(
      "argument \"translation\" is missing"
    )
  )
  expect_error(
    as.dictionary(
      data.frame(
        o = c("a", "b"),
        n = c("A", "B")
      ),
      translation = "a"
    ),
    paste(
      "argument \"col_old\" is missing"
    )
  )
  expect_error(
    as.dictionary(
      data.frame(
        o = c("a", "b"),
        n = c("A", "B")
      ),
      translation = "a",
      col_old = "o"
    ),
    paste(
      "argument \"col_new\" is missing"
    )
  )
  expect_error(
    as.dictionary(
      data.frame(
        o = c("a", "b"),
        n = c("A", "B")
      ),
      translation = "a",
      col_old = "o",
      col_new = 2
    ),
    paste(
      "Invalid argument 'col_new':",
      "Object must be a character vector."
    )
  )
  expect_error(
    as.dictionary(
      data.frame(
        o = c("a", "b"),
        n = c("A", "B")
      ),
      translation = "a",
      col_old = 2,
      col_new = "n"
    ),
    paste(
      "Invalid argument 'col_old':",
      "Object must be a character vector."
    )
  )
  expect_error(
    as.dictionary(
      data.frame(
        o = c("a", "b"),
        n = c("A", "B")
      ),
      translation = "a",
      col_old = c("o", "X"),
      col_new = "n"
    ),
    paste(
      "Invalid argument 'col_old':",
      "Object must be of the same length as the character vector in argument 'translation'."
    )
  )
  expect_error(
    as.dictionary(
      data.frame(
        o = c("a", "b"),
        n = c("A", "B")
      ),
      translation = "a",
      col_old = c("o", "X"),
      col_new = c("n", "X")
    ),
    paste(
      "Object must be of the same length as the character vector in argument 'translation'."
    )
  )
  expect_error(
    as.dictionary(
      data.frame(
        o = c("a", "b"),
        n = c("A", "B")
      ),
      translation = "a",
      col_old = "o",
      col_new = c("n", "X")
    ),
    paste(
      "Invalid argument 'col_new':",
      "Object must be of the same length as the character vector in argument 'translation'."
    )
  )
  expect_error(
    as.dictionary(
      data.frame(
        o = c("a", "b"),
        n = c("A", "B")
      ),
      translation = "a",
      col_old = "o",
      col_new = "n",
      ordering = c("row", "row")
    ),
    paste(
      "Invalid argument 'ordering':",
      "Object must be of the same length as the character vector in argument 'translation'."
    )
  )
  expect_error(
    as.dictionary(
      data.frame(
        o = c("a", "b"),
        n = c("A", "B")
      ),
      translation = "a",
      col_old = "o",
      col_new = "n",
      ordering = 3
    ),
    paste(
      "Invalid argument 'ordering':",
      "Object must be a character vector."
    )
  )
  expect_error(
    as.dictionary(
      data.frame(
        o = c("a", "b"),
        n = c("A", "B")
      ),
      translation = "a",
      col_old = "o",
      col_new = "n",
      ordering = "X"
    ),
    paste(
      "Invalid argument 'ordering':",
      "Each item of 'ordering' must be one of the following strings: 'row', 'old', 'new'."
    )
  )
  expect_error(
    as.dictionary(
      data.frame(
        o = c("a", "b"),
        n = c("A", "B")
      ),
      translation = "a",
      col_old = "x",
      col_new = "n"
    ),
    paste(
      "Invalid argument 'col_old':",
      "The following entries are no valid column names in the data.frame",
      "given in '.data': 'x'.\nOnly",
      "the following column names are available: 'o', 'n'."
    )
  )
  expect_error(
    as.dictionary(
      data.frame(
        o = c("a", "b"),
        n = c("A", "B")
      ),
      translation = "a",
      col_old = "o",
      col_new = "x"
    ),
    paste(
      "Invalid argument 'col_new':",
      "The following entries are no valid column names in the data.frame",
      "given in '.data': 'x'.\nOnly",
      "the following column names are available: 'o', 'n'."
    )
  )
  expect_error(
    as.dictionary(
      data.frame(
        o = c("a", "b"),
        n = c("A", "B")
      ),
      translation = c("a", "1"),
      col_old = c("o", "o"),
      col_new = c("n", "n")
    ),
    paste(
      "Invalid argument 'translation':",
      "The following given translation names are not valid names: '1'."
    )
  )
  expect_error(
    as.dictionary(
      data.frame(
        o = c("a", "a", "b"),
        n = c("A", "A", "B")
      ),
      translation = "a",
      col_old = "o",
      col_new = "n"
    ),
    paste(
      "Error while creating translation 'a' which assigns the new labels given",
      "in column 'n' to the original values given in column 'o':",
      "The following original values given in column 'o' appear more than once: 'a'."
    )
  )
  expect_error(
    as.dictionary(
      data.frame(
        o = c(NA, NA, "b"),
        n = c("A", "A", "B")
      ),
      translation = "a",
      col_old = "o",
      col_new = "n"
    ),
    paste(
      "Error while creating translation 'a' which assigns the new labels given",
      "in column 'n' to the original values given in column 'o':",
      "The following original values given in column 'o' appear more than once: 'NA'."
    )
  )
})
