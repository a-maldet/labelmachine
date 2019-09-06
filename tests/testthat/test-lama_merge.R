context("lama_merge")
dict_a = new_dictionary(
  x = c(x = "a"), 
  y = c(y = "a"),
  z = c(z = "a")
)
dict_b = new_dictionary(
  u = c(u = "b"),
  v = c(v = "b"),
  y = c(y = "b"),
  z = c(z = "b")
)
dict_c = new_dictionary(
  z = c(z = "c"),
  w = c(w = "c"),
  u = c(u = "c")
)
# lama_merge
test_that("'lama_merge' works", {
  dict_new <- expect_warning(
    lama_merge(dict_a, dict_b, dict_c),
    "The following lama_dictionary entries will be overwritten: 'y', 'z', 'u'."
  )
  expect_translation_names(dict_new, c("u", "v", "w", "x", "y", "z"))
  expect_translation_identical(dict_new, "u", c(u = "c"))
  expect_translation_identical(dict_new, "v", c(v = "b"))
  expect_translation_identical(dict_new, "w", c(w = "c"))
  expect_translation_identical(dict_new, "x", c(x = "a"))
  expect_translation_identical(dict_new, "y", c(y = "b"))
  expect_translation_identical(dict_new, "z", c(z = "c"))
})
# lama_merge warnings
test_that("'lama_merge' throws the right warnings", {
  expect_warning(
    lama_merge(dict_a, dict_b, dict_c),
    "The following lama_dictionary entries will be overwritten: 'y', 'z', 'u'."
  )
  expect_warning(
    lama_merge(dict_a, dict_b, dict_c),
    "The following lama_dictionary entries will be overwritten: 'y', 'z', 'u'."
  )
  expect_warning(
    lama_merge(dict_a, dict_b, dict_c, show_warnings = FALSE),
    regexp = NA
  )
})
# lama_merge errors
test_that("'lama_merge' throws the right errors", {
  expect_error(
    lama_merge(dict_a),
    "There must be at least two lama_dictionary class object passed into."
  )
  expect_error(
    lama_merge(dict_a, dict_b, list(a = 1)),
    "Invalid argument at position '3': Object is not a lama_dictionary class object."
  )
  expect_error(
    lama_merge(dict_a, dict_b, show_warnings = NA),
    "The value passed into 'show_warnings' must be either 'TRUE' or 'FALSE'."
  )
})
