context("'lama_write'")
dict <- lama_read(system.file("extdata", "dictionary_exams.yaml", package = "labelmachine")) 
test_that("'lama_write' works", {
  tmp <- tempfile(fileext = ".yaml")
  lama_write(dict, tmp)
  expect_identical(file.exists(tmp), TRUE)
  dict_new <- lama_read(tmp)
  expect_dictionary(dict_new)
  expect_identical(dict_new, dict)
})
test_that("'lama_write' throws the right errors", {
  expect_error(
    lama_write(dict, "asdf/bla.x"),
    paste(
      "Error while calling 'lama_write': cannot open file 'asdf/bla.x':",
      "No such file or directory:"
    )
  )
})
