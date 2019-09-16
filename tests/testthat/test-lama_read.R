context("'lama_read'")
test_that("'lama_read' works", {
  dict <- lama_read(system.file("extdata", "dictionary_exams.yaml", package = "labelmachine")) 
  expect_dictionary(dict)
  expect_translation_names(dict, c("sub", "res", "lev"))
  expect_translation_identical(dict, "sub", c(eng = "English", mat = "Mathematics", gym = "Gymnastics"))
  expect_translation_identical(dict, "res", c("1" = "Good", "2" = "Passed", "3" = "Not passed", "4" = "Not passed", NA_ = "Missed", "0" = NA))
  expect_translation_identical(dict, "lev", c(b = "Basic", a = "Advanced"))
})
test_that("'lama_read' throws the right errors", {
  expect_error(
    lama_read("bla.x"),
    paste(
      "Error while calling 'lama_read': cannot open file 'bla.x':",
      "No such file or directory"
    )
  )
  tmp <- tempfile(fileext = ".yaml")
  fileConn <- file(tmp)
    writeLines(c("sub:", "res:"), fileConn)
  close(fileConn)
  expect_error(
    lama_read(tmp),
    paste(
      "Error while calling 'lama_read': Error while initializing the",
      "lama_dictionary class object: The passed in translation definitions",
      "are invalid: Invalid translation with name 'sub': The object is not",
      "a valid named character vector.: NULL"
    )
  )
  fileConn <- file(tmp)
    writeLines(c("sub:", "  a: '1'", "res:"), fileConn)
  close(fileConn)
  expect_error(
    lama_read(tmp),
    paste(
      "Error while calling 'lama_read': Error while initializing the",
      "lama_dictionary class object: The passed in translation definitions",
      "are invalid: Invalid translation with name 'res': The object is not",
      "a valid named character vector.: NULL"
    )
  )
})
