# labelmachine 1.0.0

* Implementation of the commands:
  * `new_lama_dictionary()`
  * `as.lama_dictionary()`
  * `lama_write()` and `lama_read()`
  * `lama_translate()` and `lama_translate_()` and `lama_translate_all()`
  * `lama_to_factor()` and `lama_to_factor_()` and `lama_to_factor_all()`
  * `lama_mutate()` and `lama_mutate_()`
  * `lama_select()` and `lama_select_()`
  * `lama_rename()` and `lama_rename_()`
  * `lama_merge()`
  * various helper functions:
    * `is.lama_dictionary()`
    * `validate_lama_dictionary()`
    * `lama_get()` and `lama_get_()`
* Major refactoring of S3 methods.
* 100% meaningful error messages.
* Fully documented.
* Full continuous integration (`travis`, `github.io`, `codecov`).
* Test coverage 85%.
* First CRAN release.
