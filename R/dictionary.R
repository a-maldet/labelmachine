#' NA replace string
#'
#' In order to replace `NA` values in yaml files and in translations
#' the following character string is used
NA_lama_ <- "NA_"

#' Create a new LabelDictionary class object
#'
#' Generates an _S3_ class object, which holds the _variable translations_.
#' There are three valid ways to use `new_dictionary` in order to create a
#' LableDictionary class object:
#'  * _No arguments_ were passed into `...`: In this case `new_dictionary`
#'    returns an empty LabelDictionary class object (e.g. `dict <- new_dictionary()`).
#'  * _The first argument is a list_: In this case only the first argument of 
#'    `new_dictionary` is used. It is not necessary to pass in a named argument.
#'    The passed in object must be a _named list object_, which contains all 
#'    translations that should be added to the new LabelDictionary class object.
#'    Each item of the named list object must be a _named character vector_ defining a translation 
#'    (e.g. `new_dictionary(list(area = c("0" = "urban", "1" = "rural"),  = c(l = "Low", h = "High")))`
#'    generates a LabelDictionary class object holding the translations `"area"` and `"density"`).
#'  * _The first argument is a character vector_: In this case, it is allowed to pass in
#'    _more than one argument_. In this case, all given arguments must be _named arguments_
#'    holding _named character vectors_ defining translations
#'    (e.g. `new_dictionary(area = c("0" = "urban", "1" = "rural"), density = c(l = "Low", h = "High"))`
#'    generates a LabelDictionary class object holding the translations `"area"` and `"density"`).
#'    The names of the passed in arguments will be used as the names,
#'    under which the given translations
#'    will be added to the new LabelDictionary class object. 
#'
#' @section Translations:
#' A _translation_ is a _named character vector_ of non zero length. 
#' This named character vector defines
#' which labels (of type character) should be assigned to which values
#' (can be of type character, logical or numeric)
#' (e.g. the translation `c("0" = "urban", "1" = "rural")` assigns the label
#' `"urban"` to the value `0` and `"rural"` to the value `1`, for example the
#' variable `x = c(0, 0, 1)` is translated to `x_new = c("urban", "urban", "rural")`).
#' Therefore, a translation (named character vector) contains the following information:
#' * The _names_ of the character vector entries correspond to the
#'   _original variable levels_.
#'   Variables of types `numeric` or `logical` are turned automatically into a
#'   character vector (e.g. `0` and `1` are treated like `"0"` and `"1"`).
#' * The _entries_ (character strings) of the character vector correspond to
#'   the new _labels_, which will be assigned to the original variable levels.
#'   It is also allowed to have missing labels (`NA`s).
#'   In this case, the original values are mappend to missing values.
#' 
#' The function [lama_translate()] is used in order to apply a translation on a variable.
#' The resulting vector with the assigned labels can be of the following types:
#' * _character_: An unordered vector holding the new character labels.
#' * _factor_ with character levels: An ordered vector holding the new character labels.
#'
#' The original variable can be of the following types:
#' * _character_ vector: This is the simplest case. The character values
#'   will replaced by the corresponding labels.
#' * _numeric_ or _logical_ vector: Vectors of type _numeric_ or _logical_
#'   will be turned
#'   into _character_ vectors automatically before the translation process and
#'   then simply processed like in the _character_ case.
#'   Therefore, it is sufficient to define the translation mapping for the
#'   _character_ case, since it also covers the _numeric_ and _logical_ case.
#' * _factor_ vector with levels of any type: When translating factor variables 
#'   one can decide whether or not to keep the original ordering. Like in the
#'   other cases the levels of the factor variable will always be turned into
#'   character strings before the translation process.
#'
#' @section Missing values:
#' It is also possible to handle missing values with [lama_translate()].
#' Therefore, the used translation must contain a information that tells how
#' to handle a missing value. In order to define such a translation 
#' the missing value (`NA`) can be escaped with the character string `"NA_"`.  
#' This can be useful in two sitations:
#' * All missing values should be labelled
#'   (e.g. the translation `c("0" = "urban", "1" = "rural", NA_ = "missing")`
#'   assigns the character string `"missing"` to all missing values of a variable).
#' * Map some original values to `NA`
#'   (e.g. the translation `c("0" = "urban", "1" = "rural", "2" = "NA_", "3" = "NA_")`
#'   assigns `NA` (the missing character) to the original values `2` and `3`).
#'   Actually, in this case the translation definition does not always have to
#'   use this escape mechanism, but only
#'   when defining the translations inside of a `YAML` file,
#'   since the `YAML` parser does not recognice missing values.
#'
#' @section LabelDictionary class objects:
#' Each _LabelDictionary_ class object can contain multiple _translations_,
#' each with a unique name under which the translation can be found.
#' The function [lama_translate()] uses a LabelDictionary class object
#' to translate a normal `vector` or to translate one or more columns in a
#' `data.frame`.
#' Sometimes it may be necessary to have different translations
#' for the same variable, in this case it is best to have multiple
#' translations with different names
#' (e.g. `area_short = c("0" = "urb", "1" = "rur")` and
#' `area = c("0" = "urban", "1" = "rural")`).
#' 
#' @param ... None, one or more named/unnamed arguments. Depending on the type of
#'  the type of the first argument passed into `new_dictionary`,
#'  there are different valid ways of using `new_dictionary`:
#'  * _No arguments_ were passed into `...`: In this case `new_dictionary`
#'    returns an empty LabelDictionary class object (e.g. `dict <- new_dictionary()`).
#'  * _The first argument is a list_: In this case, only the first argument of 
#'    `new_dictionary` is used and it is allowed to use an unnamed argument call.
#'    Furthermore, the passed in object must be a named list object, which contains all 
#'    translations that should be added to the new LabelDictionary class object.
#'    Each item of the named list object must be a named character vector defining a translation 
#'    (e.g. `new_dictionary(list(area = c("0" = "urban", "1" = "rural"),  = c(l = "Low", h = "High")))`
#'    generates a LabelDictionary class object holding the translations `"area"` and `"density"`).
#'  * _The first argument is a character vector_: In this case, it is allowed to pass in
#'    more than one argument, but all given arguments when calling `new_directory`
#'    must be _named arguments_ and each argument must be 
#'    a named character vectors defining translations
#'    (e.g. `new_dictionary(area = c("0" = "urban", "1" = "rural"), density = c(l = "Low", h = "High"))`
#'    generates a LabelDictionary class object holding the translations `"area"` and `"density"`).
#'    The names of the caller arguments will be used as names under which the given translations
#'    will be added to the new LabelDictionary class object. 
#' @return A new LabelDictionary class object holding the passed in translations.
#' @seealso [is.dictionary()], [lama_translate()], [lama_read()], [lama_write()], [lama_select()], [lama_rename()], [lama_mutate()], [lama_merge()]
#' @rdname new_dictionary
#' @export
#' @include utilities.R
new_dictionary <- function(...) {
  UseMethod("new_dictionary")
}

#' @param .data A named list object, where each list entry corresponds to a
#'   translation that should be added to the LabelDictionary object
#'   (e.g. `new_dictionary(list(area = c("0" = "urban", "1" = "rural"),  = c(l = "Low", h = "High")))`
#'   generates a LabelDictionary class object holding the translations `"area"` and `"density"`).
#'   The names of the list entries are the names under which the translation 
#'   will be added to the new LabelDictionary class object (e.g. `area` and `density`).
#'   Each list entry must be a named character vector defining a translation
#'   (e.g. `c("0" = "urban", "1" = "rural")` is the translation with
#'   the name `area` and `c(l = "Low", h = "High")` is the translation
#'   with the name `density`).
#' @rdname new_dictionary
#' @export
new_dictionary.list <- function(.data, ...) {
  if (is.null(.data) || (is.list(.data) && length(.data) == 0)) {
    .data <- list()
  } else {
    # Check the '.data' argument
    err_handler <- composerr(paste(
      "Error while initializing the LabelDictionary class object:",
      "The passed in translation definitions are invalid"
    ))
    if (!is.list(.data) || is.null(names(.data)) || any(names(.data) == ""))
      err_handler("The names of the translations are missing.")
    .data <- named_lapply(names(.data), function(var_name) {
      err_handler <- composerr_parent(
        paste0("Invalid translation object with name '", var_name, "'"),
        err_handler
      )
      translation <- .data[[var_name]]
      if (is.list(translation)) {
        # a translation can also be given as a named list, where each item
        # is a character of length 1
        invalid <- sapply(translation, function(x) !is.character(x) || length(x) != 1)
        if (any(invalid) || is.null(names(translation)) || any(names(translation) == ""))
          err_handler("The element must be a named character vector holding the variable translations.")
        translation <- unlist(translation)
      }
      if (!is.character(translation) || is.null(names(translation)) || any(names(translation) == ""))
        err_handler("The element must be a named character vector holding the variable translations.")
      escape_to_na(translation)
    })
  }
  structure(.data, class = "LabelDictionary")
}

#' @rdname new_dictionary
#' @export
new_dictionary.character <- function(...) {
  new_dictionary.list(list(...))
}

#' @rdname new_dictionary
#' @export
new_dictionary.default <- function(...) {
  new_dictionary.list(list(...))
}

#' Coerce to a [LabelDictionary][new_dictionary()] class object
#'
#' This function allows two types of arguments:
#' * _named list_: A named list object holding the translations.
#' * _data.frame_: A data.frame with one ore more column pairs. Each column
#'   pair consists of a column holding the original values, which should be relapced,
#'   and a second charachter column holding the new labels which should be
#'   assigned to the original values. Use the arguments `col_old` and `col_new`
#'   in order to define which columns are holding original values and which 
#'   columns hold the new labels. The names of the resulting translations
#'   are defined by a character vector given in argument `translation`.
#'   Furthermore, each translation can have a different ordering which can be
#'   configured by a character vector given in argument `ordering`.
#' @param .data An object holding the translations. `.data` can be of the
#'   following data types:
#'   * _named list_: A named list object, where each list element is a translation
#'     (a named character vector)
#'   * _data.frame_: A data.frame holding one or more column pairs,
#'     where each column pair consists of
#'     one column holding the original variable values and
#'     a second column holding the new labels,
#'     which should be assigned to the original values.
#' @param ... Various arguments, depending on the data type of `.data`.
#' @return A new LabelDictionary class object holding the passed in translations.
#' @inheritSection new_dictionary Translations
#' @inheritSection new_dictionary LabelDictionary class objects
#' @rdname as_dictionary
#' @export
as.dictionary <- function(.data, ...) {
  UseMethod("as.dictionary")
}

#' @rdname as_dictionary
#' @export
as.dictionary.list <- function(.data) {
  new_dictionary(.data)
}

#' @rdname as_dictionary
#' @export
as.dictionary.default <- function(...) {
  stop("Error while calling `as.dictionary`: The supplied argument must either be a list or a data.frame object")
}

#' @param translation A character vector holding the names of all translations
#' @param col_old A character vector (same length as `translation`) holding the
#'   names of the columns in the data.frame (in the argument `.data`) which hold
#'   the original variable values.
#'   These columns can be of any type: `character`, `logical`, `numerical` or `factor`.
#' @param col_new A character vector (same length as `translation`) holding the
#'   names of the columns in the data.frame (in the argument `.data`) which hold
#'   the new labels, which should be assigend to the original values.
#'   These columns can be `character` vectors or `factors` with character labels.
#' @rdname as_dictionary
#' @export
as.dictionary.data.frame <- function(
  .data, translation, col_old, col_new, ordering = "row" 
) {
  err_handler <- composerr("Error while calling 'as.dictionary'")
  if (!is.character(translation) || length(translation) == 0 || any(is.na(translation)))
    err_handler("Argument 'translation' must be a character vector holding the names of the translations.")
  duplicates <- table(translation)
  duplicates <- names(duplicates[duplicates > 1])
  if (length(duplicates) > 0)
    err_handler(paste0(
      "The following translation names given in argument 'translation' are used ",
      "more than once: ",
      stringify(duplicates),
      "."
    ))
  if (!is.character(col_old) || length(col_old) == length(translation) || any(is.na(col_old)))
    err_handler(paste(
      "Argument 'col_old' must be a character vector holding the names of the",
      "data.frame columns, which contain the original values."
    ))
  invalid <- !col_old %in% colnames(.data)
  if (any(invalid))
    err_handler(paste0(
      "The following column names given in argument 'col_old' could not ",
      "be found in the data.frame given in argument '.data': ",
      stringify(col_old[invalid]),
      ".\nOnly the following column names are available: ",
      stringify(colnames(.data)),
      "."
    ))
  if (!is.character(col_new) || length(col_new) == length(translation) || any(is.na(col_new)))
    err_handler(paste(
      "Argument 'col_new' must be a character vector holding the names of the",
      "data.frame columns, which contain the new labels which will be",
      "assigend to the original values."
    ))
  invalid <- !col_new %in% colnames(.data)
  if (any(invalid))
    err_handler(paste0(
      "The following column names given in argument 'col_new' could not ",
      "be found in the data.frame given in argument '.data': ",
      stringify(col_new[invalid]),
      ".\nOnly the following column names are available: ",
      stringify(colnames(.data)),
      "."
    ))
  if (is.null(ordering))
    ordering <- "row"
  if (is.character(ordering) && length(ordering) == 1)
    ordering <- rep(ordering, length(translation))
  if (
    !is.character(ordering) || length(ordering) != length(translation) ||
      any(!ordering %in% c("row", "old", "new"))
  )
    err_handler(paste(
      "Argument 'ordering' must be a character vector of length 1 or",
      "the same length as the vector in argument 'translation'.",
      "Each item of 'ordering' must be one of the following strings: ",
      stringify(c("row", "old", "new")),
      "."
    ))
  # create list holding the translations
  new_dictionary(named_lapply(
    translation,
    function(translation, i) {
      old <- old[i]
      new <- new[i]
      err_handler <- composerr_parent(
        paste(
          "Error while creating translation",
          stringify(translation),
          "which assigns the new labels given in column",
          stringify(new),
          "to the original values given in column",
          stringify(old)
        ),
        err_handler
      )
      if (ordering[i] == "old") {
        .data <- .data[ordering(.data[[old]]),]
      } else if (ordering[i] == "new") {
        .data <- .data[ordering(.data[[new]]),]
      }
      val_old <- .data[[old]]
      if (is.factor(val_old))
        val_old <- as.character(val_old)
      val_old <- na_to_escape(val_old)
      val_new <- .data[[new]]
      if (is.factor(val_new))
        val_new <- as.character(val_new)
      val_new <- escape_to_na(val_new)
      duplicates <- table(val_old)
      duplicates <- names(duplicates[duplicates > 1])
      duplicates[duplicates == NA_lama_] <- "NA"
      if (length(duplicates) > 0)
        err_handler(paste0(
          "The following original values given in column ",
          stringify(old),
          " appear more than once: ",
          stringify(duplicates),
          "."
        ))
      names(val_new) <- val_old
      val_new
    }
  ))
}

#' Check if an object is a [LabelDictionary][new_dictionary()] class object
#'
#' @param obj The object in question
#' @return \code{TRUE} if the object is a
#' [LabelDictionary][new_dictionary()] class object, \code{FALSE} otherwise.
#' @rdname is_dictionary
#' @seealso [validate_dictionary()], [as.dictionary()], [new_dictionary()],
#' [lama_translate()], [lama_read()], [lama_write()], [lama_select()],
#' [lama_rename()], [lama_mutate()], [lama_merge()]
#' @export
is.dictionary <- function(obj) {
  inherits(obj, "LabelDictionary")
}

#' Check if an object has a valid [LabelDictionary][new_dictionary()] structure
#'
#' This function checks if the object structure is right. It does not check 
#' class type.
#' @inheritSection new_dictionary Translations
#' @inheritSection new_dictionary Missing values
#' @inheritSection new_dictionary LabelDictionary class objects
#' @param obj An object that should be tested
#' @param err_handler An error handling function
#' @rdname validate_dictionary
#' @seealso [is_dictionary()], [as.dictionary()], [new_dictionary()],
#' [lama_translate()], [lama_read()], [lama_write()], [lama_select()],
#' [lama_rename()], [lama_mutate()], [lama_merge()]
#' @export
validate_dictionary <- function(
  obj,
  err_handler = composerr("The object has not a valid LabelDictionary structure")
) {
  err_handler <- 
  if (!is.list(obj))
    err_handler("The object must be a named list object.")
  translations <- names(obj)
  if (
      !is.character(translations) || length(translations) == 0 ||
        any(is.na(translations)) || any(translations == "")
  )
    err_handler(paste(
      "The object must be a named list object",
      "with non empty list entry names."
    ))
  lapply(
    seq_len(length(obj)),
    function(i) {
      x <- obj[[i]]
      err_handler <- composerr_parent(
        paste0("'obj$", translations[i], "' is not a valid translation"),
        err_handler
      )
      validate_translation(x, err_handler)
    }
  )
  obj
}

#' Check if an object has a valid translation structure
#'
#' This function checks if the object structure is that of a translation (named character vector). 
#' @param obj An object that should be tested
#' @param err_handler An error handling function
validate_translation <- function(
  obj,
  err_handler = composerr("The object has not a valid translation structure")
) {
  if (
    !is.character(x) || length(names(x)) == 0 || any(is.na(names(x))) ||
      any(names(x) == "")
  )
    err_handler("The element is not a valid named character vector.")
  duplicates <- table(names(x))
  duplicates <- names(duplicates[duplicates > 1])
  if (length(duplicates) > 0)
    err_handler(paste0(
      "The element has duplicate names. The following original values ",
      "are defined more than once: ",
      stringify(duplicates),
      "."
    ))
  obj
}



#' Print a [LabelDictionary][new_dictionary()] class object
#'
#' @param x The [LabelDictionary][new_dictionary()] class object that should be printed.
#' @rdname print
#' @seealso [lama_translate()], [new_dictionary()], [lama_select()], [lama_rename()], [lama_mutate()], [lama_merge()], [lama_read()], [lama_write()]
#' @method print LabelDictionary
#' @export
print.LabelDictionary <- function(x) {
  cat("\n--- LabelDictionary ---\n")
  for (name in names(x)) {
    cat(paste0("Variable '", name, "':\n"))
    print(x[[name]])
    cat("\n")
  }
}
