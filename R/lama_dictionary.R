#' NA replace string
#'
#' In order to replace `NA` values in yaml files and in translations
#' the following character string is used
NA_lama_ <- "NA_"

#' Create a new lama_dictionary class object
#'
#' Generates an _S3_ class object, which holds the _variable translations_.
#' There are three valid ways to use `new_lama_dictionary` in order to create a
#' `lama_dictionary` class object:
#'  * _No arguments_ were passed into `...`: In this case `new_lama_dictionary`
#'    returns an empty lama_dictionary class object (e.g. `dict <- new_lama_dictionary()`).
#'  * _The first argument is a list_: In this case only the first argument of 
#'    `new_lama_dictionary` is used. It is not necessary to pass in a named argument.
#'    The passed in object must be a _named list object_, which contains all 
#'    translations that should be added to the new lama_dictionary class object.
#'    Each item of the named list object must be a _named character vector_ defining a translation 
#'    (e.g. `new_lama_dictionary(list(area = c("0" = "urban", "1" = "rural"),  = c(l = "Low", h = "High")))`
#'    generates a lama_dictionary class object holding the translations `"area"` and `"density"`).
#'  * _The first argument is a character vector_: In this case, it is allowed to pass in
#'    _more than one argument_. In this case, all given arguments must be _named arguments_
#'    holding _named character vectors_ defining translations
#'    (e.g. `new_lama_dictionary(area = c("0" = "urban", "1" = "rural"), density = c(l = "Low", h = "High"))`
#'    generates a lama_dictionary class object holding the translations `"area"` and `"density"`).
#'    The names of the passed in arguments will be used as the names,
#'    under which the given translations
#'    will be added to the new lama_dictionary class object. 
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
#'   In this case, the original values are mapped onto missing values.
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
#' This can be useful in two situations:
#' * All missing values should be labeled
#'   (e.g. the translation `c("0" = "urban", "1" = "rural", NA_ = "missing")`
#'   assigns the character string `"missing"` to all missing values of a variable).
#' * Map some original values to `NA`
#'   (e.g. the translation `c("0" = "urban", "1" = "rural", "2" = "NA_", "3" = "NA_")`
#'   assigns `NA` (the missing character) to the original values `2` and `3`).
#'   Actually, in this case the translation definition does not always have to
#'   use this escape mechanism, but only
#'   when defining the translations inside of a `YAML` file,
#'   since the `YAML` parser does not recognize missing values.
#'
#' @section lama_dictionary class objects:
#' Each _lama_dictionary_ class object can contain multiple _translations_,
#' each with a unique name under which the translation can be found.
#' The function [lama_translate()] uses a lama_dictionary class object
#' to translate a normal `vector` or to translate one or more columns in a
#' `data.frame`.
#' Sometimes it may be necessary to have different translations
#' for the same variable, in this case it is best to have multiple
#' translations with different names
#' (e.g. `area_short = c("0" = "urb", "1" = "rur")` and
#' `area = c("0" = "urban", "1" = "rural")`).
#' 
#' @param ... None, one or more named/unnamed arguments. Depending on the type of
#'  the type of the first argument passed into `new_lama_dictionary`,
#'  there are different valid ways of using `new_lama_dictionary`:
#'  * _No arguments_ were passed into `...`: In this case `new_lama_dictionary`
#'    returns an empty lama_dictionary class object (e.g. `dict <- new_lama_dictionary()`).
#'  * _The first argument is a list_: In this case, only the first argument of 
#'    `new_lama_dictionary` is used and it is allowed to use an unnamed argument call.
#'    Furthermore, the passed in object must be a named list object, which contains all 
#'    translations that should be added to the new lama_dictionary class object.
#'    Each item of the named list object must be a named character vector defining a translation 
#'    (e.g. `new_lama_dictionary(list(area = c("0" = "urban", "1" = "rural"),  = c(l = "Low", h = "High")))`
#'    generates a lama_dictionary class object holding the translations `"area"` and `"density"`).
#'  * _The first argument is a character vector_: In this case, it is allowed to pass in
#'    more than one argument, but all given arguments when calling `new_directory`
#'    must be _named arguments_ and each argument must be 
#'    a named character vectors defining translations
#'    (e.g. `new_lama_dictionary(area = c("0" = "urban", "1" = "rural"), density = c(l = "Low", h = "High"))`
#'    generates a lama_dictionary class object holding the translations `"area"` and `"density"`).
#'    The names of the caller arguments will be used as names under which the given translations
#'    will be added to the new lama_dictionary class object. 
#' @return A new lama_dictionary class object holding the passed in translations.
#' @seealso [is.dictionary()], [as.lama_dictionary()], [lama_translate()], [lama_read()], [lama_write()],
#'   [lama_select()], [lama_rename()], [lama_mutate()], [lama_merge()]
#' @rdname new_lama_dictionary
#' @export
#' @include utilities.R
new_lama_dictionary <- function(...) {
  UseMethod("new_lama_dictionary")
}

#' @param .data A named list object, where each list entry corresponds to a
#'   translation that should be added to the lama_dictionary object
#'   (e.g. `new_lama_dictionary(list(area = c("0" = "urban", "1" = "rural"),  = c(l = "Low", h = "High")))`
#'   generates a lama_dictionary class object holding the translations `"area"` and `"density"`).
#'   The names of the list entries are the names under which the translation 
#'   will be added to the new lama_dictionary class object (e.g. `area` and `density`).
#'   Each list entry must be a named character vector defining a translation
#'   (e.g. `c("0" = "urban", "1" = "rural")` is the translation with
#'   the name `area` and `c(l = "Low", h = "High")` is the translation
#'   with the name `density`).
#' @rdname new_lama_dictionary
#' @examples
#'   ## Example-1: Initialize a lama-dictionary from a list object
#'   ##            holding the translations
#'   dict <- new_lama_dictionary(list(
#'     country = c(uk = "United Kingdom", fr = "France", NA_ = "other countries"),
#'     language = c(en = "English", fr = "French")
#'   ))
#'   dict
#'
#' @export
new_lama_dictionary.list <- function(.data = NULL, ...) {
  extra_args <- list(...)
  if (is.null(.data) && length(extra_args) > 0 && !is.null(names(extra_args))) {
    .data = extra_args
  } else if (length(extra_args) > 0) {
      warning(paste(
        "Warning while `new_lama_dictionary` call:",
        "If the first element is a list object,",
        "then no more arguments are needed.",
        stringify(length(extra_args)),
        "extra arguments will be ignored."
      ))
  }
  if (is.null(.data) || (is.list(.data) && length(.data) == 0)) {
    .data <- list()
  } else {
    # Check the '.data' argument
    err_handler <- composerr(paste(
      "Error while initializing the lama_dictionary class object:",
      "The passed in translation definitions are invalid"
    ))
    .data <- validate_dictionary(.data, err_handler)
  }
  structure(.data, class = "lama_dictionary")
}

#' @rdname new_lama_dictionary
#' @examples
#'   ## Example-2: Initialize the lama-dictionary directly
#'   ##            by assigning each translation to a name
#'   dict <- new_lama_dictionary(
#'     country = c(uk = "United Kingdom", fr = "France", NA_ = "other countries"),
#'     language = c(en = "English", fr = "French")
#'   )
#'   dict
#'   
#' @export
new_lama_dictionary.character <- function(...) {
  new_lama_dictionary.list(list(...))
}

#' @rdname new_lama_dictionary
#' @export
new_lama_dictionary.default <- function(...) {
  new_lama_dictionary.list(list(...))
}

#' Retrieve a translation from a [lama_dictionary][new_lama_dictionary()] class object
#'
#' The functions [lama_get()] and [lama_get_()] take a
#' [lama_dictionary][new_lama_dictionary()] and extract a specific translation.
#' The function [lama_get()] uses non-standard evaluation, whereas 
#' [lama_get_()] is the standard evaluation alternative.
#' @inheritSection new_lama_dictionary Translations
#' @inheritSection new_lama_dictionary Missing values
#' @inheritSection new_lama_dictionary lama_dictionary class objects
#' @param .data A [lama_dictionary][new_lama_dictionary()] object
#' @param translation Depending on which function was used:
#'   * `lama_get`: An unquoted translation name.
#'   * `lama_get_`: A character string holding the translation name.
#' @return The wanted translation (named character vector).
#' @rdname lama_get
#' @export
lama_get <- function(.data, translation) {
  UseMethod("lama_get")
}

#' @rdname lama_get
#' @export
lama_get.lama_dictionary <- function(.data, translation) {
  err_handler <- composerr("Error while calling 'lama_get'")
  get_translation(.data, deparse(substitute(translation)), err_handler)
}

#' @rdname lama_get
#' @export
lama_get_ <- function(.data, translation) {
  UseMethod("lama_get_")
}

#' @rdname lama_get
#' @export
lama_get_.lama_dictionary <- function(.data, translation) {
  err_handler <- composerr("Error while calling 'lama_get_'")
  get_translation(.data, translation, err_handler)
}

get_translation <- function(.data, translation, err_handler) {
  err_handler <- composerr("Invalid object passed into argument 'translation'", err_handler)
  if (!is.character(translation) || length(translation) != 1 || is.na(translation))
    err_handler("The argument must be a character string holding the name of the wanted translation.")
  if (!translation %in% names(.data))
    err_handler(paste0(
      "Translation ",
      stringify(translation),
      " is not contained in the 'lama_dictionary' class object given in ",
      "argument '.data'. Only the following translations are available: ",
      stringify(names(.data)),
      "."
    ))
  .data[[translation]]
}

#' Coerce to a [lama_dictionary][new_lama_dictionary()] class object
#'
#' This function allows two types of arguments:
#' * _named list_: A named list object holding the translations.
#' * _data.frame_: A data.frame with one ore more column pairs. Each column
#'   pair consists of a column holding the original values, which should be replaced,
#'   and a second character column holding the new labels which should be
#'   assigned to the original values. Use the arguments `col_old` and `col_new`
#'   in order to define which columns are holding original values and which 
#'   columns hold the new labels. The names of the resulting translations
#'   are defined by a character vector given in argument `translation`.
#'   Furthermore, each translation can have a different ordering which can be
#'   configured by a character vector given in argument `ordering`.
#' @param .data An object holding the translations. `.data` can be of the
#'   following data types:
#'   * _named list_: A named list object, where each list entry is a translation
#'     (a named character vector)
#'   * _data.frame_: A data.frame holding one or more column pairs,
#'     where each column pair consists of
#'     one column holding the original variable values and
#'     a second column holding the new labels,
#'     which should be assigned to the original values.
#' @param ... Various arguments, depending on the data type of `.data`.
#' @return A new lama_dictionary class object holding the passed in translations.
#' @inheritSection new_lama_dictionary Translations
#' @inheritSection new_lama_dictionary Missing values
#' @inheritSection new_lama_dictionary lama_dictionary class objects
#' @rdname as_lama_dictionary
#' @export
as.lama_dictionary <- function(.data, ...) {
  UseMethod("as.lama_dictionary")
}

#' @rdname as_lama_dictionary
#' @examples
#'   ## Example-1: Initialize a lama-dictionary from a list oject
#'   ##            holding the translations
#'   obj <- list(
#'     country = c(uk = "United Kingdom", fr = "France", NA_ = "other countries"),
#'     language = c(en = "English", fr = "French")
#'   )
#'   dict <- as.lama_dictionary(obj)
#'   dict
#'   
#' @export
as.lama_dictionary.list <- function(.data, ...) {
  new_lama_dictionary(.data)
}

#' @rdname as_lama_dictionary
#' @export
as.lama_dictionary.lama_dictionary <- function(.data, ...) {
  structure(
    validate_dictionary(
      .data,
      composerr(paste(
        "Error while calling 'as.lama_dictionary':",
        "The passed in object is a 'lama_dictionary' class object,",
        "but has invalid structure"
      ))
    ),
    class = "lama_dictionary"
  )
}

#' @rdname as_lama_dictionary
#' @export
as.lama_dictionary.default <- function(.data = NULL, ...) {
  stop("Error while calling `as.lama_dictionary`: The supplied argument must either be a list or a data.frame object")
}

#' @param translation A character vector holding the names of all translations
#' @param col_old This argument is only used, if the argument given in `.data`
#'   is a data.frame. In this case, the
#'   argument `col_old` must be a character vector (same length as `translation`) holding the
#'   names of the columns in the data.frame (in the argument `.data`) which hold
#'   the original variable values.
#'   These columns can be of any type: `character`, `logical`, `numerical` or `factor`.
#' @param col_new This argument is only used, if the argument given in `.data`
#'   is a data.frame. In this case, the
#'   argument `col_old` must be a character vector (same length as `translation`) holding the
#'   names of the columns in the data.frame (in the argument `.data`) which hold
#'   the new labels, which should be assigned to the original values.
#'   These columns can be `character` vectors or `factors` with character labels.
#' @param ordering This argument is only used, if the argument given in `.data`
#'   is a data.frame. In this case, the
#'   argument `ordering` must be a character vector (same length as `translation`) holding
#'   one of the following configuration strings configuring 
#'   the ordering of each corresponding translation:
#'   * `"row"`: The corresponding translation will be ordered exactly in the same
#'     way as the rows are ordered in the data.frame `.data`. 
#'   * `"old"`: The corresponding translation will be ordered by the given 
#'     original values which are contained in the corresponding column `col_old`.
#'     If the column contains a factor variable, then the ordering of the
#'     factor will be used. If it just contains a plain character variable,
#'     then it will be ordered alphanumerically.
#'   * `"new"`: The corresponding translation will be ordered by the given 
#'     new labels which are contained in the corresponding column `col_new`.
#'     If the column contains a factor variable, then the ordering of the
#'     factor will be used. If it just contains a plain character variable,
#'     then it will be ordered alphanumerically.
#' @rdname as_lama_dictionary
#' @examples
#'   ## Example-2: Initialize a lama-dictionary from a data frame
#'   ##            holding the label assignment rules
#'   df_map <- data.frame(
#'     c_old = c("uk", "fr", NA),
#'     c_new = c("United Kingdom", "France", "other countries"),
#'     l_old = c("en", "fr", NA),
#'     l_new = factor(c("English", "French", NA), levels = c("French", "English"))
#'   )
#'   dict <- as.lama_dictionary(
#'     df_map,
#'     translation = c("country", "language"),
#'     col_old = c("c_old", "l_old"),
#'     col_new = c("c_new", "l_new"),
#'     ordering = c("row", "new")
#'   )
#'   # 'country' is ordered as in the 'df_map'
#'   # 'language' is ordered differently ("French" first)
#'   dict
#' @export
as.lama_dictionary.data.frame <- function(
  .data, translation, col_old, col_new, ordering = rep("row", length(translation)), ...
) {
  err_handler <- composerr("Error while calling 'as.lama_dictionary'")
  err_handler_translation <- composerr("Invalid argument 'translation'", err_handler)
  if (!is.character(translation) || any(is.na(translation)))
    err_handler_translation("Object must be a character vector.")
  if (!is.character(translation) || length(translation) == 0 || any(is.na(translation)))
    err_handler_translation("Argument 'translation' must be a character vector holding the names of the translations.")
  duplicates <- table(translation)
  duplicates <- names(duplicates[duplicates > 1])
  if (length(duplicates) > 0)
    err_handler_translation(paste0(
      "The following translation names are used ",
      "more than once: ",
      stringify(duplicates),
      "."
    ))
  invalid <- !is.syntactic(translation)
  if (any(invalid))
    err_handler_translation(paste0(
      "The following given translation names are not valid names: ",
      stringify(translation[invalid]),
      "."
    ))
  err_handler_old <- composerr("Invalid argument 'col_old'", err_handler)
  if (!is.character(col_old) || any(is.na(col_old)))
    err_handler_old("Object must be a character vector.")
  if (length(col_old) != length(translation))
    err_handler_old("Object must be of the same length as the character vector in argument 'translation'.")
  invalid <- !col_old %in% colnames(.data)
  if (any(invalid))
    err_handler_old(paste0(
      "The following entries are no valid column names in the data.frame given in '.data': ",
      stringify(col_old[invalid]),
      ".\nOnly the following column names are available: ",
      stringify(colnames(.data)),
      "."
    ))
  err_handler_new <- composerr("Invalid argument 'col_new'", err_handler)
  if (!is.character(col_new) || any(is.na(col_new)))
    err_handler_new("Object must be a character vector.")
  if (length(col_new) != length(translation))
    err_handler_new("Object must be of the same length as the character vector in argument 'translation'.")
  invalid <- !col_new %in% colnames(.data)
  if (any(invalid))
    err_handler_new(paste0(
      "The following entries are no valid column names in the data.frame given in '.data': ",
      stringify(col_new[invalid]),
      ".\nOnly the following column names are available: ",
      stringify(colnames(.data)),
      "."
    ))
  err_handler_ordering <- composerr("Invalid argument 'ordering'", err_handler)
  if (is.character(ordering) && length(ordering) == 1)
    ordering <- rep(ordering, length(translation))
  if (!is.character(ordering) || any(is.na(ordering)))
    err_handler_ordering("Object must be a character vector.")
  if (length(ordering) != length(translation))
    err_handler_ordering("Object must be of the same length as the character vector in argument 'translation'.")
  if (any(!ordering %in% c("row", "old", "new")))
    err_handler_ordering(paste0(
      "Each item of 'ordering' must be one of the following strings: ",
      stringify(c("row", "old", "new")),
      "."
    ))
  # create list holding the translations
  new_lama_dictionary(named_lapply(
    translation,
    function(translation, .I) {
      col_old <- col_old[.I]
      col_new <- col_new[.I]
      ordering <- ordering[.I]
      .data <- .data[!is.na(.data[[col_old]]) | !is.na(.data[[col_new]]),]
      err_handler <- composerr_parent(
        paste(
          "Error while creating translation",
          stringify(translation),
          "which assigns the new labels given in column",
          stringify(col_new),
          "to the original values given in column",
          stringify(col_old)
        ),
        err_handler
      )
      if (ordering == "old") {
        .data <- .data[order(.data[[col_old]]),]
      } else if (ordering == "new") {
        .data <- .data[order(.data[[col_new]]),]
      }
      new <- escape_to_na(as.character(.data[[col_new]]))
      old <- na_to_escape(as.character(.data[[col_old]]))
      duplicates <- table(old)
      duplicates <- names(duplicates[duplicates > 1])
      duplicates[duplicates == NA_lama_] <- "NA"
      if (length(duplicates) > 0)
        err_handler(paste0(
          "The following original values given in column ",
          stringify(col_old),
          " appear more than once: ",
          stringify(duplicates),
          "."
        ))
      names(new) <- old
      new
    }
  ))
}

#' Check if an object is a [lama_dictionary][new_lama_dictionary()] class object
#'
#' @param obj The object in question
#' @return \code{TRUE} if the object is a
#' [lama_dictionary][new_lama_dictionary()] class object, \code{FALSE} otherwise.
#' @rdname is_dictionary
#' @seealso [validate_dictionary()], [as.lama_dictionary()], [new_lama_dictionary()],
#' [lama_translate()], [lama_read()], [lama_write()], [lama_select()],
#' [lama_rename()], [lama_mutate()], [lama_merge()]
#' @examples
#' # check if an object is a 'lama_dictionary' class object
#' dict <- new_lama_dictionary(country = c(uk = "United Kingdom", fr = "France"))
#' is.dictionary(dict)
#' @export
is.dictionary <- function(obj) {
  inherits(obj, "lama_dictionary")
}

#' Check if an object has a valid [lama_dictionary][new_lama_dictionary()] structure
#'
#' This function checks if the object structure is right. It does not check 
#' class type.
#' @inheritSection new_lama_dictionary Translations
#' @inheritSection new_lama_dictionary Missing values
#' @inheritSection new_lama_dictionary lama_dictionary class objects
#' @param obj An object that should be tested
#' @param err_handler An error handling function
#' @rdname validate_dictionary
#' @seealso [is.dictionary()], [as.lama_dictionary()], [new_lama_dictionary()],
#' [lama_translate()], [lama_read()], [lama_write()], [lama_select()],
#' [lama_rename()], [lama_mutate()], [lama_merge()]
#' @export
validate_dictionary <- function(
  obj,
  err_handler = composerr("The object has not a valid lama_dictionary structure")
) {
  if (!is.list(obj))
    err_handler("The object must be a named list object.")
  translations <- names(obj)
  if (
      !is.character(translations) || length(translations) == 0 ||
        any(is.na(translations)) || any(translations == "")
  )
    err_handler(paste(
      "The names of the translations are missing."
    ))
  duplicates <- table(translations)
  duplicates <- names(duplicates[duplicates > 1])
  if (length(duplicates) > 0)
    err_handler(paste0(
      "The following translation names are used more than once: ",
      stringify(duplicates),
      "."
    ))
  lapplI(
    obj,
    function(x, .I) {
      validate_translation(
        x,
        composerr_parent(
          paste(
            "Invalid translation with name",
            stringify(translations[.I])
          ),
          err_handler
        )
      )
    }
  )
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
  if (is.list(obj)) {
    # a translation can also be given as a named list, where each item
    # is a character of length 1
    invalid <- sapply(obj, function(x) (!is.character(x) && !is.na(x)) || length(x) != 1)
    if (any(invalid) || is.null(names(obj)) || any(names(obj) == ""))
      err_handler("The object must be a named character vector holding the variable translations.")
    obj <- unlist(obj)
  }
  if (
    !is.character(obj) || length(names(obj)) == 0 || any(is.na(names(obj))) ||
      any(names(obj) == "")
  )
    err_handler("The object is not a valid named character vector.")
  obj <- escape_to_na(obj)
  duplicates <- table(names(obj))
  duplicates <- names(duplicates[duplicates > 1])
  if (length(duplicates) > 0)
    err_handler(paste0(
      "The object has duplicate names. The following original values ",
      "are defined more than once: ",
      stringify(duplicates),
      "."
    ))
  obj
}



#' Print a [lama_dictionary][new_lama_dictionary()] class object
#'
#' @param x The [lama_dictionary][new_lama_dictionary()] class object that should be printed.
#' @param ... Unused arguments
#' @rdname print
#' @seealso [lama_translate()], [new_lama_dictionary()], [lama_select()],
#'   [lama_rename()], [lama_mutate()], [lama_merge()], [lama_read()],
#'   [lama_write()]
#' @export
print.lama_dictionary <- function(x, ...) {
  cat("\n--- lama_dictionary ---\n")
  for (name in names(x)) {
    cat(paste0("Variable '", name, "':\n"))
    print(x[[name]])
    cat("\n")
  }
}
