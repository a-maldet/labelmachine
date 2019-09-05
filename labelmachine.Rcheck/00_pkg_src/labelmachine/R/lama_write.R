#' Write a \code{yaml} file holding translations for one or multiple variables
#'
#' @param x A [LabelDictionary][new_dictionary()] class object holding the variable translations
#' @param yaml_path File path, where the yaml file should be saved
#' @export
#' @include utilities.R dictionary.R
lama_write <- function(x, yaml_path) {
  err_handler <- composerr("Error while calling 'lama_write'")
  tryCatch(
    yaml::write_yaml(dictionary_to_yaml(x), yaml_path),
    error = function(e) err_handler(e),
    warning = function(w) err_handler(w)
  )
}
