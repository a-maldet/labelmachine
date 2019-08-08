#' Write a \code{yaml} file holding translations for one or multiple variables
#'
#' @param x A [LabelDictionary] class object holding the variable translations
#' @param yaml_path File path, where the yaml file should be saved
#' @export
#' @include utilities.R dictionary.R
write_dictionary <- function(x, yaml_path) {
  yaml::write_yaml(dictionary_to_yaml(x), yaml_path)
}
