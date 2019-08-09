#' Read in a \code{yaml} file holding translations for one or multiple variables
#'
#' @param yaml_path Path to yaml file holding the labels and translations for multiple variables
#' @return A [LabelDictionary][new_dictionary()] class object holding the variable translations defined in
#' the yaml file
#' @export
#' @include utilities.R dictionary.R
read_dictionary <- function(yaml_path) {
  new_dictionary(yaml_to_dictionary(yaml::read_yaml(yaml_path)))
}
