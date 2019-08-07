#' Read in a \code{yaml} file holding label translations for one or multiple variables
#'
#' @param yaml_path Path to yaml file holding the labels and translations for multiple variables
#' @return A [LabelLexicon] class object holding the label mappings defined in
#' the yaml file
#' @export
#' @include utilities.R label_lexicon.R
read_lexicon_file <- function(yaml_path) {
  new_lexicon(yaml_to_lexicon(yaml::read_yaml(yaml_path)))
}
