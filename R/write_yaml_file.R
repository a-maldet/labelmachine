#' Write a \code{yaml} file holding label translations for one or multiple variables
#'
#' @param x A [LabelLexicon] class object holding the labels and translations
#' @param yaml_path File path, where the yaml file should be saved
#' @export
#' @include utilities.R label_lexicon.R
write_lexicon_file <- function(x, yaml_path) {
  yaml::write_yaml(lexicon_to_yaml(x), yaml_path)
}
