#' Write a \code{yaml} file holding translations for one or multiple variables
#'
#' @param x A [lama_dictionary][new_lama_dictionary()] class object holding the variable translations
#' @param yaml_path File path, where the yaml file should be saved
#' @export
#' @examples
#'   \dontrun{
#'     dict <- new_lama_dictionary(results = c(p = "Passed", f = "Failed"))
#'     lama_write(dict, "my_dictionary.yaml")
#'   }
#' @include utilities.R lama_dictionary.R
lama_write <- function(x, yaml_path) {
  err_handler <- composerr("Error while calling 'lama_write'")
  tryCatch(
    yaml::write_yaml(dictionary_to_yaml(x), yaml_path),
    error = function(e) err_handler(e),
    warning = function(w) err_handler(w)
  )
}
