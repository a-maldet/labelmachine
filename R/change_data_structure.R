#' Transform data structure from yaml format to the class input format
#'
#' When a yaml file is read in the data has the structure
#' themes (named list) > vars (named list) > labels (named list)
#' This structure is transformed to the class input structure
#' vars (named list) > themes (named list) > labels (named character vector)
yaml_to_class <- function(data) {
  theme_names <- unique(as.vector(unlist(sapply(data, names))))
  names(theme_names) <- theme_names
  lapply(theme_names, function(thm) lapply(data, function(x) unlist(x[[thm]])))
}

#' Transform data structure from class input format to the yaml format
#'
#' In the [labelmachine] class object the data has the structure
#' vars (named list) > themes (named list) > labels (named character vector)
#' This structure is transformed to the yaml file structure
#' themes (named list) > vars (named list) > labels (named list)
class_to_yaml <- function(data) {
  var_names <- unique(as.vector(unlist(sapply(data, names))))
  names(var_names) <- var_names
  lapply(var_names, function(var) lapply(data, function(x) as.list(x[[var]])))
}
