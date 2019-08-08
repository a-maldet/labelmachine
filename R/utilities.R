# Function that checks if a variable name is syntactically valid
# This function was suggested by Hadley Wickham see [http://r.789695.n4.nabble.com/Syntactically-valid-names-td3636819.html]
is.syntactic <- function(x) x == make.names(x)

#' Transform data structure from yaml format to the [LabelDictionary] class input format
#'
#' When a yaml file is read in, the data has the structure
#' vars (named list) > translations (named list)
#' This structure is transformed to the [LabelDictionary] class input structure
#' vars (named list) >  translations (named character vector)
yaml_to_dictionary <- function(data) {
  lapply(data, unlist)
}

#' Transform data structure from [LabelDictionary] class input format to the yaml format
#'
#' In the [LabelDictionary] class object the data has the structure
#' vars (named list) > translations (named character vector)
#' This structure is transformed to the yaml file structure
#' vars (named list) > translations (named list)
dictionary_to_yaml <- function(data) {
  lapply(data, as.list)
}