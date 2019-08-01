#' @export
foo <- function(x) x



yaml_path <- file.path(getwd(), "../../wtbLabels/data/valueLabels.yaml")

lm <- read_label_file(yaml_path)

df <- data.frame(IPUB = c(2, 1, 1, 1, 2), idok = c(0, 2, 1, 1, 2), was = c("2I", "1B", "1A", "1A", "2B"))

foo <- create_labelling_function(lm, "asdf")

foo(df, "ipub", "IPUB", "IPUB2")
