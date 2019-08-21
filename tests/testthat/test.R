#' @export
foo <- function(x) x


library(magrittr)

lm1 <- file.path(getwd(), "../data/thm1.yaml") %>%
  read_lexicon_file
lm2 <- file.path(getwd(), "../data/thm2.yaml") %>%
  read_lexicon_file
lm3 <- file.path(getwd(), "../data/thm3.yaml") %>%
  read_lexicon_file
yaml_path <- file.path(getwd(), "../data/thm2.yaml")
yaml_path <- file.path(getwd(), "../data/thm2.yaml")

lm <- read_label_file(yaml_path)
lm
names(lm)
lm[["idok"]]

df <- data.frame(IPUB = c(2, 1, 1, 1, 2), idok = c(0, 2, 1, 1, 2), was = c("2I", "1B", "1A", "1A", "2B"))

lm <- merge_lexicas(lm1, lm2, lm3)
lm <- lm %>%
  select(c("idok", "ipub")) %>%
  rename(c("idok", "ipub"), c("IDOK", "ipub2"))

label(df, lm2, "IDOK", "idok", "idok2")

foo <- create_labelling_function(lm, "asdf")

foo(df, "ipub", "IPUB", "IPUB2")
