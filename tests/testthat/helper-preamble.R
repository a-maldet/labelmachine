library(magrittr)
library(labelmachine)

# Load dictionaries
dict_1 <- system.file("testdata", "dict_1.yaml", package = "labelmachine") %>%
  read_dictionary
dict_2 <- system.file("testdata", "dict_2.yaml", package = "labelmachine") %>%
  read_dictionary
df <- ToothGrowth
