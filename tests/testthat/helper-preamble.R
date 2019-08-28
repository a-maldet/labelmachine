library(magrittr)
library(labelmachine)

# Load dictionaries
dict_1 <- system.file("testdata", "dict_1.yaml", package = "labelmachine") %>%
  lama_read
dict_2 <- system.file("testdata", "dict_2.yaml", package = "labelmachine") %>%
  lama_read
df <- ToothGrowth
