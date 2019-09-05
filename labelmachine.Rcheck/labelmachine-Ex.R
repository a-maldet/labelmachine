pkgname <- "labelmachine"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('labelmachine')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("lapplI")
### * lapplI

flush(stderr()); flush(stdout())

### Name: lapplI
### Title: Improve 'lapply and 'sapply' with index
### Aliases: lapplI sapplI

### ** Examples

# 'lapply' with index
lapplI(
  list("x1", "x2"),
  function(x, y, .I) list(x = x, y = y, i = .I),
  y = "extra argument"
)

# 'lapply' without index
lapplI(
  list("x1", "x2"),
  function(x, y) list(x = x, y = y),
  y = "extra argument"
)
# 'sapply' with index
sapplI(
  c("x1", "x2"),
  function(x, y, .I) paste(x, y, .I),
  y = "extra argument",
  USE.NAMES = FALSE
)

# 'sapply' without index
sapplI(
  c("x1", "x2"),
  function(x, y) paste(x, y),
  y = "extra argument",
  USE.NAMES = FALSE
)



### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
