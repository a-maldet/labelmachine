## Test environments
- R-hub windows-x86_64-devel (r-devel)
- R-hub ubuntu-gcc-release (r-release)
- R-hub fedora-clang-devel (r-devel)

## R CMD check results
Results with `rhub::check_for_cran()`:
  0 errors ✔ | 0 warnings ✔ | 2 notes ✖

Remark:
  The 2 notes are due to the fact that I am no registered
  maintainer on CRAN yet and this is my first package released on CRAN. 

Output of `rhub::check_for_cran()`:
❯ On windows-x86_64-devel (r-devel), ubuntu-gcc-release (r-release)
  checking CRAN incoming feasibility ... NOTE
  New submission
  Maintainer: 'Adrian Maldet <maldet@posteo.at>'

0 errors ✔ | 0 warnings ✔ | 2 notes ✖

## Downstream dependencies
There are currently no downstream dependencies for this package
