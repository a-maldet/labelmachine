rmarkdown::render("README.Rmd")

# check all important distribuations for CRAN release
devtools::build()

setwd("..")

rhub::validate_email("maldet@posteo.at")
chk <- rhub::check_for_cran(path = "../labelmachine_1.0.0.tar.gz")
chk$cran_summary()
sink("various/cran-comments.md")
chk$cran_summary()
sink(NULL)

chk_win <- devtools::check_win_release(pkg = ".")


devtools::spell_check()

devtools::release()
