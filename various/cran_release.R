# check all important distribuations for CRAN release
devtools::build()

chk <- rhub::check_for_cran(path = "../labelmachine_1.0.0.tar.gz")
chk$cran_summary()
sink("various/cran-comments.md")
chk$cran_summary()
sink(NULL)

chk_win <- devtools::check_win_release(pkg = ".")

rmarkdown::render("README.Rmd")

devtools::spell_check()

devtools::release()
