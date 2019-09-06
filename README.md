
labelmachine <img src="man/figures/logo.png" align="right" alt="" width=140 height=162 />
=========================================================================================

<!-- badges: start -->
[![Travis build status](https://travis-ci.org/a-maldet/labelmachine.svg?branch=master)](https://travis-ci.org/a-maldet/labelmachine) [![GitHub last commit](https://img.shields.io/github/last-commit/a-maldet/labelmachine.svg?logo=github)](https://github.com/a-maldet/labelmachine/commits/master) [![GitHub code size in bytes](https://img.shields.io/github/languages/code-size/a-maldet/labelmachine.svg?logo=github)](https://github.com/a-maldet/labelmachine)

`labelmachine` is an **R** package that helps assigning meaningful labels to R data sets. Furthermore, you can manage your labels in so called **dictionary** files, which are **yaml** files. This makes it very easy using the same label translations in multiple projects that share similar data structure.

> Labelling your data can be easy!

Installation
------------

``` r
# Install development version from GitHub
devtools::install_github('a-maldet/labelmachine', build_opts = NULL)
```

Concept
-------

The label assignments are given in so called **translations** (named character vectors), which are like a recipes, telling which original value will be mapped onto which new label. The **translations** are collected in so called **lama\_dictionary** objects. This **lama\_dictionary** objects will be used to translate your data.frame variables.

Usage
-----

Let **df** be a data.frame with marks and subjects, which should be translated

``` r
library(labelmachine)
df <- data.frame(
  pupil_id - c(1, 1, 2, 2, 3),
  subject = c("en", "ma", "ma", "en", "en"),
  result = c(2, 1, 3, 2, NA)
)
```

Create a **lama\_dictionary** object holding the translations:

``` r
dict <- new_lama_dictionary(
  subjects = c(en = "English", ma = "Mathematics", NA_ = "other subjects"),
  results = c("1" = "Excellent", "2" = "Satisfying", "3" = "Failed", NA_ = "Missed")
)
dict
```

    ## 
    ## --- lama_dictionary ---
    ## Variable 'subjects':
    ##               en               ma              NA_ 
    ##        "English"    "Mathematics" "other subjects" 
    ## 
    ## Variable 'results':
    ##            1            2            3          NA_ 
    ##  "Excellent" "Satisfying"     "Failed"     "Missed"

Translate the data.frame variables:

``` r
df_new <- lama_translate(
  df,
  dict,
  subject_new = subjects(subject),
  result_new = results(result)
)
str(df_new)
```

    ## 'data.frame':    5 obs. of  5 variables:
    ##  $ pupil_id...c.1..1..2..2..3.: num  0 0 0 0 0
    ##  $ subject                    : Factor w/ 2 levels "en","ma": 1 2 2 1 1
    ##  $ result                     : num  2 1 3 2 NA
    ##  $ subject_new                : Factor w/ 3 levels "English","Mathematics",..: 1 2 2 1 1
    ##  $ result_new                 : Factor w/ 4 levels "Excellent","Satisfying",..: 2 1 3 2 4

Highlights
----------

`labelmachine` offers the following features:

-   All types of variables can be translated: **Logical**, **Numeric**, **Character**, **Factor**
-   When translating your varaibles, you may choose between keeping the current ordering or applying a new factor ordering to your variable.
-   Assigning meaningful labels to missing values (**NA**) is no problem.
-   Assigning **NA** to existing values is no problem.
-   Merging two values into a single label is no problem.
-   Transforming **data.frame** holding label assignment lists into a **lama\_dictionary** is no problem.
-   Manage your translations in **yaml** files in order to use the same translations in different projects sharing similare data.

Further reading
---------------

More information can be found on the \[github-pages site\] for `labelmachine`:

-   A simple guide is given in the [get started vignette](https://a-maldet.github.io/labelmachine/index.html).

License
-------

[GPL-3](https://a-maldet.github.io/labelmachine/LICENSE)