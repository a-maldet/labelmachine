
labellexicon
============

`labellexicon` is an **R** package for labelling factor variables in **R** data frames. This is especially useful if you have encoded variables and want to plot your data with meaningful labels. Therefore, the `labellexicon` relabels your variables and the label mappings are stored in **yaml files**, so called **Lexicon** files. This makes it very easy to use the same label mapping in multiple projects that have similar data sets.

Furthermore, `LabelLexicon` allows you to process your LabelLexicons in a way that is very similar to [dplyr](https://cran.r-project.org/web/packages/dplyr/vignettes/dplyr.html). You can `select`, `mutate` and `rename` the entries of your LabelLexicon. You may also `merge` two or more LabelLexicas into a single LabelLexicon.

Installation
------------

``` r
# Install development version from GitHub
devtools::install_github('a-maldet/labellexicon', build_opts = NULL)
```

Usage
-----

### Create a `LabelLexicon` from a list

You can simply create a LabelLexicon from a named list object, holding named character vectors. Each entry of the named list represents a variable of your data and each named character vector is a translation mapping for your variable. The names of the character vectors represent the old values of the variable and the values of the character vectors are the new labels that should be used.

``` r
obj <- list(
  supp = c(VC = "Ascorbic acid", OJ = "Orange juice"),
  dose = c("0.5" = "Low", "1.0" = "Medium", "2.0" = "High")
)
```

### Load `LabelLexicon` file (yaml)

Structure of your Lexicon file `short_lex.yaml`

``` yaml
supp:
  VC: Ascorbic acid
  OJ: Orange juice
dose:
  0.5: Low
  1.0: Medium
  2.0: High
```

``` r
library(labellexicon)
library(magrittr)
lex <- "short_lex.yaml" %>%
  read_lexicon_file
```

The label mappings are now contained in the LabelLexicon object `lex`.

``` r
lex
```

    ## 
    ## --- LabelLexicon ---
    ## Variable 'supp':
    ##              VC              OJ 
    ## "Ascorbic acid"  "Orange juice" 
    ## 
    ## Variable 'dose':
    ##      0.5        1        2 
    ##    "Low" "Medium"   "High"

### Relabel your variables with a LabelLexicon

Now, that we have loaded a LabelLexicon `lex`, we can use it in order to translate a categorical variable of our data.frame (not necessarily a factor variable) to a factor variable holding the labels that are defined in the LabelLexicon `lex`.

``` r
ToothGrowth %>%
  translate(lex, "supp") %>%
  translate(lex, "dose")
```

    ##     len          supp   dose
    ## 1   4.2 Ascorbic acid    Low
    ## 2  11.5 Ascorbic acid    Low
    ## 3   7.3 Ascorbic acid    Low
    ## 4   5.8 Ascorbic acid    Low
    ## 5   6.4 Ascorbic acid    Low
    ## 6  10.0 Ascorbic acid    Low
    ## 7  11.2 Ascorbic acid    Low
    ## 8  11.2 Ascorbic acid    Low
    ## 9   5.2 Ascorbic acid    Low
    ## 10  7.0 Ascorbic acid    Low
    ## 11 16.5 Ascorbic acid Medium
    ## 12 16.5 Ascorbic acid Medium
    ## 13 15.2 Ascorbic acid Medium
    ## 14 17.3 Ascorbic acid Medium
    ## 15 22.5 Ascorbic acid Medium
    ## 16 17.3 Ascorbic acid Medium
    ## 17 13.6 Ascorbic acid Medium
    ## 18 14.5 Ascorbic acid Medium
    ## 19 18.8 Ascorbic acid Medium
    ## 20 15.5 Ascorbic acid Medium
    ## 21 23.6 Ascorbic acid   High
    ## 22 18.5 Ascorbic acid   High
    ## 23 33.9 Ascorbic acid   High
    ## 24 25.5 Ascorbic acid   High
    ## 25 26.4 Ascorbic acid   High
    ## 26 32.5 Ascorbic acid   High
    ## 27 26.7 Ascorbic acid   High
    ## 28 21.5 Ascorbic acid   High
    ## 29 23.3 Ascorbic acid   High
    ## 30 29.5 Ascorbic acid   High
    ## 31 15.2  Orange juice    Low
    ## 32 21.5  Orange juice    Low
    ## 33 17.6  Orange juice    Low
    ## 34  9.7  Orange juice    Low
    ## 35 14.5  Orange juice    Low
    ## 36 10.0  Orange juice    Low
    ## 37  8.2  Orange juice    Low
    ## 38  9.4  Orange juice    Low
    ## 39 16.5  Orange juice    Low
    ## 40  9.7  Orange juice    Low
    ## 41 19.7  Orange juice Medium
    ## 42 23.3  Orange juice Medium
    ## 43 23.6  Orange juice Medium
    ## 44 26.4  Orange juice Medium
    ## 45 20.0  Orange juice Medium
    ## 46 25.2  Orange juice Medium
    ## 47 25.8  Orange juice Medium
    ## 48 21.2  Orange juice Medium
    ## 49 14.5  Orange juice Medium
    ## 50 27.3  Orange juice Medium
    ## 51 25.5  Orange juice   High
    ## 52 26.4  Orange juice   High
    ## 53 22.4  Orange juice   High
    ## 54 24.5  Orange juice   High
    ## 55 24.8  Orange juice   High
    ## 56 30.9  Orange juice   High
    ## 57 26.4  Orange juice   High
    ## 58 27.3  Orange juice   High
    ## 59 29.4  Orange juice   High
    ## 60 23.0  Orange juice   High

The variables `supp` and `dose` are now factor variables holding the new labels.

In case, your variable has a different name, than the entry in the LabelLexicon you can use the `col` argument. If the resulting labled variable should be stored under a differnt name, you may use the `col_new` argument.

``` r
ToothGrowth %>%
  translate(lex, "supp") %>%
  translate(lex, "dose")
```

    ##     len          supp   dose
    ## 1   4.2 Ascorbic acid    Low
    ## 2  11.5 Ascorbic acid    Low
    ## 3   7.3 Ascorbic acid    Low
    ## 4   5.8 Ascorbic acid    Low
    ## 5   6.4 Ascorbic acid    Low
    ## 6  10.0 Ascorbic acid    Low
    ## 7  11.2 Ascorbic acid    Low
    ## 8  11.2 Ascorbic acid    Low
    ## 9   5.2 Ascorbic acid    Low
    ## 10  7.0 Ascorbic acid    Low
    ## 11 16.5 Ascorbic acid Medium
    ## 12 16.5 Ascorbic acid Medium
    ## 13 15.2 Ascorbic acid Medium
    ## 14 17.3 Ascorbic acid Medium
    ## 15 22.5 Ascorbic acid Medium
    ## 16 17.3 Ascorbic acid Medium
    ## 17 13.6 Ascorbic acid Medium
    ## 18 14.5 Ascorbic acid Medium
    ## 19 18.8 Ascorbic acid Medium
    ## 20 15.5 Ascorbic acid Medium
    ## 21 23.6 Ascorbic acid   High
    ## 22 18.5 Ascorbic acid   High
    ## 23 33.9 Ascorbic acid   High
    ## 24 25.5 Ascorbic acid   High
    ## 25 26.4 Ascorbic acid   High
    ## 26 32.5 Ascorbic acid   High
    ## 27 26.7 Ascorbic acid   High
    ## 28 21.5 Ascorbic acid   High
    ## 29 23.3 Ascorbic acid   High
    ## 30 29.5 Ascorbic acid   High
    ## 31 15.2  Orange juice    Low
    ## 32 21.5  Orange juice    Low
    ## 33 17.6  Orange juice    Low
    ## 34  9.7  Orange juice    Low
    ## 35 14.5  Orange juice    Low
    ## 36 10.0  Orange juice    Low
    ## 37  8.2  Orange juice    Low
    ## 38  9.4  Orange juice    Low
    ## 39 16.5  Orange juice    Low
    ## 40  9.7  Orange juice    Low
    ## 41 19.7  Orange juice Medium
    ## 42 23.3  Orange juice Medium
    ## 43 23.6  Orange juice Medium
    ## 44 26.4  Orange juice Medium
    ## 45 20.0  Orange juice Medium
    ## 46 25.2  Orange juice Medium
    ## 47 25.8  Orange juice Medium
    ## 48 21.2  Orange juice Medium
    ## 49 14.5  Orange juice Medium
    ## 50 27.3  Orange juice Medium
    ## 51 25.5  Orange juice   High
    ## 52 26.4  Orange juice   High
    ## 53 22.4  Orange juice   High
    ## 54 24.5  Orange juice   High
    ## 55 24.8  Orange juice   High
    ## 56 30.9  Orange juice   High
    ## 57 26.4  Orange juice   High
    ## 58 27.3  Orange juice   High
    ## 59 29.4  Orange juice   High
    ## 60 23.0  Orange juice   High

### Alter your LabelLexicon

Sometimes it can be useful to alter your LabelLexicon. With `select` you may pick a subset of LabelLexicon entries.

``` r
lex %>%
  select(c("supp", "dose"))
```

    ## 
    ## --- LabelLexicon ---
    ## Variable 'supp':
    ##              VC              OJ 
    ## "Ascorbic acid"  "Orange juice" 
    ## 
    ## Variable 'dose':
    ##      0.5        1        2 
    ##    "Low" "Medium"   "High"

With `mutate` you can set a new label translation (character vector) for a variable:

``` r
lex %>%
  mutate("supp", c(VC = "Ascorbic a.", OJ = "Orange j."))
```

    ## 
    ## --- LabelLexicon ---
    ## Variable 'supp':
    ##            VC            OJ 
    ## "Ascorbic a."   "Orange j." 
    ## 
    ## Variable 'dose':
    ##      0.5        1        2 
    ##    "Low" "Medium"   "High"

With `rename` you can rename a LabelLexicon entry:

``` r
lex %>%
  rename(c("supp", "dose"), c("SUPP", "DOSE"))
```

    ## 
    ## --- LabelLexicon ---
    ## Variable 'SUPP':
    ##              VC              OJ 
    ## "Ascorbic acid"  "Orange juice" 
    ## 
    ## Variable 'DOSE':
    ##      0.5        1        2 
    ##    "Low" "Medium"   "High"

### Merge two ore more LabelLexicas

Sometimes you may want to merge two or more LabelLexicas into one LabelLexicon. This can be done with \`\`

### Overriding error handlers in nested environments

The function `composerr_parent` looks up the existing error handler in the parent environment and not in the current environment. This can be useful in nested scoping situations (for example when checking if a nested list object has a valid structure) and if you want to store the nested error handler functions under the same name (overriding error handler functions).

``` r
### Example 2 ###
### check if all entries of the list object 'obj' are TRUE ###
obj <- list(x = TRUE, y = TRUE, z = FALSE)
# original error handler
err_h <- composerr("obj is invalid")
# check each list element 
sapply(names(obj), function(name) {
  # modify error handler to nested sitation
  err_h <- composerr_parent(paste("Error in", name), err_h)
  # check element and throw error FALSE
  if (!obj[[name]])
    err_h("Value is FALSE")
})
# --- resulting message ---
# "obj is invalid: Error in z: Value is FALSE"
```

License
-------

[GPL-3](https://R-package.github.io/styledTables/LICENSE)
