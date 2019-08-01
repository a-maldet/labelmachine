
labelmachine
============

`labelmachine` is an **R** package for labelling factor variables in **R** data frames. This is especially useful if you have encoded variables and want to plot your data with meaningful labels. Therefore, the `labelmachine` relabels your variables and the label mappings are stored in **yaml files**, which makes it very easy to use the same label mapping in multiple projects that have similar data sets. Furthermore, you can have multiple **labelling themes** for each variable, in order to use different label mappings for the same variable: E.g. a mapping that yields abbreviations and a different theme that yields full length notations.

> With the labelmachine you will stay on top of things.

Installation
------------

``` r
# Install development version from GitHub
devtools::install_github('a-maldet/labelmachine', build_opts = NULL)
```

Usage
-----

### Concatenating error messages

The functions `composerr` and `composerr_parent` allow the concatenation of a more detailed error message to an already existing error handler (wich already has a rather basic information).

``` r
### Example 1 ###
### check if variable 'obj' exists and holds value TRUE ###
obj <- FALSE
# original error handler
err_h <- composerr("Something is wrong with obj")
if (!exists("obj"))
  err_h("obj does not exist")
# create more precise error handler (same scope)
err_h2 <- composerr("obj has wrong value", err_h)
if (!obj)
  err_h2("Value is FALSE")
# --- resulting message ---
# "Something is wrong with obj: obj has wrong value: Value is FALSE"
```

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
