
labelmachine
============

`labelmachine` is an **R** package that helps you assigning new labels to data.frame variables. Furthermore, you can manage your label translations in so called **dictionary** files, which are **yaml** files. This makes it very easy using the same label translations in multiple projects that share similar data structure.

The most important functions are:

-   `read_dictionary`: Reads in a **yaml** file holding the label translations for one or more variables. The function returns a **dictionary object**, that can be used for the translation of variable labels later on.
-   `translate`: Use a dictionary object in order to relabel one or more variables of your data.frame. The variables may be of type `numeric`, `character` or `factor`. In case of a factor variable, you have the possibility to keep the original ordering by using the function argument `keep_ordering = TRUE`.
-   `new_dictionary`: Create a dictionary object from a named list, holding the translations for one or more variables.
-   `write_dictionary`: Write a dictionary object to a **yaml** file.
-   `select`: Pick a subset of translations in a dictionary object.
-   `mutate`: Alter the translations in a dictionary object.
-   `rename`: Rename a variable translations in a dictionary object.
-   `merge`: Merge two or more dictionary objects into a single dictionary object.

Installation
------------

``` r
# Install development version from GitHub
devtools::install_github('a-maldet/labelmachine', build_opts = NULL)
```

Usage
-----

### Load a dictionary from a yaml file

Structure of your dictionary file `my_dictionary.yaml`:

``` yaml
supp:
  VC: Ascorbic acid
  OJ: Orange juice
dose:
  "0.5": Low
  "1": Medium
  "2": High
```

The first level names are the names of the variables that should be translated. The second level names represent the original values of the variables and the third level holds the labels that should be assigned to the original values of the variables.

Load your `dictionary` file with `read_dictionary`:

``` r
library(labelmachine)
library(magrittr)
dict <- "my_dictionary.yaml" %>%
  read_dictionary
```

### Assign new labels to your variables

You can use the dictionary object `dict` in order to translate categorical variables (not necessarily factor variables) in your data.frame to a factor variable holding the labels that are defined in the dictionary `dict`.

Relable your data.frame variables with `translate`:

``` r
# data.frame with original values 
ToothGrowth %>% head

# data.frame with new labels
ToothGrowth %>%
  translate(dict, c("supp", "dose")) %>%
  head
```

Now, the columns `supp` and `dose` are factor variables, which hold the desired labels and have the same ordering as in the dictionary `dict`.

If the original variable is a factor variable and you want to keep the original ordering, you can use the function argurment `keep_ordering = TRUE`. Furthermore, you can also apply your variable translations to columns with different column names than your translation names by passing the column names into the argument `col`. If you want to save your labelled variables to a different column names, you can use the argument `col_new` in order to specify the column names of the newly generated variables.

``` r
df <- data.frame(age = factor(c(2, 2, 1, 3), levels = c(3, 2, 1)))
dict <- list(
    age_short = c("1" = "a<16", "2" = "16<=a<70", "3" = "70<=a"),
    age_long = c("1" = "young", "2" = "middle aged", "3" = "old")
  ) %>%
  new_dictionary
df %>%
  translate(
    dictionary = dict, 
    variable = c("age_short", "age_long"), 
    col = c("age", "age"), 
    col_new = c("age_s", "age_l"),
    keep_order = TRUE
  )
```

    ##   age    age_s       age_l
    ## 1   2 16<=a<70 middle aged
    ## 2   2 16<=a<70 middle aged
    ## 3   1     a<16       young
    ## 4   3    70<=a         old

### Create a dictionary object manually

Instead of reading in a yaml file you can also create a dictionary object manually from a named list object, holding named character vectors. Each entry of the named list represents a variable of your data.frame and each named character vector is a translation. The names of the character vector entries represent the original values of the variable and the values of the character vector entries are the new labels that should be assigned.

``` r
dict <- list(
    supp = c(VC = "Ascorbic acid", OJ = "Orange juice"),
    dose = c("0.5" = "Low", "1.0" = "Medium", "2.0" = "High")
  ) %>%
  new_dictionary
```

### Alter your dictionary

Sometimes it can be useful to alter your dictionary object.

With `select` you may pick a subset of dictionary entries:

``` r
dict %>%
  select(c("supp", "dose"))
```

    ## 
    ## --- LabelDictionary ---
    ## Variable 'supp':
    ##              VC              OJ 
    ## "Ascorbic acid"  "Orange juice" 
    ## 
    ## Variable 'dose':
    ##      0.5      1.0      2.0 
    ##    "Low" "Medium"   "High"

With `mutate` you can set a new translation (character vector) for a variable:

``` r
dict %>%
  mutate("supp", c(VC = "Ascorbic a.", OJ = "Orange j."))
```

    ## 
    ## --- LabelDictionary ---
    ## Variable 'supp':
    ##            VC            OJ 
    ## "Ascorbic a."   "Orange j." 
    ## 
    ## Variable 'dose':
    ##      0.5      1.0      2.0 
    ##    "Low" "Medium"   "High"

With `rename` you can rename a dictionary entry:

``` r
dict %>%
  rename(c("supp", "dose"), c("SUPP", "DOSE"))
```

    ## 
    ## --- LabelDictionary ---
    ## Variable 'SUPP':
    ##              VC              OJ 
    ## "Ascorbic acid"  "Orange juice" 
    ## 
    ## Variable 'DOSE':
    ##      0.5      1.0      2.0 
    ##    "Low" "Medium"   "High"

### Merge two ore more dictionary objects

With `merge` you can merge two or more dictionary objects into one dictionary object:

``` r
dict1 <- list(
    gender = c("0" = "female", "1" = "male"),
    age = c("0" = "<10y", "1" = ">=10y"),
    country = c("0" = "Austria", "1" = "Australia")
  ) %>%
  new_dictionary
dict2 <- list(
    school = c("0" = "Primary", "1" = "Secondary"),
    gender = c("0" = "Girl", "1" = "Boy")
  ) %>%
  new_dictionary
merge(dict1, dict2)
```

    ## Warning in merge.LabelDictionary(dict1, dict2): The following
    ## LabelDictionary entries will be overwritten: 0, 1

    ## 
    ## --- LabelDictionary ---
    ## Variable 'gender':
    ##      0      1 
    ## "Girl"  "Boy" 
    ## 
    ## Variable 'age':
    ##       0       1 
    ##  "<10y" ">=10y" 
    ## 
    ## Variable 'country':
    ##           0           1 
    ##   "Austria" "Australia" 
    ## 
    ## Variable 'school':
    ##           0           1 
    ##   "Primary" "Secondary"

The `merge` merges the arguments from left to right. Therefore, the variable translations of `dict1` are overwritten by the entries of `dict2`. In this example the resulting dictionary has contains the variable translations for `age` and `country` as defined in `dict1` and the variable translations for `school` and `gender` as defined in `dict2`. The variable translations for `gender` in `dict1` are overwritten by `dict2`.

### Save your dictionary to a yaml file

With `write_dictionary` you can save a dictionary object to a yaml file:

``` r
dict <- list(
    gender = c(d = "diverse", f = "female", m = "male"),
    age = c("super young" = "younger than 10", "super old" = "older than 90")
  ) %>%
  new_dictionary
dict %>%
  write_dictionary("my_fancy_new_dictionary.yaml")
```

The resulting file looks like this:

``` yaml
gender:
  d: diverse
  f: female
  m: male
age:
  super young: younger than 10
  super old: older than 90
```

License
-------

[GPL-3](https://R-package.github.io/styledTables/LICENSE)
