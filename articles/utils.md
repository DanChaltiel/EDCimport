# 5 - Utility Functions

## Introduction

You imported your database, but now you might want to visualize some
part of it.

There are a lot of ways to do so, so EDCimport provides functions for a
few concepts.

As in previous vignettes, we will be using
[`edc_example()`](https://danchaltiel.github.io/EDCimport/reference/edc_example.md),
but in the real world you should use EDC reading functions. See
[`vignette("reading")`](https://danchaltiel.github.io/EDCimport/articles/reading.md)
to see how.

``` r
library(EDCimport) 
library(dplyr) 
db = edc_example(N=200) %>%
  edc_unify_subjid() %>%
  edc_clean_names() 
db 
#> ── EDCimport database ──────────────────────────────────────────────────────────
load_database(db)
```

## Joins

As the primary key is almost always SUBJID, {dplyr} joins can be
simplified with default `by` and `suffix` arguments.

``` r
data_xx = enrol %>% 
  edc_left_join(data2) %>% 
  edc_right_join(data1) %>% 
  edc_full_join(ae)
dim(data_xx)
#> [1] 1240   23
names(data_xx) %>% stringr::str_subset("crfname") #suffixes are automated
#> [1] "crfname"       "crfname_data2" "crfname_data1" "crfname_ae"
```

## “Yes/No” harmonizer

Using
[`fct_yesno()`](https://danchaltiel.github.io/EDCimport/reference/fct_yesno.md),
you can harmonize “Yes/No” values into a vector with the “Yes” level
first:

``` r
set.seed(42)
x = tibble(a=c("Yes", "No"), b=c("Oui", "Non"), c=0:1, d=c(TRUE, FALSE))
x
#> # A tibble: 2 × 4
#>   a     b         c d    
#>   <chr> <chr> <int> <lgl>
#> 1 Yes   Oui       0 TRUE 
#> 2 No    Non       1 FALSE
x %>% dplyr::mutate_all(fct_yesno)
#> # A tibble: 2 × 4
#>   a     b     c     d    
#>   <fct> <fct> <fct> <fct>
#> 1 Yes   Yes   No    Yes  
#> 2 No    No    Yes   No
```

## Miscellaneous

These three helpers don’t quite fit anywhere else, but they still
deserve their moment!

- `save_session_info()` saves the output of `session_info()` to a file.

- [`edc_warn_extraction_date()`](https://danchaltiel.github.io/EDCimport/reference/edc_warn_extraction_date.md)
  triggers a warning if the extraction date of your `edc_database` is
  older than a specified threshold.

- [`edc_inform_code()`](https://danchaltiel.github.io/EDCimport/reference/edc_inform_code.md)
  outputs a message indicating the amount of code written for this
  analysis, assuming you have one file sourcing others.
