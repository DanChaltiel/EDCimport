# Harmonize the subject ID of the database

Turns the subject ID columns of all datasets into a factor containing
levels for all the subjects of the database. Avoid problems when joining
tables, and some checks can be performed on the levels.

## Usage

``` r
harmonize_subjid(datalist, preprocess = NULL, col_subjid = get_subjid_cols())
```

## Arguments

- datalist:

  a list of dataframes

- preprocess:

  an optional function to modify the subject ID column, for example
  [`as.numeric()`](https://rdrr.io/r/base/numeric.html). See examples.

- col_subjid:

  the names of the columns holding the subject ID (as character)

## Value

datalist, with subject id modified

## Examples

``` r
db = edc_example()
#> Warning: Option "edc_lookup" has been overwritten.
db$db0 = head(db$db0, 10)
db$db0$SUBJID %>% head()
#> [1] 1 2 3 4 5 6
db = harmonize_subjid(db)
db$db0$SUBJID %>% head()
#> [1] 1 2 3 4 5 6
#> 50 Levels: 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 ... 50
db = harmonize_subjid(db, preprocess=function(x) paste0("#", x))
db$db0$SUBJID %>% head()
#> [1] #1 #2 #3 #4 #5 #6
#> 50 Levels: #1 #2 #3 #4 #5 #6 #7 #8 #9 #10 #11 #12 #13 #14 #15 #16 #17 ... #50
```
