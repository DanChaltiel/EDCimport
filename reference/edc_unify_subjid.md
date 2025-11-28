# Harmonize the subject ID of the database

Turns the subject ID columns of all datasets into a factor containing
levels for all the subjects of the database. Avoid problems when joining
tables, and some checks can be performed on the levels. See
[`vignette("postprocessing")`](https://danchaltiel.github.io/EDCimport/articles/postprocessing.md)
for a real-life case.

## Usage

``` r
edc_unify_subjid(
  database,
  preprocess = NULL,
  mode = c("factor", "numeric"),
  col_subjid = NULL
)
```

## Arguments

- database:

  an
  [edc_database](https://danchaltiel.github.io/EDCimport/reference/edc_database.md)
  object, from
  [`read_trialmaster()`](https://danchaltiel.github.io/EDCimport/reference/read_trialmaster.md)
  or other EDCimport reading functions.

- preprocess:

  an optional function to modify the subject ID column (at the character
  level). Default behavior is only to remove trailing zeros if numeric.

- mode:

  the output type of the subject ID columns

- col_subjid:

  names of the subject ID columns (as character)

## Value

database, with subject id modified

## Examples

``` r
db = edc_example()
#> Warning: Option "edc_lookup" has been overwritten.
db$enrol$subjid %>% head()  #double vector
#> [1] 1 2 3 4 5 6

db2 = edc_unify_subjid(db)
db2$enrol$subjid %>% head() #factor with 50 levels
#> [1] 1 2 3 4 5 6
#> 50 Levels: 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 ... 50

db3 = edc_unify_subjid(db, preprocess=function(x) paste0("#", x))
db3$enrol$subjid %>% head()
#> [1] #1 #2 #3 #4 #5 #6
#> 50 Levels: #1 #2 #3 #4 #5 #6 #7 #8 #9 #10 #11 #12 #13 #14 #15 #16 #17 ... #50

#use numeric mode to get a numeric output
db4 = edc_unify_subjid(db, preprocess=function(x) as.numeric(x)+1, mode="numeric")
db4$enrol$subjid %>% head()
#> [1] 2 3 4 5 6 7
```
