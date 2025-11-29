# Clean up the names of all datasets

Clean the names of all the datasets in the database. By default, it
converts names to lowercase letters, numbers, and underscores only.

## Usage

``` r
edc_clean_names(database, clean_fun = NULL)
```

## Arguments

- database:

  an
  [edc_database](https://danchaltiel.github.io/EDCimport/reference/edc_database.md)
  object, from
  [`read_trialmaster()`](https://danchaltiel.github.io/EDCimport/reference/read_trialmaster.md)
  or other EDCimport reading functions.

- clean_fun:

  a cleaning function to be applied to column names.

## Value

an
[edc_database](https://danchaltiel.github.io/EDCimport/reference/edc_database.md)
object

## Examples

``` r
#db = read_trialmaster("filename.zip", pw="xx")
db = edc_example() %>% 
  edc_clean_names()
#> Warning: Option "edc_lookup" has been overwritten.
names(db$enrol)
#> [1] "subjid"     "age"        "enrol_date" "arm"        "crfname"   
#> [6] "crfstat"   
```
