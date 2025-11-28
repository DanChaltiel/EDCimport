# Select only distinct columns

Select all columns that has only one level for a given grouping scope.
Useful when dealing with mixed datasets containing both long data and
repeated short data.

## Usage

``` r
select_distinct(df, .by)
```

## Arguments

- df:

  a dataframe

- .by:

  optional grouping columns

## Value

`df` with less columns

## Examples

``` r
db = edc_example()
#> Warning: Option "edc_lookup" has been overwritten.
db$ae %>% colnames()
#> [1] "subjid"  "crfname" "aesoc"   "aegr"    "n_ae"    "sae"     "crfstat"
#`crfname` has one level for the whole dataset
db$ae %>% select_distinct() %>% colnames()
#> [1] "crfname"
#`n_ae` has one level per patient
db$ae %>% select_distinct(.by=subjid) %>% colnames()
#> [1] "subjid"  "crfname" "n_ae"   
```
