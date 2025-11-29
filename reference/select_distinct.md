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
tm = edc_example_ae()
#> Warning: Option "edc_lookup" has been overwritten.
tm$ae %>% names
#> [1] "subjid"  "aesoc"   "aegr"    "n_ae"    "sae"     "crfname"
tm$ae %>% select_distinct() %>% names
#> [1] "crfname"
tm$ae %>% select_distinct(.by=subjid) %>% names
#> [1] "subjid"  "n_ae"    "crfname"
```
