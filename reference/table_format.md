# Identify if a dataframe has a long or a wide format

A dataset is either in the wide format or in the long format
([link](https://towardsdatascience.com/long-and-wide-formats-in-data-explained-e48d7c9a06cb)).
This function identifies the format of a dataframe with respect to a
subject ID. If a dataframe has some wide and long columns, it is
considered "mixed".

## Usage

``` r
table_format(
  df,
  id = get_subjid_cols(),
  ...,
  ignore_cols = get_meta_cols(0.95),
  na_rm = FALSE,
  warn = TRUE
)
```

## Arguments

- df:

  a dataframe

- id:

  the identifying subject ID

- ...:

  not used

- ignore_cols:

  columns to ignore. Usually meta columns (see
  [get_meta_cols](https://danchaltiel.github.io/EDCimport/reference/get_meta_cols.md)).

- na_rm:

  whether to consider missing values

- warn:

  whether to warn if ID is not found

## Value

a string value in `c("wide", "long", "mixed)`

## Examples

``` r
tm = edc_example_mixed()
#> Warning: Option "edc_lookup" has been overwritten.
sapply(tm, table_format, warn=FALSE) 
#> $short
#> [1] "wide"
#> 
#> $long_pure
#> [1] "long"
#> 
#> $long_mixed
#> [1] "mixed"
#> 
#> $date_extraction
#> NULL
#> 
#> $datetime_extraction
#> NULL
#> 
#> $.lookup
#> NULL
#> 
```
