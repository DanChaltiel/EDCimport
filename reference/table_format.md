# Identify if a dataframe has a long or a wide format

A dataset is either in the wide format or in the long format. This
function identifies the format of a dataframe with respect to a subject
ID. If a dataframe has some wide and long columns, it is considered
"mixed".

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

  columns to ignore.

- na_rm:

  whether to consider missing values

- warn:

  whether to warn if ID is not found

## Value

a string value in `c("wide", "long", "mixed)`

## See also

<https://tidyr.tidyverse.org/articles/pivot.html>

## Examples

``` r
db = edc_example()
#> Warning: Option "edc_lookup" has been overwritten.
sapply(db, table_format, warn=FALSE) 
#> $enrol
#> [1] "wide"
#> 
#> $data1
#> [1] "mixed"
#> 
#> $data2
#> [1] "wide"
#> 
#> $data3
#> [1] "wide"
#> 
#> $short
#> [1] "wide"
#> 
#> $long_pure
#> [1] "long"
#> 
#> $long_mixed
#> [1] "mixed"
#> 
#> $ae
#> [1] "mixed"
#> 
#> $datetime_extraction
#> NULL
#> 
#> $date_extraction
#> NULL
#> 
#> $.lookup
#> NULL
#> 
```
