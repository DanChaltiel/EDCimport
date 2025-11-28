# Retrieve the lookup table from options

Retrieve the lookup table from options

## Usage

``` r
edc_lookup(..., check = TRUE)
```

## Arguments

- ...:

  passed on to
  [`dplyr::arrange()`](https://dplyr.tidyverse.org/reference/arrange.html)

- check:

  whether to check for internal consistency

## Value

the lookup dataframe summarizing the database import

## Examples

``` r
db = edc_example()
#> Warning: Option "edc_lookup" has been overwritten.
load_database(db)
edc_lookup()
#> ── Lookup table - EDCimport example (extraction of 2024-01-01) - EDCimport v0.6.
#>   dataset     nrow  ncol  n_id rows_per_id crfname                 
#>   <chr>      <dbl> <dbl> <int>       <dbl> <chr>                   
#> 1 long_pure    150     4    50         3   long data               
#> 2 data1        100     7    50         2   data1                   
#> 3 long_mixed   100     6    50         2   both short and long data
#> 4 data2         50     6    50         1   data2                   
#> 5 data3         50     7    50         1   data3                   
#> 6 enrol         50     6    50         1   enrol                   
#> 7 short         50     5    50         1   short data              
#> 8 ae           175     7    48         3.6 Adverse events          
edc_lookup(dataset)
#> ── Lookup table - EDCimport example (extraction of 2024-01-01) - EDCimport v0.6.
#>   dataset     nrow  ncol  n_id rows_per_id crfname                 
#>   <chr>      <dbl> <dbl> <int>       <dbl> <chr>                   
#> 1 ae           175     7    48         3.6 Adverse events          
#> 2 data1        100     7    50         2   data1                   
#> 3 data2         50     6    50         1   data2                   
#> 4 data3         50     7    50         1   data3                   
#> 5 enrol         50     6    50         1   enrol                   
#> 6 long_mixed   100     6    50         2   both short and long data
#> 7 long_pure    150     4    50         3   long data               
#> 8 short         50     5    50         1   short data              
```
