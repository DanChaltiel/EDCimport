# Retrieve the lookup table from options

Retrieve the lookup table from options

## Usage

``` r
edc_lookup(..., check_null = TRUE)
```

## Arguments

- ...:

  passed on to
  [`dplyr::arrange()`](https://dplyr.tidyverse.org/reference/arrange.html)

- check_null:

  whether to stop if lookup is NULL

## Value

the lookup dataframe summarizing the database import

## See also

[`build_lookup()`](https://danchaltiel.github.io/EDCimport/reference/build_lookup.md),
[`extend_lookup()`](https://danchaltiel.github.io/EDCimport/reference/extend_lookup.md)

## Examples

``` r
tm = edc_example()
#> Warning: Option "edc_lookup" has been overwritten.
load_list(tm)
edc_lookup()
#> ── Lookup table  ───────────────────────────────────────────────────────────────
#>   dataset  nrow  ncol
#>   <chr>   <dbl> <dbl>
#> 1 db0        50     5
#> 2 db2        50     5
#> 3 db3        50     6
#> 4 db1       100     6
edc_lookup(dataset)
#> ── Lookup table  ───────────────────────────────────────────────────────────────
#>   dataset  nrow  ncol
#>   <chr>   <dbl> <dbl>
#> 1 db0        50     5
#> 2 db1       100     6
#> 3 db2        50     5
#> 4 db3        50     6
```
