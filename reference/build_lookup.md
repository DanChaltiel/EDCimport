# Generate a lookup table

Generate a lookup table

## Usage

``` r
build_lookup(data_list)
```

## Arguments

- data_list:

  a list containing at least 1 dataframe

## Value

a dataframe summarizing column names and labels

## See also

[`extend_lookup()`](https://danchaltiel.github.io/EDCimport/reference/extend_lookup.md),
[`edc_lookup()`](https://danchaltiel.github.io/EDCimport/reference/edc_lookup.md)

## Examples

``` r
x = edc_example()
x$.lookup=NULL
lk = build_lookup(x)
lk
#> ── Lookup table  ───────────────────────────────────────────────────────────────
#>   dataset  nrow  ncol
#>   <chr>   <dbl> <dbl>
#> 1 db0        50     5
#> 2 db2        50     5
#> 3 db3        50     6
#> 4 db1       100     6
lk %>% tidyr::unnest(c(names, labels))  
#> # A tibble: 22 × 5
#>    dataset  nrow  ncol names          labels      
#>    <chr>   <dbl> <dbl> <chr>          <named list>
#>  1 db0        50     5 SUBJID         <chr [1]>   
#>  2 db0        50     5 age            <chr [1]>   
#>  3 db0        50     5 date_naissance <chr [1]>   
#>  4 db0        50     5 group          <chr [1]>   
#>  5 db0        50     5 crfname        <chr [1]>   
#>  6 db2        50     5 SUBJID         <chr [1]>   
#>  7 db2        50     5 date4          <chr [1]>   
#>  8 db2        50     5 date5          <chr [1]>   
#>  9 db2        50     5 date6          <chr [1]>   
#> 10 db2        50     5 crfname        <chr [1]>   
#> # ℹ 12 more rows
```
