# Get columns shared by most datasets

In most trialmaster exports, many datasets share a certain amount of
columns containing meta-data that are often irrelevant to the point.
This function identifies the columns that are present in at least 95% of
datasets (by default)

## Usage

``` r
get_meta_cols(min_pct = getOption("edc_meta_cols_pct", 0.95))
```

## Arguments

- min_pct:

  Default=`0.95`. The minimal proportion of datasets a column has to
  reach. Subject ID is always excluded.

## Value

a character vector

## Examples

``` r
tm = edc_example_mixed()
#> Warning: Option "edc_lookup" has been overwritten.
load_list(tm)
meta_cols = get_meta_cols()
long_mixed %>% dplyr::select(-dplyr::any_of(meta_cols))
#> # A tibble: 200 × 4
#>    SUBJID  val1b val2b val3b
#>     <int>  <dbl> <dbl> <chr>
#>  1      1  0.689 12.3  B    
#>  2      1  0.725 10.5  B    
#>  3      2  0.217 11.0  C    
#>  4      2 -0.202 10.4  C    
#>  5      3 -1.37   9.00 D    
#>  6      3 -0.309  9.40 D    
#>  7      4 -0.453 10.2  E    
#>  8      4  0.663  7.07 E    
#>  9      5  1.31   9.15 F    
#> 10      5  0.501 10.8  F    
#> # ℹ 190 more rows
```
