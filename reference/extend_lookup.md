# Extend the lookup table

This utility extends the lookup table to include:

- `n_id` the number of patients present in the dataset

- `rows_per_id` the mean number of row per patient

- `crfname` the actual name of the dataset

## Usage

``` r
extend_lookup(
  lookup,
  ...,
  id_cols = get_subjid_cols(lookup),
  crf_cols = get_crfname_cols(lookup),
  datasets = get_datasets(lookup, envir = parent.frame())
)
```

## Arguments

- lookup:

  \[`data.frame(1)`\]  
  the lookup table

- ...:

  unused

- id_cols, crf_cols:

  \[`character(n)`\]  
  for experts only

- datasets:

  \[`data.frame(n)`\]  
  for experts only

## Value

the lookup, extended

## See also

[`build_lookup()`](https://danchaltiel.github.io/EDCimport/reference/build_lookup.md),
[`edc_lookup()`](https://danchaltiel.github.io/EDCimport/reference/edc_lookup.md)

## Examples

``` r
#tm = read_trialmaster("filename.zip", pw="xx")
tm = edc_example_mixed()
#> Warning: Option "edc_lookup" has been overwritten.
load_list(tm)
.lookup
#> ── Lookup table  ───────────────────────────────────────────────────────────────
#>   dataset     nrow  ncol
#>   <chr>      <dbl> <dbl>
#> 1 short        100     4
#> 2 long_mixed   200     5
#> 3 long_pure    300     4
.lookup = extend_lookup(.lookup)
.lookup
#> ── Lookup table  ───────────────────────────────────────────────────────────────
#>   dataset     nrow  ncol  n_id rows_per_id crfname   
#>   <chr>      <dbl> <dbl> <int>       <dbl> <chr>     
#> 1 long_pure    300     4   100           3 long_pure 
#> 2 long_mixed   200     5   100           2 long_mixed
#> 3 short        100     4   100           1 short     
```
