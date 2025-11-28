# Get columns that are common to multiple datasets

**\[experimental\]** Attempt to list all columns in the database and
group the ones that are common to some datasets. Useful to find keys to
pivot or summarise data.

## Usage

``` r
get_common_cols(lookup = edc_lookup(), min_datasets = 3)

# S3 method for class 'common_cols'
summary(object, ...)
```

## Arguments

- lookup:

  the lookup table, default to
  [`edc_lookup()`](https://danchaltiel.github.io/EDCimport/reference/edc_lookup.md)

- min_datasets:

  the minimal number of datasets to be considered

- object:

  an object of class "common_cols"

- ...:

  unused

## Value

a tibble of class "common_cols"

## Examples

``` r
db = edc_example()
#> Warning: Option "edc_lookup" has been overwritten.
load_database(db)
x = get_common_cols(min_datasets=1)
x
#> # A tibble: 28 × 7
#>    column  name_in   datasets  n_datasets pct_datasets datasets_in  datasets_out
#>    <chr>   <list>    <list>         <int>        <dbl> <chr>        <chr>       
#>  1 crfname <lgl [8]> <chr [8]>          8        1     long_pure, … ""          
#>  2 subjid  <lgl [8]> <chr [8]>          8        1     long_pure, … ""          
#>  3 crfstat <lgl [8]> <chr [7]>          7        0.875 data1, long… "long_pure" 
#>  4 aegr    <lgl [8]> <chr [1]>          1        0.125 ae           "long_pure,…
#>  5 aesoc   <lgl [8]> <chr [1]>          1        0.125 ae           "long_pure,…
#>  6 age     <lgl [8]> <chr [1]>          1        0.125 enrol        "long_pure,…
#>  7 arm     <lgl [8]> <chr [1]>          1        0.125 enrol        "long_pure,…
#>  8 date1   <lgl [8]> <chr [1]>          1        0.125 data1        "long_pure,…
#>  9 date10  <lgl [8]> <chr [1]>          1        0.125 data3        "long_pure,…
#> 10 date2   <lgl [8]> <chr [1]>          1        0.125 data1        "long_pure,…
#> # ℹ 18 more rows
summary(x)
#> # A tibble: 3 × 7
#>   pct_datasets n_datasets n_distinct_datasets n_columns columns    datasets   
#>   <chr>             <int>               <int>     <int> <list>     <list>     
#> 1 100%                  8                   1         2 <chr [2]>  <list [2]> 
#> 2 88%                   7                   1         1 <chr [1]>  <list [1]> 
#> 3 12%                   1                   8        25 <chr [25]> <list [25]>
#> # ℹ 1 more variable: columns_str <chr>
```
