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
tm = edc_example()
#> Warning: Option "edc_lookup" has been overwritten.
load_list(tm)
x = get_common_cols(min_datasets=1)
x
#> # A tibble: 16 × 7
#>    column      name_in datasets n_datasets pct_datasets datasets_in datasets_out
#>    <chr>       <list>  <list>        <int>        <dbl> <chr>       <chr>       
#>  1 SUBJID      <lgl>   <chr>             4         1    db0, db2, … ""          
#>  2 crfname     <lgl>   <chr>             4         1    db0, db2, … ""          
#>  3 age         <lgl>   <chr>             1         0.25 db0         "db2, db3, …
#>  4 date1       <lgl>   <chr>             1         0.25 db1         "db0, db2, …
#>  5 date10      <lgl>   <chr>             1         0.25 db3         "db0, db2, …
#>  6 date2       <lgl>   <chr>             1         0.25 db1         "db0, db2, …
#>  7 date3       <lgl>   <chr>             1         0.25 db1         "db0, db2, …
#>  8 date4       <lgl>   <chr>             1         0.25 db2         "db0, db3, …
#>  9 date5       <lgl>   <chr>             1         0.25 db2         "db0, db3, …
#> 10 date6       <lgl>   <chr>             1         0.25 db2         "db0, db3, …
#> 11 date7       <lgl>   <chr>             1         0.25 db3         "db0, db2, …
#> 12 date8       <lgl>   <chr>             1         0.25 db3         "db0, db2, …
#> 13 date9       <lgl>   <chr>             1         0.25 db3         "db0, db2, …
#> 14 date_naiss… <lgl>   <chr>             1         0.25 db0         "db2, db3, …
#> 15 group       <lgl>   <chr>             1         0.25 db0         "db2, db3, …
#> 16 x           <lgl>   <chr>             1         0.25 db1         "db0, db2, …
summary(x)
#> # A tibble: 2 × 7
#>   pct_datasets n_datasets n_distinct_datasets n_columns columns    datasets   
#>   <chr>             <int>               <int>     <int> <list>     <list>     
#> 1 100%                  4                   1         2 <chr [2]>  <list [2]> 
#> 2 25%                   1                   4        14 <chr [14]> <list [14]>
#> # ℹ 1 more variable: columns_str <chr>
```
