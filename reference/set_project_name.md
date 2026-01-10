# Set the project name

Set or override the project name

## Usage

``` r
set_project_name(db, name)

get_project_name(lookup = edc_lookup())
```

## Arguments

- db:

  the
  [edc_database](https://danchaltiel.github.io/EDCimport/reference/edc_database.md)

- name:

  the project name

- lookup:

  the lookup table

## Value

nothing

the name of the project

## Examples

``` r
db = edc_example() %>% 
 set_project_name("My great project")
#> Warning: Option "edc_lookup" has been overwritten.
edc_lookup()
#> ── Lookup table - My great project (extraction of 2024-01-01) - EDCimport v0.7.0
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
```
