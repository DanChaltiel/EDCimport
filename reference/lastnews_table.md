# Get a table with the latest date for each patient

This function search for date columns in every tables and returns the
latest date for each patient with the variable it comes from. Useful in
survival analysis to get the right censoring time.

## Usage

``` r
lastnews_table(
  except = NULL,
  with_ties = FALSE,
  show_delta = FALSE,
  numeric_id = TRUE,
  prefer = NULL,
  regex = FALSE,
  warn_if_future = TRUE
)
```

## Arguments

- except:

  the datasets/columns that should not be searched. Example: a scheduled
  visit for which the patient may have died before attending should not
  be considered.

- with_ties:

  in case of tie, whether to return the first `origin` (FALSE) or all
  the origins that share this tie (TRUE).

- show_delta:

  whether to compute the difference between the last `prefer` date and
  the actual last date

- numeric_id:

  set to FALSE if the patient ID column is not numeric

- prefer:

  preferred origins in the event of a tie. Usually the followup table.

- regex:

  whether to consider `except` and `prefer` as regex.

- warn_if_future:

  whether to show a warning about dates that are after the extraction
  date. Can also be a csv file path to save the warning as csv (see
  `csv_path` argument in
  [edc_data_warn](https://danchaltiel.github.io/EDCimport/reference/edc_data_warn.md)).

## Value

a dataframe

## Examples

``` r
db = edc_example()
#> Warning: Option "edc_lookup" has been overwritten.
load_database(db)
lastnews_table()
#> # A tibble: 50 × 5
#>    subjid last_date  origin_data origin_col origin_label    
#>     <dbl> <date>     <chr>       <chr>      <chr>           
#>  1      1 2010-08-01 data2       date4      Date at visit 4 
#>  2      2 2010-07-31 data2       date4      Date at visit 4 
#>  3      3 2010-07-21 data2       date5      Date at visit 5 
#>  4      4 2010-07-23 data3       date10     Date at visit 10
#>  5      5 2010-07-14 data3       date10     Date at visit 10
#>  6      6 2010-07-20 data3       date10     Date at visit 10
#>  7      7 2010-07-28 data3       date9      Date at visit 9 
#>  8      8 2010-07-19 data3       date9      Date at visit 9 
#>  9      9 2010-08-10 data3       date9      Date at visit 9 
#> 10     10 2010-07-30 data3       date10     Date at visit 10
#> # ℹ 40 more rows
lastnews_table(except="data3")
#> # A tibble: 50 × 5
#>    subjid last_date  origin_data origin_col origin_label   
#>     <dbl> <date>     <chr>       <chr>      <chr>          
#>  1      1 2010-08-01 data2       date4      Date at visit 4
#>  2      2 2010-07-31 data2       date4      Date at visit 4
#>  3      3 2010-07-21 data2       date5      Date at visit 5
#>  4      4 2010-06-19 data2       date6      Date at visit 6
#>  5      5 2010-06-14 data2       date5      Date at visit 5
#>  6      6 2010-06-11 data2       date6      Date at visit 6
#>  7      7 2010-06-16 data2       date6      Date at visit 6
#>  8      8 2010-06-21 data2       date6      Date at visit 6
#>  9      9 2010-05-30 data2       date6      Date at visit 6
#> 10     10 2010-06-11 data2       date6      Date at visit 6
#> # ℹ 40 more rows
lastnews_table(except="data3$date9")
#> # A tibble: 50 × 5
#>    subjid last_date  origin_data origin_col origin_label    
#>     <dbl> <date>     <chr>       <chr>      <chr>           
#>  1      1 2010-08-01 data2       date4      Date at visit 4 
#>  2      2 2010-07-31 data2       date4      Date at visit 4 
#>  3      3 2010-07-21 data2       date5      Date at visit 5 
#>  4      4 2010-07-23 data3       date10     Date at visit 10
#>  5      5 2010-07-14 data3       date10     Date at visit 10
#>  6      6 2010-07-20 data3       date10     Date at visit 10
#>  7      7 2010-07-11 data3       date10     Date at visit 10
#>  8      8 2010-07-12 data3       date10     Date at visit 10
#>  9      9 2010-07-16 data3       date8      Date at visit 8 
#> 10     10 2010-07-30 data3       date10     Date at visit 10
#> # ℹ 40 more rows
lastnews_table(prefer="date10", show_delta=TRUE) 
#> # A tibble: 50 × 8
#>    subjid last_date  origin_data origin_col origin_label     preferred_last_date
#>     <dbl> <date>     <chr>       <chr>      <chr>            <date>             
#>  1      1 2010-08-01 data3       date10     Date at visit 10 2010-08-01         
#>  2      2 2010-07-31 data3       date10     Date at visit 10 2010-07-31         
#>  3      3 2010-07-21 data3       date10     Date at visit 10 2010-07-21         
#>  4      4 2010-07-23 data3       date10     Date at visit 10 2010-07-23         
#>  5      5 2010-07-14 data3       date10     Date at visit 10 2010-07-14         
#>  6      6 2010-07-20 data3       date10     Date at visit 10 2010-07-20         
#>  7      7 2010-07-28 data3       date9      Date at visit 9  2010-07-11         
#>  8      8 2010-07-19 data3       date9      Date at visit 9  2010-07-12         
#>  9      9 2010-08-10 data3       date9      Date at visit 9  2010-07-09         
#> 10     10 2010-07-30 data3       date10     Date at visit 10 2010-07-30         
#> # ℹ 40 more rows
#> # ℹ 2 more variables: preferred_origin <chr>, delta <drtn>
lastnews_table() %>% 
  dplyr::count(origin = glue::glue("{origin_data}${origin_col}"), 
  sort=TRUE)
#> # A tibble: 5 × 2
#>   origin           n
#>   <glue>       <int>
#> 1 data3$date10    36
#> 2 data3$date9     10
#> 3 data2$date4      2
#> 4 data2$date5      1
#> 5 data3$date8      1

csv_file = tempfile(fileext=".csv")
lastnews_table(prefer="date9", warn_if_future=csv_file) 
#> # A tibble: 50 × 5
#>    subjid last_date  origin_data origin_col origin_label    
#>     <dbl> <date>     <chr>       <chr>      <chr>           
#>  1      1 2010-08-01 data2       date4      Date at visit 4 
#>  2      2 2010-07-31 data2       date4      Date at visit 4 
#>  3      3 2010-07-21 data2       date5      Date at visit 5 
#>  4      4 2010-07-23 data3       date10     Date at visit 10
#>  5      5 2010-07-14 data3       date10     Date at visit 10
#>  6      6 2010-07-20 data3       date10     Date at visit 10
#>  7      7 2010-07-28 data3       date9      Date at visit 9 
#>  8      8 2010-07-19 data3       date9      Date at visit 9 
#>  9      9 2010-08-10 data3       date9      Date at visit 9 
#> 10     10 2010-07-30 data3       date10     Date at visit 10
#> # ℹ 40 more rows
```
