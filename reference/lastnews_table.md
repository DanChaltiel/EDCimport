# Get a table with the latest date for each patient

This function search for date columns in every tables and returns the
latest date for each patient with the variable it comes from. Useful in
survival analysis to get the right censoring time.

## Usage

``` r
lastnews_table(
  except = NULL,
  with_ties = FALSE,
  numeric_id = TRUE,
  prefer = NULL,
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

- numeric_id:

  set to FALSE if the patient ID column is not numeric

- prefer:

  preferred origins in the event of a tie. Usually the followup table.

- warn_if_future:

  whether to show a warning about dates that are after the extraction
  date

## Value

a dataframe

## Examples

``` r
tm = edc_example_plot()
#> Warning: Option "edc_lookup" has been overwritten.
load_list(tm)
lastnews_table()
#> # A tibble: 50 × 5
#>    subjid last_date           origin_data origin_col origin_label    
#>     <dbl> <dttm>              <chr>       <chr>      <chr>           
#>  1      1 2010-08-01 18:59:37 db3         date10     Date at visit 10
#>  2      2 2010-07-31 15:32:45 db3         date10     Date at visit 10
#>  3      3 2010-07-22 11:24:37 db3         date10     Date at visit 10
#>  4      4 2010-07-23 20:38:32 db3         date10     Date at visit 10
#>  5      5 2010-07-15 07:09:47 db3         date10     Date at visit 10
#>  6      6 2010-07-20 12:27:00 db3         date10     Date at visit 10
#>  7      7 2010-07-28 16:24:09 db3         date9      Date at visit 9 
#>  8      8 2010-07-19 15:24:18 db3         date9      Date at visit 9 
#>  9      9 2010-08-11 03:48:27 db3         date9      Date at visit 9 
#> 10     10 2010-07-30 20:41:23 db3         date10     Date at visit 10
#> # ℹ 40 more rows
lastnews_table(except="db3")
#> # A tibble: 50 × 5
#>    subjid last_date           origin_data origin_col origin_label   
#>     <dbl> <dttm>              <chr>       <chr>      <chr>          
#>  1      1 2010-06-12 10:53:27 db2         date6      Date at visit 6
#>  2      2 2010-06-20 02:27:29 db2         date6      Date at visit 6
#>  3      3 2010-06-12 21:21:28 db2         date6      Date at visit 6
#>  4      4 2010-06-19 20:25:02 db2         date6      Date at visit 6
#>  5      5 2010-06-15 11:26:57 db2         date5      Date at visit 5
#>  6      6 2010-06-11 22:06:25 db2         date6      Date at visit 6
#>  7      7 2010-06-17 07:46:07 db2         date6      Date at visit 6
#>  8      8 2010-06-22 10:18:23 db2         date6      Date at visit 6
#>  9      9 2010-05-31 00:51:54 db2         date6      Date at visit 6
#> 10     10 2010-06-12 03:57:46 db2         date6      Date at visit 6
#> # ℹ 40 more rows
lastnews_table(except="db3$date9")
#> # A tibble: 50 × 5
#>    subjid last_date           origin_data origin_col origin_label    
#>     <dbl> <dttm>              <chr>       <chr>      <chr>           
#>  1      1 2010-08-01 18:59:37 db3         date10     Date at visit 10
#>  2      2 2010-07-31 15:32:45 db3         date10     Date at visit 10
#>  3      3 2010-07-22 11:24:37 db3         date10     Date at visit 10
#>  4      4 2010-07-23 20:38:32 db3         date10     Date at visit 10
#>  5      5 2010-07-15 07:09:47 db3         date10     Date at visit 10
#>  6      6 2010-07-20 12:27:00 db3         date10     Date at visit 10
#>  7      7 2010-07-12 04:59:23 db3         date10     Date at visit 10
#>  8      8 2010-07-12 19:55:50 db3         date10     Date at visit 10
#>  9      9 2010-07-17 06:26:31 db3         date8      Date at visit 8 
#> 10     10 2010-07-30 20:41:23 db3         date10     Date at visit 10
#> # ℹ 40 more rows
lastnews_table(prefer="db2") 
#> # A tibble: 50 × 5
#>    subjid last_date           origin_data origin_col origin_label    
#>     <dbl> <dttm>              <chr>       <chr>      <chr>           
#>  1      1 2010-08-01 18:59:37 db3         date10     Date at visit 10
#>  2      2 2010-07-31 15:32:45 db3         date10     Date at visit 10
#>  3      3 2010-07-22 11:24:37 db3         date10     Date at visit 10
#>  4      4 2010-07-23 20:38:32 db3         date10     Date at visit 10
#>  5      5 2010-07-15 07:09:47 db3         date10     Date at visit 10
#>  6      6 2010-07-20 12:27:00 db3         date10     Date at visit 10
#>  7      7 2010-07-28 16:24:09 db3         date9      Date at visit 9 
#>  8      8 2010-07-19 15:24:18 db3         date9      Date at visit 9 
#>  9      9 2010-08-11 03:48:27 db3         date9      Date at visit 9 
#> 10     10 2010-07-30 20:41:23 db3         date10     Date at visit 10
#> # ℹ 40 more rows
```
