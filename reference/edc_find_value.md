# Search the whole database

Find a keyword in columns or values, in all the datasets of the
database.

## Usage

``` r
edc_find_value(
  keyword,
  ignore_case = TRUE,
  data = get_datasets(),
  lookup = edc_lookup()
)

edc_find_column(keyword, ignore_case = TRUE, lookup = edc_lookup())
```

## Arguments

- keyword:

  The keyword to search for. Regular expressions are only supported in
  `edc_find_column`.

- ignore_case:

  Logical. If `TRUE` (default), the search will ignore case differences.

- data:

  A list of datasets.

- lookup:

  A lookup table.

## Value

a tibble

## Examples

``` r
db = edc_example()
#> Warning: Option "edc_lookup" has been overwritten.
load_database(db)

edc_find_value("respi")
#> # A tibble: 8 × 5
#>   subjid dataset column column_label value                                      
#>   <chr>  <chr>   <chr>  <chr>        <chr>                                      
#> 1 10     ae      aesoc  AE SOC       Respiratory, thoracic and mediastinal diso…
#> 2 13     ae      aesoc  AE SOC       Respiratory, thoracic and mediastinal diso…
#> 3 13     ae      aesoc  AE SOC       Respiratory, thoracic and mediastinal diso…
#> 4 17     ae      aesoc  AE SOC       Respiratory, thoracic and mediastinal diso…
#> 5 22     ae      aesoc  AE SOC       Respiratory, thoracic and mediastinal diso…
#> 6 29     ae      aesoc  AE SOC       Respiratory, thoracic and mediastinal diso…
#> 7 44     ae      aesoc  AE SOC       Respiratory, thoracic and mediastinal diso…
#> 8 47     ae      aesoc  AE SOC       Respiratory, thoracic and mediastinal diso…
edc_find_value(2010)
#> # A tibble: 700 × 5
#>    subjid dataset column column_label    value     
#>    <chr>  <chr>   <chr>  <chr>           <chr>     
#>  1 1      data1   date1  Date at visit 1 2010-04-26
#>  2 1      data1   date1  Date at visit 1 2010-04-26
#>  3 2      data1   date1  Date at visit 1 2010-04-15
#>  4 2      data1   date1  Date at visit 1 2010-04-15
#>  5 3      data1   date1  Date at visit 1 2010-05-08
#>  6 3      data1   date1  Date at visit 1 2010-05-08
#>  7 4      data1   date1  Date at visit 1 2010-04-29
#>  8 4      data1   date1  Date at visit 1 2010-04-29
#>  9 5      data1   date1  Date at visit 1 2010-04-23
#> 10 5      data1   date1  Date at visit 1 2010-04-23
#> # ℹ 690 more rows

edc_find_column("ad")
#> # A tibble: 1 × 4
#>   dataset crfname        names labels  
#>   <chr>   <chr>          <chr> <chr>   
#> 1 ae      Adverse events aegr  AE grade
edc_find_column("date") 
#> # A tibble: 11 × 4
#>    dataset crfname names      labels           
#>    <chr>   <chr>   <chr>      <chr>            
#>  1 data1   data1   date1      Date at visit 1  
#>  2 data1   data1   date2      Date at visit 2  
#>  3 data1   data1   date3      Date at visit 3  
#>  4 data2   data2   date4      Date at visit 4  
#>  5 data2   data2   date5      Date at visit 5  
#>  6 data2   data2   date6      Date at visit 6  
#>  7 data3   data3   date7      Date at visit 7  
#>  8 data3   data3   date8      Date at visit 8  
#>  9 data3   data3   date9      Date at visit 9  
#> 10 data3   data3   date10     Date at visit 10 
#> 11 enrol   enrol   enrol_date Date of enrolment
#with regex
edc_find_column("\\d")
#> # A tibble: 16 × 4
#>    dataset    crfname                  names  labels          
#>    <chr>      <chr>                    <chr>  <chr>           
#>  1 long_pure  long data                val1a  val1a           
#>  2 long_pure  long data                val2a  val2a           
#>  3 data1      data1                    date1  Date at visit 1 
#>  4 data1      data1                    date2  Date at visit 2 
#>  5 data1      data1                    date3  Date at visit 3 
#>  6 long_mixed both short and long data long1  long1           
#>  7 long_mixed both short and long data long2  long2           
#>  8 data2      data2                    date4  Date at visit 4 
#>  9 data2      data2                    date5  Date at visit 5 
#> 10 data2      data2                    date6  Date at visit 6 
#> 11 data3      data3                    date7  Date at visit 7 
#> 12 data3      data3                    date8  Date at visit 8 
#> 13 data3      data3                    date9  Date at visit 9 
#> 14 data3      data3                    date10 Date at visit 10
#> 15 short      short data               val1   val1            
#> 16 short      short data               val2   val2            
edc_find_column("\\(") #you need to escape special characters
#> # A tibble: 1 × 4
#>   dataset crfname names labels     
#>   <chr>   <chr>   <chr> <chr>      
#> 1 enrol   enrol   age   Age (years)
```
