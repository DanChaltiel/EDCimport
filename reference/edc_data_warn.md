# Standardized warning system

When checking your data, filter your dataset to get only problematic
rows.  
Then, use either:

- `edc_data_warn()` to generate a standardized warning that can be
  forwarded to the datamanager

- `edc_data_warn()` to abort the script if the problem is too serious

Database issues should be traced in a separate file, each with an
identifying row number, and the file should be shared with the
data-manager.  
Use `edc_data_warnings()` to generate the table for such a file.

## Usage

``` r
edc_data_warn(
  df,
  message,
  ...,
  issue_n = "xx",
  max_subjid = 5,
  csv_path = FALSE,
  col_subjid = get_subjid_cols()
)

edc_data_stop(df, message, ..., issue_n, max_subjid, csv_path, col_subjid)

edc_data_warnings()
```

## Arguments

- df:

  the filtered dataframe

- message:

  the message. Can use [cli
  formats](https://cli.r-lib.org/reference/inline-markup.html#classes).
  `df` can be accessed using the `.data` special keyword (see example)

- ...:

  unused

- issue_n:

  identifying row number

- max_subjid:

  max number of subject ID to show in the message

- csv_path:

  a path to save `df` in a csv file that can be shared with the DM for
  more details.

- col_subjid:

  column name for subject ID. Set to `NULL` to ignore.

## Value

`df` invisibly

## Examples

``` r
library(dplyr)
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union
tm = edc_example()
#> Warning: Option "edc_lookup" has been overwritten.
load_list(tm)
db0 %>% 
  filter(age>70) %>% 
  edc_data_warn("Age should not be >70", issue_n=1)
#> Warning: Issue #01: Age should not be >70 (2 patients: #9 and #12)

db0 %>% 
  filter(age<25) %>% 
  edc_data_warn("Age should not be <25", issue_n=2)
#> Warning: Issue #02: Age should not be <25 (1 patient: #18)

db1 %>% 
  filter(n()>1, .by=SUBJID) %>% 
  edc_data_warn("There are duplicated patients in `db1` ({nrow(.data)} rows)", issue_n=3)
#> Warning: Issue #03: There are duplicated patients in `db1` (100 rows) (50 patients: #1,
#> #2, #3, #4, #5, …)

db0 %>% 
  filter(age<25) %>% 
  edc_data_warn("Age should not be <25", issue_n=NULL)
#> Warning: Age should not be <25 (1 patient: #18)
  
edc_data_warnings()
#> # A tibble: 4 × 4
#>   issue_n message                                           subjid     fun     
#>   <chr>   <chr>                                             <list>     <chr>   
#> 1 01      Age should not be >70                             <chr [2]>  cli_warn
#> 2 02      Age should not be <25                             <chr [1]>  cli_warn
#> 3 03      There are duplicated patients in `db1` (100 rows) <chr [50]> cli_warn
#> 4 NA      Age should not be <25                             <chr [1]>  cli_warn

if (FALSE) { # \dontrun{
db0 %>% 
  filter(age<25) %>% 
  edc_data_warn("Age should not be <25", csv_path="check/check_age_25.csv")
  
db0 %>% 
  filter(age<25) %>% 
  edc_data_stop("Age should *never* be <25")
} # }
```
