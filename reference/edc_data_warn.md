# Standardized warning system

When checking your data, filter your dataset to get only problematic
rows.  
Then, use either:

- `edc_data_warn()` to generate a standardized warning that can be
  forwarded to the datamanager.

- `edc_data_stop()` to abort the script if the problem is too serious.

Each time edc_data_warn is used, the warning is saved internally so that
a summary of all your warnings can be retrieved using
edc_data_warnings.  
The result can be saved into an Excel file using
[`save_edc_data_warnings()`](https://danchaltiel.github.io/EDCimport/reference/save_edc_data_warnings.md).

## Usage

``` r
edc_data_warn(
  df,
  message,
  ...,
  issue_n = "xx",
  max_subjid = 5,
  csv_path = FALSE,
  envir = parent.frame(),
  col_subjid = get_subjid_cols()
)

edc_data_stop(df, message, ..., issue_n, max_subjid, csv_path, envir, col_subjid)

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

- envir:

  the environment to evaluate `message` in.

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
db = edc_example()
#> Warning: Option "edc_lookup" has been overwritten.
load_database(db)
enrol %>% 
  filter(age>70) %>% 
  edc_data_warn("Age should not be >70", issue_n=1)
#> Warning: Datasets from this lookup are not available in the global environment.
#> ℹ Did you forget to use `EDCimport::load_database(db)` to load the tables?
#> This warning is displayed once per session.
#> Warning: Issue #01: Age should not be >70 (2 patients: #9 and #12)

enrol %>% 
  filter(age<25) %>% 
  edc_data_warn("Age should not be <25", issue_n=2)
#> Warning: Issue #02: Age should not be <25 (1 patient: #18)

data1 %>% 
  filter(n()>1, .by=subjid) %>% 
  edc_data_warn("There are duplicated patients in `data1` ({nrow(.data)} rows)", issue_n=3)
#> Warning: Issue #03: There are duplicated patients in `data1` (100 rows) (50 patients:
#> #1, #2, #3, #4, #5, …)

enrol %>% 
  filter(age<25) %>% 
  edc_data_warn("Age should not be <25", issue_n=NULL)
#> Warning: Age should not be <25 (1 patient: #18)
  
edc_data_warnings()
#> # A tibble: 3 × 5
#>   issue_n message                                          subjid data     type 
#>   <chr>   <chr>                                            <list> <list>   <chr>
#> 1 01      Age should not be >70                            <chr>  <tibble> WARN 
#> 2 02      Age should not be <25                            <chr>  <tibble> WARN 
#> 3 03      There are duplicated patients in `data1` (100 r… <chr>  <tibble> WARN 

if (FALSE) { # \dontrun{
enrol %>% 
  filter(age<25) %>% 
  edc_data_warn("Age should not be <25", csv_path="check/check_age_25.csv")
  
enrol %>% 
  filter(age<25) %>% 
  edc_data_stop("Age should *never* be <25")
} # }
```
