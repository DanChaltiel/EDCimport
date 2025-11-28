# Join within the EDCimport framework

Perform a join with default `by` to the Subject ID and default suffix to
the name of the `y` dataset. See `[dplyr::mutate-joins]` for the
description of the join logic.

## Usage

``` r
edc_left_join(
  x,
  y,
  by = NULL,
  suffix = NULL,
  cols = everything(),
  remove_dups = FALSE
)
```

## Arguments

- x, y:

  Data frames to join

- by:

  The key to join on, as character. Defaults to
  [`get_subjid_cols()`](https://danchaltiel.github.io/EDCimport/reference/get_subjid_cols.md)

- suffix:

  The disambiguation suffix. Defaults to the actual name of the `y`
  dataset.

- cols:

  \<[tidy-select](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  The columns to select in `y` before joining.

- remove_dups:

  Whether to remove columns in `y` that already exist in `x`.

## Value

a dataframe

## Examples

``` r
db = edc_example()
#> Warning: Option "edc_lookup" has been overwritten.
load_database(db)
data1$common = data2$common = "Common"
x = enrol %>% 
  edc_left_join(data2) %>% 
  edc_right_join(data1)
  
#crfname get a suffix, common 
names(x)
#>  [1] "subjid"        "age"           "enrol_date"    "arm"          
#>  [5] "crfname"       "crfstat"       "date4"         "date5"        
#>  [9] "date6"         "crfname_data2" "crfstat_data2" "common"       
#> [13] "date1"         "date2"         "date3"         "x"            
#> [17] "crfname_data1" "crfstat_data1" "common_data1" 
```
