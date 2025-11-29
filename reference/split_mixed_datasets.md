# Split mixed datasets

Split mixed tables, i.e. tables that hold both long data (N values per
patient) and short data (one value per patient, duplicated on N lines),
into one long table and one short table.

## Usage

``` r
split_mixed_datasets(
  datasets = get_datasets(),
  id = get_subjid_cols(),
  ...,
  ignore_cols = get_meta_cols(0.95),
  output_code = FALSE,
  verbose = TRUE
)
```

## Arguments

- datasets:

  a dataframe or a list of dataframes to split. Default to all the
  datasets from `.lookup`.

- id:

  the patient identifier, probably "SUBJID". Should be shared by all
  datasets. Case-insensitive.

- ...:

  not used

- ignore_cols:

  columns to ignore when considering a table as long. Default to
  `getOption("edc_cols_crfname", "CRFNAME")`. Case-insensitive.

- output_code:

  whether to print the code to explicitly write. Can also be a file
  path.

- verbose:

  whether to print informations about the process.

## Value

a list of the new long and short tables. Use
[`load_list()`](https://danchaltiel.github.io/EDCimport/reference/load_list.md)
to load them into the global environment.

## Examples

``` r
#tm = read_trialmaster("filename.zip", pw="xx")
tm = edc_example_mixed()
#> Warning: Option "edc_lookup" has been overwritten.
names(tm)
#> [1] "short"               "long_pure"           "long_mixed"         
#> [4] "date_extraction"     "datetime_extraction" ".lookup"            
#load_list(tm)
print(tm$long_mixed) #`val1` and `val2` are long but `val3` is short
#> # A tibble: 200 × 5
#>    SUBJID crfname     val1b val2b val3b
#>     <int> <chr>       <dbl> <dbl> <chr>
#>  1      1 long_mixed  0.689 12.3  B    
#>  2      1 long_mixed  0.725 10.5  B    
#>  3      2 long_mixed  0.217 11.0  C    
#>  4      2 long_mixed -0.202 10.4  C    
#>  5      3 long_mixed -1.37   9.00 D    
#>  6      3 long_mixed -0.309  9.40 D    
#>  7      4 long_mixed -0.453 10.2  E    
#>  8      4 long_mixed  0.663  7.07 E    
#>  9      5 long_mixed  1.31   9.15 F    
#> 10      5 long_mixed  0.501 10.8  F    
#> # ℹ 190 more rows

mixed_data = split_mixed_datasets(tm, id="subjid", verbose=TRUE)
#> ✔ There was 1 short table:
#>   "short"
#> ✔ There was 1 pure long table:
#>   "long_pure"
#> ✔ There was 1 mixed (short+long) table:
#>   "long_mixed"
#> → Use `EDCimport::load_list()` on the result to get separated long and short
#>   data.
load_list(mixed_data)
print(long_mixed_short) 
#> # A tibble: 100 × 3
#>    SUBJID crfname    val3b
#>     <int> <chr>      <chr>
#>  1      1 long_mixed B    
#>  2      2 long_mixed C    
#>  3      3 long_mixed D    
#>  4      4 long_mixed E    
#>  5      5 long_mixed F    
#>  6      6 long_mixed G    
#>  7      7 long_mixed H    
#>  8      8 long_mixed I    
#>  9      9 long_mixed J    
#> 10     10 long_mixed K    
#> # ℹ 90 more rows
print(long_mixed_long) 
#> # A tibble: 200 × 3
#>    SUBJID  val1b val2b
#>     <int>  <dbl> <dbl>
#>  1      1  0.689 12.3 
#>  2      1  0.725 10.5 
#>  3      2  0.217 11.0 
#>  4      2 -0.202 10.4 
#>  5      3 -1.37   9.00
#>  6      3 -0.309  9.40
#>  7      4 -0.453 10.2 
#>  8      4  0.663  7.07
#>  9      5  1.31   9.15
#> 10      5  0.501 10.8 
#> # ℹ 190 more rows

#alternatively, get the code and only use the datasets you need
split_mixed_datasets(tm, id="SUBJID", output_code=TRUE)
#> ✔ There was 1 short table:
#>   "short"
#> ✔ There was 1 pure long table:
#>   "long_pure"
#> ✔ There was 1 mixed (short+long) table:
#>   "long_mixed"
#> → Copy the following code in your script to separate long and short data:
#> ## `long_mixed` (dim=200x5) ---- 
#> 
#> long_mixed_short = long_mixed %>% 
#>   select(SUBJID, crfname, val3b) %>% 
#>   group_by(SUBJID) %>% 
#>   summarise(across(everything(), unify)) #dim=100x3 
#> 
#>  long_mixed_long = long_mixed %>% 
#>   select(SUBJID, val1b, val2b) #dim=200x3 
filename = tempfile("mixed_code", fileext=".R")
split_mixed_datasets(tm, id="SUBJID", output_code=filename)
#> ✔ There was 1 short table:
#>   "short"
#> ✔ There was 1 pure long table:
#>   "long_pure"
#> ✔ There was 1 mixed (short+long) table:
#>   "long_mixed"
#> → Copy the code from /tmp/RtmpDnHso8/mixed_code1a227c47aebe.R in your script to
#>   separate long and short data:
#>   `utils::browseURL(/tmp/RtmpDnHso8/mixed_code1a227c47aebe.R)`
readLines(filename)
#> [1] "## `long_mixed` (dim=200x5) ---- "                   
#> [2] ""                                                    
#> [3] "long_mixed_short = long_mixed %>% "                  
#> [4] "  select(SUBJID, crfname, val3b) %>% "               
#> [5] "  group_by(SUBJID) %>% "                             
#> [6] "  summarise(across(everything(), unify)) #dim=100x3 "
#> [7] ""                                                    
#> [8] " long_mixed_long = long_mixed %>% "                  
#> [9] "  select(SUBJID, val1b, val2b) #dim=200x3 "          
```
