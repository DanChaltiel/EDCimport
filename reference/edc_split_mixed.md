# Split mixed datasets

Split mixed tables, i.e. tables that hold both long data (N values per
patient) and short data (one value per patient, duplicated on N lines),
into one long table and one short table.

## Usage

``` r
edc_split_mixed(
  database,
  datasets = everything(),
  ...,
  ignore_cols = NULL,
  verbose = FALSE
)
```

## Arguments

- database:

  an
  [edc_database](https://danchaltiel.github.io/EDCimport/reference/edc_database.md)
  object, from
  [`read_trialmaster()`](https://danchaltiel.github.io/EDCimport/reference/read_trialmaster.md)
  or other EDCimport reading functions.

- datasets:

  \<[tidy-select](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  datasets to split in the database

- ...:

  not used, ensure arguments are named

- ignore_cols:

  columns to ignore in long tables. Default to
  `getOption("edc_cols_crfname", "CRFNAME")`. Case-insensitive. Avoid
  splitting tables for useless columns.

- verbose:

  whether to print informations about the process.

## Value

an
[edc_database](https://danchaltiel.github.io/EDCimport/reference/edc_database.md)
object

## Examples

``` r
#db = read_trialmaster("filename.zip", pw="xx")
db = edc_example() %>% 
  edc_split_mixed(c(ae, starts_with("long")), 
                  ignore_cols="crfstat")
#> Warning: Option "edc_lookup" has been overwritten.
  
names(db)
#>  [1] "enrol"               "data1"               "data2"              
#>  [4] "data3"               "short"               "long_pure"          
#>  [7] "long_mixed"          "ae"                  "datetime_extraction"
#> [10] "date_extraction"     ".lookup"             "ae_short"           
#> [13] "ae_long"             "long_mixed_short"    "long_mixed_long"    
edc_lookup()
#> ── Lookup table - EDCimport example (extraction of 2024-01-01) - EDCimport v0.7.
#>    dataset           nrow  ncol  n_id rows_per_id crfname                 
#>    <chr>            <dbl> <dbl> <int>       <dbl> <chr>                   
#>  1 long_pure          150     4    50         3   long data               
#>  2 data1              100     7    50         2   data1                   
#>  3 long_mixed         100     6    50         2   both short and long data
#>  4 long_mixed_long    100     4    50         2   both short and long data
#>  5 data2               50     6    50         1   data2                   
#>  6 data3               50     7    50         1   data3                   
#>  7 enrol               50     6    50         1   enrol                   
#>  8 long_mixed_short    50     3    50         1   both short and long data
#>  9 short               50     5    50         1   short data              
#> 10 ae                 175     7    48         3.6 Adverse events          
#> 11 ae_long            175     5    48         3.6 Adverse events          
#> 12 ae_short            48     3    48         1   Adverse events          

db$ae #`aesoc`, `aegr`, and `sae` are long, but `n_ae` is short
#> # A tibble: 175 × 7
#>    subjid crfname        aesoc                          aegr  n_ae sae   crfstat
#>  *  <int> <chr>          <chr>                         <int> <int> <fct> <chr>  
#>  1      1 Adverse events Endocrine disorders               2     5 No    Incomp…
#>  2      1 Adverse events Gastrointestinal disorders        2     5 No    Comple…
#>  3      1 Adverse events Reproductive system and brea…     2     5 No    Comple…
#>  4      1 Adverse events Renal and urinary disorders       3     5 No    Comple…
#>  5      1 Adverse events Neoplasms benign, malignant …     1     5 No    Comple…
#>  6      2 Adverse events Vascular disorders                3     5 No    Incomp…
#>  7      2 Adverse events Nervous system disorders          3     5 No    Comple…
#>  8      2 Adverse events Injury, poisoning and proced…     1     5 No    Comple…
#>  9      2 Adverse events Hepatobiliary disorders           1     5 No    Comple…
#> 10      2 Adverse events Injury, poisoning and proced…     2     5 No    Comple…
#> # ℹ 165 more rows

db$ae_short
#> # A tibble: 48 × 3
#>    subjid crfname         n_ae
#>     <int> <chr>          <int>
#>  1      1 Adverse events     5
#>  2      2 Adverse events     5
#>  3      3 Adverse events     2
#>  4      4 Adverse events     4
#>  5      5 Adverse events     3
#>  6      6 Adverse events     3
#>  7      7 Adverse events     4
#>  8      8 Adverse events     1
#>  9      9 Adverse events     4
#> 10     10 Adverse events     4
#> # ℹ 38 more rows
db$ae_long
#> # A tibble: 175 × 5
#>    subjid aesoc                                               aegr sae   crfstat
#>     <int> <chr>                                              <int> <fct> <chr>  
#>  1      1 Endocrine disorders                                    2 No    Incomp…
#>  2      1 Gastrointestinal disorders                             2 No    Comple…
#>  3      1 Reproductive system and breast disorders               2 No    Comple…
#>  4      1 Renal and urinary disorders                            3 No    Comple…
#>  5      1 Neoplasms benign, malignant and unspecified (incl…     1 No    Comple…
#>  6      2 Vascular disorders                                     3 No    Incomp…
#>  7      2 Nervous system disorders                               3 No    Comple…
#>  8      2 Injury, poisoning and procedural complications         1 No    Comple…
#>  9      2 Hepatobiliary disorders                                1 No    Comple…
#> 10      2 Injury, poisoning and procedural complications         2 No    Comple…
#> # ℹ 165 more rows
```
