# Read the `.zip` archive of a TrialMaster export

Import the `.zip` archive of a TrialMaster trial export as a list of
dataframes. The archive filename should be leaved untouched as it
contains the project name and the date of extraction.  
Generate a `.rds` cache file for future reads.  
If `7zip` is not installed or available, use
[`read_all_xpt()`](https://danchaltiel.github.io/EDCimport/reference/read_all_xpt.md)
instead.  
  
The TM export should be of type `SAS Xport`, with the checkbox "Include
Codelists" ticked.

## Usage

``` r
read_trialmaster(
  archive,
  ...,
  use_cache = "write",
  clean_names_fun = NULL,
  subdirectories = FALSE,
  pw = getOption("trialmaster_pw"),
  verbose = getOption("edc_read_verbose", 1),
  key_columns = "deprecated"
)
```

## Arguments

- archive:

  \[`character(1)`\]  
  the path to the archive

- ...:

  unused

- use_cache:

  \[`mixed(1)`: "write"\]  
  controls the `.rds` cache. If `TRUE`, read the cache if any or extract
  the archive and create a cache. If `FALSE` extract the archive without
  creating a cache file. Can also be `"read"` or `"write"`.

- clean_names_fun:

  **\[deprecated\]** use
  [`edc_clean_names()`](https://danchaltiel.github.io/EDCimport/reference/edc_clean_names.md)
  instead.

- subdirectories:

  \[`logical(1)`\]  
  whether to read subdirectories

- pw:

  \[`character(1)`\]  
  The password if the archive is protected. To avoid writing passwords
  in plain text, it is probably better to use
  `options(trialmaster_pw="xxx")` instead though.

- verbose:

  \[`numeric(1)`\]  
  one of `c(0, 1, 2)`. The higher, the more information will be printed.

- key_columns:

  deprecated

## Value

a list containing one dataframe for each `.xpt` file in the folder, the
extraction date (`datetime_extraction`), and a summary of all imported
tables (`.lookup`).

## See also

Other EDCimport reading functions:
[`read_all_csv()`](https://danchaltiel.github.io/EDCimport/reference/read_all_csv.md),
[`read_all_sas()`](https://danchaltiel.github.io/EDCimport/reference/read_all_sas.md),
[`read_all_xpt()`](https://danchaltiel.github.io/EDCimport/reference/read_all_xpt.md)
