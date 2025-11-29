# Read the `.zip` archive of a TrialMaster export

Import the `.zip` archive of a TrialMaster trial export as a list of
dataframes. The archive filename should be leaved untouched as it
contains the project name and the date of extraction.  
Generate a `.rds` cache file for future reads.  
If `7zip` is not installed or available, use
[`read_tm_all_xpt()`](https://danchaltiel.github.io/EDCimport/reference/read_all_xpt.md)
instead.

## Usage

``` r
read_trialmaster(
  archive,
  ...,
  use_cache = "write",
  clean_names_fun = NULL,
  split_mixed = FALSE,
  extend_lookup = TRUE,
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

  \[`function`\]  
  a function to clean column names, e.g.
  [tolower](https://rdrr.io/r/base/chartr.html),
  [`janitor::clean_names()`](https://sfirke.github.io/janitor/reference/clean_names.html),...

- split_mixed:

  \[`logical(1): FALSE`\]  
  whether to split mixed datasets. See
  [split_mixed_datasets](https://danchaltiel.github.io/EDCimport/reference/split_mixed_datasets.md).

- extend_lookup:

  \[`character(1): FALSE`\]  
  whether to enrich the lookup table. See
  [extend_lookup](https://danchaltiel.github.io/EDCimport/reference/extend_lookup.md).

- pw:

  \[`character(1)`\]  
  The password if the archive is protected. To avoid writing passwords
  in plain text, it is probably better to use
  `options(trialmaster_pw="xxx")` instead though.

- verbose:

  \[`logical(1)`\]  
  one of `c(0, 1, 2)`. The higher, the more information will be printed.

- key_columns:

  deprecated

## Value

a list containing one dataframe for each `.xpt` file in the folder, the
extraction date (`datetime_extraction`), and a summary of all imported
tables (`.lookup`).
