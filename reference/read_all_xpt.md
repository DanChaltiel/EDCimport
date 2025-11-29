# Read all `.xpt` files in a directory

Read all `.xpt` files in a directory (unzipped TrialMaster archive).  
If `7zip` is installed, you should probably rather use
[`read_trialmaster()`](https://danchaltiel.github.io/EDCimport/reference/read_trialmaster.md)
instead.  
If a `procformat.sas` file exists in the directory, formats will be
applied.

## Usage

``` r
read_all_xpt(
  path,
  ...,
  format_file = "procformat.sas",
  clean_names_fun = NULL,
  split_mixed = FALSE,
  extend_lookup = TRUE,
  datetime_extraction = "guess",
  verbose = getOption("edc_read_verbose", 1),
  directory = "deprecated",
  key_columns = "deprecated"
)
```

## Arguments

- path:

  \[`character(1)`\]  
  the path to the directory containing all `.xpt` files.

- ...:

  unused

- format_file:

  \[`character(1)`\]  
  the path to the file that should be used to apply formats. See
  details. Use `NULL` to not apply formats.

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

- datetime_extraction:

  \[`POSIXt(1)`\]  
  the datetime of the data extraction. Default to the most common date
  of last modification in `directory`.

- verbose:

  \[`logical(1)`\]  
  one of `c(0, 1, 2)`. The higher, the more information will be printed.

- directory:

  deprecated

- key_columns:

  deprecated

## Value

a list containing one dataframe for each `.xpt` file in the folder, the
extraction date (`datetime_extraction`), and a summary of all imported
tables (`.lookup`).

## Format file

`format_file` should contain the information about SAS formats. It can
be either

- a `procformat.sas` file, containing the whole PROC FORMAT

- or a data file (.csv or .sas7bdat) containing 3 columns: the SAS
  format name (repeated), each level, and its associated label. Use
  `options(edc_var_format_name="xxx", edc_var_level="xxx", edc_var_label="xxx")`
  to specify the names of the columns.
