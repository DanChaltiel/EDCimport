# Read all `.csv` files in a directory

Read all `.csv` files in a directory, with labels if specified.

## Usage

``` r
read_all_csv(
  path,
  ...,
  labels_from = NULL,
  format_file = NULL,
  use_cache = "write",
  subdirectories = FALSE,
  read_fun = "guess",
  datetime_extraction = "guess",
  verbose = getOption("edc_read_verbose", 1),
  clean_names_fun = NULL
)
```

## Arguments

- path:

  \[`character(1)`\]  
  path to the directory containing `.csv` files.

- ...:

  unused

- labels_from:

  \[`character(1)`\]  
  path to the file containing the labels. See section "Labels file"
  below.

- format_file:

  \[`character(1)`\]  
  the path to the file that should be used to apply formats. See section
  "Format file" below. Use `NULL` to not apply formats.

- use_cache:

  \[`mixed(1)`: "write"\]  
  controls the `.rds` cache. If `TRUE`, read the cache if any or extract
  the archive and create a cache. If `FALSE` extract the archive without
  creating a cache file. Can also be `"read"` or `"write"`.

- subdirectories:

  \[`logical(1)`\]  
  whether to read subdirectories

- read_fun:

  \[`function`\]  
  if "guess" doesn't work properly, a function to read the files in
  path, e.g. `read.csv`, `read.csv2`,...

- datetime_extraction:

  \[`POSIXt(1)`\]  
  the datetime of the data extraction. Default to the most common date
  of last modification in `path`.

- verbose:

  \[`numeric(1)`\]  
  one of `c(0, 1, 2)`. The higher, the more information will be printed.

- clean_names_fun:

  **\[deprecated\]** use
  [`edc_clean_names()`](https://danchaltiel.github.io/EDCimport/reference/edc_clean_names.md)
  instead.

## Value

a list containing one dataframe for each `.csv` file in the folder, the
extraction date (`datetime_extraction`), and a summary of all imported
tables (`.lookup`).

## Labels file

`labels_from` should contain the information about column labels. It
should be a data file (`.csv`) containing 2 columns: one for the column
name and the other for its associated label. Use
`options(edc_col_name="xxx", edc_col_label="xxx")` to specify the names
of the columns.

## Format file

`format_file` should contain the information about SAS formats. It can
be either:

- a `procformat.sas` file, containing the whole PROC FORMAT

- or a data file (`.csv` or `.sas7bdat`) containing 3 columns:

  - `FMTNAME` the SAS format name (repeated)

  - `START` the variable level

  - `LABEL` the label associated to the level

  You can get this datafile [from
  SAS](https://blogs.sas.com/content/sgf/2017/12/04/controlling-your-formats/)
  using `PROC FORMAT` with option `CNTLOUT`. Otherwise, you can use
  `options(edc_var_format_name="xxx", edc_var_level="xxx", edc_var_label="xxx")`
  to specify different column names.

## See also

Other EDCimport reading functions:
[`read_all_sas()`](https://danchaltiel.github.io/EDCimport/reference/read_all_sas.md),
[`read_all_xpt()`](https://danchaltiel.github.io/EDCimport/reference/read_all_xpt.md),
[`read_trialmaster()`](https://danchaltiel.github.io/EDCimport/reference/read_trialmaster.md)

## Examples

``` r
# Create a directory with multiple csv files and a label lookup.
path = paste0(tempdir(), "/read_all_csv")
dir.create(paste0(path, "/subdir"), recursive=TRUE)
write.csv(iris, paste0(path, "/iris.csv"))
write.csv(mtcars, paste0(path, "/mtcars.csv"))
write.csv(mtcars, paste0(path, "/subdir/mtcars.csv"))
write.csv(airquality, paste0(path, "/airquality.csv"))
labs = c(iris, mtcars, airquality) %>% names()
write.csv(data.frame(name=labs, label=toupper(labs)), paste0(path, "/labels.csv"))


db = read_all_csv(path, labels_from="labels.csv", subdirectories=TRUE) %>% 
  set_project_name("My great project")
#> Writing cache /tmp/RtmpyFwGkN/read_all_csv/EDCimport_cache_9cc943d7.rds
#> Warning: Option "edc_lookup" has been overwritten.
db
#> ── EDCimport database ──────────────────────────────────────────────────────────
#> Contains 4 tables: `airquality`, `iris`, `mtcars`, and `subdir_mtcars`
#> ℹ Use `EDCimport::load_database(db)` to load the tables in the global
#>   environment.
#> ℹ Use `EDCimport::edc_lookup()` to see the summary table.
edc_lookup()
#> ── Lookup table - My great project (extraction of 2025-12-30) - EDCimport v0.6.0
#>   dataset        nrow  ncol  n_id rows_per_id crfname
#>   <chr>         <dbl> <dbl> <int>       <dbl> <chr>  
#> 1 airquality      153     7     0          NA NA     
#> 2 iris            150     6     0          NA NA     
#> 3 mtcars           32    12     0          NA NA     
#> 4 subdir_mtcars    32    12     0          NA NA     
```
