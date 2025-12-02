# Read all `.sas7bdat` files in a directory

Read all `.sas7bdat` files in a directory. Formats (factors levels) can
be applied from a `procformat.sas` SAS file, or from a format
dictionary. See the "Format file" section below. Column labels are read
directly from the `.sas7bdat` files.

## Usage

``` r
read_all_sas(
  path,
  ...,
  format_file = "procformat.sas",
  subdirectories = FALSE,
  datetime_extraction = "guess",
  verbose = getOption("edc_read_verbose", 1),
  clean_names_fun = NULL
)
```

## Arguments

- path:

  \[`character(1)`\]  
  the path to the directory containing all `.sas7bdat` files.

- ...:

  unused

- format_file:

  \[`character(1)`\]  
  the path to the file that should be used to apply formats. See section
  "Format file" below. Use `NULL` to not apply formats.

- subdirectories:

  \[`logical(1)`\]  
  whether to read subdirectories

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

a list containing one dataframe for each `.xpt` file in the folder, the
extraction date (`datetime_extraction`), and a summary of all imported
tables (`.lookup`).

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
[`read_all_csv()`](https://danchaltiel.github.io/EDCimport/reference/read_all_csv.md),
[`read_all_xpt()`](https://danchaltiel.github.io/EDCimport/reference/read_all_xpt.md),
[`read_trialmaster()`](https://danchaltiel.github.io/EDCimport/reference/read_trialmaster.md)

## Examples

``` r
# Create a directory with multiple sas files.
path = paste0(tempdir(), "/read_all_sas")
dir.create(paste0(path, "/subdir"), recursive=TRUE)
haven::write_sas(attenu, paste0(path, "/attenu.sas7bdat"))
#> Warning: `write_sas()` was deprecated in haven 2.5.2.
#> ℹ Please use `write_xpt()` instead.
haven::write_sas(mtcars, paste0(path, "/mtcars.sas7bdat"))
haven::write_sas(mtcars, paste0(path, "/subdir/mtcars.sas7bdat"))
haven::write_sas(esoph, paste0(path, "/esoph.sas7bdat"))

db = read_all_sas(path, format_file=NULL, subdirectories=TRUE) %>% 
  set_project_name("My great project")
#> Warning: Option "edc_lookup" has been overwritten.
db
#> ── EDCimport database ──────────────────────────────────────────────────────────
#> Contains 4 tables: `attenu`, `esoph`, `mtcars`, and `subdir_mtcars`
#> ℹ Use `EDCimport::load_database(db)` to load the tables in the global
#>   environment.
#> ℹ Use `EDCimport::edc_lookup()` to see the summary table.
edc_lookup()
#> ── Lookup table - My great project (extraction of 2025-12-02) - EDCimport v0.6.0
#>   dataset        nrow  ncol  n_id rows_per_id crfname
#>   <chr>         <dbl> <dbl> <int>       <dbl> <chr>  
#> 1 attenu          182     5     0          NA NA     
#> 2 esoph            88     5     0          NA NA     
#> 3 mtcars           32    11     0          NA NA     
#> 4 subdir_mtcars    32    11     0          NA NA     
```
