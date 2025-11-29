# Save the database as an Excel file

Because RStudio is not very good at showing data, it can be more
convenient to browse the database using MS Excel. This function turns
the whole TM export (or any named list of datasets) into an Excel
workbook, with one tab for each dataset.  
Use `edc_db_to_excel()` to create the file and `edc_browse_excel()` to
open it.

## Usage

``` r
edc_db_to_excel(
  filename = tempfile(fileext = ".xlsx"),
  ...,
  datasets = get_datasets(),
  overwrite = FALSE,
  open = FALSE
)

edc_browse_excel()
```

## Arguments

- filename:

  the path to the Excel output file. Default to a temporary file. Use
  the special value `TRUE` to save in
  "data/database\_{date_extraction}.xlsx".

- ...:

  unused

- datasets:

  a named list of dataframes. Default to the TM export.

- overwrite:

  whether to overwrite any existing file. Default to `FALSE`.

- open:

  whether to open the Excel file afterward. Default to `FALSE`.

## Value

nothing

## Examples

``` r
if (FALSE) { # \dontrun{
  tm = edc_example()
  load_list(tm)  
  edc_db_to_excel() #default arguments are usually OK
  edc_db_to_excel(filename=TRUE)
} # }
```
