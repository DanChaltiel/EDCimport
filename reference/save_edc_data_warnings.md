# Save EDCimport warning to Excel

Each time
[edc_data_warn](https://danchaltiel.github.io/EDCimport/reference/edc_data_warn.md)
is used, the warning is saved internally so that a summary can be
retrieved using
[edc_data_warnings](https://danchaltiel.github.io/EDCimport/reference/edc_data_warn.md).
This summary can then be saved into a `.xlsx` file using
`save_edc_data_warnings()`.

## Usage

``` r
save_edc_data_warnings(
  edc_warnings = edc_data_warnings(),
  path = "edc_data_warnings.xlsx",
  overwrite = TRUE,
  open = FALSE
)
```

## Arguments

- edc_warnings:

  the result of
  [edc_data_warnings](https://danchaltiel.github.io/EDCimport/reference/edc_data_warn.md)

- path:

  a `.xlsx` file path

- overwrite:

  If `TRUE`, overwrite any existing file.

- open:

  If `TRUE`, overwrite any existing file.

## Value

a logical(1), whether the file could be written, invisibly
