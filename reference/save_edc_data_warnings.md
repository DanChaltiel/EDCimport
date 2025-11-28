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
  output_file = "edc_data_warnings_{project}_{date_extraction}.xlsx",
  output_dir = "output/check",
  open = FALSE,
  overwrite = TRUE,
  hide_resolved = TRUE,
  include_stops = FALSE,
  path = "deprecated"
)
```

## Arguments

- edc_warnings:

  the result of
  [edc_data_warnings](https://danchaltiel.github.io/EDCimport/reference/edc_data_warn.md)

- output_file, output_dir:

  path to a `.xlsx` file. Use special values `{proj_name}` and
  `{date_extraction}`.

- open:

  If `TRUE`, overwrite any existing file.

- overwrite:

  If `TRUE`, overwrite any existing file.

- hide_resolved:

  If `TRUE`, hide sheets with no data.

- include_stops:

  If `TRUE`, also include STOP-type warnings.

- path:

  deprecated

## Value

a logical(1), whether the file could be written, invisibly
