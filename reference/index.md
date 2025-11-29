# Package index

## Reading databases

- [`read_trialmaster()`](https://danchaltiel.github.io/EDCimport/reference/read_trialmaster.md)
  :

  Read the `.zip` archive of a TrialMaster export

- [`read_all_xpt()`](https://danchaltiel.github.io/EDCimport/reference/read_all_xpt.md)
  :

  Read all `.xpt` files in a directory

- [`read_all_sas()`](https://danchaltiel.github.io/EDCimport/reference/read_all_sas.md)
  :

  Read all `.sas7bdat` files in a directory

- [`read_all_csv()`](https://danchaltiel.github.io/EDCimport/reference/read_all_csv.md)
  :

  Read all `.csv` files in a directory

## Checking databases

- [`edc_data_warn()`](https://danchaltiel.github.io/EDCimport/reference/edc_data_warn.md)
  [`edc_data_stop()`](https://danchaltiel.github.io/EDCimport/reference/edc_data_warn.md)
  [`edc_data_warnings()`](https://danchaltiel.github.io/EDCimport/reference/edc_data_warn.md)
  : Standardized warning system
- [`assert_no_duplicate()`](https://danchaltiel.github.io/EDCimport/reference/assert_no_duplicate.md)
  : Assert that a dataframe has one row per patient
- [`edc_warn_patient_diffs()`](https://danchaltiel.github.io/EDCimport/reference/edc_warn_patient_diffs.md)
  : Check the validity of the subject ID column
- [`edc_warn_extraction_date()`](https://danchaltiel.github.io/EDCimport/reference/edc_warn_extraction_date.md)
  : Warn if extraction is too old
- [`crf_status_plot()`](https://danchaltiel.github.io/EDCimport/reference/crf_status_plot.md)
  [`edc_pal_crf()`](https://danchaltiel.github.io/EDCimport/reference/crf_status_plot.md)
  : Show the current CRF status distribution
- [`edc_population_plot()`](https://danchaltiel.github.io/EDCimport/reference/edc_population_plot.md)
  : Plot the populations

## Project management

- [`edc_inform_code()`](https://danchaltiel.github.io/EDCimport/reference/edc_inform_code.md)
  : Shows how many code you wrote

- [`search_for_newer_data()`](https://danchaltiel.github.io/EDCimport/reference/search_for_newer_data.md)
  : Search for newer data

- [`save_sessioninfo()`](https://danchaltiel.github.io/EDCimport/reference/save_sessioninfo.md)
  :

  Save [`sessionInfo()`](https://rdrr.io/r/utils/sessionInfo.html)
  output

## Searching databases

- [`find_keyword()`](https://danchaltiel.github.io/EDCimport/reference/find_keyword.md)
  : Find a keyword in the whole database
- [`lastnews_table()`](https://danchaltiel.github.io/EDCimport/reference/lastnews_table.md)
  : Get a table with the latest date for each patient
- [`edc_db_to_excel()`](https://danchaltiel.github.io/EDCimport/reference/edc_db_to_excel.md)
  [`edc_browse_excel()`](https://danchaltiel.github.io/EDCimport/reference/edc_db_to_excel.md)
  : Save the database as an Excel file
- [`get_common_cols()`](https://danchaltiel.github.io/EDCimport/reference/get_common_cols.md)
  [`summary(`*`<common_cols>`*`)`](https://danchaltiel.github.io/EDCimport/reference/get_common_cols.md)
  **\[experimental\]** : Get columns that are common to multiple
  datasets

## Options

- [`edc_options()`](https://danchaltiel.github.io/EDCimport/reference/edc_options.md)
  :

  Set global options for `EDCimport`

- [`edc_peek_options()`](https://danchaltiel.github.io/EDCimport/reference/edc_peek_options.md)
  :

  See which `EDCimport` option is currently set.

- [`edc_reset_options()`](https://danchaltiel.github.io/EDCimport/reference/edc_reset_options.md)
  :

  Reset all `EDCimport` options.

## Graphics

- [`edc_swimmerplot()`](https://danchaltiel.github.io/EDCimport/reference/edc_swimmerplot.md)
  : Swimmer plot of all dates columns

## Helpers

- [`unify()`](https://danchaltiel.github.io/EDCimport/reference/unify.md)
  : Unify a vector
- [`fct_yesno()`](https://danchaltiel.github.io/EDCimport/reference/fct_yesno.md)
  : Format factor levels as Yes/No
- [`table_format()`](https://danchaltiel.github.io/EDCimport/reference/table_format.md)
  : Identify if a dataframe has a long or a wide format
- [`save_plotly()`](https://danchaltiel.github.io/EDCimport/reference/save_plotly.md)
  : Save a plotly to an HTML file
- [`select_distinct()`](https://danchaltiel.github.io/EDCimport/reference/select_distinct.md)
  : Select only distinct columns
- [`harmonize_subjid()`](https://danchaltiel.github.io/EDCimport/reference/harmonize_subjid.md)
  : Harmonize the subject ID of the database
- [`get_datasets()`](https://danchaltiel.github.io/EDCimport/reference/get_datasets.md)
  : Retrieve the datasets as a list of data.frames
- [`get_meta_cols()`](https://danchaltiel.github.io/EDCimport/reference/get_meta_cols.md)
  : Get columns shared by most datasets
- [`get_subjid_cols()`](https://danchaltiel.github.io/EDCimport/reference/get_subjid_cols.md)
  [`get_crfname_cols()`](https://danchaltiel.github.io/EDCimport/reference/get_subjid_cols.md)
  : Get key column names
- [`get_key_cols()`](https://danchaltiel.github.io/EDCimport/reference/get_key_cols.md)
  : Important column names
- [`split_mixed_datasets()`](https://danchaltiel.github.io/EDCimport/reference/split_mixed_datasets.md)
  : Split mixed datasets

## Lookup Utils

- [`edc_lookup()`](https://danchaltiel.github.io/EDCimport/reference/edc_lookup.md)
  : Retrieve the lookup table from options
- [`extend_lookup()`](https://danchaltiel.github.io/EDCimport/reference/extend_lookup.md)
  : Extend the lookup table
- [`build_lookup()`](https://danchaltiel.github.io/EDCimport/reference/build_lookup.md)
  : Generate a lookup table

## List Utils

- [`load_list()`](https://danchaltiel.github.io/EDCimport/reference/load_list.md)
  : Load a list in an environment

- [`load_as_list()`](https://danchaltiel.github.io/EDCimport/reference/load_as_list.md)
  :

  Load a `.RData` file as a list

- [`save_list()`](https://danchaltiel.github.io/EDCimport/reference/save_list.md)
  :

  Save a list as `.RData` file

## Examples

- [`edc_example_mixed()`](https://danchaltiel.github.io/EDCimport/reference/data_example.md)
  [`edc_example()`](https://danchaltiel.github.io/EDCimport/reference/data_example.md)
  [`edc_example_plot()`](https://danchaltiel.github.io/EDCimport/reference/data_example.md)
  [`edc_example_ae()`](https://danchaltiel.github.io/EDCimport/reference/data_example.md)
  : Example databases

## Manual correction

- [`manual_correction()`](https://danchaltiel.github.io/EDCimport/reference/manual_correction.md)
  [`reset_manual_correction()`](https://danchaltiel.github.io/EDCimport/reference/manual_correction.md)
  : Manual correction
