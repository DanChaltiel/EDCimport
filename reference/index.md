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

- [`load_database()`](https://danchaltiel.github.io/EDCimport/reference/load_database.md)
  : Load a list in an environment

- [`edc_database`](https://danchaltiel.github.io/EDCimport/reference/edc_database.md)
  : EDCimport Database

- [`edc_lookup()`](https://danchaltiel.github.io/EDCimport/reference/edc_lookup.md)
  : Retrieve the lookup table from options

## Modifying databases

- [`edc_unify_subjid()`](https://danchaltiel.github.io/EDCimport/reference/edc_unify_subjid.md)
  : Harmonize the subject ID of the database
- [`edc_split_mixed()`](https://danchaltiel.github.io/EDCimport/reference/edc_split_mixed.md)
  : Split mixed datasets
- [`edc_clean_names()`](https://danchaltiel.github.io/EDCimport/reference/edc_clean_names.md)
  : Clean up the names of all datasets
- [`set_project_name()`](https://danchaltiel.github.io/EDCimport/reference/set_project_name.md)
  [`get_project_name()`](https://danchaltiel.github.io/EDCimport/reference/set_project_name.md)
  : Set the project name

## Checking databases

- [`edc_data_warn()`](https://danchaltiel.github.io/EDCimport/reference/edc_data_warn.md)
  [`edc_data_stop()`](https://danchaltiel.github.io/EDCimport/reference/edc_data_warn.md)
  [`edc_data_warnings()`](https://danchaltiel.github.io/EDCimport/reference/edc_data_warn.md)
  : Standardized warning system
- [`save_edc_data_warnings()`](https://danchaltiel.github.io/EDCimport/reference/save_edc_data_warnings.md)
  : Save EDCimport warning to Excel
- [`assert_no_duplicate()`](https://danchaltiel.github.io/EDCimport/reference/assert_no_duplicate.md)
  : Assert that a dataframe has one row per patient
- [`edc_warn_patient_diffs()`](https://danchaltiel.github.io/EDCimport/reference/edc_warn_patient_diffs.md)
  : Check the validity of the subject ID column
- [`edc_warn_extraction_date()`](https://danchaltiel.github.io/EDCimport/reference/edc_warn_extraction_date.md)
  : Warn if extraction is too old
- [`compare_databases()`](https://danchaltiel.github.io/EDCimport/reference/compare_databases.md)
  : Compare multiple EDC database extractions

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

- [`edc_find_value()`](https://danchaltiel.github.io/EDCimport/reference/edc_find_value.md)
  [`edc_find_column()`](https://danchaltiel.github.io/EDCimport/reference/edc_find_value.md)
  : Search the whole database
- [`lastnews_table()`](https://danchaltiel.github.io/EDCimport/reference/lastnews_table.md)
  : Get a table with the latest date for each patient
- [`edc_db_to_excel()`](https://danchaltiel.github.io/EDCimport/reference/edc_db_to_excel.md)
  [`edc_browse_excel()`](https://danchaltiel.github.io/EDCimport/reference/edc_db_to_excel.md)
  : Save the database as an Excel file
- [`get_common_cols()`](https://danchaltiel.github.io/EDCimport/reference/get_common_cols.md)
  [`summary(`*`<common_cols>`*`)`](https://danchaltiel.github.io/EDCimport/reference/get_common_cols.md)
  **\[experimental\]** : Get columns that are common to multiple
  datasets
- [`edc_viewer()`](https://danchaltiel.github.io/EDCimport/reference/edc_viewer.md)
  : Shiny data explorer

## Options

- [`edc_options()`](https://danchaltiel.github.io/EDCimport/reference/edc_options.md)
  :

  Set global options for `EDCimport`

- [`edc_peek_options()`](https://danchaltiel.github.io/EDCimport/reference/edc_peek_options.md)
  :

  See which `EDCimport` option is currently set

- [`edc_reset_options()`](https://danchaltiel.github.io/EDCimport/reference/edc_reset_options.md)
  :

  Reset all `EDCimport` options

## Graphics

- [`edc_swimmerplot()`](https://danchaltiel.github.io/EDCimport/reference/edc_swimmerplot.md)
  : Swimmer plot of all dates columns
- [`edc_crf_plot()`](https://danchaltiel.github.io/EDCimport/reference/edc_crf_plot.md)
  [`edc_pal_crf()`](https://danchaltiel.github.io/EDCimport/reference/edc_crf_plot.md)
  : Show the current CRF status distribution
- [`edc_patient_gridplot()`](https://danchaltiel.github.io/EDCimport/reference/edc_patient_gridplot.md)
  : Patient gridplot
- [`edc_population_plot()`](https://danchaltiel.github.io/EDCimport/reference/edc_population_plot.md)
  : Plot the populations

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
- [`get_datasets()`](https://danchaltiel.github.io/EDCimport/reference/get_datasets.md)
  : Retrieve the datasets as a list of data.frames
- [`edc_left_join()`](https://danchaltiel.github.io/EDCimport/reference/edc_left_join.md)
  : Join within the EDCimport framework

## Examples

- [`edc_example()`](https://danchaltiel.github.io/EDCimport/reference/edc_example.md)
  : Example database

## Manual correction

- [`manual_correction()`](https://danchaltiel.github.io/EDCimport/reference/manual_correction.md)
  [`reset_manual_correction()`](https://danchaltiel.github.io/EDCimport/reference/manual_correction.md)
  **\[experimental\]** : Manual correction
