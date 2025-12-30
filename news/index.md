# Changelog

## EDCimport 0.6.1 (dev)

#### New features

- New function
  [`compare_databases()`](https://danchaltiel.github.io/EDCimport/reference/compare_databases.md),
  which compares the structure of several extractions of a database:
  added/removed columns, number of patients, etc
  ([\#26](https://github.com/DanChaltiel/EDCimport/issues/26)). See the
  examples for a demo.
- Cache system is supported in all reading functions
  [`read_all_csv()`](https://danchaltiel.github.io/EDCimport/reference/read_all_csv.md),
  [`read_all_sas()`](https://danchaltiel.github.io/EDCimport/reference/read_all_sas.md),
  and
  [`read_all_xpt()`](https://danchaltiel.github.io/EDCimport/reference/read_all_xpt.md)
  ([\#75](https://github.com/DanChaltiel/EDCimport/issues/75)).
- New features in
  [`edc_viewer()`](https://danchaltiel.github.io/EDCimport/reference/edc_viewer.md):
  - Support for multiple instances on different ports with custom
    datasets
    ([\#100](https://github.com/DanChaltiel/EDCimport/issues/100),
    [\#114](https://github.com/DanChaltiel/EDCimport/issues/114))  
    For instance, you can now run
    `edc_viewer(data=lst(iris, mtcars), port=1212)`
  - New button to browse all the column labels
    ([\#113](https://github.com/DanChaltiel/EDCimport/issues/113)).

#### Bug fixes & Improvements

- Fixed modifiers
  [`edc_clean_names()`](https://danchaltiel.github.io/EDCimport/reference/edc_clean_names.md),
  [`edc_unify_subjid()`](https://danchaltiel.github.io/EDCimport/reference/edc_unify_subjid.md),
  and
  [`edc_split_mixed()`](https://danchaltiel.github.io/EDCimport/reference/edc_split_mixed.md)
  so they don’t strip database attributes (like project name)
  ([\#111](https://github.com/DanChaltiel/EDCimport/issues/111)).
- Fixed
  [`edc_data_stop()`](https://danchaltiel.github.io/EDCimport/reference/edc_data_warn.md)
  so it works without a SUBJID and defaults to no issue number
  ([\#109](https://github.com/DanChaltiel/EDCimport/issues/109)).
- Fixed
  [`assert_no_duplicate()`](https://danchaltiel.github.io/EDCimport/reference/assert_no_duplicate.md)
  so it works in table with both columns `SUBJID` and `subjid`
  ([\#105](https://github.com/DanChaltiel/EDCimport/issues/105)).
- Fixed bugs in
  [`edc_left_join()`](https://danchaltiel.github.io/EDCimport/reference/edc_left_join.md)
  with case-sensitivity on SUBJID
  ([\#108](https://github.com/DanChaltiel/EDCimport/issues/108),
  [\#117](https://github.com/DanChaltiel/EDCimport/issues/117)).
- Improved
  [`save_edc_data_warnings()`](https://danchaltiel.github.io/EDCimport/reference/save_edc_data_warnings.md)
  with options to hide the resolved issues and to not include stops, and
  better default path
  ([\#107](https://github.com/DanChaltiel/EDCimport/issues/107),
  [\#110](https://github.com/DanChaltiel/EDCimport/issues/110),
  [\#112](https://github.com/DanChaltiel/EDCimport/issues/112))
- Improved reading functions so that all tables are sorted by SUBJID
  ([\#115](https://github.com/DanChaltiel/EDCimport/issues/115)).
- Improved reading functions so that each dataset has a `label`
  attribute, taken from `FORMDESC` or `CRFNAME`
  ([\#118](https://github.com/DanChaltiel/EDCimport/issues/118)).
- Improved
  [`edc_swimmerplot()`](https://danchaltiel.github.io/EDCimport/reference/edc_swimmerplot.md)
  by removing `origin` by default
  ([\#106](https://github.com/DanChaltiel/EDCimport/issues/106)).
- Improved
  [`edc_swimmerplot()`](https://danchaltiel.github.io/EDCimport/reference/edc_swimmerplot.md)
  by adding arguments `origin_fun` to summarise `origin` at patient
  level using, and `data_list` to control the datasets.
- Improved
  [`edc_warn_extraction_date()`](https://danchaltiel.github.io/EDCimport/reference/edc_warn_extraction_date.md)
  with a strict unit “days”.
- Improved
  [`save_plotly()`](https://danchaltiel.github.io/EDCimport/reference/save_plotly.md)
  with a glue syntax for param `file`.

## EDCimport 0.6.0

CRAN release: 2025-06-24

#### Documentation

- New vignettes:
  [`vignette("reading")`](https://danchaltiel.github.io/EDCimport/articles/reading.md),
  [`vignette("postprocessing")`](https://danchaltiel.github.io/EDCimport/articles/postprocessing.md),
  [`vignette("checking")`](https://danchaltiel.github.io/EDCimport/articles/checking.md),
  [`vignette("visualizing")`](https://danchaltiel.github.io/EDCimport/articles/visualizing.md),
  and
  [`vignette("utils")`](https://danchaltiel.github.io/EDCimport/articles/utils.md)

#### New features

- New function
  [`edc_patient_gridplot()`](https://danchaltiel.github.io/EDCimport/reference/edc_patient_gridplot.md),
  which creates a ggplot matrix giving the presence of all patients in
  all datasets
  ([\#77](https://github.com/DanChaltiel/EDCimport/issues/77))
- New functions
  [`edc_left_join()`](https://danchaltiel.github.io/EDCimport/reference/edc_left_join.md),
  [`edc_right_join()`](https://danchaltiel.github.io/EDCimport/reference/edc_left_join.md),
  and
  [`edc_full_join()`](https://danchaltiel.github.io/EDCimport/reference/edc_left_join.md),
  which perform joins with defaults to subject ID as primary key
  ([\#82](https://github.com/DanChaltiel/EDCimport/issues/82))
- New function
  [`edc_viewer()`](https://danchaltiel.github.io/EDCimport/reference/edc_viewer.md),
  which runs a shiny application for easily browsing your database
  ([\#83](https://github.com/DanChaltiel/EDCimport/issues/83))
- New function
  [`set_project_name()`](https://danchaltiel.github.io/EDCimport/reference/set_project_name.md),
  to set the project name when reading from a directory
  ([\#96](https://github.com/DanChaltiel/EDCimport/issues/96))
- New function
  [`edc_find_value()`](https://danchaltiel.github.io/EDCimport/reference/edc_find_value.md),
  which searches the whole database for a value, as
  [`edc_find_column()`](https://danchaltiel.github.io/EDCimport/reference/edc_find_value.md)
  searches for column names or labels.
- New function
  [`save_edc_data_warnings()`](https://danchaltiel.github.io/EDCimport/reference/save_edc_data_warnings.md),
  to save all the warnings triggered by
  [`edc_data_warn()`](https://danchaltiel.github.io/EDCimport/reference/edc_data_warn.md)
  into a `.xlsx` file for sharing.

#### Bug fixes & Improvements

- New argument `unify(collapse_chr=TRUE)`, to collapse non-unique
  character values
  ([\#99](https://github.com/DanChaltiel/EDCimport/issues/99))
- New argument `lastnews_table(show_delta=TRUE)`, which computes the
  difference between the last `prefer` date and the actual last date
  ([\#81](https://github.com/DanChaltiel/EDCimport/issues/81))
  - Other improvements: allow regex in `except` & `prefer` (with
    `regex=TRUE`), improved warning message, and allow saving warnings
    in a csv file
    ([\#78](https://github.com/DanChaltiel/EDCimport/issues/78))
- New argument `edc_data_warn(envir)`, the environment to evaluate
  `message` in.
- New argument `edc_swimmerplot(include)`, to subset the swimmer plot on
  significant variables only.
- New argument `subdirectories` to all reading functions
  ([`read_trialmaster()`](https://danchaltiel.github.io/EDCimport/reference/read_trialmaster.md),
  [`read_all_xpt()`](https://danchaltiel.github.io/EDCimport/reference/read_all_xpt.md),
  [`read_all_sas()`](https://danchaltiel.github.io/EDCimport/reference/read_all_sas.md),
  and
  [`read_all_csv()`](https://danchaltiel.github.io/EDCimport/reference/read_all_csv.md)),
  to control whether to read sub-directories. Note that until now, those
  subdirectories were read and could overwrite root files.
- Fixed labels being sometimes duplicated.

#### Internal improvements

- [`read_trialmaster()`](https://danchaltiel.github.io/EDCimport/reference/read_trialmaster.md)
  won’t read from cache if installed EDCimport version is different from
  cache’s

#### Deprecations

- [`load_list()`](https://danchaltiel.github.io/EDCimport/reference/load_database.md),
  renamed to
  [`load_database()`](https://danchaltiel.github.io/EDCimport/reference/load_database.md)
- [`find_keyword()`](https://danchaltiel.github.io/EDCimport/reference/edc_find_value.md),
  renamed to
  [`edc_find_column()`](https://danchaltiel.github.io/EDCimport/reference/edc_find_value.md)

#### Breaking changes

I don’t think enough people are using this so that it is necessary to go
through the deprecation process.

- `split_mixed_datasets` becomes
  [`edc_split_mixed()`](https://danchaltiel.github.io/EDCimport/reference/edc_split_mixed.md)
- Removed export of internal functions: `build_lookup()`,
  `extend_lookup()`, `get_key_cols()`,
  [`get_subjid_cols()`](https://danchaltiel.github.io/EDCimport/reference/get_subjid_cols.md),
  `get_crfname_cols()`, `get_meta_cols()`, `load_as_list()`,
  `save_list()`

## EDCimport 0.5.2

CRAN release: 2024-11-14

- Fixed a bug in
  [`lastnews_table()`](https://danchaltiel.github.io/EDCimport/reference/lastnews_table.md)
  when SUBJID is not numeric
- Fixed a bug in
  [`read_all_sas()`](https://danchaltiel.github.io/EDCimport/reference/read_all_sas.md)
  causing metadata (e.g. `date_extraction`) being converted to
  dataframes

## EDCimport 0.5.1

CRAN release: 2024-10-31

- Internal fix for CRAN check

## EDCimport 0.5.0

CRAN release: 2024-10-24

#### New features

##### Read functions

- New function
  [`read_all_sas()`](https://danchaltiel.github.io/EDCimport/reference/read_all_sas.md)
  to read a database of `.sas7bdat` files.
- New function
  [`read_all_csv()`](https://danchaltiel.github.io/EDCimport/reference/read_all_csv.md)
  to read a database of `.csv` files.

##### Sanity checks alerts

- New functions
  [`edc_data_warn()`](https://danchaltiel.github.io/EDCimport/reference/edc_data_warn.md)
  and
  [`edc_data_stop()`](https://danchaltiel.github.io/EDCimport/reference/edc_data_warn.md),
  to alert if data has inconsistencies
  ([\#29](https://github.com/DanChaltiel/EDCimport/issues/29),
  [\#39](https://github.com/DanChaltiel/EDCimport/issues/39),
  [\#43](https://github.com/DanChaltiel/EDCimport/issues/43)).
  `r ae %>% filter(grade<1 | grade>5) %>% edc_data_stop("AE of invalid grade") ae %>% filter(is.na(grade)) %>% edc_data_warn("Grade is missing", issue_n=13) #> Warning: Issue `[`#13`](https://github.com/DanChaltiel/EDCimport/issues/13)`: Grade is missing (8 patients: `[`#21`](https://github.com/DanChaltiel/EDCimport/issues/21)`, `[`#28`](https://github.com/DanChaltiel/EDCimport/issues/28)`, `[`#39`](https://github.com/DanChaltiel/EDCimport/issues/39)`, `[`#95`](https://github.com/DanChaltiel/EDCimport/issues/95)`, `[`#97`](https://github.com/DanChaltiel/EDCimport/issues/97)`, ...)`

- New function
  [`edc_data_warnings()`](https://danchaltiel.github.io/EDCimport/reference/edc_data_warn.md),
  to get a dataframe of all warnings thrown by
  [`edc_data_warn()`](https://danchaltiel.github.io/EDCimport/reference/edc_data_warn.md).

- New function
  [`edc_warn_extraction_date()`](https://danchaltiel.github.io/EDCimport/reference/edc_warn_extraction_date.md),
  to alert if data is too old.

##### Miscellaneous utils

- New function
  [`select_distinct()`](https://danchaltiel.github.io/EDCimport/reference/select_distinct.md)
  to select all columns that has only one level for a given grouping
  scope ([\#57](https://github.com/DanChaltiel/EDCimport/issues/57)).
- New function
  [`edc_population_plot()`](https://danchaltiel.github.io/EDCimport/reference/edc_population_plot.md)
  to visualize which patient is in which analysis population
  ([\#56](https://github.com/DanChaltiel/EDCimport/issues/56)).
- New function
  [`edc_db_to_excel()`](https://danchaltiel.github.io/EDCimport/reference/edc_db_to_excel.md)
  to export the whole database to an Excel file, easier to browse than
  RStudio’s table viewer
  ([\#55](https://github.com/DanChaltiel/EDCimport/issues/55)). Use
  [`edc_browse_excel()`](https://danchaltiel.github.io/EDCimport/reference/edc_db_to_excel.md)
  to browse the file without knowing its name.
- New function
  [`edc_inform_code()`](https://danchaltiel.github.io/EDCimport/reference/edc_inform_code.md)
  to show how much code your project contains
  ([\#49](https://github.com/DanChaltiel/EDCimport/issues/49)).
- New function
  [`search_for_newer_data()`](https://danchaltiel.github.io/EDCimport/reference/search_for_newer_data.md)
  to search a path (e.g. Downloads) for a newer data archive
  ([\#46](https://github.com/DanChaltiel/EDCimport/issues/46)).
- New function
  [`edc_crf_plot()`](https://danchaltiel.github.io/EDCimport/reference/edc_crf_plot.md)
  to show the current database completion status
  ([\#48](https://github.com/DanChaltiel/EDCimport/issues/48)).
- New function
  [`save_sessioninfo()`](https://danchaltiel.github.io/EDCimport/reference/save_sessioninfo.md),
  to save [`sessionInfo()`](https://rdrr.io/r/utils/sessionInfo.html)
  into a text file
  ([\#42](https://github.com/DanChaltiel/EDCimport/issues/42)).
- New function
  [`fct_yesno()`](https://danchaltiel.github.io/EDCimport/reference/fct_yesno.md),
  to easily format Yes/No columns
  ([\#19](https://github.com/DanChaltiel/EDCimport/issues/19),
  [\#23](https://github.com/DanChaltiel/EDCimport/issues/23),
  [\#40](https://github.com/DanChaltiel/EDCimport/issues/40)).
- New function
  [`lastnews_table()`](https://danchaltiel.github.io/EDCimport/reference/lastnews_table.md)
  to find the last date an information has been entered for each patient
  ([\#37](https://github.com/DanChaltiel/EDCimport/issues/37)). Useful
  for survival analyses.
- New function
  [`edc_unify_subjid()`](https://danchaltiel.github.io/EDCimport/reference/edc_unify_subjid.md),
  to have the same structure for subject IDs in all the datasets of the
  database ([\#30](https://github.com/DanChaltiel/EDCimport/issues/30)).
- New function
  [`save_plotly()`](https://danchaltiel.github.io/EDCimport/reference/save_plotly.md),
  to save a `plotly` to an HTML file
  ([\#15](https://github.com/DanChaltiel/EDCimport/issues/15)).
- New experimental functions
  [`table_format()`](https://danchaltiel.github.io/EDCimport/reference/table_format.md),
  [`get_common_cols()`](https://danchaltiel.github.io/EDCimport/reference/get_common_cols.md)
  and `get_meta_cols()` that might become useful to find keys to pivot
  or summarise data.

#### Bug fixes & Improvements

- [`get_datasets()`](https://danchaltiel.github.io/EDCimport/reference/get_datasets.md)
  will now work even if a dataset is named after a base function
  ([\#67](https://github.com/DanChaltiel/EDCimport/issues/67)).
- [`read_trialmaster()`](https://danchaltiel.github.io/EDCimport/reference/read_trialmaster.md)
  will output a readable error when no password is entered although one
  is needed.
- `read_trialmaster(split_mixed="TRUE")` will work as intended.
- [`assert_no_duplicate()`](https://danchaltiel.github.io/EDCimport/reference/assert_no_duplicate.md)
  has now a `by` argument to check for duplicate in groups, for example
  by visit ([\#17](https://github.com/DanChaltiel/EDCimport/issues/17)).
- [`find_keyword()`](https://danchaltiel.github.io/EDCimport/reference/edc_find_value.md)
  is more robust and inform on the proportion of missing if possible.
- [`edc_lookup()`](https://danchaltiel.github.io/EDCimport/reference/edc_lookup.md)
  will now retrieve the lookup table. Use `build_lookup()` to build one
  from a table list.
- `extend_lookup()` will not fail anymore when the database has a faulty
  table.

#### Deprecations

- `get_key_cols()` is replaced by
  [`get_subjid_cols()`](https://danchaltiel.github.io/EDCimport/reference/get_subjid_cols.md)
  and `get_crfname_cols()`.
- [`check_subjid()`](https://danchaltiel.github.io/EDCimport/reference/edc_warn_patient_diffs.md)
  is replaced by
  [`edc_warn_patient_diffs()`](https://danchaltiel.github.io/EDCimport/reference/edc_warn_patient_diffs.md).
  It can either take a vector or a dataframe as input, and the message
  is more informative.

## EDCimport 0.4.1

CRAN release: 2023-12-19

#### Bug fixes & Improvements

- Changes in testing environment so that the package can be installed
  from CRAN despite firewall policies forbidding password-protected
  archive downloading.

- Fixed a bug where a corrupted XPT file can prevent the whole import to
  fail.

## EDCimport 0.4.0

CRAN release: 2023-12-11

#### New features

- New function
  [`check_subjid()`](https://danchaltiel.github.io/EDCimport/reference/edc_warn_patient_diffs.md)
  to check if a vector is not missing some patients
  ([\#8](https://github.com/DanChaltiel/EDCimport/issues/8)).

``` r
options(edc_subjid_ref=enrolres$subjid)
check_subjid(treatment$subjid)
check_subjid(ae$subjid)
```

- New function
  [`assert_no_duplicate()`](https://danchaltiel.github.io/EDCimport/reference/assert_no_duplicate.md)
  to abort if a table has duplicates in a subject ID
  column([\#9](https://github.com/DanChaltiel/EDCimport/issues/9)).

``` r
tibble(subjid=c(1:10, 1)) %>% assert_no_duplicate() %>% nrow()
#Error in `assert_no_duplicate()`:
#! Duplicate on column "subjid" for value 1.
```

- New function
  [`manual_correction()`](https://danchaltiel.github.io/EDCimport/reference/manual_correction.md)
  to safely hard-code a correction while waiting for the TrialMaster
  database to be updated.
- New function
  [`edc_options()`](https://danchaltiel.github.io/EDCimport/reference/edc_options.md)
  to manage `EDCimport` global parameterization.
- New argument `edc_swimmerplot(id_lim)` to subset the swimmer plot to
  some patients only.
- New option `read_trialmaster(use_cache="write")` to read from the zip
  again but still update the cache.
- You can now use the syntax
  `read_trialmaster(split_mixed=c("col1", "col2"))` to split only the
  datasets you need to
  ([\#10](https://github.com/DanChaltiel/EDCimport/issues/10)).

#### Bug fixes & Improvements

- Reading with
  [`read_trialmaster()`](https://danchaltiel.github.io/EDCimport/reference/read_trialmaster.md)
  from cache will output an error if parameters (`split_mixed`,
  `clean_names_fun`) are different
  ([\#4](https://github.com/DanChaltiel/EDCimport/issues/4)).
- `split_mixed_datasets()` is now fully case-insensitive.
- Non-UTF8 characters in labels are now identified and corrected during
  reading ([\#5](https://github.com/DanChaltiel/EDCimport/issues/5)).

#### Minor breaking changes

- `read_trialmaster(use_cache="write")` is now the default. Reading from
  cache is not stable yet, so you should opt-in rather than opt-out.
- `read_trialmaster(extend_lookup=TRUE)` is now the default.
- Options `edc_id`, `edc_crfname`, and `edc_verbose` have been
  respectively renamed `edc_cols_id`, `edc_cols_crfname`, and
  `edc_read_verbose` for more clarity.

## EDCimport 0.3.0

CRAN release: 2023-05-19

#### New features

- New function
  [`edc_swimmerplot()`](https://danchaltiel.github.io/EDCimport/reference/edc_swimmerplot.md)
  to show a swimmer plot of all dates in the database and easily find
  outliers.
- New features in
  [`read_trialmaster()`](https://danchaltiel.github.io/EDCimport/reference/read_trialmaster.md):
  - `clean_names_fun=some_fun` will clean all names of all tables. For
    instance, `clean_names_fun=janitor::clean_names()` will turn default
    SAS uppercase column names into valid R snake-case column names.
  - `split_mixed=TRUE` will split tables that contain both long and
    short data regarding patient ID into one long table and one short
    table. See `?split_mixed_datasets()` for details.
  - `extend_lookup=TRUE` will improve the lookup table with additional
    information. See `?extend_lookup()` for details.
  - `key_columns=get_key_cols()` is where you can change the default
    column names for patient ID and CRF name (used in other new
    features).
- Standalone functions `extend_lookup()` and `split_mixed_datasets()`.
- New helper
  [`unify()`](https://danchaltiel.github.io/EDCimport/reference/unify.md),
  which turns a vector of duplicate values into a vector of length 1.

#### Bug fixes

- Reading errors are now handled by
  [`read_trialmaster()`](https://danchaltiel.github.io/EDCimport/reference/read_trialmaster.md)
  instead of failing. If one XPT file is corrupted, the resulting object
  will contain the error message instead of the dataset.
- [`find_keyword()`](https://danchaltiel.github.io/EDCimport/reference/edc_find_value.md)
  is now robust to non-UTF8 characters in labels.
- Option `edc_lookup` is now set even when reading from cache.
- SAS formats containing a `=` now work as intended.

## EDCimport 0.2.1

CRAN release: 2022-12-02

- Import your data from TrialMaster using
  `tm = read_trialmaster("path/to/archive.zip")`.
- Search for a keyword in any column name or label using
  `find_keyword("date", data=tm$.lookup)`. You can also generate a
  lookup table for an arbitrary list of dataframe using
  `build_lookup(my_data)`.
- Load the datasets to the global environment using `load_list(tm)` to
  avoid typing `tm$` everywhere.
- Browse available global options using `?EDCimport_options`.

## EDCimport 0.1.0

- Draft version
