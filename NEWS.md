# EDCimport

EDCimport is a package designed to easily import data from EDC software TrialMaster. Browse code at <https://github.com/DanChaltiel/EDCimport> and read the doc at <https://danchaltiel.github.io/EDCimport/>.

# EDCimport 0.5.2

-   Fixed a bug in `lastnews_table()` when subjid is not numeric
-   Fixed a bug in `read_all_sas()` causing metadata (e.g. `date_extraction`) being converted to dataframes

# EDCimport 0.5.1

-   Internal fix for CRAN check

# EDCimport 0.5.0

### New features

#### Read functions

-   New function `read_all_sas()` to read a database of `.sas7bdat` files.

-   New function `read_all_csv()` to read a database of `.csv` files.

#### Sanity checks alerts

-   New functions `edc_data_warn()` and `edc_data_stop()`, to alert if data has inconsistencies (#29, #39, #43).

    ``` r
    ae %>% filter(grade<1 | grade>5) %>% edc_data_stop("AE of invalid grade")
    ae %>% filter(is.na(grade)) %>% edc_data_warn("Grade is missing", issue_n=13)
    #> Warning: Issue #13: Grade is missing (8 patients: #21, #28, #39, #95, #97, ...)
    ```

-   New function `edc_data_warnings()`, to get a dataframe of all warnings thrown by `edc_data_warn()`.

-   New function `edc_warn_extraction_date()`, to alert if data is too old.

#### Miscellaneous utils

-   New function `select_distinct()` to select all columns that has only one level for a given grouping scope (#57).

-   New function `edc_population_plot()` to visualize which patient is in which analysis population (#56).

-   New function `edc_db_to_excel()` to export the whole database to an Excel file, easier to browse than RStudio's table viewer (#55). Use `edc_browse_excel()` to browse the file without knowing its name.

-   New function `edc_inform_code()` to show how much code your project contains (#49).

-   New function `search_for_newer_data()` to search a path (e.g. Downloads) for a newer data archive (#46).

-   New function `crf_status_plot()` to show the current database completion status (#48).

-   New function `save_sessioninfo()`, to save `sessionInfo()` into a text file (#42).

-   New function `fct_yesno()`, to easily format Yes/No columns (#19, #23, #40).

-   New function `lastnews_table()` to find the last date an information has been entered for each patient (#37). Useful for survival analyses.

-   New function `harmonize_subjid()`, to have the same structure for subject IDs in all the datasets of the database (#30).

-   New function `save_plotly()`, to save a `plotly` to an HTML file (#15).

-   New experimental functions `table_format()`, `get_common_cols()` and `get_meta_cols()` that might become useful to find keys to pivot or summarise data.

### Bug fixes & Improvements

-   `get_datasets()` will now work even if a dataset is named after a base function (#67).
-   `read_trialmaster()` will output a readable error when no password is entered although one is needed.
-   `read_trialmaster(split_mixed="TRUE")` will work as intended.
-   `assert_no_duplicate()` has now a `by` argument to check for duplicate in groups, for example by visit (#17).
-   `find_keyword()` is more robust and inform on the proportion of missing if possible.
-   `edc_lookup()` will now retrieve the lookup table. Use `build_lookup()` to build one from a table list.
-   `extend_lookup()` will not fail anymore when the database has a faulty table.

### Deprecations

-   `get_key_cols()` is replaced by `get_subjid_cols()` and `get_crfname_cols()`.
-   `check_subjid()` is replaced by `edc_warn_patient_diffs()`. It can either take a vector or a dataframe as input, and the message is more informative.



# EDCimport 0.4.1

### Bug fixes & Improvements

-   Changes in testing environment so that the package can be installed from CRAN despite firewall policies forbidding password-protected archive downloading.

-   Fixed a bug where a corrupted XPT file can prevent the whole import to fail.


# EDCimport 0.4.0

### New features

-   New function `check_subjid()` to check if a vector is not missing some patients (#8).

``` r
options(edc_subjid_ref=enrolres$subjid)
check_subjid(treatment$subjid)
check_subjid(ae$subjid)
```

-   New function `assert_no_duplicate()` to abort if a table has duplicates in a subject ID column(#9).

``` r
tibble(subjid=c(1:10, 1)) %>% assert_no_duplicate() %>% nrow()
#Error in `assert_no_duplicate()`:
#! Duplicate on column "subjid" for value 1.
```

-   New function `manual_correction()` to safely hard-code a correction while waiting for the TrialMaster database to be updated.
-   New function `edc_options()` to manage `EDCimport` global parameterization.
-   New argument `edc_swimmerplot(id_lim)` to subset the swimmer plot to some patients only.
-   New option `read_trialmaster(use_cache="write")` to read from the zip again but still update the cache.
-   You can now use the syntax `read_trialmaster(split_mixed=c("col1", "col2"))` to split only the datasets you need to (#10).

### Bug fixes & Improvements

-   Reading with `read_trialmaster()` from cache will output an error if parameters (`split_mixed`, `clean_names_fun`) are different (#4).
-   `split_mixed_datasets()` is now fully case-insensitive.
-   Non-UTF8 characters in labels are now identified and corrected during reading (#5).

### Minor breaking changes

-   `read_trialmaster(use_cache="write")` is now the default. Reading from cache is not stable yet, so you should opt-in rather than opt-out.
-   `read_trialmaster(extend_lookup=TRUE)` is now the default.
-   Options `edc_id`, `edc_crfname`, and `edc_verbose` have been respectively renamed `edc_cols_id`, `edc_cols_crfname`, and `edc_read_verbose` for more clarity.

# EDCimport 0.3.0 <sub><sup>2023/05/19</sup></sub>

### New features

-   New function `edc_swimmerplot()` to show a swimmer plot of all dates in the database and easily find outliers.

-   New features in `read_trialmaster()`:

    -   `clean_names_fun=some_fun` will clean all names of all tables. For instance, `clean_names_fun=janitor::clean_names()` will turn default SAS uppercase column names into valid R snake-case column names.
    -   `split_mixed=TRUE` will split tables that contain both long and short data regarding patient ID into one long table and one short table. See `?split_mixed_datasets()` for details.
    -   `extend_lookup=TRUE` will improve the lookup table with additional information. See `?extend_lookup()` for details.
    -   `key_columns=get_key_cols()` is where you can change the default column names for patient ID and CRF name (used in other new features).

-   Standalone functions `extend_lookup()` and `split_mixed_datasets()`.

-   New helper `unify()`, which turns a vector of duplicate values into a vector of length 1.

### Bug fixes

-   Reading errors are now handled by `read_trialmaster()` instead of failing. If one XPT file is corrupted, the resulting object will contain the error message instead of the dataset.

-   `find_keyword()` is now robust to non-UTF8 characters in labels.

-   Option `edc_lookup` is now set even when reading from cache.

-   SAS formats containing a `=` now work as intended.

# EDCimport 0.2.1 <sub><sup>2022/11/01</sup></sub>

-   Import your data from TrialMaster using `tm = read_trialmaster("path/to/archive.zip")`.

-   Search for a keyword in any column name or label using `find_keyword("date", data=tm$.lookup)`. You can also generate a lookup table for an arbitrary list of dataframe using `build_lookup(my_data)`.

-   Load the datasets to the global environment using `load_list(tm)` to avoid typing `tm$` everywhere.

-   Browse available global options using `?EDCimport_options`.

# EDCimport 0.1.0

-   Draft version
