
# EDCimport

EDCimport is a package designed to easily import data from EDC software TrialMaster and Macro. Browse code at <https://github.com/DanChaltiel/EDCimport>.

# EDCimport 0.3.0 <sub><sup>2023/05/19</sup></sub>

#### New features

- New function `edc_swimmerplot()` to show a swimmer plot of all dates in the database and easily find outliers.

- New features in `read_trialmaster()`:
  - `clean_names_fun=some_fun` will clean all names of all tables. For instance, `clean_names_fun=janitor::clean_names()` will turn default SAS uppercase column names into valid R snake-case column names.
  - `split_mixed=TRUE` will split tables that contain both long and short data regarding patient ID into one long table and one short table. See `?split_mixed_datasets()` for details.
  - `extend_lookup =TRUE` will improve the lookup table with additional information. See `?extend_lookup()` for details.
  - `key_columns=get_key_cols()` is where you can change the default column names for patient ID and CRF name (used in other new features).
  
- Standalone functions `extend_lookup()` and `split_mixed_datasets()`.

- New helper `unify()`, which turns a vector of duplicate values into a vector of length 1.

#### Bug fixes

- Reading errors are now handled by `read_trialmaster()` instead of failing. If one XPT file is corrupted, the resulting object will contain the error message instead of the dataset.

- `find_keyword()` is now robust to non-UTF8 characters in labels.

- Option `edc_lookup` is now set even when reading from cache.

- SAS formats containing a `=` now work as intended.


# EDCimport 0.2.1 <sub><sup>2022/11/01</sup></sub>

- Import your data from TrialMaster using `tm = read_trialmaster("path/to/archive.zip")`.

- Search for a keyword in any column name or label using `find_keyword("date", data=tm$.lookup)`. You can also generate a lookup table for an arbitrary list of dataframe using `get_lookup(my_data)`.

- Load the datasets to the global environment using `load_list(tm)` to avoid typing `tm$` everywhere.

- Browse available global options using `?EDCimport_options`.


# EDCimport 0.1.0

- Draft version
