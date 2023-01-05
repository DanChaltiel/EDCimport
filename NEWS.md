
# EDCimport

EDCimport is a package designed to easily import data from EDC software TrialMaster and Macro. Browse code at <https://github.com/DanChaltiel/EDCimport>.

# EDCimport 0.2.1 <sub><sup>2022/11/01</sup></sub>

- Import your data from TrialMaster using `tm = read_trialmaster("path/to/archive.zip")`.

- Search for a keyword in any column name or label using `find_keyword("date", data=tm$.lookup)`. You can also generate a lookup table for an arbitrary list of dataframe using `get_lookup(my_data)`.

- Load the datasets to the global environment using `load_list(tm)` to avoid typing `tm$` everywhere.

- Browse available global options using `?EDCimport_options`.


# EDCimport 0.1.0

- Draft version
