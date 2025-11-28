# EDCimport Database

This class of object represents a database, as the result of an
EDCimport reading function. It has its own
[`print()`](https://rdrr.io/r/base/print.html) method.

## Functions returning `edc_database` objects

As per now, reading functions are:
[`read_trialmaster()`](https://danchaltiel.github.io/EDCimport/reference/read_trialmaster.md),
[`read_all_sas()`](https://danchaltiel.github.io/EDCimport/reference/read_all_sas.md),
[`read_all_xpt()`](https://danchaltiel.github.io/EDCimport/reference/read_all_xpt.md),
and
[`read_all_csv()`](https://danchaltiel.github.io/EDCimport/reference/read_all_csv.md).

## Structure

While it is not usually useful to query them, an `edc_database` object
is a named list containing:

- all the datasets from the source files

- `datetime_extraction` and `date_extraction` the inferred date of data
  extraction

- `.lookup` a temporary copy of the lookup table

## See also

[`read_trialmaster()`](https://danchaltiel.github.io/EDCimport/reference/read_trialmaster.md)
