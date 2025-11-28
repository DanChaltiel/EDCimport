# Example database

A list of tables that simulates the extraction of a clinical database.
Used in `EDCimport` examples and tests.

## Usage

``` r
edc_example(N = 50, seed = 42, outdated = FALSE)
```

## Arguments

- N:

  the number of patients

- seed:

  the random seed

- outdated:

  whether to simulate times after the data extraction date

## Value

A list of tables of class `edc_database`.
