# Important column names

Retrieve names of `patient_id` (usually "SUBJID" and "PATNO") and
`crfname` (usually "CRFNAME") from the actual names of the datasets

## Usage

``` r
get_key_cols(lookup = edc_lookup())
```

## Arguments

- lookup:

  the lookup table

## Value

a list(2) of characters with names `patient_id` and `crfname`
