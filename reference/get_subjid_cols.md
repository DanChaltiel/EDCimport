# Get key column names

Retrieve names of patient ID and CRF name from the actual names of the
datasets, without respect of the case. Default values should be set
through options.

## Usage

``` r
get_subjid_cols(lookup = edc_lookup())
```

## Arguments

- lookup:

  the lookup table

## Value

a character vector

## options

Use
[`edc_options()`](https://danchaltiel.github.io/EDCimport/reference/edc_options.md)
to set default values:

- `edc_cols_subjid` defaults to `c("SUBJID", "PTNO")`

- `edc_cols_crfname` defaults to `c("FORMDESC", "CRFNAME")`

## Examples

``` r
#get_subjid_cols()
#get_crfname_cols()
```
