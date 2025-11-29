# Warn if extraction is too old

Warn if extraction is too old

## Usage

``` r
edc_warn_extraction_date(max_days = 30)
```

## Arguments

- max_days:

  the max acceptable age of the data

## Value

nothing

## Examples

``` r
tm = edc_example()
#> Warning: Option "edc_lookup" has been overwritten.
load_list(tm)
edc_warn_extraction_date()
#> Error in edc_warn_extraction_date(): object 'datetime_extraction' not found
```
