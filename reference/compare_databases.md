# Compare multiple EDC database extractions

Compares several EDC database extractions and returns:

## Usage

``` r
compare_databases(databases, fun_read = read_trialmaster, ...)
```

## Arguments

- databases:

  file paths to read using `fun_read`. Can also be a list of
  `edc_database` objects.

- fun_read:

  Reading function to use on `databases`

- ...:

  arguments passed to `fun_read`

## Value

a list of `table` (a `gt` object with tooltips) and `plot` (a
`patchwork` of ggplots)

## Details

- a summary table of the detected differences in datasets/columns
  presence

- a summary plot of the differences in number of rows, columns,
  patients, and rows per patient

## Examples

``` r
#list of 3 edc_databases, each being a list of multiple datasets
databases = edc_example_multiple() 

comparison = compare_databases(databases)
#> Warning: Some database extraction dates are not unique: "extract_2024_01_01"
comparison$table


  

dataset
```
