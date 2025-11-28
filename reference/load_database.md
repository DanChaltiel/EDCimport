# Load a list in an environment

Load a list in an environment

## Usage

``` r
load_database(db, env = parent.frame(), remove = TRUE)
```

## Arguments

- db:

  an
  [edc_database](https://danchaltiel.github.io/EDCimport/reference/edc_database.md)
  object (to be fair, any list would do)

- env:

  the environment onto which the list should be loaded

- remove:

  if `TRUE`, `db` will be removed from the environment afterward

## Value

nothing, called for its side-effect

## Examples

``` r
db = edc_example()
#> Warning: Option "edc_lookup" has been overwritten.
load_database(db, remove=FALSE)
print(db)
#> ── EDCimport database ──────────────────────────────────────────────────────────
#> Contains 8 tables: `enrol`, `data1`, `data2`, …, `long_mixed`, and `ae`
#> ℹ Use `EDCimport::load_database(db)` to load the tables in the global
#>   environment.
#> ℹ Use `EDCimport::edc_lookup()` to see the summary table.
print(lengths(db))
#>               enrol               data1               data2               data3 
#>                   6                   7                   6                   7 
#>               short           long_pure          long_mixed                  ae 
#>                   5                   4                   6                   7 
#> datetime_extraction     date_extraction             .lookup 
#>                   1                   1                   9 
```
