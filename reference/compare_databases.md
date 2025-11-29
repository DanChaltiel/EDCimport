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

2024-01-01 (#0)

2024-01-01 (#1)

2024-04-01

ae

Added

Unchanged

Unchanged

data1

Added

+0 -2

+0 -1

data2

Added

+1 -1

+1 -1

data3

Added

Unchanged

Unchanged

data99

Absent

Added

Unchanged

enrol

Added

+2 -0

+2 -0

long_mixed

Added

Unchanged

Unchanged

long_pure

Added

Unchanged

Unchanged

short

Added

Unchanged

Unchanged

This table reflects changes in the dataset structure only, not in the
underlying data.

comparison\$figures \#\> NULL \#in real world, you should better use
paths with a reader function: if (FALSE) { \# \dontrun{ databases =
[c](https://rdrr.io/r/base/c.html)(
"data/MYPROJECT_ExportTemplate_xxx_SAS_XPORT_2024_06_01_12_00.zip",
"data/MYPROJECT_ExportTemplate_xxx_SAS_XPORT_2024_08_01_12_00.zip",
"data/MYPROJECT_ExportTemplate_xxx_SAS_XPORT_2024_09_01_12_00.zip", )
\#\`pw\` is passed to \`read_trialmaster()\` comparison =
compare_databases(databases, fun_read=read_trialmaster,
pw="the_password") } \# }
