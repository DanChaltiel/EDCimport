# Read all `.csv` files in a directory

Read all `.csv` files in a directory, with labels if specified.

## Usage

``` r
read_all_csv(
  path,
  ...,
  labels_from = NULL,
  clean_names_fun = NULL,
  read_fun = "guess",
  datetime_extraction = "guess",
  verbose = getOption("edc_read_verbose", 1)
)
```

## Arguments

- path:

  \[`character(1)`\]  
  path to the directory containing `.csv` files.

- ...:

  unused

- labels_from:

  \[`misc`\]  
  list of path to file containing the labels.

- clean_names_fun:

  \[`function`\]  
  a function to clean column names, e.g.
  [tolower](https://rdrr.io/r/base/chartr.html),
  [`janitor::clean_names()`](https://sfirke.github.io/janitor/reference/clean_names.html),...

- read_fun:

  \[`function`\]  
  a function to read the files in path, e.g.
  [`read.csv()`](https://rdrr.io/r/utils/read.table.html),
  [`read.csv2()`](https://rdrr.io/r/utils/read.table.html),...

- datetime_extraction:

  \[`dateish(1)`\]  
  the datetime of database extraction (database lock). If "guess", the
  datetime will be inferred from the files modification time.

- verbose:

  \[`numeric(1)`\]  
  the level of verbosity

## Value

a list containing one dataframe for each `.csv` file in the folder, the
extraction date (`datetime_extraction`), and a summary of all imported
tables (`.lookup`).

## Labels file

`labels_from` should contain the information about column labels. It
should be a data file (`.csv`) containing 2 columns: one for the column
name and the other for its associated label. Use
`options(edc_col_name="xxx", edc_col_label="xxx")` to specify the names
of the columns.
