# Shiny data explorer

Run a Shiny application that allows to browse the datasets.

## Usage

``` r
edc_viewer(data = NULL, background = TRUE, port = 1209)
```

## Arguments

- data:

  A list of dataframes to view. If `NULL`, defaults to the last datasets
  loaded using EDCimport functions.

- background:

  Whether the app should run in a background process.

- port:

  The TCP port that the application should listen on.
