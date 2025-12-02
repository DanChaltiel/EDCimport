# Shiny data explorer

Run a Shiny application that allows to browse the datasets.

## Usage

``` r
edc_viewer(
  data = NULL,
  ...,
  background = TRUE,
  title = NULL,
  port = 1209,
  replace = FALSE
)
```

## Arguments

- data:

  A list of dataframes to view. If `NULL`, defaults to the last datasets
  loaded using EDCimport functions.

- ...:

  unused

- background:

  Whether the app should run in a background process.

- title:

  The app title, in the header and the tab label.

- port:

  The TCP port that the application should listen on.

- replace:

  whether to replace a previously running app on the same port.
