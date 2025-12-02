# Save a plotly to an HTML file

Save a plotly to an HTML file

## Usage

``` r
save_plotly(p, file, ...)
```

## Arguments

- p:

  a plot object (`plotly` or `ggplot`)

- file:

  a file path to save the HTML file. Can use the `glue` syntax to add
  variables.

- ...:

  passed on to
  [htmlwidgets::saveWidget](https://rdrr.io/pkg/htmlwidgets/man/saveWidget.html)

## Value

nothing, used for side effect

## Examples

``` r
if (FALSE) { # \dontrun{
db = edc_example()
load_database(db)
p = edc_swimmerplot(id_lim=c(5,45))
save_plotly(p, "graph/swimplots_{date_extraction}/edc_swimmerplot.html", 
            title="My Swimmerplot")
} # }
```
