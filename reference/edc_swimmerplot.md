# Swimmer plot of all dates columns

Join all tables from `.lookup$dataset` on `id`

## Usage

``` r
edc_swimmerplot(
  .lookup = edc_lookup(),
  ...,
  id = get_subjid_cols(),
  group = NULL,
  origin = NULL,
  id_lim = NULL,
  exclude = NULL,
  time_unit = c("days", "weeks", "months", "years"),
  aes_color = c("variable", "label"),
  plotly = getOption("edc_plotly", FALSE)
)
```

## Arguments

- .lookup:

  the lookup table, default to
  [`edc_lookup()`](https://danchaltiel.github.io/EDCimport/reference/edc_lookup.md)

- ...:

  not used

- id:

  the patient identifier. Will be coerced as numeric.

- group:

  a grouping variable, given as "dataset\$column"

- origin:

  a variable to consider as time 0, given as "dataset\$column"

- id_lim:

  a numeric vector of length 2 providing the minimum and maximum `id` to
  subset on.

- exclude:

  a character vector of variables to exclude, in the form
  `dataset$column`. Can be a regex, but `$` symbols don't count.
  Case-insensitive.

- time_unit:

  if `origin!=NULL`, the unit to measure time. One of
  `c("days", "weeks", "months", "years")`.

- aes_color:

  either `variable` ("{dataset} - {column}") or `label` (the column
  label)

- plotly:

  whether to use `{plotly}` to get an interactive plot

## Value

either a `plotly` or a `ggplot`

## Examples

``` r
#tm = read_trialmaster("filename.zip", pw="xx")
tm = edc_example_plot()
#> Warning: Option "edc_lookup" has been overwritten.
load_list(tm)
p = edc_swimmerplot(.lookup, id_lim=c(5,45))
p2 = edc_swimmerplot(.lookup, origin="db0$date_naissance", time_unit="weeks", 
                     exclude=c("DB1$DATE2", "db3$.*"))
p3 = edc_swimmerplot(.lookup, group="db0$group", aes_color="label")
if (FALSE) { # \dontrun{
#save the plotly plot as HTML to share it
save_plotly(p, "edc_swimmerplot.html")
} # }
```
