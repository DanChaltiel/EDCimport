# Show the current CRF status distribution

Generate a barplot showing the distribution of CRF status (Complete,
Incomplete, ...) for each dataset of the database.

## Usage

``` r
edc_crf_plot(
  crfstat_col = "CRFSTAT",
  ...,
  details = FALSE,
  pal = edc_pal_crf(),
  reverse = FALSE,
  x_label = "{dataset}",
  treat_as_worst = NULL,
  datasets = get_datasets(),
  lookup = edc_lookup()
)

edc_pal_crf()
```

## Source

`ggsci:::ggsci_db$lancet[["lanonc"]] %>% dput()`

## Arguments

- crfstat_col:

  the column name of the CRF status

- ...:

  unused

- details:

  whether to show all the CRF status levels. When `FALSE` (default),
  recode the status into "Complete", "Incomplete", or "No Data".

- pal:

  the palette, defaulting to the helper `EDCimport:::edc_pal_crf()`. The
  names give the CRF status levels, from "best" to "worst". The plot is
  ordered by the "worst" level.

- reverse:

  whether to reverse the CRF status level order.

- x_label:

  a glue pattern determining the tick label in the x axis. Available
  variables are the ones of
  [`edc_lookup()`](https://danchaltiel.github.io/EDCimport/reference/edc_lookup.md):
  `c("dataset", "nrow", "ncol", "n_id", "rows_per_id", "crfname")`.

- treat_as_worst:

  a regex for levels that should be treated as worst in the ordering.

- datasets, lookup:

  internal

## Value

a ggplot

## Examples

``` r
if (FALSE) { # \dontrun{
#import a TM database and use load_database(), then:
edc_crf_plot() + ggtitle(date_extraction)
edc_crf_plot(reverse=TRUE)
edc_crf_plot(details=TRUE, treat_as_worst="No Data")
edc_crf_plot(x_label="{crfname} (N={n_id}, n={nrow})")

p = edc_crf_plot(details=TRUE)
p$data$crfstat %>% unique()
#> [1] "Incomplete"        "No Data Locked"    "No Data"           "Signed"           
#> [5] "Partial Monitored" "Monitored"         "Complete Locked"   "Complete" 
} # }
```
