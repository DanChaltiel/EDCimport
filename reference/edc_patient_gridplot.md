# Patient gridplot

Draw a gridplot giving, for each patient and each dataset, whether the
patient is present in the dataset. Data are drawn from
[get_datasets](https://danchaltiel.github.io/EDCimport/reference/get_datasets.md).

## Usage

``` r
edc_patient_gridplot(
  sort_rows = TRUE,
  sort_cols = TRUE,
  gradient = FALSE,
  axes_flip = FALSE,
  show_grid = TRUE,
  preprocess = NULL,
  palette = c(Yes = "#00468BFF", No = "#ED0000FF"),
  datasets = get_datasets(),
  lookup = edc_lookup()
)
```

## Arguments

- sort_rows:

  whether to sort patients from "present in most datasets" to "present
  in least datasets"

- sort_cols:

  whether to sort datasets from "containing the most patients" to
  "containing the least patients"

- gradient:

  whether to add a color gradient for repeating measures

- axes_flip:

  whether to flip the axes, so that patients are on the Y axis and
  datasets on the X axis

- show_grid:

  whether to show the grid

- preprocess:

  a function to preprocess the patient ID, e.g. `as.numeric`, or a
  custom function with string replacement

- palette:

  the colors to use

- datasets, lookup:

  internal

## Value

a `ggplot` object

## Examples

``` r
if (FALSE) { # \dontrun{
  tm = read_trialmaster("path/to/archive.zip")
  load_database(db)
  edc_patient_gridplot(sort_rows=FALSE, sort_cols=FALSE)
  edc_patient_gridplot(axes_flip=TRUE, show_grid=TRUE,
                       preprocess=~str_remove(.x, "\\D*")) #remove all non-digits
} # }
```
