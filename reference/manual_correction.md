# Manual correction

**\[experimental\]**

When finding wrong or unexpected values in an exported dataset, it can
be useful to temporarily correct them by hard-coding a value. However,
this manual correction should be undone as soon as the central database
is updated with the correction.

- `manual_correction()` applies a correction in a specific dataset
  column location and throws an error if the correction is already in
  place. This check applies only once per R session so you can source
  your script without errors.

- `reset_manual_correction()` resets all checks. For instance, it is
  called by
  [`read_trialmaster()`](https://danchaltiel.github.io/EDCimport/reference/read_trialmaster.md).

## Usage

``` r
manual_correction(
  data,
  col,
  rows,
  wrong,
  correct,
  verbose = getOption("edc_correction_verbose", TRUE)
)

reset_manual_correction()
```

## Arguments

- data, col, rows:

  the rows of a column of a dataframe where the error lies

- wrong:

  the actual wrong value

- correct:

  the temporary correction value

- verbose:

  whether to print informations (once)

## Value

Nothing, used for side effects

## Examples

``` r
library(dplyr)
x = iris %>% mutate(id=row_number(), .before=1) %>% as_tibble()
x$Sepal.Length[c(1,3,5)]
#> [1] 5.1 4.7 5.0

#1st correction is silent
manual_correction(x, Sepal.Length, rows=c(1,3,5),
                  wrong=c(5.1, 4.7, 5.0), correct=c(5, 4, 3))
#> Manual correction of "x$Sepal.Length":
#> ℹ Old: 5.1, 4.7, and 5
#> ℹ New: 5, 4, and 3
x$Sepal.Length[c(1,3,5)]
#> [1] 5 4 3

#further correction is silent
manual_correction(x, Sepal.Length, rows=c(1,3,5),
                  wrong=c(5.1, 4.7, 5.0), correct=c(5, 4, 3)) 
                  
#if the database is corrected, an error is thrown
if (FALSE) { # \dontrun{
reset_manual_correction()
x$Sepal.Length[c(1,3,5)] = c(5, 4, 3) #mimics db correction
manual_correction(x, Sepal.Length, rows=c(1,3,5),
                  wrong=c(5.1, 4.7, 5.0), correct=c(5, 4, 3))
} # }
```
