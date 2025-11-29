# Find a keyword in the whole database

Find a keyword in all names and labels of a list of datasets.

## Usage

``` r
find_keyword(keyword, data = edc_lookup(), ignore_case = TRUE)
```

## Arguments

- keyword:

  the keyword to search for. Can handle regular expressions (see
  examples).

- data:

  the lookup dataframe where to search the keyword. Can be set using
  `edc_options(edc_lookup=my_data)`, which is done automatically when
  calling
  [`read_trialmaster()`](https://danchaltiel.github.io/EDCimport/reference/read_trialmaster.md).

- ignore_case:

  should case differences be ignored in the match? Default to `TRUE`.

## Value

a tibble

## Examples

``` r
if (FALSE) { # \dontrun{
path = system.file("extdata/Example_Export_SAS_XPORT_2022_08_25_15_16.zip", 
                   package="EDCimport", mustWork=TRUE)
w = read_trialmaster(path, verbose=FALSE)

find_keyword("patient")

#with regex
find_keyword("patient$")
find_keyword("\\d")
find_keyword("(Trial|Form) Name")
find_keyword("\\(") #you need to escape special characters
} # }
```
