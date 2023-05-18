# EDCimport

<!-- badges: start -->

[![Package-License](http://img.shields.io/badge/license-GPL--3-brightgreen.svg?style=flat)](http://www.gnu.org/licenses/gpl-3.0.html) [![Lifecycle: stable](https://img.shields.io/badge/lifecycle-experimental-blue.svg)](https://lifecycle.r-lib.org/articles/stages.html) [![CRAN status](https://www.r-pkg.org/badges/version/EDCimport)](https://CRAN.R-project.org/package=EDCimport) <!--[![CRAN RStudio mirror downloads](https://cranlogs.r-pkg.org/badges/grand-total/EDCimport?color=blue)](https://r-pkg.org/pkg/EDCimport)  --> [![Last Commit](https://img.shields.io/github/last-commit/DanChaltiel/EDCimport)](https://github.com/DanChaltiel/EDCimport) [![minimal R version](https://img.shields.io/badge/R-%E2%89%A53.1-blue.svg)](https://cran.r-project.org/)

<!-- badges: end -->

EDCimport is a package designed to easily import data from EDC software TrialMaster and Macro.

## Installation

``` r
# Install last version available on CRAN (once published)
install.packages("EDCimport")

# Install development version on Github
devtools::install_github("DanChaltiel/EDCimport")
```

You will also need [`7-zip`](https://www.7-zip.org/download.html) installed, and preferably added to the [`PATH`](https://www.java.com/en/download/help/path.html).

### Windows-only

This package was developed to work on Windows and is unlikely to work on any other OS. Feel free to submit a PR if you manage to get it to work on another OS.

## TrialMaster

### Load the data

First, you need to request an export of type `SAS Xport`, with the checkbox "Include Codelists" ticked. This export should generate a `.zip` archive.

Then, simply use `read_trialmaster()` with the archive password (if any) to retrieve the data from the archive:

``` r
library(EDCimport)
tm = read_trialmaster("path/to/my/archive.zip", pw="foobar")
```

The resulting object `tm` is a list containing all the datasets, plus the date of extraction (`datetime_extraction`) and a dataset summary (`.lookup`).

You can now use `load_list()` to import the list in the global environment and use your tables:

``` r
load_list(tm) #this also removes `tm` to save memory
mean(dataset1$column5)
```

There are other options available, e.g. colnames cleaning & table splitting), see `?read_trialmaster` for more details.

## Utils

`EDCimport` include a set of useful tools that help with using the imported database.

### Search the whole database

`.lookup` is a dataframe containing for each dataset all its column names and labels.

Its main use is to work with `find_keyword()`. For instance, say you do not remember in which dataset and column is located the "date of ECG". `find_keyword()` will search every column name and label and will give you the answer:

``` r
find_keyword("date")
```

``` r
#> # A tibble: 10 x 3
#>    dataset names   labels                      
#>    <chr>   <chr>   <chr>                       
#>  1 pat     PTRNDT  Randomization Date          
#>  2 pat     RGSTDT  Registration Date           
#>  3 site    INVDAT  Deactivation date           
#>  4 site    TRGTDT  Target Enroll Date          
#>  5 trial   TRSPDT  End Date                    
#>  6 trial   TRSTDT  Start Date                  
#>  7 visit   VISIT2  Visit Date                  
#>  8 visit   EEXPVDT Earliest Expected Visit Date
#>  9 vs      ECGDAT  Date of ECG                 
#> 10 vs      VISITDT Visit Date
```

Note that `find_keyword()` uses the `edc_lookup` option as its second argument, automatically set by `read_trialmaster()`.

### Swimmer Plot

The `edc_swimmerplot()` function will create a swimmer plot of all date variables in the whole database.

There are 2 arguments of interest:

-   `group`, a grouping variable (e.g. the treatment arm)

-   `origin`, a date variable acting as the time zero (e.g. the date of enrollment)

``` r
edc_swimmerplot()
edc_swimmerplot(group="enrolres$arm")
edc_swimmerplot(origin="enrolres$enroldt")
```

This outputs a `plotly` interactive graph where you can select the dates of interest and zoom in with your mouse.

![](man/figures/swimmerplot.png)

Note that any modification made after running `read_trialmaster()` is taken into account. For instance, mutating a column with `as.Date()` in one of the tables will add a new group in the plot.
