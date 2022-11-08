
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

Note that this package is expected to run on Windows. If you use any other OS, please contact me so we can make it work with them too.

## TrialMaster

### Load the data

First, you need to request an export of type `SAS Xport`, with the checkbox "Include Codelists" ticked. This export should generate a zip archive.

Then, simply provide the archive password using `options()` (if any), and use `read_trialmaster()` to load the data from the archive:

``` r
library(EDCimport)
filename = "path/to/my/archive"
options(trialmaster_pw="foobar")
tm = read_trialmaster(filename)
```

### Use the data

The resulting object `tm` is a list containing all the datasets, plus the date of extraction (`datetime_extraction`) and a dataset summary (`.lookup`).

You can now either use the resulting list, or load it to the global environment. For instance,

``` r
#use it directly
mean(tm$dataset1$column5)

#load it to the global env
load_list(tm) #this also removes `tm` to save RAM
mean(dataset1$column5)
```

### Search the data

The dataset lookup table `.lookup` is a dataframe containing for each dataset all its column names and labels.

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

Note that `find_keyword()` uses the `edc_lookup` option, automatically set by `read_trialmaster()`.

## MACRO

Work in progress
