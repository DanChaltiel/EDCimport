# EDCimport

<!-- badges: start -->

[![Package-License](http://img.shields.io/badge/license-GPL--3-brightgreen.svg?style=flat)](http://www.gnu.org/licenses/gpl-3.0.html) 
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-experimental-blue.svg)](https://lifecycle.r-lib.org/articles/stages.html) 
[![CRAN status](https://www.r-pkg.org/badges/version/EDCimport)](https://CRAN.R-project.org/package=EDCimport) 
[![Last Commit](https://img.shields.io/github/last-commit/DanChaltiel/EDCimport)](https://github.com/DanChaltiel/EDCimport) 
[![minimal R version](https://img.shields.io/badge/R-%E2%89%A53.1-blue.svg)](https://cran.r-project.org/)
[![R-CMD-check](https://github.com/DanChaltiel/EDCimport/actions/workflows/check-standard.yaml/badge.svg)](https://github.com/DanChaltiel/EDCimport/actions/workflows/check-standard.yaml)
<!--[![CRAN RStudio mirror downloads](https://cranlogs.r-pkg.org/badges/grand-total/EDCimport?color=blue)](https://r-pkg.org/pkg/EDCimport)  --> 
<!-- badges: end -->

EDCimport is a package designed to easily import data from EDC softwares in clinical research.

It is an opinionated framework, built to compute usual calculations given a database of several datasets sharing a common "Subject ID" key.

It can directly read from an export of [TrialMaster](https://www.anjusoftware.com/trial-master/).

> [!WARNING]
> This package is experimental and under active developement. <br>
> Retrocompatibility is not a priority for the moment.<br>
> For reproducibility, use [renv](https://rstudio.github.io/renv/articles/renv.html) to fix the package version.


## Installation

``` r
# Install last version available on CRAN (once published)
install.packages("EDCimport")

# Install development version on Github
pak::pak("DanChaltiel/EDCimport")
```

### Load the data

Your data usually comes as multiple files inside a specific directory. 

Depending on the type of the files, you should use one of `read_all_sas()`, `read_all_xpt()`, `read_all_csv()`, or `read_trialmaster()`.

For TrialMaster, you should request an export of type `SAS Xport`, with the checkbox "Include Codelists" ticked. This export should generate a `.zip` archive that you can use in `read_trialmaster()`.

The resulting object `db` is an `edc_database` object containing all the datasets and some metadatas.

You can now use `load_database()` to import the list in the global environment and use your tables:


``` r
library(EDCimport)
db = read_all_sas("path/to/my/files/")
load_database(db) #this also removes `db` to save some RAM
mean(dataset1$column5)
```

### Database management tools

`EDCimport` include a set of useful tools that help with using the imported database. See [References](https://danchaltiel.github.io/EDCimport/reference/index.html) for a complete list.

#### Database summary

`edc_lookup()` returns a dataframe containing the number of rows, columns, patients, and the CRF 
name of each dataset.

#### Search the whole database

`find_keyword()` runs a global search of the database for a given keyword (or regex). 

For instance, say you are looking for the "date of ECG" but don't know where it is, you can run `find_keyword("date")` or `find_keyword("ecg")`.

It won't look into the actual data, though, as this would take too much computing power.

### Swimmer Plot

`edc_swimmerplot()` creates a swimmer plot of **all date variables** of the whole database. 
This is very useful to find inconsistencies and outliers, especially with the `plotly` interactive output.

### Data checking system

`edc_data_warn()` throws a warning if an inconsistency is found in a dataset. The interface allows
to perform multiple checks and get a report as a CSV file.

### Join helpers

As the primary key is almost always the Subject ID, join helpers were added to reduce code clutter. 
Currently, only `edc_left_join()`, `edc_right_join()`, and `edc_full_join()` are supported.

### Shiny browser

`edc_viewer()` runs a shiny application that browses the whole database. The HTML interface is quicker 
and less cluttered than it would be in RStudio. It also allows to filter by Subject ID.
