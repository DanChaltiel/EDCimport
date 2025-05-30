% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clean_names.R
\name{edc_clean_names}
\alias{edc_clean_names}
\title{Clean up the names of all datasets}
\usage{
edc_clean_names(database, clean_fun = NULL)
}
\arguments{
\item{database}{an \link{edc_database} object, from \code{\link[=read_trialmaster]{read_trialmaster()}} or other EDCimport reading functions.}

\item{clean_fun}{a cleaning function to be applied to column names.}
}
\value{
an \link{edc_database} object
}
\description{
Clean the names of all the datasets in the database. By default, it converts names to lowercase
letters, numbers, and underscores only.
}
\examples{
#db = read_trialmaster("filename.zip", pw="xx")
db = edc_example() \%>\% 
  edc_clean_names()
names(db$enrol)
}
