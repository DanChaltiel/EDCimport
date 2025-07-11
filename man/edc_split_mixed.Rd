% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/split_mixed.R
\name{edc_split_mixed}
\alias{edc_split_mixed}
\title{Split mixed datasets}
\usage{
edc_split_mixed(
  database,
  datasets = everything(),
  ...,
  ignore_cols = NULL,
  verbose = FALSE
)
}
\arguments{
\item{database}{an \link{edc_database} object, from \code{\link[=read_trialmaster]{read_trialmaster()}} or other EDCimport reading functions.}

\item{datasets}{<\link[dplyr:dplyr_tidy_select]{tidy-select}> datasets to split in the database}

\item{...}{not used, ensure arguments are named}

\item{ignore_cols}{columns to ignore in long tables. Default to \code{getOption("edc_cols_crfname", "CRFNAME")}. Case-insensitive. Avoid splitting tables for useless columns.}

\item{verbose}{whether to print informations about the process.}
}
\value{
an \link{edc_database} object
}
\description{
Split mixed tables, i.e. tables that hold both long data (N values per patient) and short data (one value per patient, duplicated on N lines), into one long table and one short table.
}
\examples{
#db = read_trialmaster("filename.zip", pw="xx")
db = edc_example() \%>\% 
  edc_split_mixed(c(ae, starts_with("long")), 
                  ignore_cols="crfstat")
  
names(db)
edc_lookup()

db$ae #`aesoc`, `aegr`, and `sae` are long, but `n_ae` is short

db$ae_short
db$ae_long
}
