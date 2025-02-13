% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_all_csv.R
\name{read_all_csv}
\alias{read_all_csv}
\title{Read all \code{.csv} files in a directory}
\usage{
read_all_csv(
  path,
  ...,
  labels_from = NULL,
  clean_names_fun = NULL,
  read_fun = "guess",
  subdirectories = FALSE,
  datetime_extraction = "guess",
  verbose = getOption("edc_read_verbose", 1)
)
}
\arguments{
\item{path}{[\code{character(1)}]\cr path to the directory containing \code{.csv} files.}

\item{...}{unused}

\item{labels_from}{[\code{misc}]\cr list of path to file containing the labels.}

\item{clean_names_fun}{[\code{function}]\cr a function to clean column names, e.g. \link{tolower}, \code{\link[janitor:clean_names]{janitor::clean_names()}},...}

\item{read_fun}{[\code{function}]\cr a function to read the files in path, e.g. \code{read.csv()}, \code{read.csv2()},...}

\item{subdirectories}{[\code{logical(1)}]\cr whether to read subdirectories.}

\item{datetime_extraction}{[\code{dateish(1)}]\cr the datetime of database extraction (database lock). If "guess", the datetime will be inferred from the files modification time.}

\item{verbose}{[\code{logical(1)}]\cr the level of verbosity}
}
\value{
a list containing one dataframe for each \code{.csv} file in the folder, the extraction date (\code{datetime_extraction}), and a summary of all imported tables (\code{.lookup}).
}
\description{
Read all \code{.csv} files in a directory, with labels if specified.
}
\section{Labels file}{

\code{labels_from} should contain the information about column labels. It should be a data file (\code{.csv}) containing 2 columns: one for the column name and the other for its associated label. Use \code{options(edc_col_name="xxx", edc_col_label="xxx")} to specify the names of the columns.
}

\seealso{
Other EDCimport read functions: \code{\link[=read_trialmaster]{read_trialmaster()}}, \code{\link[=read_all_sas]{read_all_sas()}}, \code{\link[=read_all_xpt]{read_all_xpt()}}, and \code{\link[=read_all_csv]{read_all_csv()}}.

Other EDCimport reading functions: 
\code{\link{read_all_sas}()},
\code{\link{read_all_xpt}()},
\code{\link{read_trialmaster}()}
}
\concept{EDCimport reading functions}
