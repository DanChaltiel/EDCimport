% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/viewer.R
\name{edc_viewer}
\alias{edc_viewer}
\title{Shiny data explorer}
\usage{
edc_viewer(data = NULL, background = TRUE, port = 1209)
}
\arguments{
\item{data}{A list of dataframes to view. If \code{NULL}, defaults to the last datasets loaded using EDCimport functions.}

\item{background}{Whether the app should run in a background process.}

\item{port}{The TCP port that the application should listen on.}
}
\description{
Run a Shiny application that allows to browse the datasets.
}
