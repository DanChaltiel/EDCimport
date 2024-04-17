% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers.R
\name{edc_data_warn}
\alias{edc_data_warn}
\title{Standardized warning system}
\usage{
edc_data_warn(df, message, issue_n = NULL)
}
\arguments{
\item{df}{the filtered dataframe}

\item{message}{the message. Can use {cli} formats.}

\item{issue_n}{(optional) identifying row number}
}
\value{
nothing
}
\description{
Database issues should be traced in a separate table file, with an identifying row number
}
\examples{
tm = edc_example_mixed()
a = tm$long_pure \%>\% filter(val1>2)
edc_data_warn(a, "{.val val1} should be lesser than 2", issue_n=1)
}