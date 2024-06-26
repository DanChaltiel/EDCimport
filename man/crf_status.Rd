% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/crf_status.R
\name{crf_status}
\alias{crf_status}
\alias{edc_crf_pal}
\title{Show the current CRF status distribution}
\source{
ggsci:::ggsci_db$lancet[\link{"lanonc"}] \%>\% dput()
}
\usage{
crf_status(
  crfstat_col = "CRFSTAT",
  pal = edc_crf_pal(),
  crfstat_lvls = names(pal),
  treat_as_worst = NULL
)

edc_crf_pal()
}
\arguments{
\item{crfstat_col}{the column name of the CRF status}

\item{pal}{the palette, defaulting to the helper \code{EDCimport:::edc_crf_pal()}}

\item{crfstat_lvls}{the CRF status levels, from "best" to "worst". The plot is ordered by the "worst" level.}

\item{treat_as_worst}{a regex for levels that should be treated as worst in the ordering}
}
\value{
a ggplot
}
\description{
Generate a barplot showing the distribution of CRF status (Complete, Incomplete, ...) for each dataset of the database.
}
\examples{
\dontrun{
#import a TM database and use load_list(), then:
crf_status() + ggtitle(date_extraction)
crf_status(pal=rev(edc_crf_pal()))
crf_status(treat_as_worst="No Data")

p = crf_status()
p
p$data$crfstat \%>\% levels()
#> [1] "Incomplete"        "No Data Locked"    "No Data"           "Signed"           
#> [5] "Partial Monitored" "Monitored"         "Complete Locked"   "Complete" 
}
}
