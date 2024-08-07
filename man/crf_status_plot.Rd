% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/crf_status.R
\name{crf_status_plot}
\alias{crf_status_plot}
\alias{edc_pal_crf}
\title{Show the current CRF status distribution}
\source{
\code{ggsci:::ggsci_db$lancet[["lanonc"]] \%>\% dput()}
}
\usage{
crf_status_plot(
  crfstat_col = "CRFSTAT",
  ...,
  details = FALSE,
  pal = edc_pal_crf(),
  crfstat_lvls = names(pal),
  x_label = "{dataset}",
  treat_as_worst = NULL
)

edc_pal_crf()
}
\arguments{
\item{crfstat_col}{the column name of the CRF status}

\item{...}{unused}

\item{details}{whether to show all the CRF status levels. When \code{FALSE} (default), recode the status into "Complete", "Incomplete", or "No Data".}

\item{pal}{the palette, defaulting to the helper \code{EDCimport:::edc_pal_crf()}}

\item{crfstat_lvls}{the CRF status levels, from "best" to "worst". The plot is ordered by the "worst" level.}

\item{x_label}{a glue pattern determining the tick label in the x axis. Available variables are \code{c("nrow", "ncol", "n_id", "rows_per_id", "crfname")}, taken from \code{\link[=edc_lookup]{edc_lookup()}}.}

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
crf_status_plot() + ggtitle(date_extraction)
crf_status_plot(pal=rev(edc_pal_crf()))
crf_status_plot(details=TRUE, treat_as_worst="No Data")
crf_status_plot(x_label="{crfname} (N={n_id}, n={nrow})")

p = crf_status_plot(details=TRUE)
p$data$crfstat \%>\% unique()
#> [1] "Incomplete"        "No Data Locked"    "No Data"           "Signed"           
#> [5] "Partial Monitored" "Monitored"         "Complete Locked"   "Complete" 
}
}
