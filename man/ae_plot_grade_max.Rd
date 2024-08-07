% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ae_table_grade.R
\name{ae_plot_grade_max}
\alias{ae_plot_grade_max}
\title{Graphic representation of AEs by grade max}
\usage{
ae_plot_grade_max(
  df_ae,
  ...,
  df_enrol,
  type = c("stack", "dodge", "fill"),
  proportion = TRUE,
  drop_levels = FALSE,
  arm = "ARM",
  subjid = "SUBJID",
  soc = "AESOC",
  grade = "AEGR"
)
}
\arguments{
\item{df_ae}{adverse event dataset, one row per AE, containing subjid, soc, and grade.}

\item{...}{unused}

\item{df_enrol}{enrollment dataset, one row per patient, containing subjid (and arm if needed). All patients should be in this dataset.}

\item{type}{the plots to be included. One of \code{c("stack", "dodge", "fill")}.}

\item{proportion}{display proportion instead of count.}

\item{drop_levels}{whether to drop unused grade levels.}

\item{arm}{name of the treatment column in \code{df_enrol}. Case-insensitive. Can be set to \code{NULL}.}

\item{subjid}{name of the patient ID in both \code{df_ae} and \code{df_enrol}. Case-insensitive.}

\item{soc}{name of the SOC column in \code{df_ae}. Case-insensitive. Grade will be considered 0 if missing (e.g. if patient if absent from \code{df_ae}).}

\item{grade}{name of the AE grade column in \code{df_ae}. Case-insensitive.}
}
\value{
a patchwork of ggplots
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#deprecated}{\figure{lifecycle-deprecated.svg}{options: alt='[Deprecated]'}}}{\strong{[Deprecated]}}
}
\details{
Produces a graphic representation of AE, counting the maximum grade each patient experienced, colored by treatment arm. Returns up to 3 representations if \code{arm!=NULL}.
}
\examples{
tm = edc_example_ae()
ae_plot_grade_max(df_ae=tm$ae, df_enrol=tm$enrolres)
ae_plot_grade_max(df_ae=tm$ae, df_enrol=tm$enrolres, type=c("dodge", "fill"), proportion=FALSE)
ae_plot_grade_max(df_ae=tm$ae, df_enrol=tm$enrolres, arm=NULL) + ggplot2::coord_flip()

#you can use modificators from the patchwork package, like "&"
\dontrun{
library(patchwork)
ae_plot_grade_max(df_ae=tm$ae, df_enrol=tm$enrolres) & labs(fill="Group")
}
}
\seealso{
\code{\link[=ae_table_grade_max]{ae_table_grade_max()}}, \code{\link[=ae_table_grade_n]{ae_table_grade_n()}}, \code{\link[=ae_table_soc]{ae_table_soc()}}, \code{\link[=ae_plot_grade_max]{ae_plot_grade_max()}}, \code{\link[=ae_plot_grade_n]{ae_plot_grade_n()}}
}
