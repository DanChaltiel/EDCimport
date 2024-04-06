% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ae_table_grade.R
\name{ae_table_grade_max}
\alias{ae_table_grade_max}
\alias{ae_plot_grade_max}
\title{Summary tables for AE by grade max}
\usage{
ae_table_grade_max(
  df_ae,
  df_enrol,
  arm = "ARM",
  subjid = "SUBJID",
  soc = "AESOC",
  grade = "AEGR",
  total = TRUE,
  digits = 0
)

ae_plot_grade_max(
  df_ae,
  df_enrol,
  type = c("stack", "dodge", "fill"),
  arm = "ARM",
  subjid = "SUBJID",
  soc = "AESOC",
  grade = "AEGR"
)
}
\arguments{
\item{arm}{name of the treatment column in \code{df}. Case-insensitive. Can be set to \code{NULL} to not group.}

\item{subjid, grade}{names of the other relevant columns in \code{df}. Case-insensitive.}

\item{soc}{name of the SOC column in \code{df}. Case-insensitive. Grade will be considered 0 is missing.}

\item{total}{whether to add totals}

\item{digits}{number of signigicant digits for percentages}

\item{df}{input data, one row per event}
}
\value{
a crosstable (dataframe)

a patchwork of ggplots
}
\description{
The function \code{ae_table_grade_max()} creates a summary table of the maximum AE grade experienced per each patient.
The resulting crosstable can be piped to \code{as_flextable()} to get a nicely formatted flextable.
}
\details{
The function \code{ae_plot_grade_max()} creates summary plots of maximum AE grades in up to 3 different ways.
}
\examples{
\dontrun{

tm = edc_example_ae()
ae_table_grade_max(df_ae=tm$ae, df_enrol=tm$enrolres)
ae_plot_grade_max(df_ae=tm$ae, df_enrol=tm$enrolres)

# 1) Apply table functions
#you can use as_flextable() to get an HTML flextable
#you can use modificators modificators from the flextable package
ae_table_grade_max(df_ae=ae, df_enrol=enrolres, arm=NULL) \%>\% 
  add_footer_lines("Percentages are given as the proportion of patients presenting at most one AE of given grade")
ae_table_grade_max(df_ae=ae, df_enrol=enrolres) \%>\%
  as_flextable(by_header="Both arms") \%>\% 
  highlight(i=~variable=="Grade 5", j=-1)
  
# 2) Apply plot functions
#you can choose the type
#you can use modificators from the patchwork package, like "&"
ae_plot_grade_max(df_ae=ae, df_enrol=enrolres) & labs(fill="Group")
ae_plot_grade_max(df_ae=ae, df_enrol=enrolres, type=c("dodge", "fill"))
ae_plot_grade_max(df_ae=ae, df_enrol=enrolres, arm=NULL) + coord_flip()
}
}