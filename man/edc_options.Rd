% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/options.R
\name{edc_options}
\alias{edc_options}
\title{Set global options for \code{EDCimport}}
\usage{
edc_options(
  ...,
  trialmaster_pw,
  path_7zip,
  edc_lookup,
  edc_subjid_ref,
  edc_plotly,
  edc_cols_id,
  edc_cols_crfname,
  edc_read_verbose,
  edc_correction_verbose,
  edc_get_key_cols_verbose,
  edc_lookup_overwrite_warn,
  .local = FALSE
)
}
\arguments{
\item{...}{unused}

\item{trialmaster_pw}{the password of the trialmaster zip archive. For instance, you can use \code{edc_options(trialmaster_pw="my_pwd")} in the console once per session, so that you don't have to write the password in clear in your R code}

\item{path_7zip}{the path to the 7zip executable. Default to \code{"C:/Program Files/7-Zip/"}.}

\item{edc_lookup}{\strong{(Internal)} a reference to the lookup table (usually \code{.lookup}). Should usually not be changed manually.}

\item{edc_subjid_ref}{\strong{used in \link{check_subjid}} the vector of the reference subject IDs. You should usually write \code{edc_options(edc_subjid_ref=enrolres$subjid)}.}

\item{edc_plotly}{\strong{used in \link{edc_swimmerplot}} whether to use plotly to visualize the plot.}

\item{edc_cols_id, edc_cols_crfname}{\strong{used in \link{get_key_cols}} the name of the columns holding the subject id (default to \code{c("ptno", "subjid")}) and the CRF form name (default to \code{c("crfname")}). It is case-insensitive.}

\item{edc_read_verbose, edc_correction_verbose, edc_get_key_cols_verbose}{the verbosity of the output of functions \link{read_trialmaster} and \link{read_tm_all_xpt}, \link{manual_correction}, and \link{get_key_cols}. For example, set \code{edc_options(edc_read_verbose=0)} to silence the first 2.}

\item{edc_lookup_overwrite_warn}{default to TRUE. Whether there should be warning when overwriting \code{.lookup} (like when reading 2 databases successively)}

\item{.local}{if TRUE, the effect will only apply to the local frame (internally using \code{rlang::local_options()})}
}
\value{
Nothing, called for its side effects
}
\description{
Use this function to manage your \code{EDCimport} parameters globally while taking advantage of autocompletion. \cr
Use \code{\link[=edc_peek_options]{edc_peek_options()}} to see which option is currently set and \code{\link[=edc_reset_options]{edc_reset_options()}} to set all options back to default.
}
