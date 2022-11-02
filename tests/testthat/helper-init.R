Sys.setenv(LANGUAGE = "en")
Sys.setenv(TZ='Europe/Paris')


options(
  encoding="UTF-8",
  # warn=0, #default, stacks
  warn=1, #immediate.=TRUE
  # warn=2, #error
  # warnPartialMatchArgs=TRUE,
  # warnPartialMatchAttr=TRUE,
  # warnPartialMatchDollar=TRUE,
  stringsAsFactors=FALSE,
  dplyr.summarise.inform=FALSE,
  # conflicts.policy="depends.ok",
  tidyverse.quiet=TRUE,
  tidyselect_verbosity ="verbose",#quiet or verbose
  lifecycle_verbosity="warning", #NULL, "quiet", "warning" or "error"
  testthat.progress.max_fails = 50
)

options(trialmaster_pw="0")
getOption("trialmaster_pw")

cachename="CRF_Dan_2022-08-25.rds"
filename="CRF_Dan_Export_SAS_XPORT_2022_08_25_15_16.zip"
filename_noformat="CRF_Dan_Export_SAS_XPORT_2022_08_25_15_16_noformat.zip"
filename_nopw="CRF_Dan_Export_SAS_XPORT_2022_08_25_15_16_nopw.zip"


if(!is_testing()){
  cachename=paste0("tests/testthat/", cachename)
  filename=paste0("tests/testthat/", filename)
  filename_noformat=paste0("tests/testthat/", filename_noformat)
  filename_nopw=paste0("tests/testthat/", filename_nopw)
}

clean_cache = function() if(file.exists(cachename)) file.remove(cachename)
v=utils::View


snapshot_review_bg = function(...){
  # brw = function(url) .Call("rs_browseURL", url, PACKAGE="(embedding)")
  brw = Sys.getenv("R_BROWSER")
  callr::r_bg(function() testthat::snapshot_review(...),
              package=TRUE,
              env = c(R_BROWSER = brw))
}

message('Helper-init loaded')
