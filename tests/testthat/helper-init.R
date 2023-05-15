Sys.setenv(LANGUAGE = "en")
Sys.setenv(TZ="Europe/Paris")


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
# getOption("trialmaster_pw")

cachename="trialmaster_export_2022-08-25 15h16.rds"
filename="CRF_Dan_Export_SAS_XPORT_2022_08_25_15_16.zip"
filename_noformat="CRF_Dan_Export_SAS_XPORT_2022_08_25_15_16_noformat.zip"
filename_nopw="CRF_Dan_Export_SAS_XPORT_2022_08_25_15_16_nopw.zip"
filename_bad="CRF_Dan_Export.zip"


if(!is_testing()){
  cachename=paste0("tests/testthat/", cachename)
  filename=paste0("tests/testthat/", filename)
  filename_noformat=paste0("tests/testthat/", filename_noformat)
  filename_nopw=paste0("tests/testthat/", filename_nopw)
  filename_bad=paste0("tests/testthat/", filename_bad)
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

temp_target = function(name){
  target = file.path2(tempdir(), "name")
  unlink(target, recursive=TRUE)
  dir.create(target, showWarnings=FALSE)
  target
}

is_testing_in_buildpane = function(){
  # Sys.getenv("RSTUDIO_CHILD_PROCESS_PANE") =="build"
  
  # print("----------")
  # # print(Sys.getenv("RSTUDIO_CHILD_PROCESS_PANE"))
  # print(getwd())
  # print(Sys.getenv())
  # print("----------")
  
  str_ends(getwd(), "testthat/?")
}

expect_classed_conditions = function(expr, message_class=NULL, warning_class=NULL, error_class=NULL){
  ms = list()
  ws = list()
  es = list()
  x = withCallingHandlers(
    withRestarts(expr, muffleStop=function() "error"),
    message=function(m){
      ms <<- c(ms, list(class(m)))
      invokeRestart("muffleMessage")
    },
    warning=function(w){
      ws <<- c(ws, list(class(w)))
      invokeRestart("muffleWarning")
    }, 
    error=function(e){
      es <<- c(es, list(class(e)))
      invokeRestart("muffleStop")
    }
  )
  
  # browser()
  expect2 = function(ok, msg, ...) expect(ok, cli::cli_fmt(cli::cli_text(msg, .envir=parent.frame())), ...)
  
  messages_ok = map_lgl(ms, \(.x) any(.x %in% message_class))
  messages_ok2 = map_lgl(message_class, \(.x) any(.x %in% unlist(ms)))
  expect2(all(messages_ok),  "`expr` is throwing an unexpected message")
  expect2(all(messages_ok2), "`expr` is not throwing message{?s} {.var {message_class[!messages_ok2]}}")

  warnings_ok = map_lgl(ws, \(.x) any(.x %in% warning_class))
  warnings_ok2 = map_lgl(warning_class, \(.x) any(.x %in% unlist(ws)))
  expect2(all(warnings_ok),  "`expr` is throwing an unexpecting warning")
  expect2(all(warnings_ok2), "`expr` is not throwing warning{?s} {.var {warning_class[!warnings_ok2]}}")
  errors_ok = map_lgl(es, \(.x) any(.x %in% error_class))
  errors_ok2 = map_lgl(error_class, \(.x) any(.x %in% unlist(es)))
  expect2(all(errors_ok), "`expr` is throwing an unexpecting error")
  expect2(all(errors_ok2), "`expr` is not throwing error{?s} {.var {error_class[!errors_ok2]}}")
  
  x
}

clean_cache()
message("Helper-init loaded")
