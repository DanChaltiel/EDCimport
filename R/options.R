


#' Set global options for `EDCimport`
#'
#' Use this function to manage your `EDCimport` parameters globally while taking advantage of autocompletion. \cr
#' Use [edc_peek_options()] to see which option is currently set and [edc_reset_options()] to set all options back to default.
#'
#' @param ... unused
#' @param trialmaster_pw the password of the trialmaster zip archive. For instance, you can use `edc_options(trialmaster_pw="my_pwd")` in the console once per session, so that you don't have to write the password in clear in your R code
#' @param path_7zip the path to the 7zip executable. Default to `"C:/Program Files/7-Zip/"`.
#' @param edc_lookup **(Internal)** a reference to the lookup table (usually `.lookup`). Should usually not be changed manually.
#' @param edc_subjid_ref **used in [check_subjid]** the vector of the reference subject IDs. You should usually write `edc_options(edc_subjid_ref=enrolres$subjid)`.
#' @param edc_plotly **used in [edc_swimmerplot]** whether to use plotly to visualize the plot.
#' @param edc_cols_id,edc_cols_crfname **used in [get_key_cols]** the name of the columns holding the subject id (default to `c("ptno", "subjid")`) and the CRF form name (default to `c("crfname")`). It is case-insensitive.
#' @param edc_read_verbose,edc_correction_verbose,edc_get_key_cols_verbose the verbosity of the output of functions [read_trialmaster] and [read_tm_all_xpt], [manual_correction], and [get_key_cols]. For example, set `edc_options(edc_read_verbose=0)` to silence the first 2.
#' @param edc_lookup_overwrite_warn default to TRUE. Whether there should be warning when overwriting `.lookup` (like when reading 2 databases successively)
#' @param .local  if TRUE, the effect will only apply to the local frame (internally using `rlang::local_options()`)
#'
#' @return Nothing, called for its side effects
#' @importFrom rlang caller_env local_options
#' @export
edc_options = function(
    ...,
    trialmaster_pw,
    path_7zip,
    edc_lookup,
    edc_subjid_ref,
    edc_plotly,
    edc_cols_id, edc_cols_crfname,
    edc_read_verbose, edc_correction_verbose, edc_get_key_cols_verbose,
    edc_lookup_overwrite_warn,
    .local=FALSE){
  rlang::check_dots_empty()
  argg = as.list(match.call()) %>% discard(is.name)
  
  
  if(.local){
    argg = c(argg, .frame = caller_env())
    do.call(local_options, argg)
  }
  else {
    do.call(options, argg)
  }
  
  invisible()
}


#' See which `EDCimport` option is currently set.
#'
#' @param keep_null set to TRUE to get a list
#'
#' @return A named list of EDCimport options
#' @importFrom purrr discard
#' @importFrom rlang peek_options
#' @export
edc_peek_options = function(keep_null=FALSE){
  x = formals(edc_options)
  rtn = peek_options(names(x))
  if(!isTRUE(keep_null)) rtn = discard(rtn, is.null)
  rtn
}

#' Reset all `EDCimport` options.
#'
#' @param except options that are not reset by default
#' @param quiet set to `TRUE` to remove the message.
#'
#' @return Nothing, called for its side effects
#' @importFrom cli cli_inform
#' @importFrom purrr map
#' @importFrom rlang set_names
#' @export
edc_reset_options = function(except=c("edc_lookup", "trialmaster_pw", "path_7zip"), quiet=FALSE){
  args_ok = names(formals(edc_options)) %>% .[!. %in% c("...", "reset", ".local")]
  args_ok = setdiff(args_ok, except)
  if(length(except)==0) except="nothing"
  argg = args_ok %>% set_names() %>% map(~NULL)
  options(argg)
  if(isFALSE(quiet)) cli_inform("All EDCimport options were set back to default (except {.val {except}}).") #nocov
  return(invisible())
}


#' @importFrom stringr str_extract str_extract_all str_remove_all str_subset
#' @noRd
#' @keywords internal
missing_options_helper = function(){
  options_found = dir("R/", full.names=T) %>% 
    map(readLines) %>% 
    map(~str_subset(.x, "getOption")) %>% 
    keep(~length(.x)>0) %>% 
    unlist() %>% 
    str_extract_all("getOption\\((.*?)\\)") %>% unlist() %>% 
    str_extract("getOption\\((.*?)(,(.*))?\\)", group=1) %>% 
    unique() %>% 
    str_subset('"') %>% 
    str_remove_all('"')
  
  options_proposed = names(formals(edc_options))
  
  options_found %>% setdiff(options_proposed)
}
