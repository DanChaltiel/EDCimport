
#' Read the `.zip` archive of a TrialMaster export
#' 
#' Import the `.zip` archive of a TrialMaster trial export as a list of dataframes. The archive filename should be leaved untouched as it contains the project name and the date of extraction. \cr
#' Generate a `.rds` cache file for future reads. \cr
#' If `7zip` is not installed or available, use [read_tm_all_xpt()] instead.
#'
#' @param archive \[`character(1)`]\cr the path to the archive
#' @param use_cache \[`mixed(1)`: "write"]\cr controls the `.rds` cache. If `TRUE`, read the cache if any or extract the archive and create a cache. If `FALSE` extract the archive without creating a cache file. Can also be `"read"` or `"write"`.
#' @param pw \[`character(1)`]\cr The password if the archive is protected. To avoid writing passwords in plain text, it is probably better to use `options(trialmaster_pw="xxx")` instead though.
#' @param ... unused
#'
#' @inherit read_tm_all_xpt return
#' @inheritParams read_tm_all_xpt
#' 
#' @export
#' @importFrom cli cli_abort cli_inform cli_warn
#' @importFrom fs dir_create file_exists path path_dir path_temp
#' @importFrom glue glue
#' @importFrom rlang check_dots_empty
#' @importFrom stringr str_remove
#' @importFrom utils object.size
read_trialmaster = function(archive, ..., use_cache="write", 
                            clean_names_fun=NULL,
                            split_mixed=FALSE,
                            extend_lookup=TRUE,
                            pw=getOption("trialmaster_pw"), 
                            verbose=getOption("edc_read_verbose", 1),
                            key_columns="deprecated"){
  # checks ----
  check_dots_empty()
  if(!missing(use_cache) && !use_cache %in% list(TRUE, FALSE, "read", "write")){
    cli_abort("{.arg use_cache} should be one of {.val c(TRUE, FALSE, 'read', 'write')}.")
  }
  
  if(!file_exists(archive)){
    cli_abort("Archive {.val {archive}} does not exist.", 
             class="edc_tm_404")
  }
  
  # parse datetime ----
  extract_datetime = parse_file_datetime(archive)
  if(is.na(extract_datetime)){
    cli_warn(c("Extraction datetime could not be read from archive's name.", 
               x="Archive's name should contain the datetime as {.code SAS_XPORT_yyyy_mm_dd_hh_MM}", 
               i="Actual archive's name: {.val {archive}}"), 
             class="edc_tm_bad_name")
  }
  
  # read (+/-cache) ----
  directory = path_dir(archive)
  cache_file = glue("{directory}/trialmaster_export_{format_ymdhm(extract_datetime)}.rds")
  if(file_exists(cache_file) && (isTRUE(use_cache) || use_cache=="read")){
    if(verbose>0) cli_inform("Reading cache: {.file {cache_file}}", class="read_tm_cache")
    rtn = readRDS(cache_file)
    lookup_verbose = TRUE
    
    a = rtn$.lookup %>% attr("split_mixed") %>% 
      identical(split_mixed)
    b = rtn$.lookup %>% attr("clean_names_fun") %>% 
      identical(get_clean_names_fun(clean_names_fun), ignore.bytecode=TRUE)
    if(!a || !b){
      cli_abort(c("Cannot use cache with different parameters, set `use_cache=FALSE` to continue.", 
                  i="Same parameter {.arg split_mixed}: {a}", 
                  i="Same parameter {.arg clean_names_fun}: {b}"), 
                class="read_tm_cache_bad_param")
    }
  } else {
    if(verbose>0) cli_inform("Unzipping {.file {archive}}", class="read_tm_zip")
    temp_folder = basename(archive) %>% str_remove("\\.zip") %>% path_temp()
    dir_create(temp_folder, recurse=TRUE)
    msg = extract_7z(archive, temp_folder, pw)
    if(verbose>1) cli_inform(msg)
    if(is.na(extract_datetime)) extract_datetime = get_folder_datetime(temp_folder)
    format_file = path(temp_folder, "procformat.sas")
    if(!file_exists(format_file)){
      cli_warn("No file {.val procformat.sas} found in {.arg directory}. 
             Data formats cannot be applied.", 
             class="edc_tm_no_procformat_warning") 
      format_file = NULL
    }
    rtn = read_all_xpt(temp_folder, format_file=format_file, 
                       clean_names_fun=clean_names_fun, 
                       split_mixed=split_mixed,
                       extend_lookup=extend_lookup,
                       key_columns=key_columns,
                       datetime_extraction=extract_datetime, 
                       verbose=verbose)
    lookup_verbose = FALSE
    
    if(isTRUE(use_cache) || use_cache=="write"){
      if(verbose>0) cli_inform("Writing cache file {.file {cache_file}}", class="read_tm_zip")
      saveRDS(rtn, cache_file)
    }
  }
  
  # update lookup ----
  rtn$.lookup = rtn$.lookup %>% 
    structure(project_name = parse_file_projname(archive))
  set_lookup(rtn$.lookup, verbose=lookup_verbose)
  
  # out ----
  if(verbose>0){
    size = object.size(rtn) %>% format("auto")
    cli_inform(c(v="Database loaded: {length(rtn)} tables, {size}"))
  }
  
  rtn
}
