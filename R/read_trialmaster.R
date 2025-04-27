
#' Read the `.zip` archive of a TrialMaster export
#' 
#' Import the `.zip` archive of a TrialMaster trial export as a list of dataframes. The archive filename should be leaved untouched as it contains the project name and the date of extraction. \cr
#' Generate a `.rds` cache file for future reads. \cr
#' If `7zip` is not installed or available, use [read_all_xpt()] instead.\cr\cr
#' The TM export should be of type `SAS Xport`, with the checkbox 
#' "Include Codelists" ticked.
#' 
#'
#' @param archive \[`character(1)`]\cr the path to the archive
#' @param use_cache \[`mixed(1)`: "write"]\cr controls the `.rds` cache. If `TRUE`, read the cache if any or extract the archive and create a cache. If `FALSE` extract the archive without creating a cache file. Can also be `"read"` or `"write"`.
#' @param pw \[`character(1)`]\cr The password if the archive is protected. To avoid writing passwords in plain text, it is probably better to use `options(trialmaster_pw="xxx")` instead though.
#' @param ... unused
#'
#' @inherit read_all_xpt return
#' @inheritParams read_all_xpt
#' @family EDCimport reading functions
#' 
#' @export
#' @importFrom cli cli_inform
#' @importFrom fs file_exists path_dir
#' @importFrom rlang check_dots_empty
#' @importFrom utils object.size packageVersion
read_trialmaster = function(archive, ..., use_cache="write", 
                            clean_names_fun=NULL,
                            subdirectories=FALSE,
                            pw=getOption("trialmaster_pw"), 
                            verbose=getOption("edc_read_verbose", 1),
                            key_columns="deprecated"){
  
  check_dots_empty()
  .check_use_cache(use_cache)
  assert_file_exists(archive, msg="Archive {.val {archive}} does not exist.", 
                     class="edc_tm_404")
  
  extract_datetime = parse_file_datetime(archive, warn=TRUE)
  directory = path_dir(archive)
  cache_file = .get_tm_cache(directory, extract_datetime)
  read_from_cache = file_exists(cache_file) && (isTRUE(use_cache) || use_cache=="read")
  cache_outdated = FALSE
  
  if(read_from_cache){
    rtn = .read_tm_cache(cache_file, clean_names_fun, verbose) %>% 
      structure(source="cache")
    cache_version = attr(rtn$.lookup, "EDCimport_version")
    cache_outdated = packageVersion("EDCimport") > cache_version
    if(cache_outdated){
      cli_inform(c(i="Updating cache with latest {.pkg EDCimport} version
                      (v{cache_version} to v{packageVersion('EDCimport')})"))
    }
  }
  
  if(!read_from_cache || cache_outdated){
    rtn = .read_tm_zip(archive=archive, pw=pw, extract_datetime=extract_datetime,
                       clean_names_fun=clean_names_fun, key_columns=key_columns, 
                       subdirectories=subdirectories, use_cache=use_cache, 
                       cache_file=cache_file, verbose=verbose) %>% 
      structure(source="zip")
  }

  if(verbose>0){
    size = object.size(rtn) %>% format("auto")
    l = rtn %>% keep(is.data.frame) %>% discard(is_lookup) %>% length()
    cli_inform(c(v="Database loaded: {l} tables, {size}"))
  }
  
  rtn
}


# Utils ---------------------------------------------------------------------------------------

#' @noRd
#' @keywords internal
#' @importFrom cli cli_abort
.check_use_cache <- function(use_cache) {
  if(!missing(use_cache) && !use_cache %in% list(TRUE, FALSE, "read", "write")){
    cli_abort("{.arg use_cache} should be one of {.val c(TRUE, FALSE, 'read', 'write')}.")
  }
}

#' @noRd
#' @keywords internal
#' @importFrom glue glue
.get_tm_cache = function(directory, extract_datetime){
  glue("{directory}/trialmaster_export_{format_ymdhm(extract_datetime)}.rds")
}

#' @noRd
#' @keywords internal
#' @importFrom cli cli_abort cli_inform
.read_tm_cache <- function(cache_file, clean_names_fun, verbose) {
  if(verbose>0) cli_inform("Reading cache: {.file {cache_file}}", class="read_tm_cache")
  rtn = readRDS(cache_file)
  lookup_verbose = TRUE
  
  #TODO utiliser .lookup pour faire la comparaison
  a = rtn$.lookup %>% attr("split_mixed") %>% 
    identical(FALSE)
  a=TRUE
  b = rtn$.lookup %>% attr("clean_names_fun") %>% 
    identical(.get_clean_names_fun(clean_names_fun), ignore.bytecode=TRUE)
  if(!a || !b){
    cli_abort(c("Cannot use cache with different parameters, set `use_cache=FALSE` to continue.", 
                i="Same parameter {.arg split_mixed}: {a}", 
                i="Same parameter {.arg clean_names_fun}: {b}"), 
              class="read_tm_cache_bad_param")
  }
  
  .set_lookup(rtn$.lookup)
  
  rtn
}

#' @noRd
#' @keywords internal
#' @importFrom cli cli_inform cli_warn
#' @importFrom fs dir_create file_exists path path_temp
#' @importFrom stringr str_remove
.read_tm_zip <- function(archive, pw, extract_datetime, clean_names_fun, 
                         key_columns, subdirectories, use_cache, cache_file, 
                         verbose) {
  
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
                     key_columns=key_columns,
                     subdirectories=subdirectories,
                     datetime_extraction=extract_datetime, 
                     verbose=verbose)
  lookup_verbose = FALSE
  
  rtn$.lookup = rtn$.lookup %>% 
    structure(project_name = parse_file_projname(archive))
  .update_lookup(new=rtn$.lookup)
  
  if(isTRUE(use_cache) || use_cache=="write"){
    if(verbose>0) cli_inform("Writing cache file {.file {cache_file}}", class="edc_create_cache")
    saveRDS(rtn, cache_file)
  }
  
  class(rtn) = "edc_database"
  rtn
}
