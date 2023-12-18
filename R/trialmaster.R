
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
  
  if(!file.exists(archive)){
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
  directory = dirname(archive)
  cache_file = glue("{directory}/trialmaster_export_{format_ymdhm(extract_datetime)}.rds")
  if(file.exists(cache_file) && (isTRUE(use_cache) || use_cache=="read")){
    if(verbose>0) cli_inform("Reading cache: {.file {cache_file}}", class="read_tm_cache")
    rtn = readRDS(cache_file)
    
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
    set_lookup(rtn$.lookup)
  } else {
    if(verbose>0) cli_inform("Unzipping {.file {archive}}", class="read_tm_zip")
    temp_folder = file.path2(tempdir(), str_remove(basename(archive), "\\.zip"))
    dir.create(temp_folder, recursive=TRUE, showWarnings=FALSE)
    msg = extract_7z(archive, temp_folder, pw)
    if(verbose>1) cli_inform(msg)
    if(is.na(extract_datetime)) extract_datetime = get_folder_datetime(temp_folder)
    format_file = file.path2(temp_folder, "procformat.sas")
    if(!file.exists(format_file)){
      cli_warn("No file {.val procformat.sas} found in {.arg directory}. 
             Data formats cannot be applied.", 
             class="edc_tm_no_procformat_warning") 
      format_file = NULL
    }
    rtn = read_tm_all_xpt(temp_folder, format_file=format_file, 
                          clean_names_fun=clean_names_fun, 
                          split_mixed=split_mixed,
                          extend_lookup=extend_lookup,
                          key_columns=key_columns, 
                          datetime_extraction=extract_datetime, 
                          verbose=verbose)
    
    if(isTRUE(use_cache) || use_cache=="write"){
      if(verbose>0) cli_inform("Writing cache file {.file {cache_file}}", class="read_tm_zip")
      saveRDS(rtn, cache_file)
    }
  }
  
  # out ----
  if(verbose>0){
    size = object.size(rtn) %>% format("auto")
    cli_inform(c(v="Database loaded: {length(rtn)} tables, {size}"))
  }
  
  rtn
}

#' Read all `.xpt` files in a directory
#' 
#' Read all `.xpt` files in a directory (unzipped TrialMaster archive). \cr
#' If `7zip` is installed, you should probably rather use [read_trialmaster()] instead. \cr
#' If a `procformat.sas` file exists in the directory, formats will be applied.
#'
#' @param directory \[`character(1)`]\cr the path to the unzipped archive using SAS_XPORT format. Will read the extraction date from the directory name.
#' @param format_file \[`character(1)`]\cr the path to the `procformat.sas` file that should be used to apply formats. Use `NULL` to not apply formats.
#' @param datetime_extraction \[`POSIXt(1)`]\cr the datetime of the data extraction. Default to the most common date of last modification in `directory`.
#' @param ... unused
#' @param split_mixed \[`logical(1): FALSE`]\cr whether to split mixed datasets. See [split_mixed_datasets]. 
#' @param extend_lookup \[`character(1): FALSE`]\cr whether to enrich the lookup table. See [extend_lookup].
#' @param clean_names_fun \[`function`]\cr a function to clean column names, e.g. [tolower], [janitor::clean_names()],...
#' @param verbose \[`logical(1)`]\cr one of `c(0, 1, 2)`. The higher, the more information will be printed.
#' @param key_columns deprecated
#'
#' @return a list containing one dataframe for each `.xpt` file in the folder, the extraction date (`datetime_extraction`), and a summary of all imported tables (`.lookup`). If not set yet, option `edc_lookup` is automatically set to `.lookup`.
#' @export
#' @importFrom cli cli_abort cli_warn
#' @importFrom dplyr across mutate na_if
#' @importFrom forcats as_factor
#' @importFrom haven read_xpt
#' @importFrom purrr imap keep keep_at map_lgl
#' @importFrom rlang check_dots_empty is_error set_names
#' @importFrom stringr str_remove
#' @importFrom tibble as_tibble
#' @importFrom tidyselect where
read_tm_all_xpt = function(directory, ..., format_file="procformat.sas", 
                           clean_names_fun=NULL, 
                           split_mixed=FALSE,
                           extend_lookup=TRUE,
                           datetime_extraction=NULL, 
                           verbose=getOption("edc_read_verbose", 1),
                           key_columns="deprecated"){
  check_dots_empty()
  reset_manual_correction()
  clean_names_fun = get_clean_names_fun(clean_names_fun)
  
  datasets = dir(directory, pattern = "\\.xpt$", full.names=TRUE)
  datasets_names = basename(datasets) %>% str_remove("\\.xpt")
  if(is.null(datetime_extraction)) datetime_extraction=get_folder_datetime(directory)
  
  # reading ----
  rtn = datasets %>% 
    set_names(tolower(datasets_names)) %>% 
    imap(~tryCatch(read_xpt(.x), error=function(e) e))
  
  # applying formats ----
  if(!is.null(format_file)){
    procformat = file.path2(directory, format_file)
    if(!file.exists(procformat)) procformat = format_file
    if(!file.exists(procformat)) {
      cli_abort("File {.file {format_file}} does not exist. Set {.arg format_file=NULL} to override.", 
                class="edc_tm_no_procformat_error")
    }
    sas_formats = read_sas_format(procformat)
    rtn = rtn %>% 
      imap(~{
        if(is_error(.x)) return(.x)
        .x %>% 
          as_tibble() %>% 
          mutate(across(where(~is.character(.x)), ~try(na_if(.x, y=""), silent=TRUE))) %>% 
          flatten_error_columns() %>% 
          apply_sas_formats(sas_formats) %>%
          clean_names_fun() %>% 
          haven::as_factor()
      })
  }
  
  # strip out non-UTF8 (and warn if needed) ----
  .lookup = get_lookup(rtn)
  bad_utf8 = check_invalid_utf8(.lookup, warn=verbose>1)
  bad_utf8 %>%
    purrr::pwalk(function(...) {
      x = tibble(...)
      attr(rtn[[x$dataset]][[x$names]], "label") <<- x$valid_labels
    })
  .lookup = get_lookup(rtn)
  .lookup = .lookup %>% 
    structure(clean_names_fun=clean_names_fun, 
              split_mixed=split_mixed)
  
  # split mixed datasets (with short and long format) ----
  key_columns = get_key_cols(.lookup)
  patient_id = key_columns$patient_id
  id_found = map_lgl(rtn, ~any(tolower(patient_id) %in% tolower(names(.x))))
  if(!isFALSE(split_mixed)){
    split_mixed_names = split_mixed
    if(isTRUE(split_mixed)) split_mixed_names = names(rtn)
    if(!is.character(split_mixed)){
      cli_abort("{.arg split_mixed} should be either FALSE, TRUE, or a character vector of column names.")
    }
    if(any(id_found)){
      mixed = rtn %>% 
        keep_at(split_mixed_names) %>% 
        split_mixed_datasets(id=patient_id, verbose=FALSE)
      if(!isTRUE(split_mixed) && length(mixed)==0){
        cli_warn("Dataset{?s} {.val {split_mixed_names}} are not mixed (either short or long) and cannot be split", 
                 class="edc_read_cannot_split_mixed_warn")
      }
      rtn = c(rtn, mixed)
    } else {
      cli_warn("Patient ID column {.val {patient_id}} was not found in any dataset")
    }
  }
  
  # faulty tables ----
  errs = keep(rtn, is_error)
  if(length(errs)>0){
    cli_warn(c("SAS dataset{?s} {.val {names(errs)}} could not be read from 
               the archive using {.fun haven::read_xpt}.",
               i="You can print the object{?s} to see the error message (e.g. 
               run {.run print({names(errs[1])})})."), 
             class="edc_tm_problem_warning")
  }
  
  # faulty columns ----
  rtn %>% 
    purrr::iwalk(function(data, name){
      if(is_error(data)) return(data)
      a = data %>% 
        select(where(~inherits(.x, "edc_error_col"))) %>% 
        dplyr::distinct()
      if(nrow(a)>1) cli_warn("Error 489 ({name}), please contact the developer.")
      if(nrow(a)>0){
        unlist(a) %>% unique() %>% 
          purrr::walk(~{
            columns = names(a)[a==.x]
            cli_warn(c("Error when reading table {.val {name}} on {qty(columns)} column{?s} {.val {columns}}", 
                       x=.x),
                     class="edc_read_column_error_warning")
            
          })
      }
    })
  
  # out ----
  rtn$date_extraction = format_ymd(datetime_extraction)
  rtn$datetime_extraction = datetime_extraction
  rtn$.lookup = .lookup
    
  if(isTRUE(extend_lookup)){
    rtn$.lookup = extend_lookup(rtn$.lookup, datasets=rtn)
  }
  
  set_lookup(rtn$.lookup)
  
  rtn
}


# Utils ---------------------------------------------------------------------------------------

#' @noRd
#' @keywords internal
#' @importFrom cli cli_abort
#' @importFrom rlang as_function is_formula
get_clean_names_fun = function(f){
  if(is.null(f)) return(identity)
  if(is_formula(f)) f = as_function(f)
  if(!is.function(f)) cli_abort("{.arg {caller_arg(f)}} should be a function or a lambda-function, not a {.cls {class(f)}}.")
  f
}
