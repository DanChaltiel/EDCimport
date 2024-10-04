
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
#' @importFrom dplyr across distinct mutate na_if select
#' @importFrom fs file_exists path
#' @importFrom haven read_xpt
#' @importFrom purrr imap iwalk keep keep_at map_lgl pwalk walk
#' @importFrom rlang check_dots_empty is_error set_names
#' @importFrom stringr str_remove
#' @importFrom tibble as_tibble tibble
#' @importFrom tidyselect where
#' @importFrom utils packageVersion
read_all_xpt = function(directory, ..., format_file="procformat.sas", 
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
    procformat = path(directory, format_file)
    if(!file_exists(procformat)) procformat = format_file
    if(!file_exists(procformat)) {
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
  .lookup = build_lookup(rtn)
  bad_utf8 = check_invalid_utf8(.lookup, warn=verbose>1)
  bad_utf8 %>%
    pwalk(function(...) {
      x = tibble(...)
      attr(rtn[[x$dataset]][[x$names]], "label") <<- x$valid_labels
    })
  .lookup = build_lookup(rtn)
  .lookup = .lookup %>% 
    structure(clean_names_fun=clean_names_fun, 
              split_mixed=split_mixed,
              datetime_extraction=datetime_extraction,
              EDCimport_version=packageVersion("EDCimport"))
  
  # split mixed datasets (with short and long format) ----
  patient_id = get_subjid_cols(lookup=.lookup)
  id_found = map_lgl(rtn, ~any(tolower(patient_id) %in% tolower(names(.x))))
  
  if(!isFALSE(split_mixed) & !isTRUE(split_mixed) & !is.character(split_mixed)){
    cli_abort("{.arg split_mixed} should be either FALSE, TRUE, 
                or a character vector of column names.")
  }
  if(!isFALSE(split_mixed)){
    split_mixed_names = split_mixed
    if(isTRUE(split_mixed)) split_mixed_names = names(rtn)
    if(any(id_found)){
      mixed = rtn %>% 
        keep_at(split_mixed_names) %>% 
        split_mixed_datasets(id=patient_id, verbose=FALSE)
      if(!isTRUE(split_mixed) && length(mixed)==0){
        cli_warn("Dataset{?s} {.val {split_mixed_names}} are not mixed 
                 (either short or long) and cannot be split", 
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
    iwalk(function(data, name){
      if(is_error(data)) return(data)
      a = data %>% 
        select(where(~inherits(.x, "edc_error_col"))) %>% 
        distinct()
      if(nrow(a)>1) cli_warn("Error 489 ({name}), please contact the developer.")
      if(nrow(a)>0){
        unlist(a) %>% unique() %>% 
          walk(~{
            columns = names(a)[a==.x]
            cli_warn(c("Error when reading table {.val {name}} on {qty(columns)} column{?s} {.val {columns}}", 
                       x=.x),
                     class="edc_read_column_error_warning")
            
          })
      }
    })
  
  # lookup ----
  if(isTRUE(extend_lookup)){
    .lookup = extend_lookup(.lookup, datasets=rtn)
  }
  set_lookup(.lookup)
  
  # out ----
  rtn$date_extraction = format_ymd(datetime_extraction)
  rtn$datetime_extraction = datetime_extraction
  rtn$.lookup = .lookup
  
  class(rtn) = "tm_database"
  rtn
}


#' @rdname read_all_xpt
#' @export
#' @usage NULL
read_tm_all_xpt = read_all_xpt


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


#' Change a `try-error` column to a simpler character column of class "edc_error_col"
#' @noRd
#' @keywords internal
#' @importFrom dplyr across mutate
#' @importFrom tidyselect where
flatten_error_columns = function(df){
  df %>% 
    mutate(across(where(~inherits(.x, "try-error")), ~{
      attr(.x, "condition")$message %>% `class<-`("edc_error_col")
    }))
}