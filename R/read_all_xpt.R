
#' Read all `.xpt` files in a directory
#' 
#' Read all `.xpt` files in a directory (unzipped TrialMaster archive). \cr
#' If `7zip` is installed, you should probably rather use [read_trialmaster()] instead. \cr
#' If a `procformat.sas` file exists in the directory, formats will be applied.
#'
#' @param path \[`character(1)`]\cr the path to the directory containing all `.xpt` files.
#' @param format_file \[`character(1)`]\cr the path to the file that should be used to apply formats. See details. Use `NULL` to not apply formats.
#' @param datetime_extraction \[`POSIXt(1)`]\cr the datetime of the data extraction. Default to the most common date of last modification in `directory`.
#' @param ... unused
#' @param split_mixed \[`logical(1): FALSE`]\cr whether to split mixed datasets. See [split_mixed_datasets]. 
#' @param extend_lookup \[`character(1): FALSE`]\cr whether to enrich the lookup table. See [extend_lookup].
#' @param clean_names_fun \[`function`]\cr a function to clean column names, e.g. [tolower], [janitor::clean_names()],...
#' @param verbose \[`logical(1)`]\cr one of `c(0, 1, 2)`. The higher, the more information will be printed.
#' @param directory deprecated
#' @param key_columns deprecated
#' 
#' @section Format file: 
#' `format_file` should contain the information about SAS formats. It can be either 
#'  - a `procformat.sas` file, containing the whole PROC FORMAT
#'  - or a data file (.csv or .sas7bdat) containing 3 columns: the SAS format name (repeated), 
#'  each level, and its associated label. Use `options(edc_var_format_name="xxx", edc_var_level="xxx", edc_var_label="xxx")` to specify the names of the columns.
#'
#' @return a list containing one dataframe for each `.xpt` file in the folder, the extraction date (`datetime_extraction`), and a summary of all imported tables (`.lookup`).
#' @export
#' @importFrom fs dir_ls is_dir
#' @importFrom rlang check_dots_empty
#' @importFrom utils packageVersion
read_all_xpt = function(path, ..., 
                        format_file="procformat.sas", 
                        clean_names_fun=NULL, 
                        split_mixed=FALSE,
                        extend_lookup=TRUE,
                        datetime_extraction="guess", 
                        verbose=getOption("edc_read_verbose", 1),
                        directory="deprecated",
                        key_columns="deprecated"){
  check_dots_empty()
  reset_manual_correction()
  if(missing(path)) path = directory
  assert(is_dir(path))
  
  if(identical(datetime_extraction, "guess") || is.null(datetime_extraction)){
    datetime_extraction = get_folder_datetime(path, verbose=verbose)
  }
  assert_class(datetime_extraction, c("POSIXt", "Date"))
  format_file = .locate_file(format_file, path)
  
  rtn = dir_ls(path, regexp="\\.xpt$") %>% 
    .read_all(haven::read_xpt, clean_names_fun=clean_names_fun) %>%
    .clean_labels_utf8() %>% 
    .apply_sas_formats(format_file) %>% 
    .add_lookup_and_date(
      datetime_extraction=datetime_extraction,
      extend_lookup=extend_lookup,
      clean_names_fun=.get_clean_names_fun(clean_names_fun), 
      split_mixed=split_mixed,
      EDCimport_version=packageVersion("EDCimport")
    ) %>% 
    .apply_split_mixed(split_mixed)
  
  .warn_bad_tables(rtn)
  .warn_bad_columns(rtn)
  .set_lookup(rtn$.lookup)
  
  class(rtn) = "tm_database"
  rtn
}


#' @rdname read_all_xpt
#' @export
#' @usage NULL
read_tm_all_xpt = read_all_xpt

#' @noRd
#' @keywords internal
#' @importFrom cli cli_abort cli_warn
#' @importFrom dplyr setdiff
#' @importFrom purrr keep_at map_lgl
.apply_split_mixed = function(rtn, split_mixed){
  .lookup = rtn$.lookup
  patient_id = get_subjid_cols(lookup=.lookup)
  id_found = map_lgl(rtn, ~any(tolower(patient_id) %in% tolower(names(.x))))
  
  if(!isFALSE(split_mixed) & !isTRUE(split_mixed) & !is.character(split_mixed)){
    cli_abort("{.arg split_mixed} should be either FALSE, TRUE, 
                or a character vector of column names.")
  }
  if(!isFALSE(split_mixed)){
    split_mixed_names = split_mixed
    if(isTRUE(split_mixed)) split_mixed_names = names(rtn) %>% setdiff(".lookup")
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
  rtn
}


#' @noRd
#' @keywords internal
#' @importFrom cli cli_warn
#' @importFrom purrr keep
.warn_bad_tables = function(rtn){
  errs = keep(rtn, is_error)
  if(length(errs)>0){
    cli_warn(c("SAS dataset{?s} {.val {names(errs)}} could not be read from 
               the archive using {.fun haven::read_xpt}.",
               i="You can print the object{?s} to see the error message (e.g. 
               run {.run print({names(errs[1])})})."), 
             class="edc_tm_problem_warning")
  }
}


#' @noRd
#' @keywords internal
#' @importFrom cli cli_warn
#' @importFrom dplyr distinct select
#' @importFrom purrr iwalk keep walk
#' @importFrom rlang is_error
#' @importFrom tidyselect where
.warn_bad_columns = function(rtn){
  rtn %>% 
    keep(is.data.frame) %>% 
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
}
