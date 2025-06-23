
#' Read all `.xpt` files in a directory
#' 
#' Read all `.xpt` files in a directory (unzipped TrialMaster archive). \cr
#' If `7zip` is installed, you should probably rather use [read_trialmaster()] instead. \cr
#' Formats (factors levels) can be applied from a `procformat.sas` SAS file, or from a format dictionary. See the "Format file" section below. Column labels are read directly from the `.xpt` files.
#'
#' @param path \[`character(1)`]\cr the path to the directory containing all `.xpt` files.
#' @param format_file \[`character(1)`]\cr the path to the file that should be used to apply formats. See section "Format file" below. Use `NULL` to not apply formats.
#' @param datetime_extraction \[`POSIXt(1)`]\cr the datetime of the data extraction. Default to the most common date of last modification in `path`.
#' @param ... unused
#' @param clean_names_fun `r lifecycle::badge("deprecated")` use [edc_clean_names()] instead.
#' @param subdirectories \[`logical(1)`]\cr whether to read subdirectories
#' @param verbose \[`numeric(1)`]\cr one of `c(0, 1, 2)`. The higher, the more information will be printed.
#' @param directory deprecated in favor for `path`
#' @param key_columns deprecated
#' 
#' @section Format file: 
#' `format_file` should contain the information about SAS formats. It can be either:
#'  - a `procformat.sas` file, containing the whole PROC FORMAT
#'  - or a data file (`.csv` or `.sas7bdat`) containing 3 columns: 
#'    -  `FMTNAME` the SAS format name (repeated)
#'    -  `START` the variable level
#'    -  `LABEL` the label associated to the level 
#'    
#'    You can get this datafile [from SAS](https://blogs.sas.com/content/sgf/2017/12/04/controlling-your-formats/) using `PROC FORMAT` with option `CNTLOUT`. 
#'    Otherwise, you can use `options(edc_var_format_name="xxx", edc_var_level="xxx", edc_var_label="xxx")` to specify different column names.
#' 
#'
#' @return a list containing one dataframe for each `.xpt` file in the folder, the extraction date (`datetime_extraction`), and a summary of all imported tables (`.lookup`).
#' @export
#' @family EDCimport reading functions
#' 
#' @importFrom fs dir_ls is_dir
#' @importFrom rlang check_dots_empty
#' @importFrom utils packageVersion
#' 
#' @examples
#' # Create a directory with multiple .xpt files.
#' path = paste0(tempdir(), "/read_all_xpt")
#' dir.create(paste0(path, "/subdir"), recursive=TRUE)
#' haven::write_xpt(attenu, paste0(path, "/attenu.xpt"))
#' haven::write_xpt(mtcars, paste0(path, "/mtcars.xpt"))
#' haven::write_xpt(mtcars, paste0(path, "/subdir/mtcars.xpt"))
#' haven::write_xpt(esoph, paste0(path, "/esoph.xpt"))
#' 
#' db = read_all_xpt(path, format_file=NULL, subdirectories=TRUE) %>% 
#'   set_project_name("My great project")
#' db
#' edc_lookup()
read_all_xpt = function(path, ..., 
                        format_file="procformat.sas", 
                        datetime_extraction="guess", 
                        subdirectories=FALSE,
                        verbose=getOption("edc_read_verbose", 1), 
                        clean_names_fun=NULL,
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
  rtn = dir_ls(path, regexp="\\.xpt$", recurse=subdirectories) %>% 
    .read_all(haven::read_xpt, clean_names_fun=clean_names_fun, path=path) %>%
    .clean_labels_utf8() %>% 
    .apply_sas_formats(format_file) %>% 
    .add_lookup_and_date(
      datetime_extraction=datetime_extraction,
      clean_names_fun=.get_clean_names_fun(clean_names_fun), 
      EDCimport_version=packageVersion("EDCimport")
    )
  
  .warn_bad_tables(rtn)
  .warn_bad_columns(rtn)
  .set_lookup(rtn$.lookup)
  
  class(rtn) = "edc_database"
  rtn
}


#' @rdname read_all_xpt
#' @export
#' @usage NULL
read_tm_all_xpt = read_all_xpt



#' @noRd
#' @keywords internal
#' @importFrom cli cli_warn
#' @importFrom purrr keep
.warn_bad_tables = function(rtn){
  errs = keep(rtn, is_edc_error)
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
#' @importFrom dplyr distinct select where
#' @importFrom purrr iwalk keep walk
#' @importFrom rlang is_error
.warn_bad_columns = function(rtn){
  rtn %>% 
    keep(is.data.frame) %>% 
    iwalk(function(data, name){
      if(is_error(data)) return(data)
      a = data %>% 
        select(where(is_edc_error)) %>% 
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
