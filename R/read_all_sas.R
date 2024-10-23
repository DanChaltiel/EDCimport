
#' Read all `.sas7bdat` files in a directory
#' 
#' Read all `.sas7bdat` files in a directory. Formats can be applied from a `procformat.sas` SAS file, from a .
#'
#' @param path \[`character(1)`]\cr the path to the directory containing all `.sas7bdat` files.
#' @inheritParams read_all_xpt
#' 
#' @section Format file: 
#' `format_file` should contain the information about SAS formats. It can be either 
#'  - a `procformat.sas` file, containing the whole PROC FORMAT
#'  - a catalog file (`.sas7bcat`)
#'  - or a data file (`.csv` or `.sas7bdat`) containing 3 columns: the SAS format name (repeated), 
#'  each level, and its associated label. Use `options(edc_var_format_name="xxx", edc_var_level="xxx", edc_var_label="xxx")` to specify the names of the columns.
#'
#'
#' @return a list containing one dataframe for each `.xpt` file in the folder, the extraction date (`datetime_extraction`), and a summary of all imported tables (`.lookup`).
#' @export
read_all_sas = function(path, ..., 
                        format_file="procformat.sas", 
                        clean_names_fun=NULL, 
                        datetime_extraction="guess", 
                        verbose=getOption("edc_read_verbose", 1)){
  check_dots_empty()
  reset_manual_correction()
  assert(is_dir(path))
  
  if(identical(datetime_extraction, "guess") || is.null(datetime_extraction)){
    datetime_extraction = get_folder_datetime(path, verbose=verbose)
  }
  assert_class(datetime_extraction, c("POSIXt", "Date"))
  
  format_file = .locate_file(format_file, path)
  catalog_file = if(path_ext(format_file)=="sas7bcat") format_file else NULL
  
  rtn = dir_ls(path, regexp="\\.sas7bdat$") %>% 
    .read_all(haven::read_sas, clean_names_fun=clean_names_fun, 
              catalog_file=catalog_file) %>%
    .clean_labels_utf8() %>% 
    .add_lookup_and_date(
      datetime_extraction=datetime_extraction,
      clean_names_fun=.get_clean_names_fun(clean_names_fun), 
      EDCimport_version=packageVersion("EDCimport")
    )
  
  if(path_ext(format_file) %in% c("sas", "sas7bdat", "csv")){
    rtn = rtn %>% 
      .apply_sas_formats(format_file)
  }
  
  .warn_bad_tables(rtn)
  .warn_bad_columns(rtn)
  .set_lookup(rtn$.lookup)
  
  class(rtn) = "tm_database"
  rtn
}