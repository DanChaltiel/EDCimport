
#' Read all `.sas7bdat` files in a directory
#' 
#' Read all `.sas7bdat` files in a directory. Formats (factors levels) can be applied from a `procformat.sas` SAS file, or from a format dictionary. See the "Format file" section below. Column labels are read directly from the `.sas7bdat` files.
#'
#' @param path \[`character(1)`]\cr the path to the directory containing all `.sas7bdat` files.
#' @inheritParams read_all_xpt
#' @inheritSection read_all_xpt Format file
#'
#' @return a list containing one dataframe for each `.xpt` file in the folder, the extraction date (`datetime_extraction`), and a summary of all imported tables (`.lookup`).
#' @export
#' @family EDCimport reading functions
#' 
#' @importFrom fs dir_exists dir_ls path_ext
#' @importFrom rlang check_dots_empty
#' @importFrom utils packageVersion
#' 
#' @examples
#' # Create a directory with multiple sas files.
#' path = paste0(tempdir(), "/read_all_sas")
#' dir.create(paste0(path, "/subdir"), recursive=TRUE)
#' haven::write_sas(attenu, paste0(path, "/attenu.sas7bdat"))
#' haven::write_sas(mtcars, paste0(path, "/mtcars.sas7bdat"))
#' haven::write_sas(mtcars, paste0(path, "/subdir/mtcars.sas7bdat"))
#' haven::write_sas(esoph, paste0(path, "/esoph.sas7bdat"))
#' 
#' db = read_all_sas(path, format_file=NULL, subdirectories=TRUE) %>% 
#'   set_project_name("My great project")
#' db
#' edc_lookup()
read_all_sas = function(path, ..., 
                        format_file="procformat.sas", 
                        subdirectories=FALSE,
                        datetime_extraction="guess", 
                        verbose=getOption("edc_read_verbose", 1), 
                        clean_names_fun=NULL){
  check_dots_empty()
  reset_manual_correction()
  assert(dir_exists(path), msg="Directory {.path {path}} does not exist.")
  
  if(identical(datetime_extraction, "guess") || is.null(datetime_extraction)){
    datetime_extraction = get_folder_datetime(path, verbose=verbose)
  }
  assert_class(datetime_extraction, c("POSIXt", "Date"))
  format_file = .locate_file(format_file, path)
  catalog_file = if(.is_catalog(format_file)) format_file else NULL
  
  rtn = dir_ls(path, regexp="\\.sas7bdat$", recurse=subdirectories) %>% 
    .read_all(haven::read_sas, clean_names_fun=clean_names_fun, 
              catalog_file=catalog_file, path=path) %>%
    .clean_labels_utf8() %>% 
    .add_lookup_and_date(
      datetime_extraction=datetime_extraction,
      clean_names_fun=.get_clean_names_fun(clean_names_fun), 
      EDCimport_version=packageVersion("EDCimport")
    ) %>% 
    .apply_sas_formats(format_file)
  
  .warn_bad_tables(rtn)
  .warn_bad_columns(rtn)
  .set_lookup(rtn$.lookup)
  
  class(rtn) = "edc_database"
  rtn
}

.is_catalog = function(x) !is.null(x) && path_ext(x)=="sas7bcat"
.is_not_catalog = function(x) !is.null(x) && path_ext(x) %in% c("sas", "sas7bdat", "csv")
