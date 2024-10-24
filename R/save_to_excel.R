

#' Save the database as an Excel file
#'
#' Because RStudio is not very good at showing data, it can be more convenient to browse the 
#' database using MS Excel. This function turns the whole TM export (or any named list of datasets)
#' into an Excel workbook, with one tab for each dataset.\cr
#' Use `edc_db_to_excel()` to create the file and `edc_browse_excel()` to open it.
#'
#' @param filename the path to the Excel output file. Default to a temporary file. Use the special value `TRUE` to save in "data/database_\{date_extraction\}.xlsx".
#' @param datasets a named list of dataframes. Default to the TM export.
#' @param overwrite whether to overwrite any existing file. Default to `FALSE`.
#' @param open whether to open the Excel file afterward. Default to `FALSE`.
#' @param ... unused 
#'
#' @return nothing
#' @export
#' @examples
#' \dontrun{
#'   tm = edc_example()
#'   load_list(tm)  
#'   edc_db_to_excel() #default arguments are usually OK
#'   edc_db_to_excel(filename=TRUE)
#' }
#' @importFrom cli cli_abort cli_inform
#' @importFrom dplyr across arrange mutate
#' @importFrom fs dir_create file_exists file_size
#' @importFrom glue glue
#' @importFrom purrr iwalk map_lgl
#' @importFrom rlang check_dots_empty check_installed is_named sym
#' @importFrom stringr str_ends str_sub
#' @importFrom tidyselect where
#' @importFrom utils browseURL
edc_db_to_excel = function(filename=tempfile(fileext=".xlsx"),
                           ..., 
                           datasets=get_datasets(),
                           overwrite=FALSE, 
                           open=FALSE){
  check_installed("openxlsx", "for `edc_save_to_excel()` to work.")
  check_dots_empty()
  assert(is_named(datasets))
  assert(is.list(datasets))
  assert(length(datasets)>0)
  assert(all(lengths(datasets)>0))
  assert(all(map_lgl(datasets, is.data.frame)))
    
  if(isTRUE(filename) && exists("date_extraction")){
    filename = glue("data/database_{str_replace_all(date_extraction,'/','-')}.xlsx")
  }
  if(file_exists(filename) && !overwrite){
    cli_inform("Browse database at {.path {filename}}.")
    edcimport_env$excel_db_path = filename
    if(open) browseURL(filename)
    return(invisible(filename))
  }
  assert(str_ends(filename, ".xlsx"))
  filename = clean_filename(filename)
  dir_create(dirname(filename))
  datasets = datasets[order(names(datasets))]
  
  wb = openxlsx::createWorkbook()
  
  datasets %>% iwalk(~{
    if(get_subjid_cols() %in% names(.x)) .x = arrange(.x, !!sym(get_subjid_cols()))
    label_row = get_label(.x) %>% as.data.frame()
    .x = .x %>% #in TM, the last char is sometimes invalid UTF-8
      mutate(across(where(~any(is_invalid_utf8(.x))), ~str_sub(.x, end=-2)))

    openxlsx::addWorksheet(wb, sheetName=.y)
    openxlsx::writeData(wb, sheet=.y, x=label_row, startCol=2, startRow=2, colNames=FALSE)
    openxlsx::writeDataTable(wb, sheet=.y, x=.x, startCol=2, startRow=3)
    openxlsx::setColWidths(wb, sheet=.y, cols=2:ncol(.x), widths="auto")
    openxlsx::setColWidths(wb, sheet=.y, cols=1, widths=2.14)
    
  }, .progress=TRUE)
  rslt = openxlsx::saveWorkbook(wb, filename, overwrite=overwrite, returnValue=TRUE)
  if(!rslt){
    cli_abort("Could not create the Excel workbook.")
    return(FALSE)
  }
  
  edcimport_env$excel_db_path = filename
  
  size = file_size(filename)
  cli_inform("Excel file {.path {filename}} has been created ({size}).")
  if(open){
    browseURL(filename)
  }
  
  invisible(filename)
}

#' @rdname edc_db_to_excel
#' @export
#' @importFrom utils browseURL
edc_browse_excel = function(){
  browseURL(edcimport_env$excel_db_path)
}

#' @noRd
#' @keywords internal
#' @importFrom fs path path_sanitize
clean_filename = function(filename){
  a = dirname(filename)
  b = basename(filename) %>% path_sanitize()
  path(a, b)
}
