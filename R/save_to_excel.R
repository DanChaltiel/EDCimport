

#' Save the database as an Excel file
#'
#' Because RStudio is not very good at showing data, it can be more convenient to browse the 
#' database using MS Excel. This function turns the whole TM export (or any named list of datasets)
#' into an Excel workbook, with one tab for each dataset.
#'
#' @param datasets a named list of dataframes. Default to the TM export.
#' @param filename the path to the Excel output file. Default to a dated file in the "data" folder.
#' @param overwrite whether to overwrite any existing file. Default to `TRUE`.
#' @param open whether to open the Excel file afterward. Default to `TRUE`.
#' @param ... unused 
#'
#' @return nothing
#' @export
#' @examples
#' \dontrun{
#'   tm = edc_example()
#'   load_list(tm)  
#'   edc_save_db_to_excel() #default arguments are usually OK
#' }
#' @importFrom cli cli_abort cli_inform
#' @importFrom dplyr arrange
#' @importFrom fs dir_create file_size
#' @importFrom glue glue
#' @importFrom purrr iwalk
#' @importFrom rlang check_dots_empty check_installed is_named sym
#' @importFrom stringr str_ends
#' @importFrom utils browseURL
edc_save_db_to_excel = function(..., 
                                datasets=get_datasets(),
                                filename=NULL,
                                overwrite=TRUE, 
                                open=TRUE){
  check_installed("openxlsx", "for `edc_save_to_excel()` to work.")
  check_dots_empty()
  assert(is_named(datasets))
  if(is.null(filename)) filename = glue("data/database_{str_replace_all(date_extraction,'/','-')}.xlsx")
  assert(str_ends(filename, ".xlsx"))
  filename = clean_filename(filename)
  dir_create(dirname(filename))
  datasets = datasets[order(names(datasets))]
  
  wb = openxlsx::createWorkbook()
  
  datasets %>% iwalk(~{
    if(get_subjid_cols() %in% names(.x)) .x = arrange(.x, !!sym(get_subjid_cols()))
    label_row = get_label(.x) %>% as.data.frame()
    openxlsx::addWorksheet(wb, sheetName=.y)
    openxlsx::writeData(wb, sheet=.y, x=label_row, startCol=2, startRow=2, colNames=FALSE)
    openxlsx::writeDataTable(wb, sheet=.y, x=.x, startCol=2, startRow=3)
    openxlsx::setColWidths(wb, sheet=.y, cols=2:ncol(.x), widths="auto")
    openxlsx::setColWidths(wb, sheet=.y, cols=1, widths=2.14)
    
  })
  rslt = openxlsx::saveWorkbook(wb, filename, overwrite=overwrite, returnValue=TRUE)
  if(!rslt){
    cli_abort("Could not create the Excel workbook.")
    return(FALSE)
  }
  
  size = file_size(filename)
  cli_inform("Excel file {.path {filename}} has been created ({size}).")
  if(open){
    browseURL(filename)
  }
  
  invisible(filename)
}


#' @importFrom fs path path_sanitize
clean_filename = function(filename){
  a = dirname(filename)
  b = basename(filename) %>% path_sanitize()
  path(a, b)
}
