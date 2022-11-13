


#' Improve file.path but remove duplicated separators
#'
#' @param ... passed on to base::file.path()
#'
#' @return a file path
#' @noRd
#' @keywords internal
file.path2 = function(...){
  fsep = .Platform$file.sep
  file.path(...) %>% str_replace_all(paste0(fsep, "+"), fsep)
}

#' Lookup a variable name
#' 
#' @source [checkmate::vname()]
#' @return Variable name
#' @noRd
#' @keywords internal
vname = function(x) {
  paste0(deparse(eval.parent(substitute(substitute(x))), width.cutoff = 500L), 
         collapse = "\n")
}


#' Parse a file name to get the date of data extraction
#'
#' @param x a file
#' @noRd
#' @keywords internal
parse_file_datetime = function(x){
  x %>% 
    basename() %>% 
    str_match("SAS_XPORT_(\\d{4}_\\d{2}_\\d{2}_\\d{2}_\\d{2})") %>% 
    .[,2] %>%
    strptime(format="%Y_%m_%d_%H_%M") %>% 
    as.POSIXct()
}

#' Get the date of data extraction from the modification time of all files
#'
#' @param folder a folder
#' @return date as a POSIXct scalar
#' @noRd
#' @keywords internal
get_folder_datetime = function(folder){
  rtn = dir(folder, full.names=TRUE) %>% file.info() %>% pull("mtime") %>% unique()
  if(length(rtn)>1) cli::cli_warn("Folder {folder} had files with different dates: {rtn}")
  rtn[[1]]
}



#' Parse a file name to get the name of the project
#'
#' @param x a file
#' @noRd
#' @keywords internal
parse_file_project = function(x){
  x %>% 
    basename() %>% 
    str_match("(.*)_Export") %>% 
    .[,2]
}


#' @noRd
#' @keywords internal
format_ymd = function(x){
  stopifnot(inherits(x, "POSIXct") || inherits(x, "Date"))
  format(x, "%Y-%m-%d")
}
#' @noRd
#' @keywords internal
format_ymdhm = function(x){
  stopifnot(inherits(x, "POSIXct") || inherits(x, "Date"))
  format(x, "%Y-%m-%d %Hh%M")
}
