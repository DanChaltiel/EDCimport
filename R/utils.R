


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
get_folder_datetime = function(folder, verbose=TRUE){
  mtime=NULL
  rtn = dir(folder, full.names=TRUE) %>% file.info() %>% count(mtime=round(mtime, "secs"))
  if(isTRUE(verbose) && nrow(rtn)>1){
    cli::cli_warn(c("Folder {.file {folder}} contains files with different modification times. 
                    The most frequent one was returned.", 
                    i="Times: {.val {rtn$mtime}}"),
                  class="get_folder_datetime_modiftime_warning")
  }
  rtn %>% slice_max(n) %>% .[1,"mtime"]
}


can_be_numeric = function(x){
  stopifnot(is.atomic(x) || is.list(x))
  xnum_na <- suppressWarnings(is.na(as.numeric(x)))
  all(is.na(x)==xnum_na)
}

#' @noRd
#' @keywords internal
format_ymd = function(x){
  stopifnot(inherits(x, "POSIXt") || inherits(x, "Date"))
  format(x, "%Y-%m-%d")
}
#' @noRd
#' @keywords internal
format_ymdhm = function(x){
  stopifnot(inherits(x, "POSIXct") || inherits(x, "Date"))
  format(x, "%Y-%m-%d %Hh%M")
}

#' @noRd
#' @keywords internal
is.Date = function (x) {
  inherits(x, "POSIXt") || inherits(x, "POSIXct") || inherits(x, "Date")
}
