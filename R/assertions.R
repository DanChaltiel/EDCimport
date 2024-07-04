#' @noRd
#' @keywords internal
#' @examples
#' assert(1+1==2)
#' assert(1+1==4)
assert = function(x, msg=NULL){
  if(is.null(msg)){
    x_str = caller_arg(x)
    msg = glue("`{x_str}` is FALSE")
  }
  if(!x){
    cli_abort(msg)
  }
  invisible(TRUE)
}


#' @noRd
#' @keywords internal
#' @examples
#' assert_file_exists("R/data.R")
#' assert_file_exists("R/data.SAS")
assert_file_exists = function(x, msg=NULL){
  if(is.null(msg)){
    x_str = caller_arg(x)
    msg = glue("{x_str} doesn't exist.")
  }
  if(!file.exists(x)){
    cli_abort(msg)
  }
  invisible(TRUE)
}


#' @noRd
#' @keywords internal
can_be_numeric = function(x){
  stopifnot(is.atomic(x) || is.list(x))
  xnum_na <- suppressWarnings(is.na(as.numeric(x)))
  all(is.na(x)==xnum_na)
}
#' @noRd
#' @keywords internal
is.Date = function (x) {
  inherits(x, "POSIXt") || inherits(x, "POSIXct") || inherits(x, "Date")
}

#' @noRd
#' @keywords internal
#' @source https://stackoverflow.com/a/57261396/3888000
is_invalid_utf8 = function(x){
  !is.na(x) & is.na(iconv(x, "UTF-8", "UTF-8"))
}

#' @noRd
#' @keywords internal
#' @importFrom cli cli_warn
#' @importFrom dplyr arrange desc filter mutate
#' @importFrom glue glue
#' @importFrom rlang set_names
#' @importFrom tidyr unnest
check_invalid_utf8 = function(lookup=get_lookup(), warn=FALSE){
  stopifnot(!is.null(lookup))
  x = lookup %>% 
    arrange(desc(nrow)) %>% 
    unnest(c(names, labels)) %>% 
    filter(is_invalid_utf8(labels)) %>% 
    mutate(
      dataset, names, labels, 
      valid_labels=iconv(labels, to="UTF-8"),
      .keep = "none"
    )
  
  bad_utf8 = glue("{x$dataset}${x$names} ({x$valid_labels}) ") %>% set_names("i")
  if(nrow(x)>0 && isTRUE(warn)){
    cli_warn(c("Found {length(bad_utf8)} invalid UTF-8 label{?s}:", bad_utf8))
  }
  
  x
}