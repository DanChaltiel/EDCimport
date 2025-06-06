

# Assertions ----------------------------------------------------------------------------------
#to avoid dependency on checkmate


#' @noRd
#' @keywords internal
#' @examples
#' assert(1+1==2)
#' assert(1+1==4)
#' @importFrom cli cli_abort
#' @importFrom glue glue
#' @importFrom rlang caller_arg
assert = function(x, msg=NULL, call=parent.frame(), class=NULL){
  if(is.null(msg)){
    msg = glue("`{x_str}` is FALSE", x_str=caller_arg(x))
  }
  if(isFALSE(as.logical(x))){
    cli_abort(msg, .envir=call, class=class)
  }
  invisible(TRUE)
}


#' @noRd
#' @keywords internal
#' @examples
#' assert_file_exists("R/data.R")
#' assert_file_exists("R/data.SAS")
#' @importFrom cli cli_abort
#' @importFrom fs file_exists
assert_file_exists = function(x, msg=NULL, class=NULL){
  missing_files = !file_exists(x)
  if(is.null(msg)){
    msg = "Missing file{?s}: {.path {x[missing_files]}}"
  }
  if(any(missing_files)){
    cli_abort(msg, call=parent.frame(), class=class)
  }
}


#' @noRd
#' @keywords internal
#' @importFrom cli cli_abort
assert_class = function(x, class, null.ok=TRUE){
  if(is.null(x) && null.ok) return(invisible(TRUE))
  if(!inherits(x, class)){
    cli_abort("{.arg {caller_arg(x)}} should be of class {.cls {class}}, not  {.cls {class(x)}}", 
              call=parent.frame())
  }
  invisible(TRUE)
}


#' @noRd
#' @keywords internal
#' @importFrom cli cli_abort
#' @importFrom purrr discard
#' @importFrom rlang caller_arg
assert_names_exists = function(df, l){
  df_name = caller_arg(df)
  not_found = l %>% 
    discard(is.null) %>%
    discard(~tolower(.x) %in% tolower(names(df)))
  if(length(not_found)>0){
    a = paste0(names(not_found), "='", not_found, "'")
    cli_abort("Columns not found in {.arg df_name}: {.val {a}}",
              class="edc_name_notfound_error")
  }
}


# UTF8 ----------------------------------------------------------------------------------------



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
check_invalid_utf8 = function(lookup=edc_lookup(), warn=FALSE){
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



# Misc ----------------------------------------------------------------------------------------


#' @noRd
#' @keywords internal
can_be_numeric = function(x){
  if(length(x)==0) return(NA)
  if(is.factor(x)) return(FALSE)
  stopifnot(is.atomic(x) || is.list(x))
  xnum_na = suppressWarnings(is.na(as.numeric(x)))
  wrong = x[is.na(x)!=xnum_na]
  structure(length(wrong)==0, wrong=wrong)
}


#' @noRd
#' @keywords internal
is.Date = function (x) {
  inherits(x, "POSIXt") || inherits(x, "POSIXct") || inherits(x, "Date")
}


#' @noRd
#' @keywords internal
is_edc_error = function(x){
  inherits(x, "edc_error") || inherits(x, "edc_error_col") || inherits(x, "edc_error_data")
}
