

#' Read all files using a specific read function, returning a named list of tibbles
#' @param ... passed to `read_function`
#' @noRd
#' @keywords internal
#' @importFrom fs path_ext_remove
#' @importFrom purrr map
#' @importFrom rlang set_names
#' @importFrom tibble as_tibble
.read_all = function(files, read_function, clean_names_fun=NULL, ...){
  assert_file_exists(files)
  file_names = basename(files) %>% tolower() %>% path_ext_remove()
  clean_names_fun = .get_clean_names_fun(clean_names_fun)
  
  files %>% 
    set_names(file_names) %>% 
    map(function(.x) {
      tbl = tryCatch(read_function(.x, ...), 
                     error = function(e) .flatten_error(e))
      tbl %>% 
        as_tibble() %>% 
        clean_names_fun() %>% 
        mutate(across(where(bad_hms), fix_hms))
    })
}

#' Build and add lookup, add datetime_extraction, and add `...` as attributes
#' @noRd
#' @keywords internal
.add_lookup_and_date = function(datalist, datetime_extraction, extend_lookup=FALSE, ...){
  assert_class(datetime_extraction, c("POSIXt", "Date"))
  .lookup = build_lookup(datalist)
  if(!is.null(extend_lookup)){
    .lookup = extend_lookup(.lookup, datasets=datalist)
  }
  .lookup = .lookup %>% 
    structure(datetime_extraction=datetime_extraction,
              ...)
  
  datalist$datetime_extraction = datetime_extraction
  datalist$date_extraction = format_ymd(datetime_extraction)
  datalist$.lookup = .lookup
  
  datalist
}

#' Apply `.flatten_error` to all `try-error` columns
#' @noRd
#' @keywords internal
#' @importFrom dplyr across mutate
#' @importFrom tidyselect where
.flatten_error_columns = function(df){
  df %>% 
    mutate(across(where(~inherits(.x, "try-error")), .flatten_error))
}

#' Change a `try-error` into a simpler character column of class "edc_error_col"
#' @noRd
#' @keywords internal
.flatten_error = function(e){
  if(!inherits(e, c("try-error", "error"))) return(e)
  if(inherits(e, c("try-error"))) e = attr(e, "condition")
  e = e$message
  class(e) = "edc_error_col"
  e
}


#' @noRd
#' @keywords internal
#' @importFrom cli cli_abort
#' @importFrom rlang as_function is_formula
.get_clean_names_fun = function(f){
  if(is.null(f)) return(identity)
  if(is_formula(f)) f = as_function(f)
  if(!is.function(f)) cli_abort("{.arg {caller_arg(f)}} should be a function or a lambda-function, not a {.cls {class(f)}}.")
  f
}

#' clean all names using `clean_names_fun`, or do nothing if `NULL`
#' @noRd
#' @keywords internal
#' @importFrom purrr map
.clean_names = function(datalist, clean_names_fun){
  #TODO: merge with .get_clean_names_fun()
  clean_names_fun = .get_clean_names_fun(clean_names_fun)
  datalist %>% map(clean_names_fun)
}


#' clean all labels for non-UTF8 characters
#' @noRd
#' @keywords internal
#' @importFrom purrr map modify
.clean_labels_utf8 = function(datalist, warn=FALSE){
  datalist %>% 
    map(function(df){
      df %>% modify(~{
        attr(.x, "label") = .repair_invalid_utf8(attr(.x, "label"))
        .x
      })
    })
}
