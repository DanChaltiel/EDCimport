


#' EDCimport Database
#' 
#' This class of object represents a database, as the result of an EDCimport reading function.
#' It has its own `print()` method.
#' 
#' @section Functions returning `edc_database` objects:
#' As per now, reading functions are: [read_trialmaster()], [read_all_sas()], [read_all_xpt()], and [read_all_csv()].
#' 
#' @section Structure:
#' While it is not usually useful to query them, an `edc_database` object is a named list containing:
#' - all the datasets from the source files
#' - `datetime_extraction` and `date_extraction` the inferred date of data extraction
#' - `.lookup` a temporary copy of the lookup table
#' 
#' @seealso [read_trialmaster()]
#' 
#' @name edc_database
#' @docType class
NULL

#' Read all files using a specific read function, returning a named list of tibbles
#' @param ... passed to `read_function`
#' @noRd
#' @keywords internal
#' @importFrom dplyr across as_tibble mutate where
#' @importFrom fs path_ext_remove
#' @importFrom purrr map
#' @importFrom rlang hash set_names
#' @importFrom stringr fixed str_remove str_replace_all
.read_all = function(files, read_function, clean_names_fun=NULL, path=NULL, ...){
  assert_file_exists(files)
  clean_names_fun = .get_clean_names_fun(clean_names_fun)
  if(!is.null(path)){
    path = normalizePath(path, winslash="/")
    file_names = files %>% normalizePath(winslash="/") %>% 
      str_remove(fixed(as.character(path))) %>% str_remove("^/") %>% 
      str_replace_all("/", "_") %>% tolower() %>% path_ext_remove()
  } else {
    file_names = basename(files) %>% tolower() %>% path_ext_remove()
  }
  files %>% 
    set_names(file_names) %>% 
    map(function(.x) {
      tbl = tryCatch(read_function(.x, ...), 
                     error = function(e) .flatten_error(e, class="edc_error_data"))
      if(is_edc_error(tbl)) return(tbl)
      rtn = tbl %>% 
        as_tibble() %>% 
        clean_names_fun() %>% 
        mutate(across(where(bad_hms), fix_hms))
      attr(rtn, "hash") = hash(rtn)
      rtn
    })
}

#' Build and add lookup, add datetime_extraction, and add `...` as attributes
#' @noRd
#' @keywords internal
.add_lookup_and_date = function(datalist, datetime_extraction, extend_lookup=TRUE, ...){
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
#' @importFrom dplyr across mutate where
.flatten_error_columns = function(df){
  df %>% 
    mutate(across(where(~inherits(.x, "try-error")), .flatten_error))
}

#' Change a `try-error` into a simpler character column of class "edc_error_col"
#' @noRd
#' @keywords internal
.flatten_error = function(e, class="edc_error_col"){
  if(!inherits(e, c("try-error", "error"))) return(e)
  if(inherits(e, c("try-error"))) e = attr(e, "condition")
  e = e$message
  class(e) = class
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
#' @importFrom purrr map_if modify
.clean_labels_utf8 = function(datalist, warn=FALSE){
  datalist %>% 
    map_if(is.data.frame, function(df){
      df %>% modify(~{
        attr(.x, "label") = .repair_invalid_utf8(attr(.x, "label"))
        .x
      })
    })
}
