

#' Read all files using a specific read function, returning a named list
#' @noRd
#' @keywords internal
.read_all = function(files, read_function, ...){
  assert_file_exists(files)
  file_names = basename(files) %>% str_remove("\\.[^.]*$") %>% tolower()
  files %>% 
    set_names(file_names) %>% 
    map(function(.x) tryCatch(read_function(.x, ...), error=function(e) e))
}


#' Change all `try-error` columns to a simpler character column of class "edc_error_col"
#' @noRd
#' @keywords internal
#' @importFrom dplyr across mutate
#' @importFrom tidyselect where
.flatten_error_columns = function(df){
  df %>% 
    mutate(across(where(~inherits(.x, "try-error")), ~{
      attr(.x, "condition")$message %>% `class<-`("edc_error_col")
    }))
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
.clean_names = function(datalist, clean_names_fun){
  #TODO: merge with .get_clean_names_fun()
  clean_names_fun = .get_clean_names_fun(clean_names_fun)
  datalist %>% map(clean_names_fun)
}


#' clean all labels for non-UTF8 characters
#' @noRd
#' @noRd
#' @keywords internal
#' @importFrom purrr map
.clean_labels_utf8 = function(datalist, warn=FALSE){
  datalist %>% 
    map(function(df){
      df %>% modify(~{
        attr(.x, "label") = .repair_invalid_utf8(attr(.x, "label"))
        .x
      })
    })
}
