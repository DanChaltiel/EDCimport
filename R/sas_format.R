
#' Turn a `procformat.sas` file to a list of vectors with `name=label`, `value=level`
#' 
#' @importFrom cli cli_abort
#' @importFrom fs file_exists
#' @importFrom purrr compact map
#' @importFrom readr read_file
#' @importFrom rlang set_names
#' @importFrom stringr regex str_match str_remove_all str_split str_starts str_trim
#' @noRd
#' @keywords internal
.read_proc_format = function(file){
  if(!file_exists(file)){
    cli_abort("File {file} does not exist.")
  }
  source = read_file(file)
  calls = source %>% str_split(";") %>% map(~str_trim(.x)) %>% .[[1]]
  
  formats_names = calls %>% 
    sapply(function(.x){
      if(str_starts(tolower(.x), "value")){
        str_match(.x, "value\\s+([$\\w]*)\\s+")[2]
      } else {
        NULL
      }
    }, USE.NAMES = FALSE)
  
  formats_values = calls %>% 
    sapply(function(.x){
      if(str_starts(tolower(.x), "value")){
        format_name = str_match(.x, "value\\s+([$\\w]*)\\s+")[2]
        format_values = str_match(.x, regex("value\\s+[$\\w]*\\s+(.*)", dotall=TRUE))[2] %>% 
          str_split("[\\r\\n]{1,2}") %>%
          .[[1]]
        format_values %>% sapply(function(kv){
          kv = str_match(kv, "(.*?)=(.*)")[-1]
          rtn = kv[1] %>% str_remove_all("^'|'$")
          names(rtn) = kv[2] %>% str_remove_all("^'|'$")
          rtn
        }, USE.NAMES = FALSE)
      } else {
        NULL
      }
    }, USE.NAMES = FALSE) 
  
  
  formats_values %>% 
    set_names(formats_names) %>% 
    compact()
}

#' Turn a label lookup file into a list of vectors with `name=label`, `value=level`
#' Default names from `formats.sas7bdat`
#' @noRd
#' @keywords internal
#' @importFrom dplyr all_of distinct mutate pull select
#' @importFrom purrr map
.read_format_lookup = function(file, format_name="FMTNAME", level="START", label="LABEL"){
  read_fun = guess_read_function(file)
  format_name = getOption("edc_var_format_name", default=format_name)
  level = getOption("edc_var_level", default=level)
  label = getOption("edc_var_label", default=label)
  
  read_fun(file) %>% 
    select(name=all_of(format_name), level=all_of(level), label=all_of(label)) %>% 
    mutate(level=as.numeric(level)) %>% 
    distinct() %>% 
    split(.$name) %>% 
    map(~pull(.x, level, name=label))
}


#' @noRd
#' @keywords internal
.format_sas_column =  function(x, formats){
  fname = attr(x, "format.sas") #set by haven::read_xpt
  if (is.null(fname) || is.null(formats) || !fname %in% names(formats)){
    return(x)
  }
  x %>% 
    structure(labels = formats[[fname]]) %>% 
    add_class("haven_labelled")
}


#' Read a sas procformat file and apply it to a dataset list
#' @noRd
#' @keywords internal
#' @importFrom dplyr across as_tibble everything mutate na_if where
#' @importFrom purrr map
.apply_sas_formats = function(datalist, format_file){
  if(is.null(format_file)) return(datalist)
  assert_file_exists(format_file)
  
  sas_formats = .read_sas_formats(format_file)
  datalist %>% 
    map(~{
      if(!is.data.frame(.x) || is_lookup(.x)) return(.x)
      .x %>% 
        as_tibble() %>% 
        mutate(
          across(where(~is.character(.x)), ~try(na_if(.x, ""), silent=TRUE)),
          across(everything(), ~.format_sas_column(.x, sas_formats)),
          across(where(~inherits(.x, c("try-error", "error"))), .flatten_error)
          ) %>% 
        haven::as_factor()
    })
}


#' @noRd
#' @keywords internal
#' @importFrom fs path_ext
.read_sas_formats = function(format_file){
  ext = path_ext(format_file)
  if(ext=="sas7bcat") return(NULL)
  if(ext=="sas") {
    sas_formats = .read_proc_format(format_file)
  } else {
    sas_formats = .read_format_lookup(format_file)
  }
  sas_formats
}
