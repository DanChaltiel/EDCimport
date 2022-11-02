
#' @importFrom readr read_file
read_sas_format = function(file){
  if(!file.exists(file)){
    cli_abort("File {file} does not exist.")
  }
  source = read_file(file)
  calls = source %>% str_split(";") %>% map(str_trim) %>% .[[1]]
  
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
        format_name=str_match(.x, "value\\s+([$\\w]*)\\s+")[2]
        format_values = str_match(.x, regex("value\\s+[$\\w]*\\s+(.*)", dotall=TRUE))[2] %>% 
          str_split("[\\r\\n]{1,2}") %>%
          .[[1]]
        format_values %>% sapply(function(kv){
          kv = str_match(kv, "(.*)=(.*)")[-1]
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


apply_sas_formats = function(df, formats){
  df %>% map_df(~{
    fname = attr(.x, "format.sas")
    if(!is.null(fname) && fname %in% names(formats)){
      attr(.x, "labels") = formats[[fname]]
      class(.x) = c("haven_labelled", class(.x)) %>% unique
      .x
    } else {
      .x
    }
  })
}

