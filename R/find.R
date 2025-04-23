

#' Search the whole database
#' 
#' Find a keyword in columns or values, in all the datasets of the database.
#'
#' @param keyword The keyword to search for. Regular expressions are only supported in `edc_find_column`.
#' @param ignore_case Logical. If `TRUE` (default), the search will ignore case differences.
#' @param lookup A lookup table.
#' @param data A list of datasets.
#'
#' @return a tibble
#' @export
#' @importFrom dplyr across any_of arrange as_tibble filter mutate mutate_all pull select slice
#' @importFrom purrr list_rbind map
#' @importFrom stringr fixed str_detect
#' @importFrom tidyr pivot_longer unnest
#' 
#' @examples
#' db = edc_example()
#' load_database(db)
#' 
#' edc_find_value("respi")
#' edc_find_value(2010)
#' 
#' edc_find_column("ad")
#' edc_find_column("date") 
#' #with regex
#' edc_find_column("\\d")
#' edc_find_column("\\(") #you need to escape special characters
edc_find_value = function(keyword, ignore_case=TRUE, data=get_datasets(), lookup=edc_lookup()){
  subjid = get_subjid_cols(lookup)
  data_labels = lookup %>% 
    as_tibble() %>% 
    unnest(labels) %>% 
    pull(labels)
  
  a = data %>% 
    keep(is.data.frame) %>% 
    map(function(dataset){
      dataset %>%
        mutate_all(as.character) %>% 
        pivot_longer(-any_of(subjid), names_to="column") %>% 
        filter(str_detect(value, fixed(as.character(keyword), ignore_case=ignore_case)))
    }) %>% 
    list_rbind(names_to="dataset")
  
  if(nrow(a)==0){
    return(a)
  }
  
  a %>% 
    mutate(column_label = unlist(data_labels[column]) %0% NA) %>% 
    select(any_of(subjid), dataset, column, column_label, value) %>% 
    mixed_arrange(dataset, column, any_of(subjid))
}



#' @rdname edc_find_value
#' @export
#' @importFrom cli cli_warn
#' @importFrom dplyr any_of filter mutate pull select where
#' @importFrom purrr map2_chr map2_dbl
#' @importFrom stringr str_detect str_trim
#' @importFrom tidyr unite unnest
edc_find_column = function(keyword, ignore_case=TRUE, lookup=edc_lookup()){
  invalid=names2=labels2=x=NULL
  f = if(isTRUE(ignore_case)) tolower else identity
  f2 = function(x,y) map2_chr(x, y, ~if(.y) {.x} else {f(.x)})
  keyword = f(str_trim(keyword))
  tmp = lookup %>% 
    select(-any_of(c("nrow", "ncol", "n_id", "rows_per_id"))) %>% 
    unnest(c(names, labels)) %>% 
    mutate(
      labels=unlist(labels), 
      invalid=is_invalid_utf8(labels),
      names2=f2(names, invalid),
      labels2=f2(labels, invalid),
    ) %>% 
    filter(str_detect(names2, keyword) | str_detect(labels2, keyword)) %>% 
    select(-names2, -labels2) %>% 
    mutate(prop_na = map2_dbl(dataset, names, ~{
      if(!exists(.x, envir=globalenv(), mode="list")) return(NA)
      x=get(.x, envir=globalenv())[[.y]]
      mean(is.na(x))
    })) %>% 
    select(-where(~all(is.na(.x))))
  if(nrow(tmp)==0) return(tmp)
  
  if(isTRUE(ignore_case) && any(tmp[["invalid"]])){
    cols = tmp %>% filter(invalid) %>% unite("x", c("dataset", "names"), sep="$") %>% pull(x)
    cli_warn(c("Some columns have labels containing non UTF8 characters. {.arg ignore_case} has been ignored for these.", 
               i="Columns: {.code {cols}}.",
               i='For instance, try to run: {.code attr({cols[1]}, "label")}.'), 
             class="find_keyword_utf8_warning")
  }
  
  tmp %>% 
    select(-invalid, -any_of("subjids"))
}


#' @rdname edc_find_value
#' @usage NULL
#' @export
find_keyword = deprecatedly(edc_find_column, when="0.6.0", what="find_keyword()")

