

#' Get a table with the latest date for each patient
#' 
#' This function search for date columns in every tables and returns the latest date 
#' for each patient with the variable it comes from. Useful in survival analysis to get 
#' the right censoring time.
#'
#' @param except the datasets/columns that should not be searched. Example: a scheduled visit for which the patient may have died before attending should not be considered.
#' @param with_ties in case of tie, whether to return the first `origin` (FALSE) or all the origins that share this tie (TRUE).
#' @param show_delta whether to compute the difference between the last `prefer` date and the actual last date 
#' @param numeric_id set to FALSE if the patient ID column is not numeric
#' @param prefer preferred origins in the event of a tie. Usually the followup table.
#' @param regex whether to consider `except` and `prefer` as regex.
#' @param warn_if_future whether to show a warning about dates that are after the extraction date. Can also be a csv file path to save the warning as csv (see `csv_path` argument in [edc_data_warn]).
#'
#' @return a dataframe
#' @export
#' @importFrom cli cli_abort
#' @importFrom dplyr arrange filter left_join mutate rowwise select slice_max ungroup
#' @importFrom purrr discard discard_at imap list_rbind map_dbl
#' @importFrom stringr str_detect
#' @importFrom tidyr replace_na
#'
#' @examples
#' db = edc_example()
#' load_database(db)
#' lastnews_table()
#' lastnews_table(except="data3")
#' lastnews_table(except="data3$date9")
#' lastnews_table(prefer="date10", show_delta=TRUE) 
#' lastnews_table() %>% 
#'   dplyr::count(origin = glue::glue("{origin_data}${origin_col}"), 
#'   sort=TRUE)
#' 
#' csv_file = tempfile(fileext=".csv")
#' lastnews_table(prefer="date9", warn_if_future=csv_file) 
lastnews_table = function(except=NULL, with_ties=FALSE, show_delta=FALSE, numeric_id=TRUE, 
                          prefer=NULL, regex=FALSE, warn_if_future=TRUE) {
  subjid_cols = get_subjid_cols()
  except = regexify(except, regex)
  
  rtn = get_datasets(envir=parent.frame()) %>% 
    discard_at(~str_detect(.x, except)) %>% 
    imap(~.extract_date_columns(.x, .y)) %>% 
    discard(is.null) %>% 
    list_rbind()
  
  if(nrow(rtn)==0){
    cli_abort("No data with dates could be found, verify your export settings.",
              class="edc_no_columns_error")
  }
  
  if(numeric_id && can_be_numeric(rtn$subjid)) {
    rtn$subjid = as.numeric(as.character(rtn$subjid))
  }
  
  if(isTRUE(show_delta)){
    f = if(!isTRUE(regex)) fixed else identity 
    rtn_delta = rtn %>% 
      arrange(order(mixedorder(subjid))) %>%
      select(subjid, last_date=value, origin_data=dataset, origin_col=name, origin_label) %>%
      mutate(origin = paste0(origin_data, "$", origin_col)) %>%
      filter(!str_detect(origin, except)) %>%
      filter(!str_detect(origin_data , except)) %>%
      filter(!str_detect(origin_col, except))
    
    rtn_delta = rtn_delta %>%
      slice_max(last_date, by=c(subjid, origin), with_ties=TRUE) %>%
      mutate(
        preferred = map_dbl(origin, ~which(str_detect(.x, f(as.character(prefer))))[1]) %>% 
          replace_na(Inf)
      )  %>% 
      filter(!is.infinite(preferred))
    
    if(nrow(rtn_delta)>0){
      rtn_delta = rtn_delta %>%
        arrange(preferred) %>% 
        slice_max(last_date, by=subjid, with_ties=FALSE) %>% 
        select(subjid, preferred_last_date=last_date, preferred_origin=origin) %>% 
        arrange(order(mixedorder(subjid)))
    } else {
      cli_warn("No {.cls date} column corresponding to {.arg prefer}={.val {prefer}} could be 
               found, so {.arg show_delta} has been set back to FALSE.",
               class="edc_no_preferred_column_warning")
      show_delta = FALSE
    }
  }
  
  rtn = rtn %>% 
    select(subjid, last_date=value, origin_data=dataset, origin_col=name, origin_label) %>% 
    mutate(origin = paste0(origin_data, "$", origin_col)) %>% 
    filter(!str_detect(origin, except)) %>% 
    filter(!str_detect(origin_data , except)) %>% 
    filter(!str_detect(origin_col, except)) %>% 
    slice_max(last_date, by=subjid, with_ties=TRUE) %>% 
    arrange(order(mixedorder(subjid)))
  
  if(!isTRUE(with_ties)){
    rtn = rtn %>% 
      rowwise() %>%  
      mutate(preferred = .get_preferred(c(origin, origin_data, origin_col),
                                        needle=prefer, regex=regex)) %>%
      ungroup() %>% 
      arrange((preferred)) %>% 
      select(-preferred, -origin) %>%
      slice_max(last_date, by=subjid, with_ties=FALSE) %>% 
      arrange(order(mixedorder(subjid)))
  }
  
  if(isTRUE(show_delta)){
    rtn = rtn %>% 
      left_join(rtn_delta, by="subjid") %>% 
      mutate(delta = last_date-preferred_last_date)
  }
  
  
  
  datetime_extraction = .get_extraction_date()
  if(!is.null(datetime_extraction)){
    if(!isFALSE(warn_if_future)){
      csv_path = if(!isTRUE(warn_if_future)) warn_if_future else FALSE
      rtn %>% 
        filter(as.Date(last_date)>as.Date(datetime_extraction)) %>% 
        mutate(origin=paste0(origin_data,"$",origin_col)) %>% 
        edc_data_warn("Date of last news after the extraction date on 
                      column{?s} {.val {unique(.data$origin)}}",
                      csv_path=csv_path,
                      issue_n=NA)
    }
  }
  class(rtn) = c("edc_lastnews_table", class(rtn))
  attr(rtn, "prefer") = prefer
  rtn
}


# Utils ---------------------------------------------------------------------------------------


#' @noRd
#' @keywords internal
#' @importFrom purrr map_lgl
#' @importFrom stringr str_detect
.get_preferred = function(input, needle, regex) {
  f = if(!isTRUE(regex)) fixed else identity
  x = map_lgl(needle, ~any(str_detect(input, f(.x))))
  suppressWarnings(min(which(x), na.rm=TRUE))
}


#' @noRd
#' @keywords internal
#' @importFrom dplyr filter mutate select where
#' @importFrom tidyr pivot_longer
.extract_date_columns = function(data, data_name) {
  subjid_cols = get_subjid_cols()
  if(!is.data.frame(data) || !any(subjid_cols %in% names(data))) return(NULL)
  a = data %>% 
    select(subjid = any_of2(subjid_cols), where(is.Date)) %>% 
    mutate(subjid = as.character(subjid))
  if(ncol(a)<=1) return(NULL) #only subjid
  a %>% 
    pivot_longer(-subjid) %>% 
    filter(!is.na(value)) %>% 
    mutate(origin_label=unlist(get_label(data)[name]),
           dataset=data_name)
}


#' @noRd
#' @keywords internal
.get_extraction_date = function(){
  l = edc_lookup()
  rtn = attr(l, "datetime_extraction")
  if(is.null(rtn) && exists("datetime_extraction")) rtn = datetime_extraction
  rtn
}


#' @noRd
#' @keywords internal
#' @importFrom stringr str_escape
regexify = function(x, regex){
  if(is.null(x)) return("$^") #impossible pattern
  if(!isTRUE(regex)) x = str_escape(x)
  x = paste(x, collapse="|")
  x
}
