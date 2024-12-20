

#' Get a table with the latest date for each patient
#' 
#' This function search for date columns in every tables and returns the latest date 
#' for each patient with the variable it comes from. Useful in survival analysis to get 
#' the right censoring time.
#'
#' @param except the datasets/columns that should not be searched. Example: a scheduled visit for which the patient may have died before attending should not be considered.
#' @param with_ties in case of tie, whether to return the first `origin` (FALSE) or all the origins that share this tie (TRUE).
#' @param numeric_id set to FALSE if the patient ID column is not numeric
#' @param prefer preferred origins in the event of a tie. Usually the followup table.
#' @param warn_if_future whether to show a warning about dates that are after the extraction date. Can also be a csv file path to save the warning as csv (see [edc_data_warn(csv_path)]).
#'
#' @return a dataframe
#' @export
#'
#' @examples
#' tm = edc_example_plot()
#' load_list(tm)
#' lastnews_table()
#' lastnews_table(except="db3")
#' lastnews_table(except="db3$date9")
#' lastnews_table(prefer="db2", warn_if_future="check/check_db2.csv") 
#' @importFrom cli cli_abort
#' @importFrom dplyr arrange filter mutate rowwise select slice_max ungroup where
#' @importFrom purrr discard discard_at imap list_rbind
#' @importFrom tidyr pivot_longer
lastnews_table = function(except=NULL, with_ties=FALSE, numeric_id=TRUE, 
                          prefer=NULL,
                          warn_if_future=TRUE) {
  subjid_cols = get_subjid_cols()
  rtn = get_datasets(envir=parent.frame()) %>% 
    discard_at(as.character(except)) %>% 
    imap(~{
      if(!is.data.frame(.x) || !any(subjid_cols %in% names(.x))) return(NULL)
      a = .x %>% 
        select(subjid = any_of2(subjid_cols), where(is.Date)) %>% 
        mutate(subjid = as.character(subjid))
      if(ncol(a)<=1) return(NULL) #only subjid
      a %>% 
        pivot_longer(-subjid) %>% 
        filter(!is.na(value)) %>% 
        mutate(origin_label=unlist(get_label(.x)[name]),
               dataset=.y)
    }) %>% 
    discard(is.null) %>% 
    list_rbind()  %>% 
    select(subjid, last_date=value, origin_data=dataset, origin_col=name, origin_label) %>% 
    mutate(origin = paste0(origin_data, "$", origin_col))
  if(nrow(rtn)==0){
    cli_abort("No data with dates could be found, verify your export settings.")
  }
  if(numeric_id && can_be_numeric(rtn$subjid)) {
    rtn$subjid = as.numeric(as.character(rtn$subjid))
  }
  
  rtn = rtn %>% 
    filter(!origin %in% except) %>% 
    filter(!origin_data %in% except) %>% 
    filter(!origin_col %in% except) %>% 
    slice_max(last_date, by=subjid, with_ties=TRUE) %>% 
    arrange(order(mixedorder(subjid)))
  
  if(!isTRUE(with_ties)){
    rtn = rtn %>% 
      rowwise() %>%  
      mutate(prefered = which(prefer %in% c(origin, origin_data, origin_col)) %0% Inf) %>% 
      ungroup() %>% 
      arrange((prefered)) %>% 
      select(-prefered, -origin) %>% 
      slice_max(last_date, by=subjid, with_ties=FALSE) %>% 
      arrange(order(mixedorder(subjid)))
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
  
  rtn
}


# Utils ---------------------------------------------------------------------------------------

#' @noRd
#' @keywords internal
.get_extraction_date = function(){
  l = edc_lookup()
  rtn = attr(l, "datetime_extraction")
  if(is.null(rtn) && exists("datetime_extraction")) rtn = datetime_extraction
  rtn
}