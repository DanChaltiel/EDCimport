


#' Show the current CRF status distribution
#' 
#' Generate a barplot showing the distribution of CRF status (Complete, Incomplete, ...) for each dataset of the database.
#'
#' @param crfstat_col the column name of the CRF status 
#' @param pal the palette, defaulting to the helper `EDCimport:::edc_crf_pal()`
#' @param details whether to show all the CRF status levels. When `FALSE` (default), recode the status into "Complete", "Incomplete", or "No Data".
#' @param crfstat_lvls the CRF status levels, from "best" to "worst". The plot is ordered by the "worst" level. 
#' @param treat_as_worst a regex for levels that should be treated as worst in the ordering
#'
#' @return a ggplot
#' @export
#'
#' @examples
#' \dontrun{
#' #import a TM database and use load_list(), then:
#' crf_status_plot() + ggtitle(date_extraction)
#' crf_status_plot(pal=rev(edc_crf_pal()))
#' crf_status_plot(details=TRUE, treat_as_worst="No Data")
#' 
#' p = crf_status_plot(details=TRUE)
#' p$data$crfstat %>% unique()
#' #> [1] "Incomplete"        "No Data Locked"    "No Data"           "Signed"           
#' #> [5] "Partial Monitored" "Monitored"         "Complete Locked"   "Complete" 
#' }
#' @importFrom dplyr count last mutate select
#' @importFrom forcats fct_reorder2 fct_rev
#' @importFrom ggplot2 aes geom_col ggplot labs position_fill scale_fill_manual scale_x_continuous
#' @importFrom purrr list_rbind map
#' @importFrom scales label_percent
#' @importFrom stringr str_subset
#' @importFrom tibble tibble
crf_status_plot = function(crfstat_col="CRFSTAT", 
                           ..., 
                           details=FALSE,
                           pal = edc_crf_pal(), 
                           crfstat_lvls = names(pal), 
                           treat_as_worst=NULL){
  check_dots_empty()
  
  completion_reorder = function(x,y) {
    incomplete = last(crfstat_lvls)
    if(!is.null(treat_as_worst)) incomplete = c(incomplete, str_subset(crfstat_lvls, treat_as_worst))
    if(!any(x %in% incomplete)) return(0)
    sum(y[x %in% incomplete])/sum(y)
  }
  
  df = get_datasets(envir=parent.frame()) %>% 
    map(~{
      if(!any(crfstat_col %in% names(.x))) return(tibble())
      .x %>% select(crfstat = any_of2(crfstat_col))
    }) %>% 
    list_rbind(names_to="dataset") %>% 
    mutate(crfstat = edf_crfstat_recode(crfstat, do=!details)) %>% 
    count(dataset, crfstat) %>% 
    mutate(
      # a = browser(),
      crfstat = factor(crfstat, levels=crfstat_lvls) %>% fct_drop() %>% fct_rev(),
      dataset = fct_reorder2(dataset, crfstat, n,
                             .fun=function(x,y) y[x=="Complete"]/sum(y)) %>% fct_rev(),
      dataset = fct_reorder2(dataset, crfstat, n, .fun=completion_reorder)
    )
  
  
  df %>% 
    ggplot(aes(y=dataset, x=n, fill=fct_rev(crfstat))) +
    geom_col(position=position_fill(reverse=TRUE)) +
    scale_fill_manual(values=pal) +
    scale_x_continuous(labels=label_percent()) +
    labs(x=NULL, y="Dataset", fill="CRF Status")
}



#' @rdname crf_status_plot
#' @export
#' @source ggsci:::ggsci_db$lancet[["lanonc"]] %>% dput()
edc_crf_pal = function(){
  c("Complete"="#000e8b", 
    "Complete Locked"="#0053a5", 
    "Complete Signed"="#006dd8", 
    "Signed"="#925E9F", 
    "Monitored"="#42B540FF", 
    "Partial Monitored"="#0099B4", 
    "No Data"="#C8CECE", 
    "No Data Locked"="#ADB6B6", 
    "Incomplete"="#ED0000"
  )
}

#' @noRd
#' @keywords internal
#' @importFrom forcats fct_expand fct_recode fct_drop
edf_crfstat_recode = function(x, do=TRUE){
  if(!do) return(x)
  lvl_recoding = c(  
    "Complete" = "Monitored",
    "Complete" = "Partial Monitored",
    "Complete" = "Signed",
    "Complete" = "Complete Signed",
    "Complete" = "Complete Locked",
    "Complete" = "Complete",
    "No Data" = "No Data Locked",
    "No Data" = "No Data",
    "Incomplete" = "Incomplete"
  )
  
  x %>% fct_expand(lvl_recoding) %>% fct_recode(!!!lvl_recoding) %>% fct_drop()
}

