


#' Show the current CRF status distribution
#' 
#' Generate a barplot showing the distribution of CRF status (Complete, Incomplete, ...) for each dataset of the database.
#'
#' @param crfstat_col the column name of the CRF status 
#' @param details whether to show all the CRF status levels. When `FALSE` (default), recode the status into "Complete", "Incomplete", or "No Data".
#' @param pal the palette, defaulting to the helper `EDCimport:::edc_pal_crf()`. The names give the CRF status levels, from "best" to "worst". The plot is ordered by the "worst" level. 
#' @param reverse whether to reverse the CRF status level order. 
#' @param x_label a glue pattern determining the tick label in the x axis. Available variables are the ones of [edc_lookup()]: `c("dataset", "nrow", "ncol", "n_id", "rows_per_id", "crfname")`.
#' @param treat_as_worst a regex for levels that should be treated as worst in the ordering.
#' @param datasets,lookup internal
#' @param ... unused
#'
#' @return a ggplot
#' @export
#'
#' @examples
#' \dontrun{
#' #import a TM database and use load_database(), then:
#' edc_crf_plot() + ggtitle(date_extraction)
#' edc_crf_plot(reverse=TRUE)
#' edc_crf_plot(details=TRUE, treat_as_worst="No Data")
#' edc_crf_plot(x_label="{crfname} (N={n_id}, n={nrow})")
#' 
#' p = edc_crf_plot(details=TRUE)
#' p$data$crfstat %>% unique()
#' #> [1] "Incomplete"        "No Data Locked"    "No Data"           "Signed"           
#' #> [5] "Partial Monitored" "Monitored"         "Complete Locked"   "Complete" 
#' }
#' @importFrom dplyr arrange count left_join mutate select
#' @importFrom forcats fct_drop fct_inorder fct_reorder2 fct_rev
#' @importFrom ggplot2 aes geom_col ggplot labs position_fill scale_fill_manual scale_x_continuous theme_minimal
#' @importFrom glue glue
#' @importFrom purrr list_rbind map
#' @importFrom rlang check_dots_empty
#' @importFrom scales label_percent
#' @importFrom stringr str_subset
#' @importFrom tibble tibble
edc_crf_plot = function(crfstat_col="CRFSTAT", 
                        ..., 
                        details=FALSE,
                        pal = edc_pal_crf(), 
                        reverse = FALSE,
                        x_label = "{dataset}",
                        treat_as_worst=NULL,
                        datasets=get_datasets(),
                        lookup=edc_lookup()){
  check_dots_empty()
  assert_class(crfstat_col, "character")
  assert_class(datasets, "list")
  assert(length(datasets)>0)
  
  if(isTRUE(reverse)) pal = rev(pal)
  crfstat_lvls = names(pal)
  
  completion_reorder = function(x, y, which_lvl) {
    incomplete = which_lvl(crfstat_lvls)
    if(!is.null(treat_as_worst)) {
      incomplete = c(incomplete, str_subset(crfstat_lvls, treat_as_worst))
    }
    if(!any(x %in% incomplete)) return(0)
    sum(y[x %in% incomplete])/sum(y)
  }
  
  df = datasets %>% 
    map(~{
      if(!any(tolower(crfstat_col) %in% tolower(names(.x)))) return(tibble())
      .x %>% select(crfstat = any_of2(crfstat_col))
    }) %>% 
    list_rbind(names_to="dataset")
  if(nrow(df)==0) return(NULL)
  
  df = df %>% 
    mutate(crfstat = edf_crfstat_recode(crfstat, do=!details)) %>% 
    count(dataset, crfstat) %>% 
    left_join(lookup, by="dataset") %>% 
    mutate(
      crfstat = factor(crfstat, levels=crfstat_lvls) %>% fct_drop() %>% fct_rev(),
      #arrange by first (eg. complete) then by last (eg. incomplete)
      dataset = fct_reorder2(dataset, crfstat, n, .fun=completion_reorder, 
                             which_lvl=dplyr::first) %>% fct_rev(),
      dataset = fct_reorder2(dataset, crfstat, n, .fun=completion_reorder, 
                             which_lvl=dplyr::last)
    ) %>% 
    arrange(dataset)
  
  df %>%
    mutate(title = glue(x_label) %>% fct_inorder()) %>%
    ggplot(aes(y=title, x=n, fill=fct_rev(crfstat))) +
    geom_col(position=position_fill(reverse=TRUE)) +
    scale_fill_manual(values=pal) +
    scale_x_continuous(labels=label_percent()) +
    labs(x=NULL, y="Dataset", fill="CRF Status") +
    theme_minimal()
}

#' @rdname edc_crf_plot
#' @usage NULL
#' @export
crf_status_plot = edc_crf_plot

#' @rdname edc_crf_plot
#' @export
#' @source `ggsci:::ggsci_db$lancet[["lanonc"]] %>% dput()`
edc_pal_crf = function(){
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
#' @importFrom forcats fct_drop fct_expand fct_recode
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
