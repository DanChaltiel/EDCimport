


#' Generate a waterfall plot
#'
#' @param data_recist 
#' @param rc_date 
#' @param rc_sum 
#' @param rc_resp 
#' @param arm 
#' @param warn_missing 
#'
#' @return
#' @export
#'
#' @examples
#' waterfall_plot(rc, rc_date=RCDT, rc_sum=RCTLSUM, rc_resp=RCRESP)
#' rc %>% 
#'   left_join(arm, by="SUBJID") %>%
#'   waterfall_plot(rc_date=RCDT, rc_sum=RCTLSUM, rc_resp=RCRESP, arm=ARM) +
#'   NULL
waterfall_plot = function(data_recist, rc_date, rc_sum, rc_resp, arm=NULL, warn_missing=TRUE) {
  subjid = get_key_cols()$patient_id
  armname =  as_label(enquo(arm))
  
  data_recist = data_recist %>% 
    select(subjid=!!sym(subjid), date={{rc_date}}, sum={{rc_sum}}, resp={{rc_resp}}, 
           arm=any_of(armname))
  
  db_wf = data_recist %>%
    filter(!is.na(sum)) %>%
    # filter(!is.na(resp)) %>%
    distinct() %>% 
    summarise(
      arm = unify(arm),
      first_date = min(date),
      first_sum = first(sum, order_by=date), 
      last_date = max(date),
      last_sum = last(sum, order_by=date), 
      final_resp = last(resp, order_by=date),
      min_sum = min(sum),
      diff_first = (last_sum - first_sum)/first_sum,
      diff_min = (last_sum - min_sum)/min_sum,
      .by=subjid
    ) %>% 
    mutate(subjid = fct_reorder(factor(subjid), diff_first, .desc=TRUE))
  
  #TODO gérer les missings selon ce qu'on prend comme data dans la macro
  
  # missings = db_wf %>% summarise(across(-subjid, anyNA)) %>% unlist()
  # missings2 = data_recist %>% summarise(across(-subjid, anyNA)) %>% unlist()
  # if(any(missings) & warn_missing) {
  #   cli_warn(c("Missing values, the waterfall plot will be incomplete."))
  # }
  
  p = db_wf %>%
    ggplot(aes(x=subjid, y=diff_first, group=subjid, fill=final_resp)) +
    geom_hline(yintercept=c(-.3, .2), linetype="dashed") +
    geom_col(color='black') +
    scale_x_discrete(labels = NULL, breaks = NULL) + 
    scale_y_continuous(labels=label_percent(), limits=c(-1, .5)) +
    labs(x = "", y="Percent change from baseline", fill="Final global response \n(RECIST v1.1)")
  
  if(!missing(arm)) p = p + facet_wrap(~arm, scales="free_x", ncol=1)
  p
}
