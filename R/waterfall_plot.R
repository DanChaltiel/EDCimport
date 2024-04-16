


#' Generate a waterfall plot
#'
#' @param data_recist recist table, one row
#' @param rc_sum name of the target lesions length sum column in `data_recist`, usually "RCTLSUM". 
#' @param rc_resp name of the response column in `data_recist`, usually "RCRESP". 
#' @param rc_star name of the logical column in `data_recist` that triggers the .  Can be set to `NULL`.
#' @param arm name of the treatment column in `data_recist`. Can be set to `NULL` to not group.
#' @param type one of `c("best_resp", "worst_resp")`
#' @param warn_missing whether to warn about missing values
#' @param rc_date unused for now
#' @param timepoint unused for now
#'
#' @return a ggplot
#' @export
#' @importFrom dplyr arrange desc distinct filter first last mutate select summarise
#' @importFrom forcats as_factor
#' @importFrom ggplot2 aes facet_wrap geom_col geom_hline geom_text ggplot labs scale_fill_manual scale_x_discrete scale_y_continuous
#' @importFrom rlang as_label enquo
#' @importFrom scales breaks_width label_percent
#' @importFrom stringr str_detect
#'
#' @examples
#' 
#' \dontrun{
#' waterfall_plot(rc, rc_date=RCDT, rc_sum=RCTLSUM, rc_resp=RCRESP)
#' waterfall_plot(rc_date=RCDT, rc_sum=RCTLSUM, rc_resp=RCRESP, rc_star="RCNEW")
#' rc %>% 
#'   left_join(arm, by="SUBJID") %>%
#'   waterfall_plot(rc_date=RCDT, rc_sum=RCTLSUM, rc_resp=RCRESP, arm=ARM) +
#'   NULL
#'}
waterfall_plot = function(data_recist, rc_sum="RCTLSUM", rc_resp="RCRESP", rc_date="RCDT", 
                          type = c("best_resp", "worst_resp", "timepoint"), timepoint=NULL,
                          rc_star=NULL, arm=NULL, warn_missing=TRUE) {
  subjid = get_subjid_cols()
  armname =  as_label(enquo(arm))

  if(!is.null(timepoint)){
    cli_warn("timepoint not implemented yet")
    timepoint = NULL
  }
  if(is.null(timepoint)) type = setdiff(type, "timepoint")
  
  browser()
  
  
  # cc = c("1-Complete response"="green", "2-Partial response"="blue", "3-Stable disease"="yellow",
  #        "4-Progressive disease"="red", "5-Not evaluable"="white")
  responses = c("Complete response"="green", "Partial response"="blue", 
                "Stable disease"="yellow", "Progressive disease"="red", "Not evaluable"="white")
  
  db_wf = data_recist %>% 
    select(subjid=any_of2(subjid), date={{rc_date}}, sum={{rc_sum}}, 
           resp={{rc_resp}}, star={{rc_star}}, 
           arm=any_of2(armname)) %>% 
    filter(!is.na(sum)) %>%
    # filter(!is.na(resp)) %>%
    distinct() %>% 
    mutate(
      resp2 = case_when(
        resp=="CR" | str_detect(resp, "(?i)complete") ~ "Complete response",
        resp=="PR" | str_detect(resp, "(?i)partial")  ~ "Partial response",
        resp=="SD" | str_detect(resp, "(?i)stable")   ~ "Stable disease",
        resp=="PD" | str_detect(resp, "(?i)progr")    ~ "Progressive disease",
        is.na(resp) | str_detect(resp, "(?i)not [eval|avai]") ~ "Not available",
      ),
      resp_num = case_when(
        resp=="CR" | str_detect(resp, "(?i)complete") ~ 1,
        resp=="PR" | str_detect(resp, "(?i)partial")  ~ 2,
        resp=="SD" | str_detect(resp, "(?i)stable")   ~ 3,
        resp=="PD" | str_detect(resp, "(?i)progr")    ~ 4,
      ),
    ) %>% 
    summarise(
      arm = unify(arm),
      star_txt = ifelse(any(star, na.rm=TRUE), "*", ""),
      first_date = min(date),
      first_sum = first(sum, order_by=date), 
      last_date = max(date),
      last_sum = last(sum, order_by=date), 
      best_resp = names(responses)[min_narm(resp_num)] %>% 
        replace_na("Not evaluable") %>% factor(levels=names(responses)),
      worst_resp = names(responses)[max_narm(resp_num)] %>% 
        replace_na("Not evaluable") %>% factor(levels=names(responses)),
      final_resp = last(resp, order_by=date),
      min_sum = min(sum),
      diff_first = (last_sum - first_sum)/first_sum,
      diff_min = (last_sum - min_sum)/min_sum,
      .by=subjid
    ) %>% 
    # mutate(subjid = fct_reorder2(factor(subjid), final_resp, diff_first, .desc=TRUE)) %>% 
    arrange(desc(diff_first), best_resp) %>% 
    mutate(subjid = as_factor(as.character(subjid)))
  
  #TODO gérer les missings selon ce qu'on prend comme data dans la macro
  # missings = db_wf %>% summarise(across(-subjid, anyNA)) %>% unlist()
  # missings2 = data_recist %>% summarise(across(-subjid, anyNA)) %>% unlist()
  # if(any(missings) & warn_missing) {
  #   cli_warn(c("Missing values, the waterfall plot will be incomplete."))
  # }
  
  # grad = scales::seq_gradient_pal(low="blue", high="red")
  # 
  # grad(length(unique(db_wf$final_resp)))
  # cc = scales::seq_gradient_pal(low="blue", high="red")(length(unique(db_wf$final_resp)))
  # 
  # cc = scales::seq_gradient_pal(low="blue", high="red")((as.numeric(db_wf$final_resp)-1)/4)
  # cc = scales::seq_gradient_pal(low="blue", high="red")(seq(0, 1, length.out=length(levels(db_wf$final_resp))))

  #TODO mettre des couleurs potables
  #TODO abstractiser les niveaux
  # cc = c("1-Complete response"="green", "2-Partial response"="blue", "3-Stable disease"="yellow",
  #        "4-Progressive disease"="red", "5-Not evaluable"="white")


  star_nudge = 0.05
  p =
    db_wf %>%
    ggplot(aes(x=subjid, y=diff_first, group=subjid, fill=best_resp)) +
    geom_hline(yintercept=c(-.3, .2), linetype="dashed") +
    geom_col(color='black') +
    geom_text(aes(y=diff_first + sign(diff_first)*star_nudge, label=star_txt)) +
    scale_x_discrete(labels = NULL, breaks = NULL) + 
    scale_y_continuous(labels=label_percent(), breaks=breaks_width(0.2)) +
    scale_fill_manual(values=responses) +
    labs(x = "", y="Percent change from baseline", fill="Best global response \n(RECIST v1.1)")
  
  # browser()
  if(!missing(arm)) p = p + facet_wrap(~arm, scales="free_x", ncol=1)
  p
}
