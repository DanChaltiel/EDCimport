


#' Generate a waterfall plot
#'
#' @param data_recist recist dataset
#' @param rc_sum name of the target lesions length sum column in `data_recist`, usually "RCTLSUM". 
#' @param rc_resp name of the response column in `data_recist`, usually "RCRESP". 
#' @param rc_date name of the date column in `data_recist`, usually "RCDT". 
#' @param rc_star name of the logical column in `data_recist` that triggers the .  Can be set to `NULL`.
#' @param arm name of the treatment column in `data_recist`. Can be set to `NULL` to not group.
#' @param type one of `c("best_resp", "worst_resp")`
#' @param warn_missing whether to warn about missing values
#'
#' @section Methods: 
#' Data are ordered on `rc_date`. 
#' 
#' 
#' @return a ggplot
#' @export
#' @importFrom cli cli_warn
#' @importFrom dplyr arrange case_when desc distinct filter first last mutate select setdiff summarise
#' @importFrom forcats as_factor
#' @importFrom ggplot2 aes facet_wrap geom_col geom_hline geom_text ggplot labs scale_fill_manual scale_x_discrete scale_y_continuous
#' @importFrom rlang as_label enquo
#' @importFrom scales breaks_width label_percent
#' @importFrom stringr str_detect
#' @importFrom tidyr replace_na
#'
#' @examples
#' 
#' \dontrun{
#' waterfall_plot(rc, rc_date="RCDT", rc_sum="RCTLSUM", rc_resp="RCRESP")
#' waterfall_plot(rc, rc_date="RCDT", rc_sum="RCTLSUM", rc_resp="RCRESP")
#' rc %>% 
#'   left_join(arm, by="SUBJID") %>%
#'   mutate(star = any(RCNEW=="1-Yes", na.rm=TRUE), .by=SUBJID) %>% 
#'   waterfall_plot(rc_date="RCDT", rc_sum="RCTLSUM", rc_resp="RCRESP", arm="ARM", rc_star="star")
#'}
waterfall_plot = function(data_recist, rc_sum="RCTLSUM", rc_resp="RCRESP", rc_date="RCDT",
                          type = c("best_resp", "worst_resp"), 
                          rc_star=NULL, arm=NULL, warn_missing=TRUE) {
  type = match.arg(type)
  subjid = get_subjid_cols()
  responses = c("Complete response"="#42B540FF", "Partial response"="#006dd8", 
                "Stable disease"="#925E9F", "Progressive disease"="#ED0000", "Missing"="white")
  
  if(type=="best_resp") fun = min_narm else fun = max_narm
  
  coalesce_resp_num = function(resp_num, f){
    i = f(resp_num) %>% replace_na(5)
    names(responses)[i]
  }
  
  db_wf = data_recist %>% 
    select(subjid=any_of2(subjid), resp=any_of2(rc_resp), sum=any_of2(rc_sum), 
           date=any_of2(rc_date),
           rc_star=any_of2(rc_star), 
           arm=any_of2(arm), 
           )
  
  if(anyNA(db_wf$sum)){
    cli_warn("Rows with missing target lesions length sum were ignored.")
  }
  if(anyNA(db_wf$date)){
    cli_warn("Rows with missing recist evaluation date were ignored.")
  }
  db_wf %>% 
    filter(date==min(date) & !is.na(resp), .by=subjid) %>% 
    edc_data_warn("Response is not missing at first date", issue_n=FALSE)
  db_wf %>% 
    filter(n_distinct(date)<2, .by=subjid) %>% 
    edc_data_warn("Patients with <2 recist evaluations were ignored.", issue_n=FALSE)

  db_wf2 = db_wf %>% 
    filter(!is.na(sum)) %>%
    filter(!is.na(date)) %>% 
    filter(n_distinct(date)>2, .by=subjid) %>%
    arrange(subjid) %>%
    # filter(subjid==16) %>%
    distinct() %>% 
    mutate(
      first_date = min_narm(date, na.rm=TRUE),
      min_sum = min_narm(sum, na.rm=TRUE),
      max_sum = max_narm(sum, na.rm=TRUE),
      first_sum = sum[date==first_date],
      .by=subjid,
    ) %>% 
    mutate(
      first_date = date==first_date,
      # time = if_else(date==first_date, "baseline", "visit"),
      resp_num = case_when(
        resp=="CR" | str_detect(resp, "(?i)complete") ~ 1,
        resp=="PR" | str_detect(resp, "(?i)partial")  ~ 2,
        resp=="SD" | str_detect(resp, "(?i)stable")   ~ 3,
        resp=="PD" | str_detect(resp, "(?i)progr")    ~ 4,
        is.na(resp) | str_detect(resp, "(?i)not [eval|avai]") ~ NA,
        .default=-99,
      ),
      resp2 = names(responses)[replace_na(resp_num, 5)],
      resp2 = factor(resp2, levels=names(responses)),
      stop("TODO")
      #TODO fun(resp_num) enleve les NA donc pas bien. Idée : resp_num = +/-Inf selon `type`
      # best_resp = coalesce_resp_num(resp_num, min_narm),
      # worst_resp = coalesce_resp_num(resp_num, max_narm),
    ) %>% 
    filter(resp_num==fun(resp_num), .by=subjid) %>% 
    filter(sum==fun(sum), .by=c(subjid, resp_num)) %>% 
    filter(date==min_narm(date), .by=c(subjid, resp_num)) %>% 
    complete(subjid=db_wf$subjid) %>% 
    assert_no_duplicate() %>% 
    mutate(
      star_txt = ifelse(rc_star, "*", ""),
      diff_first = (sum - first_sum)/first_sum,
      diff_min = (sum - min_sum)/min_sum
    )  %>% 
    arrange(desc(diff_first), resp2) %>% 
    mutate(subjid = as_factor(as.character(subjid)))
  # db_wf2 %>% filter(subjid==1)
  # db_wf2 %>% filter(subjid==31)
  
  # db_wf2 %>% 
  #   count(subjid) %>%
  #   filter(n!=2) %>% 
  #   edc_data_warn("Internal error, waterfall plot may be slightly irrelevant. Code=count2", 
  #                 issue_n=FALSE)
  db_wf2 %>% 
    filter(resp_num==-99) %>% 
    edc_data_warn("Internal error, waterfall plot may be slightly irrelevant. Code=resp_num_error", 
                  issue_n=FALSE)
  
  
  
  browser()
  # 1
  db_wf3 = db_wf2 %>% 
    mutate(
      # a = browser(),
      star_txt = ifelse(rc_star, "*", ""),
      
      diff_first = (sum - first_sum)/first_sum,
      diff_min = (sum - min_sum)/min_sum
    )  %>% 
    arrange(desc(diff_first), resp2) %>% 
    mutate(subjid = as_factor(as.character(subjid)))
  
  db_wf3 = db_wf2 %>% 
    summarise(
      # a = browser(),
      arm = unify(arm),
      resp = unify(resp),
      star_txt = ifelse(unify(rc_star), "*", ""),
      
      
      first_sum = sum[first_date],
      current_sum = sum[!first_date],
      last_date = max(date),
      last_sum = last(sum, order_by=date),
      min_sum = min(sum),
      diff_first = (last_sum - first_sum)/first_sum,
      diff_min = (last_sum - min_sum)/min_sum,
      .by=subjid
    ) %>% 
    assert_no_duplicate() %>% 
    # mutate(subjid = fct_reorder2(factor(subjid), final_resp, diff_first, .desc=TRUE)) %>% 
    arrange(desc(diff_first), !!ensym(type)) %>% 
    mutate(subjid = as_factor(as.character(subjid)))
  
  
  setdiff(db_wf$subjid, db_wf3$subjid) %>% length()
  db_wf$subjid %>% unique %>% length()
  db_wf3$subjid %>% unique %>% length()
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

  
  fill_lab = "Best global response \n(RECIST v1.1)"
  if(type=="worst_resp") fill_lab = "Worst global response \n(RECIST v1.1)"

  star_nudge = 0.05
  # p =
  db_wf2 %>%
    # ggplot(aes(x=subjid, y=diff_first, group=subjid, fill=!!ensym(type))) +
    ggplot(aes(x=subjid, y=diff_first, group=subjid, fill=resp2)) +
    geom_hline(yintercept=c(-.3, .2), linetype="dashed") +
    geom_col(color='black') +
    geom_text(aes(y=diff_first + sign(diff_first)*star_nudge, label=star_txt)) +
    scale_x_discrete(labels = NULL, breaks = NULL) + 
    scale_y_continuous(labels=label_percent(), breaks=breaks_width(0.2)) +
    scale_fill_manual(values=responses) +
    labs(x = "", y="Percent change from baseline", fill=fill_lab)
  
  if(!missing(arm)) p = p + facet_wrap(~arm, scales="free_x", ncol=1)
  p
}
