


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
#' @importFrom dplyr arrange case_when desc distinct filter first last mutate n_distinct select setdiff summarise
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
#' waterfall_plot(rc, rc_date="RCDT", rc_sum="RCTLSUM", rc_resp="RCRESP", type="worst_resp")
#' rc %>% 
#'   left_join(arm, by="SUBJID") %>%
#'   mutate(star = any(RCNEW=="1-Yes", na.rm=TRUE), .by=SUBJID) %>% 
#'   waterfall_plot(rc_date="RCDT", rc_sum="RCTLSUM", rc_resp="RCRESP", arm="ARM", rc_star="star")
#'}
waterfall_plot = function(data_recist, rc_sum="RCTLSUM", rc_resp="RCRESP", rc_date="RCDT",
                          type = c("best_resp", "worst_resp"), 
                          rc_star=NULL, arm=NULL, warn_missing=TRUE) {
  type = match.arg(type)
  assert_class(data_recist, class="data.frame")
  assert_class(rc_sum, class="character")
  assert_class(rc_resp, class="character")
  assert_class(rc_date, class="character")
  assert_class(rc_star, class="character")
  assert_class(arm, class="character")
  assert_class(warn_missing, class="logical")
  subjid = get_subjid_cols()
  responses = c("Complete response"="#42B540FF", "Partial response"="#006dd8", 
                "Stable disease"="#925E9F", "Progressive disease"="#ED0000", "Missing"="white")
  
  if (type == "best_resp"){
    fun = min_narm
    miss_resp_infinite = +Inf
  } else{
    fun = max_narm
    miss_resp_infinite = -Inf
  }
  
  db_wf = data_recist %>% 
    select(subjid=any_of2(subjid), resp=any_of2(rc_resp), sum=any_of2(rc_sum), 
           date=any_of2(rc_date),
           rc_star=any_of2(rc_star), 
           arm=any_of2(arm), 
           )
  
  db_wf %>% 
    filter(is.na(sum)) %>% 
    edc_data_warn("Rows with missing target lesions length sum were ignored.", issue_n=FALSE)
  db_wf %>% 
    filter(is.na(date)) %>% 
    edc_data_warn("Rows with missing target evaluation date were ignored.", issue_n=FALSE)
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
      resp_num = case_when(
        resp=="CR" | str_detect(resp, "(?i)complete") ~ 1,
        resp=="PR" | str_detect(resp, "(?i)partial")  ~ 2,
        resp=="SD" | str_detect(resp, "(?i)stable")   ~ 3,
        resp=="PD" | str_detect(resp, "(?i)progr")    ~ 4,
        is.na(resp) | str_detect(resp, "(?i)not [eval|avai]") ~ miss_resp_infinite,
        .default=-99,
      ),
      resp2 = names(responses)[replace_na(resp_num, 5)],
      resp2 = factor(resp2, levels=names(responses)),
    ) %>% 
    filter(resp_num==fun(resp_num), .by=subjid) %>% 
    filter(sum==fun(sum), .by=c(subjid, resp_num)) %>% 
    filter(date==min_narm(date), .by=c(subjid, resp_num)) %>% 
    # complete(subjid=db_wf$subjid) %>% 
    assert_no_duplicate() %>% 
    mutate(
      diff_first = (sum - first_sum)/first_sum,
      diff_min = (sum - min_sum)/min_sum
    )  %>% 
    arrange(desc(diff_first), resp2) %>% 
    mutate(subjid = as_factor(as.character(subjid)))
  
  db_wf2 %>% 
    filter(resp_num==-99) %>% 
    edc_data_warn("Internal error, waterfall plot may be slightly irrelevant. Code=resp_num_error", 
                  issue_n=FALSE)

  #TODO gérer les missings selon ce qu'on prend comme data dans la macro
  # missings = db_wf %>% summarise(across(-subjid, anyNA)) %>% unlist()
  # missings2 = data_recist %>% summarise(across(-subjid, anyNA)) %>% unlist()
  # if(any(missings) & warn_missing) {
  #   cli_warn(c("Missing values, the waterfall plot will be incomplete."))
  # }
  
  star_layer = NULL
  if(!is.null(rc_star)){
    star_nudge = 0.05
    db_wf2$star_txt = ifelse(db_wf2$rc_star, "*", "")
    star_layer = geom_text(aes(y=diff_first + sign(diff_first)*star_nudge, label=star_txt))
  }
  
  fill_lab = "Best global response \n(RECIST v1.1)"
  if(type=="worst_resp") fill_lab = "Worst global response \n(RECIST v1.1)"

  p =
  db_wf2 %>%
    ggplot(aes(x=subjid, y=diff_first, group=subjid, fill=resp2)) +
    geom_hline(yintercept=c(-.3, .2), linetype="dashed") +
    geom_col(color='black') +
    star_layer  +
    scale_x_discrete(labels = NULL, breaks = NULL) + 
    scale_y_continuous(labels=label_percent(), breaks=breaks_width(0.2)) +
    scale_fill_manual(values=responses) +
    labs(x = "", y="Percent change from baseline", fill=fill_lab)
  
  if(!missing(arm)) p = p + facet_wrap(~arm, scales="free_x", ncol=1)
  p
}
