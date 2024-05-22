

# Grade maximum -------------------------------------------------------------------------------


#' Summary tables for AE by grade max
#' 
#' The function `ae_table_grade_max()` creates a summary table of the maximum AE grade experienced per each patient. 
#' The resulting crosstable can be piped to `as_flextable()` to get a nicely formatted flextable.
#' 
#' @inheritParams ae_table_soc
#' @inherit ae_table_soc seealso
#'
#' @return a crosstable (dataframe)
#' @export
#' @importFrom dplyr arrange full_join mutate rename_with select summarise
#'
#' @examples
#' 
#' tm = edc_example_ae()
#' ae_table_grade_max(df_ae=tm$ae, df_enrol=tm$enrolres)
#' ae_table_grade_max(df_ae=tm$ae, df_enrol=tm$enrolres, arm=NULL)
#' 
#' \dontrun{
#' #you can use as_flextable() to get an HTML flextable
#' #you can use modificators modificators from the flextable package
#' library(flextable)
#' ae_table_grade_max(df_ae=tm$ae, df_enrol=tm$enrolres, arm=NULL) %>% 
#'   as_flextable() %>% 
#'   add_footer_lines("Percentages are given as the proportion of patients 
#'                     presenting at most one AE of given grade")
#' ae_table_grade_max(df_ae=tm$ae, df_enrol=tm$enrolres) %>%
#'   as_flextable(by_header="Both arms") %>% 
#'   highlight(i=~variable=="Grade 5", j=-1)
#' }
ae_table_grade_max = function(
    df_ae, ..., df_enrol, 
    arm="ARM", subjid="SUBJID", soc="AESOC", grade="AEGR", total=TRUE, digits=0
){
  check_installed("crosstable", "for `ae_table_grade_max()` to work")
  check_dots_empty()
  null_arm = is.null(arm)
  
  df_ae = df_ae %>% 
    select(subjid_=any_of2(subjid), soc_=any_of2(soc), grade_=any_of2(grade))
  df = df_enrol %>% 
    select(subjid_=any_of2(subjid), arm_=any_of2(arm)) %>%
    full_join(df_ae, by="subjid_") %>% 
    arrange(subjid_) %>% 
    mutate(
      grade_ = if_else(is.na(soc_), 0, fix_grade(grade_)),
    ) %>% 
    summarise(grade_max = max_narm(grade_), .by=any_of(c("subjid_", "arm_")))
  
  df %>% 
    mutate(grade_max = ifelse(is.na(grade_max), "NA", paste("Grade", grade_max)),
           grade_max = recode(grade_max, "Grade 0"="No AE")) %>% 
    crosstable::apply_labels(grade_max = "Max grade") %>% 
    crosstable::crosstable(grade_max, by=any_of("arm_"), total=total, 
                           percent_digits=digits, margin="col") 
}

#' Graphic representation of AEs by grade max
#'
#' Produces a graphic representation of AE, counting the maximum grade each patient experienced, colored by treatment arm. Returns up to 3 representations if `arm!=NULL`.
#' 
#' @inheritParams ae_table_soc 
#' @inherit ae_table_soc seealso
#' @param type the plots to be included. One of `c("stack", "dodge", "fill")`.
#' @param drop_levels whether to drop unused grade levels.
#'
#' @return a patchwork of ggplots
#' @importFrom dplyr arrange full_join mutate rename_with select summarise
#' @importFrom ggplot2 aes geom_bar ggplot labs scale_x_continuous scale_y_discrete theme waiver
#' @importFrom purrr map
#' @importFrom rlang set_names
#' @export
#' 
#' @examples
#' tm = edc_example_ae()
#' ae_plot_grade_max(df_ae=tm$ae, df_enrol=tm$enrolres)
#' ae_plot_grade_max(df_ae=tm$ae, df_enrol=tm$enrolres, type=c("dodge", "fill"), proportion=FALSE)
#' ae_plot_grade_max(df_ae=tm$ae, df_enrol=tm$enrolres, arm=NULL) + ggplot2::coord_flip()
#' 
#' #you can use modificators from the patchwork package, like "&"
#' \dontrun{
#' library(patchwork)
#' ae_plot_grade_max(df_ae=tm$ae, df_enrol=tm$enrolres) & labs(fill="Group")
#' }
ae_plot_grade_max = function(
    df_ae, ..., df_enrol, 
    type = c("stack", "dodge", "fill"),
    proportion = TRUE,
    drop_levels = FALSE,
    arm="ARM", subjid="SUBJID", soc="AESOC", grade="AEGR"
){
  check_installed("patchwork", "for `ae_plot_grade_max()` to work")
  check_dots_empty()
  
  df_ae = df_ae %>% 
    select(subjid=any_of2(subjid), soc=any_of2(soc), grade=any_of2(grade)) 
  a = df_enrol %>% 
    select(subjid=any_of2(subjid), arm=any_of2(arm)) %>%
    full_join(df_ae, by="subjid") %>% 
    arrange(subjid) %>% 
    mutate(grade = ifelse(is.na(soc), 0, fix_grade(grade)))
  
  by_cols = if(is.null(arm)) "subjid" else c("subjid", "arm")
  x = a %>% 
    summarise(grade_max = max_narm(grade), .by=any_of(by_cols)) %>%  
    mutate(grade_max = factor(grade_max, levels=0:5, labels=c("No AE", paste("Grade", 1:5))))
  
  if(isTRUE(proportion)){
    type = setdiff(type, "fill")
    x2 = x %>% 
      mutate(n_arm = n(), .by=arm) %>% 
      summarise(n=n(), p=n()/n_arm, 
                .by=c(grade_max, arm)) %>% 
      distinct() %>% 
      mutate(label = paste0("N=",n))
    p_list = type %>% set_names() %>% 
      map(~{
        if(.x=="dodge") .x = position_dodge(width=0.9)
        p =
          x2 %>% 
          ggplot(aes(y=grade_max, x=p, fill=arm, by=grade_max)) +
          geom_col(position=.x) +
          # geom_text(aes(label=label), position=.x, hjust=1) +
          scale_y_discrete(drop=drop_levels) +
          scale_x_continuous(labels=scales::percent) +
          labs(y="Max AE grade experienced", x="Proportion of patients", fill="Treatment")
        # StatProp = ggstats:::StatProp
        # if(.x=="fill") p = 
        #   p + geom_text(stat="prop", position = position_fill(.5))
        p
      })
  } else {
    if(is.null(arm)) type="stack"
    p_list = type %>% set_names() %>% 
      map(~{
        y_lab = if(.x=="fill") "Proportion" else "Count"
        p =
          x %>% 
          ggplot(aes(y=grade_max, fill=arm, by=grade_max)) +
          geom_bar(position=.x) +
          scale_y_discrete(drop=drop_levels) +
          scale_x_continuous(labels = if(.x=="fill") scales::percent else waiver()) +
          labs(y="Max AE grade experienced", x=y_lab, fill="Treatment")
        # StatProp = ggstats:::StatProp
        # if(.x=="fill") p = 
        #   p + geom_text(stat="prop", position = position_fill(.5))
        p
      })
  }
  
  
  
  
  patchwork::wrap_plots(p_list) + 
    patchwork::plot_layout(guides="collect") & 
    theme(legend.position="top")
  
}
# Nb of grades --------------------------------------------------------------------------------


#' Summary tables for AE
#' 
#' @inheritParams ae_table_soc
#' @inherit ae_table_soc seealso
#'
#' @return a crosstable
#' @importFrom dplyr across arrange count cur_column distinct filter full_join mutate rename_with select
#' @importFrom rlang int
#' @importFrom tibble deframe
#' @export
#'
#' @examples
#' \dontrun{
#' tm = edc_example_ae()
#' 
#' ae_table_grade_n(df_ae=tm$ae, df_enrol=tm$enrolres) %>% 
#'   as_flextable() %>% 
#'   flextable::add_footer_lines("Percentages are given as the proportion of patients 
#'                                presenting at least one AE of given grade")
#' 
#' ae_table_grade_n(df_ae=tm$ae, df_enrol=tm$enrolres, arm=NULL) %>% 
#'   as_flextable(by_header=F) %>% 
#'   flextable::set_header_labels(values=c("","","N (%)")) 
#'   
#' #To get SAE only, filter df_ae first
#' tm$ae %>% filter(sae==TRUE) %>% ae_table_grade_n(df_enrol=tm$enrolres, arm=NULL)
#' }
ae_table_grade_n = function(
    df_ae, ..., df_enrol, 
    arm="ARM", grade="AEGR", subjid="SUBJID", soc="AESOC",
    total=FALSE, digits=0
){
  check_installed("crosstable", "for `ae_table_grade_n()` to work")
  check_dots_empty()
  
  df_ae = df_ae %>% rename_with(tolower) %>%
    select(subjid=tolower(subjid), soc=tolower(soc), grade=tolower(grade))
  df_enrol = df_enrol %>% rename_with(tolower) %>%
    select(subjid=tolower(subjid), arm=tolower(arm))
  df = df_enrol %>%
    full_join(df_ae, by=tolower(subjid)) %>% 
    arrange(subjid) %>% 
    mutate(grade = fix_grade(grade)) %>% 
    filter(!is.na(soc)) 
  
  default_arm = "All patients" 
  # `:=` = rlang::`:=`
  # browser()
  npat = rlang::int(!!default_arm:=nrow(df_enrol)) 
  if(!is.null(arm)){
    npat = deframe(count(df_enrol, arm))
    npat["Total"] = sum(npat)
  }
  total = if(total) "row" else FALSE 
  
  if(!any(names(df)=="arm")) df$arm=default_arm %>% set_label("Treatment arm")
  rtn = df %>% 
    distinct(subjid, arm, grade) %>% 
    mutate(arm) %>%
    mutate(grade = ifelse(is.na(grade), "NA", paste("Grade", grade)) %>% copy_label_from(grade)) %>% 
    crosstable::crosstable(grade, by=arm, total=total,
               percent_pattern=crosstable::get_percent_pattern("none")) %>% 
    mutate(across(-(.id:variable), function(x){
      x = as.numeric(x)
      tot = npat[cur_column()]
      p = crosstable::format_fixed(x/tot, digits, percent=TRUE)
      paste0(x, " (", p, ")")
    }))
  attr(rtn, "by_table")[] = npat[!is.na(names(npat)) & names(npat)!="Total"] #zarb que crosstable oublie les NA dans by_table non?
  rtn
}


#' Graphic representation of AEs
#' 
#' Produce a graphic representation of AE, counting AE as bars for each patient, colored by grade. Can be faceted by treatment arm.
#'
#' @param low the color of Grade 1 AE
#' @param high the color of Grade 5 AE
#' @inheritParams ae_table_soc 
#' @inherit ae_table_soc seealso
#'
#' @return a ggplot
#' @export
#' @importFrom ggplot2 scale_fill_steps theme element_blank geom_col labs facet_grid vars 
#'
#' @examples
#' tm = edc_example_ae()
#' ae_plot_grade_n(df_ae=tm$ae, df_enrol=tm$enrolres)
#' ae_plot_grade_n(df_ae=tm$ae, df_enrol=tm$enrolres, arm=NULL)
ae_plot_grade_n = function(
    df_ae, ..., df_enrol, 
    low="#ffc425", high="#d11141", 
    arm="ARM", grade="AEGR", subjid="SUBJID"
){
  check_dots_empty()
  df_ae = df_ae %>% rename_with(tolower) %>%
    select(subjid=tolower(subjid), grade=tolower(grade)) 
    
  df_enrol = df_enrol %>% rename_with(tolower) %>%
    select(subjid=tolower(subjid), arm=tolower(arm))
  df = df_enrol %>%
    full_join(df_ae, by=tolower(subjid)) %>% 
    mutate(grade = fix_grade(grade)) %>% 
    arrange(subjid)
  
  default_arm = "All patients"
  npat = rlang::int(!!default_arm:=nrow(df_enrol)) 
  if(!is.null(arm)){
    npat = deframe(count(df_enrol, arm))
    npat["Total"] = sum(npat)
  }

  # if(!any(names(df)=="arm")) df$arm=default_arm %>% set_label("Treatment arm")
  
  # browser()
  rtn =
    df %>% 
    mutate(subjid = forcats::fct_infreq(factor(subjid))) %>% 
    count(across(c(subjid, grade, any_of("arm")))) %>% 
      mutate(n = ifelse(is.na(grade), 0.1, n)) %>% 
    # ggplot(aes(x=subjid, y=n, fill=fct_rev(factor(grade)))) + geom_col() +
    ggplot(aes(x=subjid, y=n, fill=grade)) + geom_col() + 
    scale_fill_steps(low=low, high=high) +
    theme(axis.text.x=element_blank(),
          axis.ticks.x=element_blank()) +
    labs(x="Patient", y="Count", fill="AE grade")
    
    
  if(!is.null(arm)) rtn = rtn + facet_grid(cols=vars(arm), scales="free_x")
  
  rtn
}



# Utils ---------------------------------------------------------------------------------------

fix_grade = function(x){
  as.numeric(na_if(as.character(x), "NA"))
}
