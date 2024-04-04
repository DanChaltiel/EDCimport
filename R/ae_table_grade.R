

# Grade maximum -------------------------------------------------------------------------------


#' Summary tables for AE by grade max
#' 
#' The function `ae_table_grade_max()` creates a summary table of the maximum AE grade experienced per each patient. 
#' The resulting crosstable can be piped to `as_flextable()` to get a nicely formatted flextable.
#' 
#' The function `ae_plot_grade_max()` creates summary plots of maximum AE grades in up to 3 different ways. 
#'
#'
#' @param df input data, one row per event
#' @param arm name of the treatment column in `df`. Case-insensitive. Can be set to `NULL` to not group.
#' @param soc name of the SOC column in `df`. Case-insensitive. Grade will be considered 0 is missing.
#' @param subjid,grade names of the other relevant columns in `df`. Case-insensitive. 
#' @param total whether to add totals
#' @param digits number of signigicant digits for percentages
#'
#' @return a crosstable (dataframe)
#' @export
#'
#' @examples
#' \dontrun{
#' 
#' tm = edc_example_ae()
#' ae_table_grade_max(df_ae=tm$ae, df_enrol=tm$enrolres)
#' ae_plot_grade_max(df_ae=tm$ae, df_enrol=tm$enrolres)
#' 
#' # 1) Apply table functions
#' #you can use as_flextable() to get an HTML flextable
#' #you can use modificators modificators from the flextable package
#' ae_table_grade_max(df_ae=ae, df_enrol=enrolres, arm=NULL) %>% 
#'   add_footer_lines("Percentages are given as the proportion of patients presenting at most one AE of given grade")
#' ae_table_grade_max(df_ae=ae, df_enrol=enrolres) %>%
#'   as_flextable(by_header="Both arms") %>% 
#'   highlight(i=~variable=="Grade 5", j=-1)
#'   
#' # 2) Apply plot functions
#' #you can choose the type
#' #you can use modificators from the patchwork package, like "&"
#' ae_plot_grade_max(df_ae=ae, df_enrol=enrolres) & labs(fill="Group")
#' ae_plot_grade_max(df_ae=ae, df_enrol=enrolres, type=c("dodge", "fill"))
#' ae_plot_grade_max(df_ae=ae, df_enrol=enrolres, arm=NULL) + coord_flip()
#' }
#' @importFrom crosstable apply_labels crosstable
#' @importFrom dplyr arrange full_join mutate rename_with select summarise
ae_table_grade_max = function(
    df_ae, df_enrol, 
    arm="ARM", subjid="SUBJID", soc="AESOC", grade="AEGR", total=TRUE, digits=0
){
  
  
  df_ae = df_ae %>% rename_with(tolower) %>%
    select(subjid=tolower(subjid), soc=tolower(soc), grade=tolower(grade))
  df = df_enrol %>% rename_with(tolower) %>%
    select(subjid=tolower(subjid), arm=tolower(arm)) %>%
    full_join(df_ae, by=tolower(subjid)) %>% 
    arrange(subjid) %>% 
    mutate(grade = ifelse(is.na(soc), 0, grade))
  
  df %>% 
    summarise(grade_max = max_narm(grade), .by=c(subjid, arm)) %>% 
    mutate(grade_max = ifelse(is.na(grade_max), "NA", paste("Grade", grade_max))) %>% 
    crosstable::apply_labels(grade_max = "Max grade") %>% 
    crosstable::crosstable(grade_max, by=arm, total=total, percent_digits=digits, margin="col") 
}

#' @rdname ae_table_grade_max
#' @return a patchwork of ggplots
#' @importFrom dplyr arrange full_join mutate rename_with select summarise
#' @importFrom ggplot2 aes geom_bar ggplot labs scale_x_continuous theme waiver
#' @importFrom patchwork plot_layout wrap_plots
#' @importFrom purrr map
#' @importFrom rlang set_names
#' @export
ae_plot_grade_max = function(
    df_ae, df_enrol, type = c("stack", "dodge", "fill"),
    arm="ARM", subjid="SUBJID", soc="AESOC", grade="AEGR"
){
  
  df_ae = df_ae %>% rename_with(tolower) %>%
    select(subjid=tolower(subjid), soc=tolower(soc), grade=tolower(grade)) 
  x = df_enrol %>% rename_with(tolower) %>%
    select(subjid=tolower(subjid), arm=tolower(arm)) %>%
    full_join(df_ae, by=tolower(subjid)) %>% 
    arrange(subjid) %>% 
    mutate(grade = ifelse(is.na(soc), 0, grade)) %>% 
    summarise(grade_max = max_narm(grade), .by=c(subjid, arm)) %>% 
    mutate(grade_max = ifelse(is.na(grade_max), "NA", paste("Grade", grade_max)))
  if(is.null(arm)) type="stack"
  p_list = type %>% set_names() %>% 
    map(~{
      y_lab = if(.x=="fill") "Proportion" else "Count"
      p = x %>% 
        ggplot(aes(y=grade_max, fill=arm, by=factor(grade_max))) +
        geom_bar(position=.x) +
        scale_x_continuous(labels = if(.x=="fill") scales::percent else waiver()) +
        labs(y="Max AE grade experienced", x=y_lab, fill="Treatment")
      # StatProp = ggstats:::StatProp
      # if(.x=="fill") p = 
      #   p + geom_text(stat="prop", position = position_fill(.5))
      p
    })
  
  patchwork::wrap_plots(p_list) + 
    patchwork::plot_layout(guides="collect") & 
    theme(legend.position="top")
  
}
# Nb of grades --------------------------------------------------------------------------------


#' Title
#' 
#' @param df_ae adverse event table, one row per AE, containing subjid, soc, and grade
#' @param df_enrol enrollment table, one row per patient, containing subjid (and arm if needed)
#' @inheritParams ae_table_grade_max
#'
#' @return a crosstable
#' @importFrom crosstable crosstable format_fixed get_percent_pattern
#' @importFrom dplyr across arrange count cur_column distinct filter full_join mutate rename_with select
#' @importFrom rlang int
#' @importFrom tibble deframe
#' @export
#'
#' @examples
#' \dontrun{
#' library(flextable)
#' tm = edc_example_ae()
#' 
#' ae_table_grade_n(df_ae=tm$ae, df_enrol=tm$enrolres) %>% 
#'   as_flextable() %>% 
#'   add_footer_lines("¹ Percentages are given as the proportion of patients presenting at least one AE of given grade")
#' 
#' ae_table_grade_n(df_ae=ae, df_enrol=enrolres, arm=NULL) %>% 
#'   af(by_header=F) %>% 
#'   set_header_labels(values=c("","N (%)")) 
#'   
#' #To get SAE only, filter df_ae first
#' ae %>% filter(AESER=="1-Yes") %>% ae_table_grade_n(df_enrol=enrolres, arm=NULL)
#' }
ae_table_grade_n = function(
    df_ae, df_enrol, 
    arm="ARM", grade="AEGR", subjid="SUBJID", soc="AESOC",
    total=FALSE, digits=0
){
  df_ae = df_ae %>% rename_with(tolower) %>%
    select(subjid=tolower(subjid), soc=tolower(soc), grade=tolower(grade))
  df_enrol = df_enrol %>% rename_with(tolower) %>%
    select(subjid=tolower(subjid), arm=tolower(arm))
  df = df_enrol %>%
    full_join(df_ae, by=tolower(subjid)) %>% 
    arrange(subjid) %>% 
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
      p = format_fixed(x/tot, digits, percent=TRUE)
      paste0(x, " (", p, ")")
    }))
  attr(rtn, "by_table")[] = npat[names(npat)!="Total"]
  rtn
}
