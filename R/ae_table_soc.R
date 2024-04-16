
#TODO min_percent=1 -> n minimal for percents ?
#TODO total by arm OK, total total aussi?
#TODO vline dans as_flextable ?

#' Summary tables for AE by SOC
#' 
#' The function `ae_table_soc()` creates a summary table of maximum AE grades for each patient according to term and SOC CTCAE. 
#' The resulting dataframe can be piped to `as_flextable()` to get a nicely formatted flextable.
#' 
#' @param df_ae adverse event table, one row per AE, containing subjid, soc, and grade
#' @param df_enrol enrollment table, one row per patient, containing subjid (and arm if needed)
#' @param subjid name of the patient ID in both `df_ae` and `df_enrol`. Case-insensitive.
#' @param arm name of the treatment column in `df_enrol`. Case-insensitive. Can be set to `NULL` to not group.
#' @param grade  name of the AE grade column in `df_ae`. Case-insensitive.
#' @param soc name of the SOC column in `df_ae`. Case-insensitive. Grade will be considered 0 if missing(e.g. if patient if absent from `df_ae`).
#' @param term name of the the CTCAE term column in `df_ae`. Case-insensitive. Can be set to `NULL`.
#' @param sort_by_ae should the table be sorted by number or alphabetically
#' @param total whether to add a `total` column for each arm
#' @param digits significant digits for percentages
#' @param warn_miss whether to warn for missing values
#'
#' @return a dataframe (`ae_table_soc()`) or a flextable (`as_flextable()`).
#' 
#' @seealso [ae_table_grade_max()], [ae_table_grade_n()], [ae_table_soc()], [ae_plot_grade_max()], [ae_plot_grade_n()]
#' 
#' @export
#'
#' @examples
#' 
#' tm = edc_example_ae()
#' ae_table_soc(df_ae=tm$ae, df_enrol=tm$enrolres, term=NULL)
#' 
#' \dontrun{
#' #the resulting flextable can be customized using the flextable package
#' library(flextable)
#' ae_table_soc(tm$ae, df_enrol=tm$enrolres, total=FALSE) %>% 
#'   as_flextable() %>% 
#'   hline(i=~soc=="" & soc!=dplyr::lead(soc))
#' ae_table_soc(tm$ae, df_enrol=tm$enrolres, term=NULL, sort_by_ae=FALSE) %>% 
#'   as_flextable() %>% 
#'   hline()
#' ae_table_soc(tm$ae, df_enrol=tm$enrolres, term=NULL, arm=NULL) %>% 
#'   as_flextable()
#' }
#' @importFrom cli cli_abort cli_warn
#' @importFrom dplyr any_of arrange bind_rows count desc filter full_join if_else lag left_join mutate pull rename_with select summarise transmute
#' @importFrom forcats fct_relabel
#' @importFrom glue glue
#' @importFrom purrr discard iwalk keep map
#' @importFrom rlang ensym is_empty set_names
#' @importFrom stringr str_replace
#' @importFrom tibble deframe lst
#' @importFrom tidyr pivot_wider
ae_table_soc = function(
    df_ae, df_enrol, 
    arm="ARM", term="AETERM", soc="AESOC", grade="AEGR", subjid="SUBJID",
    sort_by_ae=TRUE, total=TRUE, digits=0, warn_miss=FALSE
){
  
  null_term = is.null(term)
  null_arm = is.null(arm)
  
  # browser()
  not_found1 = lst(term, soc, grade, subjid) %>% discard(is.null) %>% discard(~tolower(.x) %in% tolower(names(df_ae)))
  not_found2 = lst(arm, subjid) %>% discard(is.null) %>% discard(~tolower(.x) %in% tolower(names(df_enrol)))
  not_found = c(not_found1, not_found2)
  if(length(not_found)>0){
    a = paste0(names(not_found), "='", not_found, "'")
    cli_abort("AE columns not found in {.arg df}: {.val {a}}",
              class="edc_ae_cols_notfound_error")
  }
  
  df_ae = df_ae %>% rename_with(tolower) %>%
    select(subjid=tolower(subjid), soc=tolower(soc), term=tolower(term), grade=tolower(grade))
  df_enrol = df_enrol %>% rename_with(tolower) %>%
    select(subjid=tolower(subjid), arm=tolower(arm)) 
  df = df_enrol %>%
    full_join(df_ae, by=tolower(subjid)) %>% 
    filter(!is.na(soc))  %>% 
    arrange(subjid)
  
  #check missing data
  if(warn_miss){
    miss = names(df) %>% set_names() %>% map(~{
      df %>% filter(is.na(!!ensym(.x))) %>% pull(subjid) %>% unique() %>% sort()
    }) %>% keep(~!is_empty(.x))
    miss %>% iwalk(~{
      cli_warn("{.fn ae_table_soc}: Missing values in column {.val {.y}} for patients {.val {.x}}.",
               class="edc_ae_missing_values_warning")
    })
  }
  
  max_na = function(x, na.rm=TRUE) if(all(is.na(x))) NA else max(x, na.rm=na.rm)
  df = df %>% 
    summarise(grade=max_na(grade), 
              .by=c(subjid, arm, soc, term))
  
  rtn = df %>% count(arm, soc, term, grade=as.character(grade))
  
  if(total){
    rtn = rtn %>% 
      bind_rows(
        count(df, arm, soc, term, grade="Tot")
      )
  }
  
  if(!null_arm){
    n_patients = count(df_enrol, arm, name="n_arm")
    rtn = rtn %>% left_join(n_patients, by="arm")
    header = n_patients %>% 
      transmute(name=edc_make_clean_name(arm),
                value=glue("{arm} (N={n_arm})") %>% as.character()) %>% 
      deframe()
  } else {
    n_patients = nrow(df_enrol)
    rtn = rtn %>% mutate(arm="all", n_arm=n_patients)
    header = glue("All patients (N={n_patients})") %>% set_names("all")
  }
  
  rtn =
    rtn %>% 
    arrange(arm, soc, term, grade) %>% 
    mutate(
      n_soc=sum(n[grade!="Tot"], na.rm=TRUE),
      .by=soc
    ) %>% 
    mutate(
      n_term=sum(n[grade!="Tot"], na.rm=TRUE),
      .by=term
    ) %>% 
    mutate(
      n2 = glue("{n} ({round(100*n/n_arm,digits)}%)"),
      .by=arm,
    ) %>% 
    mutate(
      soc = as.character(soc),
      grade2 = paste0(fct_relabel(arm, janitor::make_clean_names), "_G", grade),
      grade2 = grade2 %>% str_replace("_GNA", "_NA") %>% str_replace("_GTot", "_Tot")
    ) %>% 
    arrange(grade2) %>%
    select(-arm, -grade, -n, -n_arm) %>% 
    pivot_wider(id_cols=c("soc", if(!null_term) c("term", "n_term"), "n_soc"), 
                names_from="grade2", values_from="n2") %>% 
    arrange(soc)
  # browser()
  
  if(sort_by_ae){
    rtn = rtn %>% arrange(desc(n_soc), if(!null_term) desc(n_term))
  }
  rtn = rtn %>% 
    select(-n_soc, -any_of("n_term")) %>% 
    mutate(
      soc=if_else(!is.na(lag(soc)) & soc==lag(soc), "", soc),
    )
  
  class(rtn) = c("ae_table_soc", class(rtn))
  attr(rtn, "header") = header
  rtn
}

# https://coolors.co/palette/dbe5f1-b8cce4-f2dcdb-e5b9b7-ebf1dd-d7e3bc-e5e0ec-ccc1d9-dbeef3-b7dde8
#' Turns an `ae_table_soc` object into a formatted `flextable`
#'
#' @param x a dataframe, resulting of `ae_table_soc()`
#' @param arm_colors colors for the arm groups
#'
#' @return a formatted flextable
#' @rdname ae_table_soc
#' @exportS3Method flextable::as_flextable

#' @importFrom dplyr lag
#' @importFrom purrr map map_int
#' @importFrom rlang set_names
#' @importFrom stringr str_detect str_replace_all str_starts
as_flextable.ae_table_soc = function(x, arm_colors=c("#f2dcdb", "#dbe5f1", "#ebf1dd", "#e5e0ec")
){
  table_ae_header = attr(x, "header")
  arm_cols = names(table_ae_header) %>% set_names() %>% map_int(~sum(str_starts(names(x), .x)))
  
  col1 = min(which(str_detect(names(x), "G1"))) - 1 #moche mais marche...
  colwidths = c(col1, arm_cols)
  header_labels = set_names(names(x)) %>% map(~str_replace_all(.x, ".*_", ""))
  header_labels$soc = "CTCAE SOC"
  header_labels$term = "CTCAE v4.0 Term"
  
  rtn = x %>%
    flextable::flextable() %>%
    flextable::set_header_labels(values=header_labels) %>%
    flextable::add_header_row(values=c(" ", table_ae_header), colwidths = colwidths) %>%
    flextable::align(i=1, part="header", align="center") %>%
    flextable::align(j=seq(col1), part="all", align="right") %>%
    flextable::padding(padding.top=0, padding.bottom=0) %>%
    flextable::autofit() %>% 
    flextable::fontsize(size=8, part="all") %>%
    flextable::bold(part="header")
  
  a = cumsum(colwidths)[-1]
  for(i in seq_along(a)){
    from = lag(a, default=col1)[i] + 1
    to = a[i]
    rtn = rtn %>% flextable::bg(j=seq(from, to), bg = arm_colors[i], part="all")
  }
  
  rtn
}

