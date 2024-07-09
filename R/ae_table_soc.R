
#TODO min_percent=1 -> n minimal for percents ?
#TODO total by arm OK, total total aussi?
#TODO vline dans as_flextable ?

#' Summary tables for AE by SOC
#' 
#' The function `ae_table_soc()` creates a summary table of maximum AE grades for each patient according to term and SOC CTCAE. 
#' The resulting dataframe can be piped to `as_flextable()` to get a nicely formatted flextable.
#' 
#' @param df_ae adverse event table, one row per AE, containing subjid, soc, and grade
#' @param df_enrol enrollment table, one row per patient, containing subjid (and arm if needed). All patients should be in this table.
#' @param subjid name of the patient ID in both `df_ae` and `df_enrol`. Case-insensitive.
#' @param arm name of the treatment column in `df_enrol`. Case-insensitive. Can be set to `NULL` to not group.
#' @param grade  name of the AE grade column in `df_ae`. Case-insensitive.
#' @param soc name of the SOC column in `df_ae`. Case-insensitive. Grade will be considered 0 if missing(e.g. if patient if absent from `df_ae`).
#' @param term name of the the CTCAE term column in `df_ae`. Case-insensitive. Can be set to `NULL`.
#' @param sort_by_ae should the table be sorted by number or alphabetically
#' @param total whether to add a `total` column for each arm
#' @param digits significant digits for percentages
#' @param warn_miss whether to warn for missing values
#' @param ... unused
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
#' ae_table_soc(df_ae=tm$ae, df_enrol=tm$enrolres, term=NULL, arm=NULL)
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
#' @importFrom dplyr across any_of arrange bind_rows count desc filter full_join if_else lag left_join mutate pull rename_with select summarise transmute
#' @importFrom forcats fct_relabel
#' @importFrom glue glue
#' @importFrom purrr discard iwalk keep map
#' @importFrom rlang check_dots_empty ensym is_empty set_names
#' @importFrom stringr str_remove str_replace
#' @importFrom tibble deframe lst
#' @importFrom tidyr pivot_wider
ae_table_soc = function(
    df_ae, ..., df_enrol, 
    arm="ARM", term="AETERM", soc="AESOC", grade="AEGR", subjid="SUBJID",
    sort_by_ae=TRUE, total=TRUE, digits=0, warn_miss=FALSE
){
  check_dots_empty()
  null_term = is.null(term)
  null_arm = is.null(arm)
  
  not_found1 = lst(term, soc, grade, subjid) %>% discard(is.null) %>%
    discard(~tolower(.x) %in% tolower(names(df_ae)))
  not_found2 = lst(arm, subjid) %>% discard(is.null) %>% 
    discard(~tolower(.x) %in% tolower(names(df_enrol)))
  not_found = c(not_found1, not_found2)
  if(length(not_found)>0){
    a = paste0(names(not_found), "='", not_found, "'")
    cli_abort("AE columns not found in {.arg df}: {.val {a}}",
              class="edc_ae_cols_notfound_error")
  }
  
  df_ae = df_ae %>% 
    select(subjid_=any_of2(subjid), soc_=any_of2(soc), 
           term_=any_of2(term), grade_=any_of2(grade))
  df_enrol = df_enrol %>% 
    select(subjid_=any_of2(subjid), arm_=any_of2(arm)) 
  df = df_enrol %>%
    full_join(df_ae, by="subjid_") %>% 
    filter(!is.na(soc_))  %>% 
    arrange(subjid_)
  
  #check missing data
  if(warn_miss){
    miss = names(df) %>% set_names() %>% map(~{
      df %>% filter(is.na(!!ensym(.x))) %>% pull(subjid_) %>% unique() %>% sort()
    }) %>% keep(~!is_empty(.x))
    miss %>% iwalk(~{
      cli_warn("{.fn ae_table_soc}: Missing values in column {.val {.y}} for patients {.val {.x}}.",
               class="edc_ae_missing_values_warning")
    })
  }
  
  max_na = function(x, na.rm=TRUE) if(all(is.na(x))) NA else max(x, na.rm=na.rm)
  df = df %>% 
    summarise(grade_=max_na(grade_) %>% as.character(), 
              .by=any_of(c("subjid_", "arm_", "soc_", "term_")))
  
  rtn = df %>% count(across(any_of(c("arm_", "soc_", "term_", "grade_"))))
  # rtn = df %>% count(arm_, soc_, if(!null_term) term_, grade_=as.character(grade_))
  
  if(total){
    rtn = rtn %>% 
      bind_rows(
        count(df, across(any_of(c("arm_", "soc_", "term_"))), grade_="Tot")
        # count(df, arm_, soc_, if(!null_term) term_, grade_="Tot")
      )
  }
  
  if(!null_arm){
    n_patients = count(df_enrol, arm_, name="n_arm")
    rtn = rtn %>% left_join(n_patients, by="arm_")
    header = n_patients %>% 
      transmute(name=edc_make_clean_name(arm_),
                value=glue("{arm_} (N={n_arm})") %>% as.character()) %>% 
      deframe()
  } else {
    n_patients = nrow(df_enrol)
    rtn = rtn %>% mutate(arm_="all", n_arm=n_patients)
    header = glue("All patients (N={n_patients})") %>% set_names("all")
  }
  
  rtn =
    rtn %>% 
    arrange(arm_, soc_, if(!null_term) term_, grade_) %>% 
    mutate(
      n_soc=sum(n[grade_!="Tot"], na.rm=TRUE),
      .by=soc_
    ) %>% 
    mutate(
      n_term=sum(n[grade_!="Tot"], na.rm=TRUE),
      .by=any_of("term_")
    ) %>% 
    mutate(
      n2 = glue("{n} ({round(100*n/n_arm,digits)}%)"),
      .by=any_of("arm_"),
    ) %>% 
    mutate(
      soc_ = as.character(soc_),
      grade2 = paste0(fct_relabel(arm_, edc_make_clean_name), "_G", grade_),
      grade2 = grade2 %>% str_replace("_GNA", "_NA") %>% str_replace("_GTot", "_Tot")
    ) %>% 
    arrange(grade2) %>%
    select(-arm_, -grade_, -n, -n_arm) %>% 
    pivot_wider(id_cols=c("soc_", if(!null_term) c("term_", "n_term"), "n_soc"), 
                names_from="grade2", values_from="n2") %>% 
    arrange(soc_)
  # browser()
  
  if(sort_by_ae){
    rtn = rtn %>% arrange(desc(n_soc), if(!null_term) desc(n_term))
  }
  rtn = rtn %>% 
    select(-n_soc, -any_of("n_term")) %>% 
    rename_with(~str_remove(.x, "_$")) %>% 
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

#' @importFrom dplyr case_match lag lead transmute
#' @importFrom purrr map map_int
#' @importFrom rlang check_installed set_names
#' @importFrom stringr str_detect str_replace_all
#' @importFrom tibble as_tibble_col
#' @importFrom tidyr separate_wider_regex
as_flextable.ae_table_soc = function(x, arm_colors=c("#f2dcdb", "#dbe5f1", "#ebf1dd", "#e5e0ec")
){
  check_installed("flextable")
  table_ae_header = attr(x, "header")
  if(FALSE){
    arm_cols = names(table_ae_header) %>% set_names() %>%
      map_int(~{
        pattern = paste0("^", .x, "_(G\\d|NA|Tot)$")
        sum(str_detect(names(x), pattern))
      })
    table_ae_header = table_ae_header[arm_cols>0]
    arm_cols = arm_cols[arm_cols>0]

    col1 = names(x) %>% str_detect(names(table_ae_header)[1]) %>% which() %>% min() - 1
    colwidths = c(col1, arm_cols)
    header_labels = set_names(names(x)) %>% map(~str_replace_all(.x, ".*_", ""))
    header_labels$soc = "CTCAE SOC"
    header_labels$term = "CTCAE v4.0 Term"
  }
  # https://github.com/tidyverse/tidyr/issues/1551
  header_df = names(x) %>% 
    as_tibble_col("col_keys") %>% 
    separate_wider_regex(col_keys, c(h1 = ".*", "_", h2 = ".*"), too_few="align_start", cols_remove=FALSE) %>% 
    transmute(
      col_keys,
      row1 = case_match(h1, 
                        "soc" ~ "", 
                        "term" ~ "", 
                        .default=table_ae_header[h1]),
      row2 = case_match(h1, 
                        "soc" ~ "CTCAE SOC", 
                        "term" ~ "CTCAE v4.0 Term", 
                        .default=h2)
    )
  
  col1 = header_df$col_keys %in% c("soc", "term") %>% which() %>% max()
  
  sep_cols = with(header_df, !col_keys %in% c("soc", "term") & row1!=lead(row1)) %>% 
    which() %>% unname() %>% c(ncol(x))
  
  rtn = x %>%
    flextable::flextable() %>%
    flextable::set_header_df(mapping=header_df) %>%
    # flextable::hline_top(part="header") %>% 
    flextable::hline_bottom(part="header") %>% 
    flextable::merge_h(part="header") %>%
    # flextable::set_header_labels(values=header_labels) %>%
    # flextable::add_header_row(values=c(" ", table_ae_header), colwidths = colwidths) %>%
    flextable::align(i=1, part="header", align="center") %>%
    flextable::align(j=seq(col1), part="all", align="right") %>%
    flextable::padding(padding.top=0, padding.bottom=0) %>%
    flextable::set_table_properties(layout="autofit") %>% 
    flextable::fontsize(size=8, part="all") %>%
    flextable::bold(part="header")
  
  # a = cumsum(colwidths)[-1]
  a = sep_cols
  for(i in seq_along(a)){
    from = lag(a, default=col1)[i] + 1
    to = a[i]
    rtn = rtn %>% flextable::bg(j=seq(from, to), bg = arm_colors[i], part="all")
  }
  
  rtn
}


#' Graphic representation of AEs by soc (Butterfly plot)
#' 
#' Produces a graphic representation of AE, counting the maximum grade each patient experienced, colored by treatment arm. Returns up to 3 representations if `arm!=NULL`.
#' 
#' The function `ae_plot_soc()` creates a summary table of the maximum AE grade experienced per each patient. 
#' The resulting crosstable can be piped to `as_flextable()` to get a nicely formatted flextable.
#' 
#' @inheritParams ae_table_soc
#' @inherit ae_table_soc seealso
#' @param severe name of the logical column in `df_ae` telling wheter an AE is severe. Case-insensitive. 
#'
#' @return a crosstable (dataframe)
#' @export
#' @importFrom cli cli_abort
#' @importFrom dplyr any_of arrange count filter full_join left_join mutate pull select summarise
#' @importFrom forcats fct_reorder
#' @importFrom ggplot2 aes facet_grid geom_col ggplot labs scale_x_continuous theme unit vars
#' @importFrom glue glue
#' @importFrom rlang check_dots_empty
#' @importFrom scales label_percent
#' @importFrom stats na.omit
#' @importFrom stringr str_remove
#' @importFrom utils head
#'
#' @examples
#' 
#' tm = edc_example_ae()
#' tm$ae %>% 
#'   #dplyr::mutate(severe = aeser=="Yes") %>% 
#'   dplyr::mutate(severe = aegr>=3) %>% 
#'   ae_plot_soc(df_enrol=tm$enrolres)
ae_plot_soc = function(
    df_ae, ..., df_enrol, 
    arm="ARM", subjid="SUBJID", soc="AESOC", severe="SEVERE"
){
  check_dots_empty()
  df_ae = df_ae %>% 
    select(subjid_=any_of2(subjid), soc_=any_of2(soc), severe_=any_of2(severe))
  df_enrol = df_enrol %>% 
    select(subjid_=any_of2(subjid), arm_=any_of2(arm)) 
  df = df_enrol %>%
    full_join(df_ae, by="subjid_") %>% 
    filter(!is.na(soc_))  %>% 
    arrange(subjid_)
  
  arms = df$arm_ %>% unique() %>% na.omit()
  if(length(arms)!=2){
    cli_abort(c("{.fn EDCimport::ae_plot_soc} needs exactly 2 arms.", 
                i="Arms: {.val {arms}}"))
  }
  if(!is.logical(df_ae$severe_)){
    cli_abort(c("{.arg severe} should be a logical column, not a {.type {df_ae$severe_}}. Did you forget to mutate it with `==`?"))
  }
  
  df_arm = df_enrol %>% 
    count(arm_, name="n_arm") %>% 
    mutate(label=glue("{arm_} (N={n_arm})"))
  
  # left_arm = df_arm %>% dplyr::slice_min(n_arm, n=1) %>% pull(arm_)
  left_arm = df_enrol %>% pull(arm_) %>% factor() %>% levels() %>% head(1)
  
  a = df %>% 
    summarise(any_ae = TRUE, 
              any_severe = any(severe_, na.rm=TRUE),
              .by=any_of(c("subjid_", "arm_", "soc_"))) %>% 
    summarise(n_ae = sum(any_ae, na.rm=TRUE), 
              n_severe = sum(any_severe, na.rm=TRUE),
              .by=any_of(c("arm_", "soc_"))) %>%  
    left_join(df_arm, by="arm_") %>% 
    mutate(
      soc_ = fct_reorder(soc_, n_ae),
      n_ae = n_ae * ifelse(arm_==left_arm, -1, 1),
      n_severe = n_severe * ifelse(arm_==left_arm, -1, 1),
      pct_ae = n_ae/n_arm,
      pct_severe = n_severe/n_arm,
    )
  
  label_percent_positive = \(x) label_percent()(x) %>% str_remove("-")
  
  a %>% 
    ggplot(aes(y=soc_, fill=arm_)) +
    geom_col(aes(x=pct_ae), alpha=0.6) +
    geom_col(aes(x=pct_severe), color="grey40", width=0.6) +
    # scale_x_continuous(labels=label_percent_positive, limit=c(-1,1)) +
    scale_x_continuous(labels=label_percent_positive) +
    facet_grid(cols=vars(label), scales="free_x") +
    labs(y=NULL, fill=NULL, x="Proportion of patients presenting at least 1 adverse event") +
    theme(
      legend.position="bottom",
      panel.spacing.x=unit(1, "mm")
    )
}
