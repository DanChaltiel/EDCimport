

#' Compare multiple EDC database extractions
#' 
#' Compares several EDC database extractions and returns:
#'
#' - a summary table of the detected differences,
#' - a set of diagnostic figures.
#' #' 
#' @param archives file paths to read using `fun_read`. Can also be a list of `edc_database` objects.
#' @param fun_read Reading function to use on `archives`
#' @param ... arguments passed to `fun_read`
#'
#' @export
#' @importFrom cli cli_warn
#' @importFrom dplyr lst
#' @importFrom glue glue
#' @importFrom purrr map map_chr map_lgl
#' @examples
#' 
#' db1 = edc_example()
#' db2 = edc_example(N=60) %>% 
#'   mutate_list(
#'     data99 = data1, #new data
#'     enrol = enrol %>% mutate(a=1, b=2), #add columns
#'     data1 = data1 %>% select(-date2, -date3), #remove columns
#'     data2 = data2 %>% mutate(a=1, date5=NULL), #both
#'     datetime_extraction = as.POSIXct("2024-02-01")
#'   )
#' db3 = db2 %>% 
#'   mutate_list(
#'     data99 = data1, #new data
#'     enrol = enrol %>% mutate(c=1, d=2), #add columns
#'     data1 = data1 %>% select(-crfstat), #remove columns
#'     data2 = data2 %>% mutate(b=1, date6=NULL), #both
#'     datetime_extraction = as.POSIXct("2024-04-01")
#'   )
#' 
#' comparison = compare_databases(list(db1, db2, db3))
#'   
#' comparison$table
#' comparison$figures
#' 
#' #in real world, you should better use archive paths:
#' \dontrun{
#'   archives = c(
#'     "data/MYPROJECT_ExportTemplate_xxx_SAS_XPORT_2024_06_01_12_00.zip",
#'     "data/MYPROJECT_ExportTemplate_xxx_SAS_XPORT_2024_08_01_12_00.zip",
#'     "data/MYPROJECT_ExportTemplate_xxx_SAS_XPORT_2024_09_01_12_00.zip",
#'   )
#'   comparison = compare_databases(archives, read_trialmaster, pw="the_password")
#' }
compare_databases = function(archives, fun_read=read_trialmaster, ...){
  db_list = archives
  is_database = map_lgl(archives, ~inherits(.x, "edc_database"))
  if(!all(is_database)){
    #FIXME faire ça dans un environnement controlé ? sinon ça plombe un appel antérieur à read_tm
    db_list = archives %>% 
      map(~fun_read(.x, ...)) %>% 
      suppressWarnings()
  }
  
  names(db_list) = db_list %>% 
    map_chr(~{
      dt = .x$.lookup %>% attr("datetime_extraction")
      glue("extract_{d}", d=format(dt, "%Y_%m_%d"))
    })
  
  dup = unique(names(db_list)[duplicated(names(db_list))])
  if(length(dup)>0){
    cli_warn("Some database extraction dates are not unique: {.val {dup}}",
             class="edc_compare_databases_unique_date_warning")
    names(db_list) = names(db_list) %>% 
      make.unique(sep="_")
  }
  
  tbl = .compare_databases_table(db_list)
  fig = .compare_databases_plots(db_list)
  
  lst(table=tbl, figures=fig)
}


#' @importFrom dplyr all_of arrange as_tibble bind_rows mutate select
#' @importFrom ggplot2 aes geom_line geom_point ggplot labs theme
#' @importFrom purrr imap map
.compare_databases_plots = function(db_list){
  lk = db_list %>% 
    map(~{
      .x$.lookup %>% mutate(date=attr(.x$.lookup, "datetime_extraction"), .after=1)
    }) %>% 
    bind_rows(.id="source") %>% 
    tidyr::complete(source, dataset) %>% 
    arrange(dataset) %>% 
    as_tibble()
  
  # https://github.com/tidyverse/ggplot2/issues/6719 for dodging
  v = c("Number of rows"="nrow", "Number of columns"="ncol", 
        "Number of patients"="n_id", "Number of rows per patient"="rows_per_id")
  p_list = v %>% imap(~{
    lk %>% 
      select(source, dataset, value=all_of(unname(.x))) %>% 
      ggplot() +
      aes(x=value, y=dataset, color=source, group=dataset) +
      # geom_point(aes(order=source), na.rm=TRUE, position=position_dodge(width=1.5, orientation="y")) +
      # geom_jitter(width=0) +
      geom_point(na.rm=TRUE) + 
      geom_line(na.rm=TRUE) +
      labs(x=.y, y=NULL) +
      theme(legend.position="top")
  })
  patchwork::wrap_plots(p_list, guides="collect") & theme(legend.position="top")
}


#' @importFrom dplyr arrange case_when filter lag mutate pull row_number select setdiff
#' @importFrom glue glue
#' @importFrom purrr imap keep list_rbind map map_chr map2
#' @importFrom stringr str_replace str_split_fixed
#' @importFrom tibble tibble
#' @importFrom tidyr pivot_wider unnest
.compare_databases_table = function(db_list){
  
  #TODO formatter dataset, ajouter get_label en tooltip
  x = db_list %>% 
    imap(~{
      .x = .x %>% keep(is_dataset)
      tibble(dataset=names(.x), names=map(.x, names))
    }) %>% 
    list_rbind(names_to="db") %>% 
    filter(dataset!=".lookup") %>% 
    arrange(dataset, db)
  
  f=function(x, prefix, fun){
    map_chr(x, ~{
      if(length(.x)==0) return("")
      glue("{prefix} [{fun(.x)}]")
    })
  }
  df = x %>% 
    tidyr::complete(db, dataset) %>% 
    mutate(
      ncol = lengths(names),
      tmp = map2(names, lag(names), ~{
        tibble(plus=list(setdiff(.x, .y)), minus=list(setdiff(.y, .x)))
      }),
      .by=dataset
    ) %>% 
    unnest(tmp) %>% 
    mutate(
      plus_str = f(plus, "+", fun=toString),
      minus_str = f(minus, "-", fun=toString),
      plus_dbl = lengths(plus),
      minus_dbl = lengths(minus),
      tot_dbl = plus_dbl + minus_dbl,
      diff_str = ifelse(ncol==0|row_number()==1, "", glue("{plus_str}\n{minus_str}")),
      diff_dbl = case_when(
        ncol==0 ~ "Absent",
        ncol==plus_dbl ~ "Added", 
        .default = glue("+{plus_dbl} -{minus_dbl}")
      ),
      # tooltip = glue("<span title=\"{.escape_html(diff_str)}\">{.escape_html(diff_dbl)}</span>"),
      tooltip = glue("{.escape_html(diff_str)}____{diff_dbl}"),
      .by=dataset
    ) %>% 
    arrange(dataset)
  
  rng = df %>% filter(ncol>0 & ncol!=plus_dbl) %>% pull(tot_dbl) %>% range(na.rm = TRUE)
  pal = .palette_compare(rng)
  table = df %>% 
    mutate(
      tooltip = glue("{.escape_html(diff_str)}____{diff_dbl}")
    ) %>% 
    select(db, dataset, tooltip) %>%
    pivot_wider(names_from=db, values_from=tooltip) %>% 
    gt::gt() %>%
    gt::text_transform(
      locations = gt::cells_body(columns = -dataset),
      fn = function(x){
        a2 = x %>% str_replace("\n", gt::html("&#013;")) %>% str_split_fixed("____", 2)
        glue("<span title=\"{a2[,1]}\">{a2[,2]}</span>")
        # glue("<span class='tip' data-tip='{a2[,1]}'>{a2[,2]}</span>")
      }
    ) %>%
    gt::data_color(
      columns = -dataset,
      fn = pal
    )
}



.escape_html = function(x){
  x %>%
    str_replace_all("&", "&amp;") %>%
    str_replace_all("<", "&lt;") %>%
    str_replace_all(">", "&gt;") %>%
    str_replace_all('"', "&quot;") %>%
    str_replace_all("'", "&#39;")
}

#' @importFrom purrr map_dbl
#' @importFrom stringr str_match_all str_remove
.parse_sum = function(x){
  #(?s) = option dotall
  x %>% str_remove("(?s).*____") %>% str_match_all("\\d+") %>% map_dbl(~sum(as.numeric(.x)))
}

#paramétriser les labels added/absent ?
#' @importFrom dplyr case_when
#' @importFrom stringr str_ends
.palette_compare = function(rng){
  if (any(!is.finite(rng))) rng = c(0, 1)
  pal_num = scales::col_numeric(c("#e8f0fe", "#174ea6"), domain = rng, na.color = "transparent")
  function(x){
    v = .parse_sum(x)
    case_when(
      str_ends(x, "Added") ~ "#2e7d32",
      str_ends(x, "Absent") ~ "#c62828",
      v==0 ~ "white", 
      .default = pal_num(v)
    )
  }
}

