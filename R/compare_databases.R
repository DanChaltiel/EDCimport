

#' Compare multiple EDC database extractions
#' 
#' Compares several EDC database extractions and returns:
#'
#' - a summary table of the detected differences in datasets/columns presence
#' - a summary plot of the differences in number of rows, columns, patients, and rows per patient
#'  
#' @param databases file paths to read using `fun_read`. Can also be a list of `edc_database` objects.
#' @param fun_read Reading function to use on `databases`
#' @param ... arguments passed to `fun_read`
#'
#' @returns a list of `table` (a `gt` object with tooltips) and `plot` (a `patchwork` of ggplots)
#' @export
#' @importFrom cli cli_warn
#' @importFrom dplyr lst
#' @importFrom glue glue
#' @importFrom purrr map map_chr map_lgl
#' @examples
#' #list of 3 edc_databases, each being a list of multiple datasets
#' databases = edc_example_multiple() 
#' 
#' comparison = compare_databases(databases)
#' comparison$table
#' comparison$plot
#' 
#' #in real world, you should better use paths with a reader function:
#' \dontrun{
#'   databases = c(
#'     "data/MYPROJECT_ExportTemplate_xxx_SAS_XPORT_2024_06_01_12_00.zip",
#'     "data/MYPROJECT_ExportTemplate_xxx_SAS_XPORT_2024_08_01_12_00.zip",
#'     "data/MYPROJECT_ExportTemplate_xxx_SAS_XPORT_2024_09_01_12_00.zip",
#'   )
#'   #`pw` is passed to `read_trialmaster()`
#'   comparison = compare_databases(databases, fun_read=read_trialmaster, pw="the_password")
#' }
compare_databases = function(databases, fun_read=read_trialmaster, ...){
  check_installed(c("gt", "patchwork"), reason="for `compare_databases()` to work.")
  db_list = databases
  is_database = map_lgl(databases, ~inherits(.x, "edc_database"))
  if(!all(is_database)){
    check_installed(c("callr"), reason="for `compare_databases()` to read files.")
    db_list = callr::r(
      function(databases, fun_read, ...){
        suppressWarnings(purrr::map(databases, function(.x) fun_read(.x, ...)))
      }, 
      args=list(databases=databases, fun_read=fun_read, ...)
    )
  }
  
  names(db_list) = db_list %>% 
    map_chr(~{
      dt = .x$datetime_extraction
      glue("extract_{d}", d=format(dt, "%Y_%m_%d"))
    })
  
  dup = unique(names(db_list)[duplicated(names(db_list))])
  if(length(dup)>0){
    cli_warn("Some database extraction dates are not unique: {.val {dup}}",
             class="edc_compare_databases_unique_date_warning")
    names(db_list) = names(db_list) %>% 
      make.unique(sep="__") %>% 
      map_chr(~ifelse(.x %in% dup, paste0(.x, "__0"), .x))
  }
  
  tbl = .compare_databases_table(db_list)
  fig = .compare_databases_plots(db_list)
  
  list(table=tbl, plot=fig)
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
  
  v = c("Number of rows"="nrow", "Number of columns"="ncol", 
        "Number of patients"="n_id", "Number of rows per patient"="rows_per_id")
  p_list = v %>% imap(~{
    lk %>% 
      select(source, dataset, value=all_of(unname(.x))) %>% 
      mutate(group_dodge = paste(source, dataset)) %>% 
      ggplot() +
      aes(x=value, y=dataset, color=source, group=dataset) +
      # geom_point(aes(order=source), na.rm=TRUE, position=position_dodge(width=1.5, orientation="y")) +
      # geom_jitter(width=0) +
      # geom_point(na.rm=TRUE, position=position_dodge(width=1.5, orientation="y")) + 
      # geom_line(aes(group=group_dodge), position=position_dodge(width=1.5, orientation="y"), na.rm=TRUE) +
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
  
  x = db_list %>% 
    map(~{
      .x$.lookup %>% as_tibble() %>% select(dataset, any_of("crfname"), names)
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
    arrange(dataset) %>% 
    mutate(
      ncol = lengths(names),
      tmp = map2(names, lag(names), ~{
        tibble(plus=list(setdiff(.x, .y)), minus=list(setdiff(.y, .x)))
      }),
      crfname = replace_na(na.omit(crfname)[1], dataset[1]),
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
      diff_numbers = case_when(
        ncol==0 ~ "Absent",
        ncol==plus_dbl ~ "Added", 
        plus_dbl==0 & minus_dbl==0 ~ "Unchanged",
        .default = glue("+{plus_dbl} -{minus_dbl}")
      ),
      tooltip = glue("{.escape_html(diff_str)}____{diff_numbers}"),
      dataset = glue("{crfname}____{dataset}"),
      .by=dataset
    )
  
  rng = df %>% filter(ncol>0 & ncol!=plus_dbl) %>% pull(tot_dbl) %>% range(na.rm = TRUE)
  pal = .palette_compare(rng)
  
  labs = names(db_list) %>% 
    str_remove("extract_") %>% 
    str_replace("__(\\d+)", " (#\\1)") %>% #duplicates
    str_replace_all("_", "-") %>% 
    set_names(names(db_list))
  
  df %>% 
    select(db, dataset, tooltip) %>%
    pivot_wider(names_from=db, values_from=tooltip) %>% 
    gt::gt() %>%
    gt::text_transform(
      locations = gt::cells_body(columns = everything()),
      fn = function(x){
        a2 = x %>% str_replace("\n", gt::html("&#013;")) %>% str_split_fixed("____", 2)
        glue("<span title=\"{a2[,1]}\">{a2[,2]}</span>")
        # glue("<span class='tip' data-tip='{a2[,1]}'>{a2[,2]}</span>")
      }
    ) %>%
    gt::cols_label(.list=labs) %>%
    gt::data_color(
      columns = -dataset,
      fn = pal
    ) %>% 
    gt::tab_footnote("This table reflects changes in the dataset structure only, 
                     not in the underlying data.")
  
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

