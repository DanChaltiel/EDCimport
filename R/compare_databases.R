


compare_databases = function(data_list){
  library(flextable)
  library(tidyverse)
  library(patchwork)
  data_list = read_hrnbl2_multi()
  tm_all = data_list
  
  
  # x = compare_databases(tm_all)
  
  
  tm_names_all = tm_all %>% map(names) %>% 
    unlist() %>% unique() %>% sort() %>% 
    setdiff(c(".lookup", "date_extraction", "datetime_extraction"))
  
  
  # Table present / absent ----------------------------------------------------------------------
  
  table_presence = tm_all %>% 
    map(~{
      ifelse(tm_names_all %in% names(.x), "Present", "Absent")
    }) %>% 
    bind_cols(dataset=tm_names_all, .)
  
  table_presence %>% 
    flextable() %>% 
    bg(j=-1, bg=function(x)ifelse(x=="Present", "darkgreen", "red")) 
  
  
  
  # Lookups -------------------------------------------------------------------------------------
  
  
  lk = tm_all %>% 
    map(".lookup") %>% 
    bind_rows(.id="source") %>% 
    tidyr::complete(source, dataset) %>% 
    arrange(dataset) %>% 
    as_tibble()
  
  ## Graphique d'évolution de nrow, ncol, n_id, rows_per_id -----
  
  ### as facets -----
  fig1 = lk %>% 
    select(source, dataset, nrow, ncol, n_id, rows_per_id) %>% 
    pivot_longer(-c(source, dataset)) %>% 
    ggplot() +
    aes(x=value, y=dataset, color=source, group=dataset) +
    geom_point(na.rm=TRUE) + geom_line(na.rm=TRUE) +
    facet_wrap(~name, scales="free_x")
  
  ### as list -----
  v = c("Number of rows"="nrow", "Number of columns"="ncol", 
        "Number of patients"="n_id", "Number of rows per patient"="rows_per_id")
  fig1b = v %>% imap(~{
    lk %>% 
      select(source, dataset, value=all_of(unname(.x))) %>% 
      ggplot() +
      aes(x=value, y=dataset, color=source, group=dataset) +
      geom_point(na.rm=TRUE) + geom_line(na.rm=TRUE) +
      xlab(.y) +
      theme(legend.position="top")
  })
  fig1b %>% wrap_plots(guides="collect") & theme(legend.position="top")
  
  
  
  ## Evolution des names/labels -----
  lk2 = lk %>% 
    select(source, dataset, names) %>% 
    arrange(dataset, source) %>% 
    mutate(
      added = purrr::map2_chr(names, lag(names), \(cur, prev){
        if(is.null(prev)) return(NA)
        setdiff(cur, prev) %>% paste(collapse=", ") %>% na_if("")
      }),
      removed = purrr::map2_chr(names, lag(names), \(cur, prev){
        if(is.null(prev)) return(NA)
        setdiff(prev, cur) %>% paste(collapse=", ") %>% na_if("")
      }),
      .by=dataset
    ) %>% 
    select(-names)
  
  lk2 %>%
    # filter(any(!is.na(added)) | any(!is.na(removed)),
    #        .by=dataset) %>% 
    filter(!is.na(added) | !is.na(removed)) %>% 
    arrange(source) %>% 
    flextable() %>% autofit() %>% 
    merge_v(j="source") %>%
    hline(i=~source!=lead(source, default=source[1])) %>% 
    hline(j="source") %>% 
    fix_border_issues()
  
  
  
  
  
}
