
#' Get a swimmer plot of all dates in the database
#'
#' @param .lookup the lookup table, loaded along with the database or result of [get_lookup()]
#' @param id an identifier for a given row
#' @param origin a variable to consider as time 0, given as "dataset$column"
#' @param group a grouping variable, given as "dataset$column"
#' @param aes_color either `variable` ("{dataset} - {column}") or `label` (column label)
#' @param plotly whether to use `{plotly}` to get an interactive plot
#'
#' @return a plot
#' @export
swimmerplot = function(.lookup=getOption("edc_lookup", NULL), id="SUBJID", 
                       origin=NULL, group=NULL,
                       aes_color=c("variable", "label"), plotly=TRUE){
  check_installed("ggplot2", reason="for `swimmerplot()` to work.")
  aes_color = match.arg(aes_color)
  parent = parent.frame()
  
  dbs = .lookup$dataset %>%
    set_names() %>% 
    map(~get(.x, envir=parent))
  if(length(dbs)==0){
    cli_abort("Unexpected error, contact the developper")
  }
  
  dbs = dbs %>% 
    discard(~!id %in% names(.x))
  if(length(dbs)==0){
    cli_abort(c("None of the datasets contains an identifier column", i="{.arg id}={.val {id}}"))
  }
  
  dbs = dbs %>% 
    map(~.x %>% select(id=!!id, where(is.Date))) %>% 
    discard(~ncol(.x)<2)
  if(length(dbs)==0){
    cli_abort(c("None of the datasets contains a date column"))
  }
  
  dat = dbs %>% 
    imap(~{
      .x %>% 
        pivot_longer(-id) %>% 
        mutate(label=unlist(var_label(.x)[name]) %||% name,
               dataset=.y,
               variable=paste0(toupper(dataset), " - ", toupper(name)))
    }) %>% 
    list_rbind()
  
  if(!is.null(group)){
    dat_group = parse_var(group, id, parent)
    if(anyDuplicated(dat_group$id)!=0){
      cli_abort("{.arg group} ({group}) should identify subjects ({id}) uniquely.", 
                class="edc_swimplot_group_dup")
    }
    
    dat = dat %>% left_join(dat_group, by="id")
  }
  
  if(can_be_numeric(dat$id)) dat$id=as.numeric(dat$id)
  else if(all(str_detect(dat$id, "\\d+"))){
    if(is_installed("gtools")) dat=slice(dat, gtools::mixedorder(id))
    else cli_warn(c("{.arg id} contains numbers, you will need the 
                    {.pkg gtools} package to sort it properly.", 
                    i='Run {.run utils::install.package("gtools")}'))
  }
  
  x_label = "Calendar date"
  if(!is.null(origin)){
    dat_origin = parse_var(origin, id, parent)
    dat = dat %>%
      left_join(dat_origin, by="id") %>% 
      mutate(
        value_bak = value,
        value = as.double(value-origin, units="days")
      )
    x_label = glue("Date difference from `{origin}` (in days)")
  }
  
  aes_label = "variable"
  if(aes_color=="variable"){ 
    aes_label = "label"
  }
  
  p = dat %>% 
    mutate(id=as_factor(id)) %>% 
    ggplot2::ggplot(ggplot2::aes(x=value, y=id, group=id)) + 
    ggplot2::aes(color=!!sym(aes_color), label=!!sym(aes_label)) +
    ggplot2::geom_line(na.rm=TRUE) +
    ggplot2::geom_point(na.rm=TRUE) +
    ggplot2::labs(x=x_label, y=id, color="Variable")
  
  if(!is.null(group)){
    p = p + ggplot2::facet_wrap(~group, scales="free_y")
  }
  
  if(isTRUE(plotly)){
    check_installed("plotly", reason="for `swimmerplot(plotly=TRUE)` to work.")
    p = plotly::ggplotly(p)
  }
  
  p
}



parse_var = function(input, id, env){
  input_name = rlang::caller_arg(input)
  
  if(!str_detect(input, "^.*\\$.*$")){
    cli_abort(c(x="{.arg {input_name}} is not in the form `dataset$column`.", 
                i="{.arg {input_name}} = {.val {input}}"), 
              class="edc_swimplot_parse", 
              call=parent.frame())
  }
  input2 = str_split(input, "\\$", 2)[[1]]
  
  if(!exists(input2[1], envir=env)){
    cli_abort(c(x="{.arg {input_name}} is wrong: no dataset {.val {input2[1]}} was found.", 
                i="{.arg {input_name}} = {.val {input}}"), 
              class="edc_swimplot_parse_dataset", 
              call=parent.frame())
  }
  
  dat_input = get(input2[1], envir=env)
  
  if(!input2[2] %in% names(dat_input)){
    cli_abort(c(x="{.arg {input_name}} is wrong: no column {.val {input2[2]}} in dataset {.val {input2[1]}} was found.", 
                i="{.arg {input_name}} = {.val {input}}"), 
              class="edc_swimplot_parse_column", 
              call=parent.frame())
  }
  
  dat_input %>% 
    select(id=!!id, !!input_name:=!!input2[2])
}


# plotly::ggplotly()