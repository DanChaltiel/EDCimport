

# TODO edc_swimmerplot ajouter tooltip? avec date si origin!=NULL


#' Swimmer plot of all dates columns
#' 
#' Join all tables from `.lookup$dataset` on `id` 
#'
#' @param .lookup the lookup table, loaded along with the database or result of [get_lookup()]
#' @param id the patient identifier
#' @param group a grouping variable, given as "dataset$column"
#' @param origin a variable to consider as time 0, given as "dataset$column"
#' @param time_unit if `origin!=NULL`, the unit to measure time. One of `c("days", "weeks", "months", "years")`.
#' @param aes_color either `variable` ("{dataset} - {column}") or `label` (the column label)
#' @param plotly whether to use `{plotly}` to get an interactive plot
#' @param ... not used
#'
#' @return either a `plotly` or a `ggplot`
#' @export
#' 
#' @examples
#' #tm = read_trialmaster("filename.zip", pw="xx")
#' tm = edc_example_plot()
#' load_list(tm)
#' p = edc_swimmerplot(.lookup)
#' p2 = edc_swimmerplot(.lookup, origin="db0$date_naissance", time_unit="weeks")
#' p3 = edc_swimmerplot(.lookup, group="db0$group", aes_color="label")
#' \dontrun{
#' #save the plotly plot as HTML to share it
#' htmlwidgets::savewidget(p, "edc_swimmerplot.html", selfcontained=TRUE)
#' }
#' @importFrom cli cli_abort cli_warn
#' @importFrom dplyr left_join mutate select slice
#' @importFrom forcats as_factor
#' @importFrom ggplot2 aes facet_wrap geom_line geom_point ggplot labs
#' @importFrom glue glue
#' @importFrom labelled var_label
#' @importFrom purrr discard imap list_rbind map
#' @importFrom rlang check_dots_empty check_installed is_installed set_names sym
#' @importFrom stringr str_detect str_ends str_remove
#' @importFrom tidyr pivot_longer
#' @importFrom tidyselect where
edc_swimmerplot = function(.lookup=getOption("edc_lookup", NULL), ..., 
                       id="SUBJID", group=NULL, origin=NULL, 
                       time_unit=c("days", "weeks", "months", "years"),
                       aes_color=c("variable", "label"), plotly=TRUE){
  check_dots_empty()
  time_unit = match.arg(time_unit[1], c(time_unit, str_remove(time_unit, "s$")))
  if(!str_ends(time_unit, "s")) time_unit = paste0(time_unit, "s")
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
    list_rbind() %>% 
    mutate(date=value)
  
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
  tooltip = c("x", "y", "color", "label")
  x_label = "Calendar date"
  if(!is.null(origin)){
    dat_origin = parse_var(origin, id, parent)
    values = c(days=1, weeks=7, months=365.24/12, years=365.24)
    dat = dat %>%
      left_join(dat_origin, by="id") %>% 
      mutate(
        date = value,
        value = as.double(value-origin, units="days") / values[time_unit]
      )
    x_label = glue("Date difference from `{origin}` (in {time_unit})")
    tooltip = c(tooltip, "date")
  }
  
  aes_label = "variable"
  if(aes_color=="variable"){ 
    aes_label = "label"
  }
  
  p = dat %>% 
    mutate(id=as_factor(id)) %>% 
    ggplot(aes(x=value, y=id, group=id, date=date)) + 
    aes(color=!!sym(aes_color), label=!!sym(aes_label)) +
    geom_line(na.rm=TRUE) +
    geom_point(na.rm=TRUE) +
    labs(x=x_label, y=id, color="Variable")
  
  if(!is.null(group)){
    p = p + facet_wrap(~group, scales="free_y")
  }
  
  if(isTRUE(plotly)){
    check_installed("plotly", reason="for `edc_swimmerplot(plotly=TRUE)` to work.")
    p = plotly::ggplotly(p, tooltip=tooltip)
  }
  
  p
}



#' @importFrom cli cli_abort
#' @importFrom dplyr select
#' @importFrom rlang caller_arg
#' @importFrom stringr str_detect str_split
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
