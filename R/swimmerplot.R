

# TODO edc_swimmerplot ajouter tooltip? avec date si origin!=NULL


#' Swimmer plot of all dates columns
#' 
#' Join all tables on `id` with only date columns to build a ggplot (or a 
#' plotly if `plotly=TRUE`) showing all dates for each patients. This allows 
#' outliers to be easily identified.
#'
#' @param id the patient identifier. Will be coerced as numeric if possible.
#' @param group a grouping variable, given as "dataset$column"
#' @param origin a variable to consider as time 0, given as "dataset$column"
#' @param id_lim a numeric vector of length 2 providing the minimum and maximum `id` to subset on. 
#' @param exclude a character vector of variables to exclude, in the form `dataset$column`. Can be a regex, but `$` symbols don't count. Case-insensitive.
#' @param time_unit if `origin!=NULL`, the unit to measure time. One of `c("days", "weeks", "months", "years")`.
#' @param aes_color either `variable` ("\{dataset\} - \{column\}") or `label` (the column label)
#' @param plotly whether to use `{plotly}` to get an interactive plot
#' @param .lookup deprecated
#' @param ... not used
#'
#' @return either a `plotly` or a `ggplot`
#' @export
#' 
#' @examples
#' #tm = read_trialmaster("filename.zip", pw="xx")
#' tm = edc_example()
#' load_list(tm)
#' p = edc_swimmerplot(id_lim=c(5,45))
#' p2 = edc_swimmerplot(origin="db0$date_naissance", time_unit="weeks", 
#'                      exclude=c("DB1$DATE2", "db3$.*"))
#' p3 = edc_swimmerplot(group="db0$group", aes_color="label")
#' \dontrun{
#' #save the plotly plot as HTML to share it
#' save_plotly(p, "edc_swimmerplot.html")
#' }
#' @importFrom cli cli_abort cli_warn
#' @importFrom dplyr between filter left_join mutate rename select slice sym where
#' @importFrom forcats as_factor
#' @importFrom ggplot2 aes facet_wrap geom_line geom_point ggplot labs
#' @importFrom glue glue
#' @importFrom purrr discard imap list_rbind map
#' @importFrom rlang check_dots_empty check_installed is_installed set_names
#' @importFrom stringr str_detect str_ends str_remove str_replace_all
#' @importFrom tidyr pivot_longer
#' @importFrom tidyselect matches
edc_swimmerplot = function(..., 
                           group=NULL, origin=NULL, 
                           id_lim=NULL,
                           exclude=NULL,
                           id=get_subjid_cols(), 
                           time_unit=c("days", "weeks", "months", "years"),
                           aes_color=c("variable", "label"), 
                           plotly=getOption("edc_plotly", FALSE),
                           .lookup="deprecated"){
  check_dots_empty()
  aes_color = match.arg(aes_color)
  aes_label = if(aes_color=="variable") "label" else "variable"
  time_unit = match.arg(time_unit[1], c(time_unit, str_remove(time_unit, "s$")))
  if(!str_ends(time_unit, "s")) time_unit = paste0(time_unit, "s")
  parent = parent.frame()
  
  dat = get_datasets() %>% 
    .discard_if_no_id(id=id) %>% 
    .select_dates(id=id) %>% 
    .pivot_dates(id=id) %>% 
    list_rbind() %>% 
    .exclude_columns(exclude) %>% 
    .parse_id_to_numeric(id=id, id_lim=id_lim)%>% 
    arrange(variable)
  
  if(!is.null(group)){
    dat_group = parse_var(group, id, parent)
    if(anyDuplicated(dat_group$id)!=0){
      cli_abort("{.arg group} ({group}) should identify subjects ({id}) uniquely.", 
                class="edc_swimplot_group_dup")
    }
    dat = dat %>% left_join(dat_group, by="id")
  }
  
  tooltip = c("x", "y", "color", "label")
  x_label = "Calendar date"
  aes_x = "date"
  if(!is.null(origin)){
    dat_origin = parse_var(origin, id, parent)
    values = c(days=1, weeks=7, months=365.24/12, years=365.24)
    dat = dat %>%
      left_join(dat_origin, by="id") %>% 
      mutate(
        time = as.double(date-origin, units="days") / values[time_unit]
      )
    x_label = glue("Date difference from `{origin}` (in {time_unit})")
    tooltip = c(tooltip, "date")
    aes_x = "time"
  }
  
  p = dat %>% 
    ggplot(aes(x=!!sym(aes_x), y=id, group=id, date=date)) + 
    aes(color=!!sym(aes_color), label=!!sym(aes_label)) +
    geom_line(na.rm=TRUE) +
    geom_point(na.rm=TRUE) +
    labs(x=x_label, y="Patient", color="Variable")
  
  if(!is.null(group)){
    p = p + facet_wrap(~group, scales="free_y")
  }
  
  if(isTRUE(plotly)){
    check_installed("plotly", reason="for `edc_swimmerplot(plotly=TRUE)` to work.")
    p = plotly::ggplotly(p, tooltip=tooltip)
  }
  
  p
}



# Utils -------------------------------------------------------------------


#' @noRd
#' @keywords internal
.discard_if_no_id = function(datasets, id){
  has_id = datasets %>% 
    map(~tolower(names(.x))) %>% 
    map(~intersect(.x, tolower(id))) %>% 
    map_lgl(~length(.x)>0)
  if(!any(has_id)){
    cli_abort(c("None of the datasets contains an identifier column", 
                i="{.arg id}={.val {id}}"), 
              call=parent.frame())
  }
  datasets[has_id]
}


#' @noRd
#' @keywords internal
.select_dates = function(datasets, id){
  data_dates = datasets %>% 
    map(~{
      .x %>% 
        select(id=any_of2(id), where(is.Date)) %>% 
        rename(id=1)
    }) %>% 
    discard(~ncol(.x)<2)
  if(length(data_dates)==0){
    cli_abort(c("None of the datasets contains a date column"), 
              call=parent.frame())
  }
  data_dates
}

.pivot_dates = function(datasets, id){
  datasets %>% 
    imap(~{
      .x %>% 
        pivot_longer(-id, values_to="date") %>% 
        mutate(
          label=unlist(get_label(.x)[name]) %||% name,
          dataset=.y,
          variable=paste0(dataset, "$", name)
        )
    })
}

.exclude_columns = function(data, exclude) {
  if(!is.null(exclude)){
    excl = exclude %>% paste(collapse="|") %>% tolower() %>% 
      str_replace_all("\\$", "\\\\$")
    data = data %>% 
      filter(!str_detect(tolower(variable), excl))
  }
  data
}

.parse_id_to_numeric = function (data, id, id_lim) {
  if(is.null(id_lim)) id_lim =c(-Inf, Inf)
  if(!is.numeric(id_lim) && length(id_lim)!=2) {
    cli_abort("{.arg id_lim} should be a numeric vector of length 2.")
  }
  
  if(can_be_numeric(data$id)) {
    data = data %>% 
      mutate(id = as.numeric(id)) %>% 
      filter(id>=id_lim[1] & id<=id_lim[2])
  } else if(all(str_detect(data$id, "\\d+"))){
    if(is_installed("gtools")){
      data = data %>% 
        slice(gtools::mixedorder(id)) %>% 
        mutate(id=as_factor(id))
    } else {
      cli_warn(c("{.arg id} contains numbers, you will need the 
                    {.pkg gtools} package to sort it properly.", 
                 i='Run {.run utils::install.package("gtools")}'),
               call=parent.frame())
    }
  }
  data
}

#' @importFrom cli cli_abort
#' @importFrom dplyr rename select
#' @importFrom rlang caller_arg
#' @importFrom stringr str_detect str_split
#' @importFrom tidyselect matches
#' @noRd
#' @keywords internal
parse_var = function(input, id, env){
  input_name = caller_arg(input)
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
    select(matches(id), !!input_name:=!!input2[2]) %>%
    rename(id=1)
}




# Helper ------------------------------------------------------------------

#' Save a plotly to an HTML file
#'
#' @param p a plot object (`plotly` or `ggplot`)
#' @param file a file path to save the HTML file
#' @param ... passed on to [htmlwidgets::saveWidget]
#'
#' @export
#' @return nothing, used for side effect
#'
#' @examples
#' \dontrun{
#' tm = edc_example_plot()
#' p = edc_swimmerplot(tm$.lookup, id_lim=c(5,45))
#' save_plotly(p, "graph/swimplots/edc_swimmerplot.html", title="My Swimmerplot")
#' }
#' @importFrom fs dir_create path_dir
#' @importFrom rlang check_installed
save_plotly = function(p, file, ...){
  check_installed("plotly", reason="for `save_plotly()` to work.")
  check_installed("htmlwidgets", reason="for `save_plotly()` to work.")
  if(inherits(p, "ggplot")) p = plotly::ggplotly(p)
  dir_create(path_dir(file), recurse=TRUE)
  wd = setwd(path_dir(file))
  on.exit(setwd(wd))
  htmlwidgets::saveWidget(p, file=basename(file), ...)
}