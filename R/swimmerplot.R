

# TODO edc_swimmerplot ajouter tooltip? avec date si origin!=NULL


#' Swimmer plot of all dates columns
#' 
#' Join all tables on `id` with only date columns to build a ggplot (or a 
#' plotly if `plotly=TRUE`) showing all dates for each subject. \cr
#' This allows outliers to be easily identified.
#'
#' @param group a grouping variable, given as "dataset$column".
#' @param origin a variable to consider as time 0, given as "dataset$column".
#' @param include,exclude a character vector of variables to exclude/include, in the form `dataset$column`. Can be a regex (apart from `$` symbols that will be automatically escaped). Case-insensitive.
#' @param data_list a named list of data.frames to get the dates from. Default to [get_datasets], which retrieve all raw datasets.
#' @param id_subset the subjects to include in the plot.
#' @param id_sort whether to sort subjects by date (or time).
#' @param id_cols the subject identifiers columns. Identifiers be coerced as numeric if possible. See [get_subjid_cols] if needed.
#' @param time_unit if `origin!=NULL`, the unit to measure time. One of `c("days", "weeks", "months", "years")`.
#' @param origin_fun function to summarise the `origin` date at the id level if needed. Should be named, or at least have a meaningful function name (see example "summarised origin".
#' @param aes_color either `variable` ("\{dataset\} - \{column\}") or `label` (the column label).
#' @param plotly whether to use `{plotly}` to get an interactive plot.
#' @param id deprecated
#' @param id_lim deprecated
#' @param .lookup deprecated
#' @param ... not used
#'
#' @return either a `plotly` or a `ggplot`
#' @export
#' 
#' @examples
#' #db = read_trialmaster("filename.zip", pw="xx")
#' db = edc_example()
#' load_database(db)
#' edc_swimmerplot(id_lim=c(5,45))
#' 
#' #fixed origin
#' edc_swimmerplot(origin="enrol$enrol_date", time_unit="months", 
#'                 include=c("data1", "data3"),
#'                 exclude=c("DATA1$DATE2", "data3$date\\d\\d"), 
#'                 id_sort=TRUE)
#' 
#' #summarised origin
#' edc_swimmerplot(origin="data1$date2", time_unit="months", 
#'                 origin_fun=c("average"=~mean(.x, na.rm=TRUE)),
#'                 include=c("data1", "data3"),
#'                 exclude=c("DATA1$DATE2", "data3$date\\d\\d"), 
#'                 id_sort=TRUE)
#' 
#' #id_subset
#' edc_swimmerplot(group="enrol$arm", id_subset=1:10, aes_color="label")
#' 
#' \dontrun{
#' p = edc_swimmerplot(plotly=TRUE)
#' save_plotly(p, "edc_swimmerplot.html")
#' }
#' @importFrom cli cli_abort
#' @importFrom dplyr arrange left_join mutate n_distinct sym
#' @importFrom ggplot2 aes facet_wrap geom_line geom_point geom_vline ggplot labs
#' @importFrom glue glue
#' @importFrom purrr list_rbind
#' @importFrom rlang check_dots_empty check_installed
#' @importFrom stringr str_ends str_remove
edc_swimmerplot = function(..., 
                           include=NULL,
                           exclude=NULL,
                           group=NULL, origin=NULL, 
                           data_list=get_datasets(),
                           id_subset="all",
                           id_sort=FALSE, 
                           id_cols=get_subjid_cols(), 
                           time_unit=c("days", "weeks", "months", "years"),
                           origin_fun="min",
                           aes_color=c("variable", "label"), 
                           plotly=getOption("edc_plotly", FALSE),
                           id="deprecated",
                           id_lim="deprecated",
                           .lookup="deprecated"){
  check_dots_empty()
  assert_class(data_list, "list")
  map(data_list, ~assert_class(.x, "data.frame"))
  
  aes_color = match.arg(aes_color)
  aes_label = if(aes_color=="variable") "label" else "variable"
  time_unit = match.arg(time_unit[1], c(time_unit, str_remove(time_unit, "s$")))
  if(!str_ends(time_unit, "s")) time_unit = paste0(time_unit, "s")
  parent = parent.frame()
  if(!missing(id)) {
    deprecate_warn("0.5.2", "edc_swimmerplot(id)", "edc_swimmerplot(id_cols)")
    id_cols = id
  }
  
  dat = data_list %>% 
    .discard_if_no_id(id=id_cols) %>% 
    .select_dates(id=id_cols) %>% 
    .pivot_dates(id=id_cols) %>% 
    list_rbind() %>% 
    .select_columns(include, dir="include") %>% 
    .select_columns(exclude, dir="exclude") %>% 
    arrange(variable)
  
  if(!isTRUE(id_subset=="all")){
    if(is.numeric(id_subset) && !is.numeric(dat$id)){
      id_subset = unique(dat$id)[id_subset]
    }
    dat = dat %>% 
      filter(id %in% id_subset)
    if(nrow(dat)==0) return(NULL)
  }
  
  if(!is.null(group)){
    dat_group = parse_var(group, id_cols, parent) %>% 
      mutate(id=as.character(id))
    if(anyDuplicated(dat_group$id)!=0){
      cli_abort("{.arg group} ({group}) should identify subjects ({id}) uniquely.", 
                class="edc_swimplot_group_dup")
    }
    dat = dat %>% left_join(dat_group, by="id")
  }
  
  tooltip = c("x", "y", "color", "label")
  x_label = "Calendar date"
  aes_x = "date"
  vline = NULL
  if(!is.null(origin)){
    origin_fun = .parse_origin_fun(origin_fun)
    dat_origin = parse_var(origin, id_cols, parent) %>% 
      mutate(id=as.character(id)) %>% 
      summarise(n=n_distinct(origin),
                origin=origin_fun$fun(origin), 
                .by=id)
    
    values = c(days=1, weeks=7, months=365.24/12, years=365.24)
    if(!inherits(dat_origin$origin, c("Date", "POSIXt"))){
      cli_abort("Column {.arg origin} ({.val {origin}}) should be of class
                {.cls Date} or {.cls POSIXt}, not {.cls {class(dat_origin$origin)}}.", 
                class="edc_swimplot_origin_notdate")
    }
    
    dat = dat %>%
      left_join(dat_origin, by="id") %>% 
      mutate(
        time = as.double(date-origin, units="days") / values[time_unit]
      )
    x_label = glue("Date difference from `{origin}` (in {time_unit})")
    if(max(dat_origin$n)>1){
      x_label = glue("Date difference from `{origin_fun$name}({origin})` (in {time_unit})")
    }
    tooltip = c(tooltip, "date")
    aes_x = "time"
    vline = geom_vline(xintercept=0)
  }
  
  if(isTRUE(id_sort)){
    v = if(!is.null(origin)) dat$time else dat$date
    dat$id = fct_reorder(dat$id, v, .fun=min, na.rm=TRUE)
  }
  
  p = dat %>% 
    .parse_id_to_numeric(id=id, id_lim=id_lim) %>% 
    ggplot(aes(x=!!sym(aes_x), y=id, group=id, date=date)) + 
    aes(color=!!sym(aes_color), label=!!sym(aes_label)) +
    .get_scale_color(n_levels=n_distinct(dat$variable)) +
    vline +
    geom_line(na.rm=TRUE) +
    geom_point(na.rm=TRUE) +
    labs(x=x_label, y="Subject", color="Variable")
  
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
#' @importFrom cli cli_abort
#' @importFrom purrr map map_lgl
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
#' @importFrom cli cli_abort
#' @importFrom dplyr rename select where
#' @importFrom purrr discard map
.select_dates = function(datasets, id){
  data_dates = datasets %>% 
    map(~{
      .x %>% 
        select(id=any_of2(id), where(is.Date)) %>% 
        rename(id=1) %>%
        mutate(id=as.character(id)) 
    }) %>% 
    discard(~ncol(.x)<2)
  if(length(data_dates)==0){
    cli_abort(c("None of the datasets contains a date column"), 
              call=parent.frame())
  }
  data_dates
}

#' @importFrom dplyr mutate
#' @importFrom purrr imap
#' @importFrom tidyr pivot_longer
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

#' @importFrom dplyr filter
#' @importFrom stringr str_detect str_replace_all
.select_columns = function(data, cols, dir) {
  cols = cols[nchar(cols)>0]
  if(length(cols)>0){
    needle = cols %>% paste(collapse="|") %>% tolower() %>% 
      str_replace_all("\\$", "\\\\$")
    if(dir=="exclude"){
      data = data %>% 
        filter(!str_detect(tolower(variable), needle))
    } else {
      data = data %>% 
        filter(str_detect(tolower(variable), needle))
    }
  }
  data
}

#' @importFrom cli cli_abort cli_warn
#' @importFrom dplyr filter mutate slice
#' @importFrom forcats as_factor
#' @importFrom rlang is_installed
#' @importFrom stringr str_detect
.parse_id_to_numeric = function (data, id, id_lim) {
  if(is.null(id_lim) || identical(id_lim, "deprecated")) id_lim =c(-Inf, Inf)
  if(!is.numeric(id_lim) && length(id_lim)!=2) {
    cli_abort("{.arg id_lim} should be a numeric vector of length 2.")
  }
  
  if(can_be_numeric(data$id)) {
    data = data %>% 
      mutate(id = as.numeric(id)) %>% 
      filter(id>=id_lim[1] & id<=id_lim[2])
  } else if(all(str_detect(data$id, "\\d+"))){
      data = data %>% 
        mixed_arrange(id) %>% 
        mutate(id=as_factor(id))
  }
  data
}

#' @importFrom dplyr n_distinct
.get_scale_color = function(n_levels){
  rtn = getOption("ggplot2.discrete.colour", default=ggplot2::scale_colour_hue)()
  chk = suppressWarnings(rtn$palette(n_levels))
  if(any(is.na(chk))) rtn = ggplot2::scale_colour_hue()
  rtn
}

#' @importFrom tibble lst
.parse_origin_fun = function(origin_fun){
  if(identical(origin_fun, "min")) origin_fun = c(min=min_narm)
  else if(identical(origin_fun, "max")) origin_fun = c(max=max_narm)
  if(!is.list(origin_fun)) origin_fun = lst(origin_fun)
  if(length(origin_fun)!=1) cli_abort("{.arg origin_fun} should be of length {.val 1}.")
  origin_fun = map(origin_fun, as_function)
  list(name=names(origin_fun), fun=unname(origin_fun[[1]]))
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
#' @param file a file path to save the HTML file. Can use the `glue` syntax to add variables.
#' @param ... passed on to [htmlwidgets::saveWidget]
#'
#' @export
#' @return nothing, used for side effect
#'
#' @examples
#' \dontrun{
#' db = edc_example()
#' load_database(db)
#' p = edc_swimmerplot(id_lim=c(5,45))
#' save_plotly(p, "graph/swimplots_{date_extraction}/edc_swimmerplot.html", 
#'             title="My Swimmerplot")
#' }
#' @importFrom cli cli_abort
#' @importFrom fs dir_create path_dir
#' @importFrom glue glue
#' @importFrom rlang check_installed
#' @importFrom stringr str_ends
save_plotly = function(p, file, ...){
  check_installed("plotly", reason="for `save_plotly()` to work.")
  check_installed("htmlwidgets", reason="for `save_plotly()` to work.")
  if(inherits(p, "ggplot")) p = plotly::ggplotly(p)
  if(!str_ends(file, "\\.html")) cli_abort('File name should end in ".html"')
  file = glue(file, .envir=parent.frame())
  dir_create(path_dir(file), recurse=TRUE)
  wd = setwd(path_dir(file))
  on.exit(setwd(wd))
  htmlwidgets::saveWidget(p, file=basename(file), ...)
}
