

#' Generate a lookup table
#'
#' @param data_list a list containing at least 1 dataframe
#'
#' @return a dataframe summarizing column names and labels 
#' @export
#'
#' @examples
#' tl = list(r=crosstable::iris2, x=mtcars) %>% get_lookup()
#' tl
#' library(tidyr)
#' tl %>% unnest(everything()) %>% unnest(everything())
get_lookup = function(data_list){
  if(!is.list(data_list)){
    cli_abort(c("{.code data_list} should be a list.", 
                i="{.code class(data_list)}: {.val {class(data_list)}}"), 
              class="edc_lookup_empty")
  }
  data_list_n = format(names(data_list))
  data_list[".lookup"] = data_list["date_extraction"] = data_list["datetime_extraction"] = NULL
  if(length(data_list)==0){
    cli_abort(c("{.code data_list} is empty or contains only non-dataframe elements.", 
                i="{.code names(data_list)}: {.val {data_list_n}}"), 
              class="edc_lookup_empty")
  }
  if(!is_named(data_list)){
    cli_abort("Datasets in {.code data_list} should have a name.", 
              class="edc_lookup_unnamed")
  }
  f = function(.x, expr, default) if(is.data.frame(.x)) expr else default
  
  tibble(dataset=tolower(names(data_list))) %>% 
    mutate(
      names=map(data_list, ~f(.x, names(.x), NULL)), 
      labels=map(data_list, ~f(.x, var_label(.x, unlist=TRUE), NULL)), 
      nrow=map_dbl(data_list, ~f(.x, nrow(.x), 0)), 
      ncol=map_dbl(data_list, ~f(.x, ncol(.x), 0)), 
    )
}

#' Find a keyword
#' 
#' Find a keyword in all names and labels of a list of datasets. 
#'
#' @param keyword the keyword to search for
#' @param data the dataframe where to search the keyword. Can be set using `options(edc_lookup=my_data)`, which is done automatically when calling [read_trialmaster()].
#' @param ignore_case should case differences be ignored in the match?
#'
#' @section EDCimport:
#' Usually, 
#'
#' @return a filtered tibble
#' @export
#' @examples 
#' library(crosstable)
#' library(dplyr)
#' library(purrr)
#' 
#' #Using a custom table list
#' lookup = list(i=crosstable::iris2, m=crosstable::mtcars2) %>% get_lookup()
#' find_keyword("hp", data=lookup)
#' find_keyword("number|date", data=lookup)
#' find_keyword("number|date", data=lookup, ignore_case=FALSE)
#' 
#' #Using lookup from [read_trialmaster()]
#' \dontrun{
#' path = system.file("extdata/Example_Export_SAS_XPORT_2022_08_25_15_16.zip", 
#'                    package = "EDCimport", mustWork=TRUE)
#' w = read_trialmaster(path, verbose=FALSE)
#' options(edc_lookup=w$.lookup) #optional
#' find_keyword("patient")
#' }
find_keyword = function(keyword, data=getOption("edc_lookup", NULL), ignore_case=TRUE){
  stopifnot(!is.null(data))
  invalid=names2=labels2=x=NULL
  f = if(isTRUE(ignore_case)) tolower else identity
  keyword = f(keyword)
  f2 = function(x,y) map2_chr(x, y, ~if(.y) {.x} else {f(.x)})
  tmp = data %>% 
    unnest(c(names, labels)) %>% 
    mutate(
      labels=unlist(labels), 
      invalid=invalid_utf8(labels),
      names2=f2(names, invalid),
      labels2=f2(labels, invalid),
    ) %>% 
    filter(str_detect(names2, keyword) | str_detect(labels2, keyword)) %>% 
    select(-names2, -labels2)
  
  if(isTRUE(ignore_case) && any(tmp$invalid)){
    cols = tmp %>% filter(invalid) %>% unite("x", c("dataset", "names"), sep="$") %>% pull(x)
    cli_warn(c("Some columns have labels containing non UTF8 characters. {.arg ignore_case} has been ignored for these.", 
             i="Columns: {.code {cols}}.",
             i='For instance, try to run: {.code attr({cols[1]}, "label")}.'), 
             class="find_keyword_utf8_warning")
  }
  
  tmp %>% 
    select(-invalid)
}


#' @source https://stackoverflow.com/a/57261396/3888000
invalid_utf8 = function(x){
  !is.na(x) & is.na(iconv(x, "UTF-8", "UTF-8"))
}

#' Load a list in an environment
#'
#' @param x a list
#' @param env the environment onto which the list should be loaded
#' @param remove if `TRUE`, `x` will be removed from the environment afterward
#'
#' @return nothing, called for its side-effect
#' @export
#'
#' @examples
#' 
#' x=list(a=1, b=mtcars)
#' load_list(x, remove=FALSE)
#' print(a)
#' print(nrow(b))
#' 
load_list = function(x, env=parent.frame(), remove=TRUE){
  nz = nzchar(names(x))
  if(any(!nz)){
    cli_abort(c("Every member of {.arg x} should have a name.", 
                i="Unnamed member{?s} ind{?ex/ices} of {.arg x}: {as.character(which(!nz))}"), 
              class="load_list_unnamed_error")
  }
  list2env(x, env)
  
  if(remove) remove(list=vname(x), envir=env)
}

#' Load a `.RData` file as a list
#' 
#' Instead of loading a `.RData` file in the global environment, extract every object into a list.
#'
#' @param filename the filename, with the `.RData` extension.
#'
#' @return a list
#' @export
#'
#' @examples
#' 
#' x = list(a=1, b=mtcars)
#' save_list(x, "test.RData")
#' y = load_as_list("test.RData")
#' print(y$a)
load_as_list = function(filename){
  temp_env = new.env()
  load(filename, temp_env)
  as.list(temp_env)
}

#' Save a list as `.RData` file
#'
#' @param x a list
#' @param filename the filename, with the `.RData` extension.
#'
#' @return nothing, called for its side-effect
#' @export
#'
#' @examples
#' x=list(a=1, b=mtcars)
#' save_list(x, "test.RData")
#' load("test.RData")
#' file.remove("test.RData")
#' print(a)
#' print(nrow(b))
save_list = function(x, filename){
  if(!str_ends(tolower(filename), "\\.rdata")){
    cli_abort(c("{.val filename} should have the `.RData` extension.", 
                i="Current filename: {.val {filename}}"))
  }
  save(list=names(x), file=filename, envir=as.environment(x))
}



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
  
  # group = "enrolres$arm"
  if(!is.null(group)){
    if(!str_detect(group, "^.*\\$.*$")){
      cli_abort("{.arg group} is not in the form `dataset$column`.", 
                class="edc_swimplot_group")
    }
    group2 = str_split(group, "\\$", 2)[[1]]
    dat_group = get(group2[1], envir=parent) %>% select(id=!!id, group=!!group2[2])
    if(anyDuplicated(dat_group$id)!=0){
      cli_abort("{.arg group} ({group}) should identify subjects ({id}) uniquely.", 
                class="edc_swimplot_group_dup")
    }
    
    dat = dat %>% left_join(dat_group, by="id")
    
  }
  
  
  x_label = "Calendar date"
  if(!is.null(origin)){
    if(!str_detect(origin, "^.*\\$.*$")){
      cli_abort("{.arg origin} is not in the form `dataset$column`.", 
                class="edc_swimplot_origin")
    }
    origin2 = str_split(origin, "\\$", 2)[[1]]
    dat_origin = dat %>% 
      filter(dataset==origin2[1])
    if(nrow(dat_origin)==0){
      cli_abort(c("{.arg origin} is wrong: no dataset {.val {origin2[1]}} was found."), 
                class="edc_swimplot_origin_dataset")
    }
    dat_origin = dat_origin %>% 
      filter(name==origin2[2]) %>% 
      select(id, origin=value)
    if(nrow(dat_origin)==0){
      cli_abort(c("{.arg origin} is wrong: no column {.val {origin2[2]}} in dataset {.val {origin2[1]}} was found."), 
                class="edc_swimplot_origin_column")
    }
    # browser()
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
    mutate(id=factor(id)) %>% 
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


# plotly::ggplotly()