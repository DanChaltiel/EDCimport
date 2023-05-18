

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
#' @importFrom cli cli_abort
#' @importFrom dplyr arrange mutate
#' @importFrom labelled var_label
#' @importFrom purrr map map_dbl
#' @importFrom rlang is_named
#' @importFrom tibble tibble
get_lookup = function(data_list){
  if(!is.list(data_list)){
    cli_abort(c("{.code data_list} should be a list.", 
                i="{.code class(data_list)}: {.cls {class(data_list)}}"), 
              class="edc_lookup_not_list")
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
      nrow=map_dbl(data_list, ~f(.x, nrow(.x), 0)), 
      ncol=map_dbl(data_list, ~f(.x, ncol(.x), 0)), 
      names=map(data_list, ~f(.x, names(.x), NULL)), 
      labels=map(data_list, ~f(.x, var_label(.x, unlist=TRUE), NULL)), 
    ) %>% 
    arrange(nrow)
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
#' @importFrom cli cli_warn
#' @importFrom dplyr filter mutate pull select
#' @importFrom purrr map2_chr
#' @importFrom stringr str_detect
#' @importFrom tidyr unite unnest
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
      invalid=is_invalid_utf8(labels),
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
#' @importFrom cli cli_abort cli_warn
#' @importFrom rlang caller_arg
load_list = function(x, env=parent.frame(), remove=TRUE){
  if(length(x)==0){
    cli_warn("List was empty.")
    return(invisible())
  }
  nz = nzchar(names(x))
  if(any(!nz)){
    cli_abort(c("Every member of {.arg x} should have a name.", 
                i="Unnamed member{?s} ind{?ex/ices} of {.arg x}: {as.character(which(!nz))}"), 
              class="load_list_unnamed_error")
  }
  list2env(x, env)
  
  if(remove) remove(list=caller_arg(x), envir=env)
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
#' @importFrom cli cli_abort
#' @importFrom stringr str_ends
save_list = function(x, filename){
  if(!str_ends(tolower(filename), "\\.rdata")){
    cli_abort(c("{.val filename} should have the `.RData` extension.", 
                i="Current filename: {.val {filename}}"))
  }
  save(list=names(x), file=filename, envir=as.environment(x))
}


#' Extend the lookup table
#' 
#' This utility extends the lookup table to include:\cr
#'   * `n_id` the number of patients present in the dataset
#'   * `rows_per_id` the mean number of row per patient
#'   * `crfname` the actual name of the dataset
#'
#' @param lookup \[`data.frame(1)`]\cr the lookup table
#' @param datasets \[`data.frame(n)`]\cr for experts only
#' @inheritParams read_tm_all_xpt
#'
#' @return the lookup, extended
#' @export
#' @importFrom cli cli_abort
#' @importFrom dplyr arrange desc mutate relocate select
#' @importFrom purrr map_chr map_int
#' @importFrom rlang check_dots_empty
#' @importFrom tidyselect last_col matches
#' @examples
#' #tm = read_trialmaster("filename.zip", pw="xx")
#' tm = edc_example_mixed()
#' load_list(tm)
#' .lookup
#' .lookup = extend_lookup(.lookup)
#' .lookup
extend_lookup = function(lookup, ..., 
                         key_columns = get_key_cols(),
                         datasets = get_datasets(lookup)){
  check_dots_empty()
  #case-insensitive column selection
  f = function(x, colname){
    rtn = select(datasets[[x]], any_of2(colname))
    if(ncol(rtn)==0) return(NA)
    if(ncol(rtn)>1) cli_warn("Several columns named {.val {colname}}: {.val {names(rtn)}}.")
    rtn[[1]]
  }
  rtn = lookup %>% 
    mutate(
      n_id = map_int(dataset, ~length(unique(f(.x, key_columns$patient_id)))),
      rows_per_id = round(nrow/n_id,1),
      crfname = map_chr(dataset, ~get_data_name(datasets[[.x]]), crfname=key_columns$crfname)
    ) %>% 
    arrange(n_id, desc(nrow)) %>% 
    relocate(c(names, labels), .after=last_col())
  rtn
}



#' Retrieve the datasets
#' 
#' Get the datasets from the lookup table as a list of data.frames.
#'
#' @param lookup the lookup table
#' @param envir (internal use)
#'
#' @return a list of all datasets
#' @export
#' @importFrom purrr map
#' @importFrom rlang set_names
get_datasets = function(lookup=getOption("edc_lookup", NULL), envir=parent.frame()){
  lookup$dataset %>% 
    set_names() %>% 
    map(~get(.x, envir=envir))
}


#' Important column names
#'
#' @param patient_id the name of the columns containing the patient ID
#' @param crfname the name of the columns containing the name of the CRF page
#' @param ... unused
#'
#' @export
#' @importFrom tibble lst
get_key_cols = function(patient_id = getOption("edc_id", c("ptno", "subjid")), 
                        crfname = getOption("edc_crfname", "crfname"), 
                        ...){
  lst(patient_id, crfname)
}
