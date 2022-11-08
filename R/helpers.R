

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
  data_list = data_list %>% keep(is.data.frame)
  data_list[".lookup"] = NULL
  if(length(data_list)==0){
    cli_abort(c("{.code data_list} is empty or contains only non-dataframe elements.", 
                i="{.code names(data_list)}: {.val {data_list_n}}"), 
              class="edc_lookup_empty")
  }
  if(!is_named(data_list)){
    cli_abort("Datasets in {.code data_list} should have a name.", 
              class="edc_lookup_unnamed")
  }
  tibble(dataset=tolower(names(data_list))) %>% 
    mutate(names=map(data_list, ~names(.x)), 
           labels=map(data_list, ~var_label(.x, unlist=TRUE)), 
           nrow=map_dbl(data_list, nrow), 
           ncol=map_dbl(data_list, ncol))
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
  f = if(isTRUE(ignore_case)) tolower else identity
  
  data %>% 
    unnest(c(names, labels)) %>% 
    mutate(labels=unlist(labels)) %>% 
  # mutate(labels=map_chr(labels, ~ifelse(is.null(.x), NA, .x))) %>%  #better ? 
    filter(str_detect(f(names), f(keyword)) |
             str_detect(f(labels), f(keyword)))
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
