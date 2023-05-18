
#' Split mixed datasets
#' 
#' Split mixed tables, i.e. tables that hold both long data (N values per patient) and short data (one value per patient, duplicated on N lines), into one long table and one short table.
#'
#' @param datasets the datasets to consider. Use the helper [get_datasets()] if needed.
#' @param id the patient identifier, probably "SUBJID". Should be shared by all datasets.
#' @param ignore_cols columns to ignore when considering a table as long. default to 
#' @param output_code whether to print the code to explicitly write. Can also be a file path.
#' @param verbose whether to print informations about the process.
#' @param ... not used
#'
#' @return a list of the new long and short tables. Use [load_list()] to load them into the global environment.
#' @export
#'
#' @examples
#' #tm = read_trialmaster("filename.zip", pw="xx")
#' tm = edc_example_mixed()
#' names(tm)
#' #load_list(tm)
#' print(tm$long_mixed) #`val1` and `val2` are long but `val3` is short
#' 
#' mixed_data = split_mixed_datasets(tm, id="SUBJID", verbose=TRUE)
#' load_list(mixed_data)
#' print(long_mixed_short) 
#' print(long_mixed_long) 
#' 
#' #alternatively, get the code and only use the datasets you need
#' split_mixed_datasets(tm, id="SUBJID", output_code=TRUE)
#' filename = tempfile("mixed_code", fileext=".R")
#' split_mixed_datasets(tm, id="SUBJID", output_code=filename)
#' readLines(filename)
#' @importFrom cli cli_bullets
#' @importFrom dplyr across group_by select summarise summarise_all
#' @importFrom glue glue
#' @importFrom purrr discard imap keep list_flatten map_chr
#' @importFrom rlang check_dots_empty
#' @importFrom tibble lst
#' @importFrom tidyselect all_of everything
split_mixed_datasets = function(datasets=get_datasets(), id, ..., 
                                ignore_cols=getOption("edc_crfname", "CRFNAME"), 
                                output_code=FALSE,
                                verbose=TRUE){
  check_dots_empty()
  datasets = datasets %>% keep(~is.data.frame(.x))
  dataset_mean_nval = datasets %>% 
    imap(~{
      if(!any(id %in% names(.x))) return(NULL)
      if(nrow(.x)==0 || ncol(.x)==0) return(NULL)
      .x %>% 
        group_by(across(any_of(id))) %>% 
        summarise_all(~length(unique(.x))) %>% 
        select(-any_of(id)) %>% 
        summarise_all(~mean(.x)) %>%
        unlist()
    })
  
  #TODO option pour faire plutôt length(unique(na.omit(.x))) ?
  #si c'est manquant sur une ligne et pas sur une autre on 
  #peut sans doute unifier quand même
  
  not_found = dataset_mean_nval %>% keep(is.null) %>% names()
  if(length(not_found)>0 && verbose){
    cli_bullets(c("!"="{.val {id}} was not found in {length(not_found)} table{?s}:", 
                  " "="{.val {not_found}}."))
  }
  
  short = dataset_mean_nval %>% 
    discard(is.null) %>% 
    keep(~all(.x==1))
  
  if(length(short)>0 && verbose){
    cli_bullets(c(v="There {?was/were} {length(short)} short table{?s}:", 
                  " "="{.val {names(short)}}"))
  }
  
  long = dataset_mean_nval %>% 
    discard(is.null) %>% 
    discard(~all(.x==1))
  f = function(x) length(unique(x[!tolower(names(x)) %in% ignore_cols]))
  pure_long = long %>% keep(~f(.x)==1)
  
  if(length(pure_long)>0 && verbose){
    cli_bullets(c(v="There {?was/were} {length(pure_long)} pure long table{?s}:", 
                  " "="{.val {names(pure_long)}}"))
  }
  
  mixed_long = long %>% keep(~f(.x)!=1) %>% 
    structure(label=glue("Mean number of unique values per {id}"))
  
  if(length(mixed_long)==0){
    if(verbose) cli_bullets(c("v"="There was no mixed table, no change needed."))
    return(invisible(list()))
  }
  
  rtn = mixed_long %>% 
    imap(~{
      a = paste(names(.x[.x==1]), collapse=', ')
      b = paste(names(.x[.x!=1]), collapse=', ')
      dat = datasets[[.y]]
      id = intersect(id, names(dat))[1]
      short = dat %>% 
        select(all_of(id), all_of(names(.x[.x==1]))) %>% 
        group_by(across(all_of(id))) %>% 
        summarise(across(everything(), unify))
      long = dat %>% 
        select(all_of(id), all_of(names(.x[.x!=1])))
      
      short_code = glue("{.y}",
                        "select({id}, {a})",
                        "group_by({id})",
                        "summarise(across(everything(), unify))", 
                        .sep=" %>% \n  ", .trim=FALSE, .null="ERROR")
      short_code = glue("{.y}_short = {short_code} #dim={nrow(short)}x{ncol(short)}")
      long_code = glue("{.y}",
                       "select({id}, {b})",
                       .sep=" %>% \n  ", .trim=FALSE, .null="ERROR")
      long_code = glue("{.y}_long = {long_code} #dim={nrow(long)}x{ncol(long)}")
      
      code = glue("## `{.y}` (dim={nrow(dat)}x{ncol(dat)}) ---- \n\n", 
                  "{short_code} \n\n {long_code}", .null="ERROR")
      attr(short, "data_name") = get_data_name(dat)
      attr(long, "data_name") = get_data_name(dat)
      lst(short, long, code)
    })
  
  # browser()
  code = rtn %>% 
    map_chr("code") %>% 
    paste(collapse="\n\n\n") %>% 
    paste("\n")
  
  if(length(mixed_long)>0 && verbose){
    cli_bullets(c(v="There {?was/were} {length(mixed_long)} mixed (short+long) table{?s}:", 
                  " "="{.val {names(mixed_long)}}"))
  }
  
  if(isTRUE(output_code) || output_code=="console"){
    cli_bullets(c(">"="Copy the following code in your script to separate long and short data: "))
    cat(code)
  } else if(is.character(output_code)){
    cli_bullets(c(">"="Copy the code from {.path {output_code}} in your script 
                       to separate long and short data: ", 
                  " "="{.run utils::browseURL({output_code})}"))
    cat(code, file=output_code)
  } else if(verbose){
    cli_bullets(c(">"="Use {.fun EDCimport::load_list} on the result to get separated long and short data."))
  }
  
  rtn %>% 
    list_flatten() %>% 
    keep(is.list) %>% 
    structure(mean_unique_values="mixed_long") %>% 
    invisible()
}



#' Unify a vector
#' 
#' Turn a vector of length N to a vector of length 1 after checking that there is only one unique value. Useful to safely flatten a duplicated table. This preserves the `label` attribute if set.
#'
#' @param x a vector
#'
#' @return a vector of length 1
#' @export
#' @importFrom cli cli_warn
#' @importFrom labelled var_label
#' @importFrom stats na.omit
#'
#' @examples
#' unify(c(1,1,1,1))
#' #unify(c(1,1,2,1)) #warning
#' 
#' library(dplyr)
#' x=tibble(id=rep(letters[1:5],10), value=rep(1:5,10))
#' x %>% group_by(id) %>% summarise(value=unify(value)) #safer than `value=value[1]`
#' x$value[2]=1
#' #x %>% group_by(id) %>% summarise(value=unify(value)) #warning about that non-unique value
unify = function(x){
  rtn = x[1]
  lu = length(unique(na.omit(x)))
  if(lu>1){
    cli_warn(c("Unifying multiple values in {.val {caller_arg(x)}}, returning the first one ({.val {rtn})}", 
               i="Unique values: {.val {unique(na.omit(x))}}"))
  }
  rtn_label = var_label(x)
  if(!is.null(rtn_label)) attr(rtn, "label") = rtn_label
  rtn
}
