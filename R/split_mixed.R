

#' Identify if a dataframe has a long or a wide format
#' 
#' A dataset is either in the wide format or in the long format ([link](https://towardsdatascience.com/long-and-wide-formats-in-data-explained-e48d7c9a06cb)). 
#' This function identifies the format of a dataframe with respect to a subject ID. 
#' If a dataframe has some wide and long columns, it is considered "mixed".
#' 
#'
#' @param df a dataframe
#' @param id the identifying subject ID
#' @param ... not used
#' @param ignore_cols columns to ignore. Usually meta columns (see [get_meta_cols]).
#' @param na_rm whether to consider missing values
#' @param warn whether to warn if ID is not found
#'
#' @return a string value in `c("wide", "long", "mixed)`
#' @export
#' 
#' @examples
#' tm = edc_example_mixed()
#' sapply(tm, table_format, warn=FALSE) 
table_format = function(df, id=get_subjid_cols(), ..., 
                        ignore_cols=get_meta_cols(0.95), 
                        na_rm=FALSE,
                        warn=TRUE){
  mean_nval = .table_format(df=df, id=id, ignore_cols=ignore_cols, na_rm=na_rm, warn=warn)
  if(is.null(mean_nval)) return(NULL)
  if(all(mean_nval==1)) return("wide")
  if(all(mean_nval>1)) return("long")
  return("mixed")
}


#' @noRd
#' @keywords internal
#' @importFrom cli cli_abort cli_warn
#' @importFrom dplyr across select summarise
#' @importFrom purrr map_dbl
#' @importFrom rlang check_dots_empty
#' @importFrom tidyselect everything
.table_format = function(df, id=get_subjid_cols(), ..., 
                         ignore_cols=get_meta_cols(0.95), 
                         na_rm=FALSE,
                         warn=TRUE){
  check_dots_empty()
  # .x = df
  if(!is.data.frame(df)) return(NULL)
  if(nrow(df)==0 || ncol(df)==0) return(NULL)
  
  if(!any(tolower(id) %in% tolower(names(df)))){
    if(isTRUE(warn)){
      cli_warn("{.val {id}} was not found in {.arg df}. Returning {.val NULL}")
    }
    return(NULL)
  }
  f = if(isTRUE(na_rm)) na.omit else identity
  mean_nval = df %>% 
    select(subjid=any_of2(id), everything(), -any_of2(ignore_cols)) %>% 
    summarise(across(everything(), ~length(unique(f(.x)))), 
              .by=subjid) %>% 
    select(-subjid) %>% 
    map_dbl(mean)
  
  if(any(is.na(mean_nval))) cli_abort("This should not happen, code 263467")
  mean_nval
}


#' Split mixed datasets
#' 
#' Split mixed tables, i.e. tables that hold both long data (N values per patient) and short data (one value per patient, duplicated on N lines), into one long table and one short table.
#'
#' @param datasets a dataframe or a list of dataframes to split. Default to all the datasets from `.lookup`.
#' @param id the patient identifier, probably "SUBJID". Should be shared by all datasets. Case-insensitive.
#' @param ignore_cols columns to ignore when considering a table as long. Default to `getOption("edc_cols_crfname", "CRFNAME")`. Case-insensitive.
#' @param output_code whether to print the code to explicitly write. Can also be a file path.
#' @param verbose whether to print informations about the process.
#' @param ... not used
#'
#' @return a list of the new long and short tables. Use [load_list()] to load them into the global environment.
#' @export
#' @importFrom cli cli_bullets cli_warn
#' @importFrom dplyr across group_by select summarise summarise_all ungroup
#' @importFrom glue glue
#' @importFrom purrr discard discard_at imap keep list_flatten map_chr
#' @importFrom rlang check_dots_empty
#' @importFrom tibble lst
#' @importFrom tidyselect all_of everything
#' @importFrom utils head
#'
#' @examples
#' #tm = read_trialmaster("filename.zip", pw="xx")
#' tm = edc_example_mixed()
#' names(tm)
#' #load_list(tm)
#' print(tm$long_mixed) #`val1` and `val2` are long but `val3` is short
#' 
#' mixed_data = split_mixed_datasets(tm, id="subjid", verbose=TRUE)
#' load_list(mixed_data)
#' print(long_mixed_short) 
#' print(long_mixed_long) 
#' 
#' #alternatively, get the code and only use the datasets you need
#' split_mixed_datasets(tm, id="SUBJID", output_code=TRUE)
#' filename = tempfile("mixed_code", fileext=".R")
#' split_mixed_datasets(tm, id="SUBJID", output_code=filename)
#' readLines(filename)
split_mixed_datasets = function(datasets=get_datasets(), id=get_subjid_cols(), ..., 
                                ignore_cols=get_meta_cols(0.95), 
                                output_code=FALSE,
                                verbose=TRUE){
  check_dots_empty()
  if(is.data.frame(datasets)) datasets = list(datasets)
  override_ignore_cols = options("edc_override_ignore_cols")[[1]]
  if(!is.null(override_ignore_cols)) ignore_cols = override_ignore_cols
  datasets = datasets %>% keep(is.data.frame) %>% discard_at(".lookup")
  
  dataset_mean_nval = datasets %>%
    imap( ~ {
      if (!any(tolower(id) %in% tolower(names(.x))))
        return(NULL)
      if (nrow(.x) == 0 || ncol(.x) == 0)
        return(NULL)
      .x %>%
        group_by(across(any_of2(id))) %>%
        summarise_all( ~ length(unique(.x))) %>%
        ungroup() %>%
        select(-any_of2(id)) %>%
        summarise_all( ~ mean(.x)) %>%
        unlist()
    })
  
  #TODO pas possible car il faut laisser les ignore_cols dans les tables wide!
  # dataset_mean_nval = datasets %>% 
  #   map(~.table_format(df=.x, id=id, ignore_cols=ignore_cols, na_rm=F, warn=warn))
  
  #TODO option pour faire plutôt length(unique(na.omit(.x))) ?
  #si c'est manquant sur une ligne et pas sur une autre on 
  #peut sans doute unifier quand même
  
  not_found = dataset_mean_nval %>% keep(is.null) %>% names()
  if(length(not_found) == length(datasets)){
    cli_warn("{.val {id}} was not found in any table. Returning {.val NULL}")
    return(NULL)
  }
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
  f = function(x) length(unique(x[!tolower(names(x)) %in% tolower(ignore_cols)]))
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
      id = dat %>% select(any_of2(id)) %>% names() %>% head(1)
      short = dat %>% 
        select(all_of(id), all_of(names(.x[.x==1]))) %>% 
        group_by(across(all_of(id))) %>% 
        summarise(across(everything(), unify)) %>% 
        copy_label_from(dat)
      long = dat %>% 
        select(all_of(id), all_of(names(.x[.x!=1]))) %>% 
        copy_label_from(dat)
      
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
    cat(code, file=output_code, append=TRUE)
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
  rtn_label = get_label(x)
  if(!is.null(rtn_label)) attr(rtn, "label") = rtn_label
  rtn
}
