
#TODO unify
#TODO https://github.com/r-lib/cli/issues 
#TODO en attendant https://github.com/r-lib/cli/issues/507
#' Unify a vector
#' 
#' Turn a vector of length N to a vector of length 1 after checking that there is only one unique value. Useful to safely flatten a duplicated table. This preserves the `label` attribute if set.
#'
#' @param x a vector
#'
#' @return a vector of length 1
#' @export
#'
#' @examples
#' unify(c(1,1,1,1))
#' #unify(c(1,1,2,1)) #warning
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



# TODO getOption(EDCimport_id, "SUBJID")


# library(scales)


# palette = "viridis"
# turnover = 5
# colourer = scales::col_factor(palette, domain=NULL)
# 
# # library(flextable)
# diary %>% 
#   head(6) %>% 
#   mutate_all(~as.numeric(as.factor(.x)) %% turnover) %>% 
#   flextable::flextable() %>% autofit() %>% 
#   flextable::fontsize() %>% 
#   flextable::rotate(rotation=c("lrtb", "tbrl", "btlr")[3], part="header") %>% 
#   flextable::bg(bg=colourer, part="body")


#' Split mixed datasets
#' 
#' Split mixed tables, i.e. tables that hold both long data (N values per patient) and short data (one value per patient, duplicated on N lines), into one long table and one short table.
#'
#' @param id the patient identifier, probably "SUBJID". Should be shared by all datasets.
#' @param output_code whether to print the code. Can also be a file path.
#' @param verbose whether to print informations about the process.
#' @param lookup the lookup table
#' @param ... not used
#'
#' @return a list of the new long and short tables. Use [load_list()] to load them in the global environment.
#' @export
#'
#' @examples
#' \dontrun{
#' tm = read_trialmaster("filename.zip", pw="xx")
#' load_list(tm)
#' 
#' mixed = split_mixed_datasets("SUBJID")
#' load_list(mixed) 
#' }
split_mixed_datasets = function(id, ..., 
                                verbose=TRUE, output_code=FALSE, 
                                lookup=getOption("edc_lookup", NULL)){
  check_dots_empty()
  envir = parent.frame()
  datasets = lookup$dataset %>% 
    set_names() %>% 
    map(~get(.x, envir=envir))
  
  
  # dataset_mean_nval = lookup$dataset %>%
  #   set_names() %>%
  #   map(~{
  #     dat = get(.x, envir=envir)
  #     if(!id %in% names(dat)) return(NULL)
  #     if(nrow(dat)==0 || ncol(dat)==0) return(NULL)
  #     # dat %>%
  #     #   group_by(across(all_of(id))) %>%
  #     #   summarise_all(~length(unique(.x))) %>%
  #     #   select(-all_of(id)) %>%
  #     #   summarise_all(~length(unique(.x))) %>%
  #     #   unlist()
  #     dat %>%
  #       group_by(across(all_of(id))) %>%
  #       summarise_all(~length(unique(.x))) %>%
  #       # select(all_of(id), GRPINSNO) %>%
  #       select(-all_of(id)) %>%
  #       summarise_all(~mean(.x)) %>%
  #       # summarise_all(~length(.x)) %>%
  #       unlist()
  #   })
  dataset_mean_nval = datasets %>% 
    imap(~{
      if(!id %in% names(.x)) return(NULL)
      if(nrow(.x)==0 || ncol(.x)==0) return(NULL)
      .x %>% 
        group_by(across(all_of(id))) %>% 
        summarise_all(~length(unique(.x))) %>% 
        select(-all_of(id)) %>% 
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
  pure_long = long %>% keep(~length(unique(.x))==1)
  
  if(length(pure_long)>0 && verbose){
    cli_bullets(c(v="There {?was/were} {length(pure_long)} pure long table{?s}:", 
                  " "="{.val {names(pure_long)}}"))
  }
  
  
  mixed_long = long %>% keep(~length(unique(.x))!=1) %>% 
    structure(label=glue("Mean number of unique values per {id}"))
  
  if(length(mixed_long)==0){
    cli_bullets(c("v"="There was no mixed table, no change needed."))
    return(invisible(list()))
  }
  
  rtn = mixed_long %>% 
    imap(~{
      a = paste(names(.x[.x==1]), collapse=', ')
      b = paste(names(.x[.x!=1]), collapse=', ')
      dat = get(.y)
      short = dat %>% 
        select({{id}}, all_of(names(.x[.x==1]))) %>% 
        group_by(across(all_of(id))) %>% 
        summarise(across(everything(), unify))
      long = dat %>% 
        select({{id}}, all_of(names(.x[.x!=1])))
      
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
      lst(short, long, code)
    })
  
  code = rtn %>% 
    map_chr("code") %>% 
    paste(collapse="\n\n\n")
  
  if(length(mixed_long)>0 && verbose){
    cli_bullets(c(v="There {?was/were} {length(mixed_long)} mixed (short+long) table{?s}:", 
                  " "="{.val {names(mixed_long)}}"))
  }
  
  if(output_code=="console"){
    cli_bullets(c(">"="Copy the following code in your script to separate long and short data: "))
    cat(code)
  } else if(isTRUE(output_code)){
    cli_bullets(c(">"="Copy the code from {.path {output_code}} in your script 
                       to separate long and short data: ", 
                  " "="{.run browseURL({output_code})}"))
    cat(code, file=output_code)
  } else {
    cli_bullets(c(">"="Use {.fun EDCimport::load_list} on the result to get separated long and short data."))
  }
  
  
  rtn %>% 
    list_flatten() %>% 
    keep(is.list) %>% 
    structure(mean_unique_values="mixed_long") %>% 
    invisible()
}
