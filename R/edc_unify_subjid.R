#' Harmonize the subject ID of the database
#' 
#' Turns the subject ID columns of all datasets into a factor containing levels for all 
#' the subjects of the database. Avoid problems when joining tables, and some checks can
#' be performed on the levels.
#'
#' @param database an [edc_database] object, from [read_trialmaster()] or other EDCimport reading functions.
#' @param preprocess an optional function to modify the subject ID column. Default to `as.numeric()` if applicable and `identity()` otherwise. See examples.
#' @param col_subjid the names of the columns holding the subject ID (as character)
#'
#' @return database, with subject id modified
#' @export
#' @importFrom dplyr across any_of mutate select
#' @importFrom purrr discard_at keep map map_lgl modify_if
#' @importFrom rlang as_function
#'
#' @examples
#' db = edc_example()
#' db$enrol = head(db$enrol, 10)
#' db$enrol$subjid %>% head()
#' db = edc_unify_subjid(db)
#' db$enrol$subjid %>% head()
#' db = edc_unify_subjid(db, preprocess=function(x) paste0("#", x))
#' db$enrol$subjid %>% head()
edc_unify_subjid = function(database, preprocess=NULL, col_subjid=NULL){
  if(is.null(col_subjid)) col_subjid=get_subjid_cols(database$.lookup)
  
  all_subjid = database %>% 
    keep(is.data.frame) %>% 
    discard_at(".lookup") %>% 
    map(~select(.x, any_of(col_subjid)) %>% unlist(use.names=FALSE) %>% unique()) %>% 
    keep(~length(.x)>0)
  
  all_numeric = all_subjid %>% map_lgl(can_be_numeric) %>% all()
  if(is.null(preprocess)){
    if(all_numeric) preprocess = as.numeric else preprocess = as.character
  } else {
    preprocess = as_function(preprocess)
  }
  assert_class(preprocess, "function")
  
  all_subjid = all_subjid %>% 
    unlist() %>%
    as.character() %>%
    preprocess() %>%
    unique() %>%
    mixedsort()
  
  a = database %>% 
    modify_if(is.data.frame, function(df){
      df %>% 
        mutate(across(any_of(col_subjid), ~{
          .x %>% 
            preprocess() %>% 
            factor(levels=all_subjid) %>%
            copy_label_from(.x)
        }))
    }) %>% 
    structure(all_subjid=all_subjid)
  
  a
}

#' @rdname edc_unify_subjid
#' @export
#' @usage NULL
harmonize_subjid = deprecatedly(edc_unify_subjid, when="0.6.0", what="harmonize_subjid()")
