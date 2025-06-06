
#' Harmonize the subject ID of the database
#' 
#' Turns the subject ID columns of all datasets into a factor containing levels for all 
#' the subjects of the database. Avoid problems when joining tables, and some checks can
#' be performed on the levels. See `vignette("postprocessing")` for a real-life case.
#'
#' @param database an [edc_database] object, from [read_trialmaster()] or other EDCimport reading functions.
#' @param preprocess an optional function to modify the subject ID column (at the character level). Default behavior is only to remove trailing zeros if numeric. 
#' @param mode the output type of the subject ID columns
#' @param col_subjid names of the subject ID columns (as character)
#'
#' @return database, with subject id modified
#' @export
#' @importFrom dplyr across any_of mutate select
#' @importFrom purrr discard_at keep map map_lgl modify_if
#' @importFrom rlang arg_match as_function
#'
#' @examples
#' 
#' db = edc_example()
#' db$enrol$subjid %>% head()  #double vector
#' 
#' db2 = edc_unify_subjid(db)
#' db2$enrol$subjid %>% head() #factor with 50 levels
#' 
#' db3 = edc_unify_subjid(db, preprocess=function(x) paste0("#", x))
#' db3$enrol$subjid %>% head()
#' 
#' #use numeric mode to get a numeric output
#' db4 = edc_unify_subjid(db, preprocess=function(x) as.numeric(x)+1, mode="numeric")
#' db4$enrol$subjid %>% head()
edc_unify_subjid = function(database, preprocess=NULL, mode=c("factor", "numeric"),
                            col_subjid=NULL){
  if(is.null(col_subjid)) col_subjid=get_subjid_cols(database$.lookup)
  mode = arg_match(mode)
  
  subjid_list = .get_subjid_list(database, col_subjid)
  preprocess = .get_preprocess(preprocess, subjid_list)
  subjid_vec = .get_subjid_vec(subjid_list, preprocess, mode)
  
  assert_class(preprocess, "function")
  
  numeric_subjid = subjid_list %>% map_lgl(~can_be_numeric(preprocess(.x)))
  
  if(mode=="factor"){
    unify_subjid = function(x) preprocess(x) %>% factor(levels=subjid_vec)
  } else if(mode=="numeric"){
    if(!all(numeric_subjid)){
      cli_abort(c('Cannot use `mode="numeric"` on non-numeric subject IDs.',
                  i="Datasets containing non-numeric subject IDs: {.val {names(numeric_subjid[!numeric_subjid])}}"),
                class="edc_unify_subjid_non_numeric_error")
    }
    # subjid_vec = as.numeric(subjid_vec) %>% sort()
    unify_subjid = function(x) preprocess(x) %>% as.numeric()
  }
  
  
  a = database %>% 
    modify_if(is.data.frame, function(df){
      df %>% 
        mutate(across(any_of(col_subjid), ~{
          .x %>% 
            unify_subjid() %>% 
            copy_label_from(.x)
        }))
    }) %>% 
    structure(all_subjid=subjid_vec)
  
  a
}

#' @rdname edc_unify_subjid
#' @export
#' @usage NULL
harmonize_subjid = deprecatedly(edc_unify_subjid, when="0.6.0", what="harmonize_subjid()")


# Utils ---------------------------------------------------------------------------------------


#' If NULL and all can be numeric, parsed to numeric (to merge 001 with 1)
#' If NULL and not numeric, parsed to character
#' If not NULL, parsed to rlang function
#' @noRd
.get_preprocess = function(preprocess, subjid_list) {
  if(is.null(preprocess)){
    numeric_subjid = subjid_list %>% map_lgl(~can_be_numeric(.x))
    if(all(numeric_subjid)) preprocess = as.numeric else preprocess = as.character
  } else {
    preprocess = as_function(preprocess)
  }
  preprocess
}

#' list of SUBJID vectors for each dataset
#' @noRd
.get_subjid_list = function(database, col_subjid) {
  database %>% 
    keep(is.data.frame) %>% 
    discard_at(".lookup") %>%
    map(~{
      select(.x, any_of(col_subjid)) %>% unlist(use.names = FALSE) %>% unique()
    }) %>%
    keep(~length(.x) > 0)
}

#' vector of unique SUBJID throughout the whole database
#' @noRd
.get_subjid_vec = function(subjid_list, preprocess, mode) {
  
  rtn = subjid_list %>% 
    unlist() %>%
    as.character() %>%
    preprocess()
  
  if(mode=="numeric"){
    rtn = as.numeric(rtn) %>% 
      suppressWarnings() %>% #coercion errors are addressed later
      unique() %>%
      sort()
  } else {
    rtn = as.character(rtn) %>% 
      unique() %>%
      mixedsort()
  }
  
  rtn
}
