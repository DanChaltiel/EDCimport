

#' Check the completion of the subject ID column
#' 
#' Compare a subject ID vector to the study's reference subject ID (usually something like `enrolres$subjid`). `check_subjid()` is the old, deprecated name.
#'
#' @param x the subject ID column to check, or a dataframe which ID column will be guessed
#' @param ref the reference for subject ID. Should usually be set through `edc_options(edc_subjid_ref=xxx)`. See example.
#'
#' @return nothing, called for errors/warnings
#' @importFrom cli cli_abort cli_warn
#' @importFrom dplyr any_of pull select setdiff
#' @importFrom rlang caller_arg
#' @export
#'
#' @examples
#' tm = edc_example()
#' load_list(tm)
#' options(edc_subjid_ref=db0$SUBJID)
#' #usually, you set something like:
#' #options(edc_subjid_ref=enrolres$subjid)
#' assert_no_missing_patient(db1)
#' db1 %>% dplyr::filter(SUBJID>1) %>% assert_no_missing_patient()
#' assert_no_missing_patient(c(db1$SUBJID, 99, 999))
assert_no_missing_patient = function(x, ref=getOption("edc_subjid_ref")){
  if(is.null(ref)){
    cli_abort("{.arg ref} cannot be NULL in {.fun assert_no_missing_patient}. See {.help EDCimport::assert_no_missing_patient} to see how to set it.")
  }
  x_name = caller_arg(x)
  if(is.data.frame(x)){
    x = x %>% select(any_of(get_subjid_cols())) %>% pull()
  }
  ref = sort(unique(ref))
  m = setdiff(ref, x) %>% sort()
  if(length(m) > 0){
    cli_warn("Missing {length(m)} subject{?s} ID in {.arg {x_name}}: {.val {m}}",
             class = "edc_assert_no_missing_patient_miss")
  }
  m = setdiff(x, ref) %>% sort()
  if(length(m)>0){
    cli_warn("Additional {length(m)} subject{?s} ID in {.arg {x_name}}: {.val {m}}",
             class="edc_assert_no_missing_patient_additional")
  }
  invisible(x)
}


#' @rdname assert_no_missing_patient
#' @usage NULL
#' @export
#' @importFrom lifecycle deprecate_warn
check_subjid = function(x){
  deprecate_warn("5.0.0", "check_subjid()", "assert_no_missing_patient()")
}


#' Assert that a dataframe has one row per patient
#' 
#' Check that there is no duplicate on the column holding patient ID in a pipeable style. \cr
#' Mostly useful after joining two datasets.
#'
#' @param df a dataframe
#' @param by *(optional)* grouping columns
#' @param id_col the name of the columns holding patient ID
#'
#' @return the `df` dataset, unchanged
#' @importFrom cli cli_abort
#' @importFrom dplyr across count filter select
#' @importFrom rlang current_env
#' @importFrom tidyselect everything
#' @importFrom utils head
#' @export
#'
#' @examples
#' \dontrun{
#' #without duplicate => no error, continue the pipeline
#' tibble(subjid=c(1:10)) %>% assert_no_duplicate() %>% nrow()
#' 
#' #with duplicate => throws an error
#' tibble(subjid=c(1:10, 1:2)) %>% assert_no_duplicate() %>% nrow()
#' 
#' #By groups
#' df = tibble(subjid=rep(1:10, 4), visit=rep(c("V1", "V2"), 2, each=10), 
#'             group=rep(c("A", "B"), each=20))
#' df %>% assert_no_duplicate() #error
#' df %>% assert_no_duplicate(by=c(visit, group)) #no error
#' }
assert_no_duplicate = function(df, by=NULL, id_col=get_subjid_cols()){
  env = current_env()
  id_col_selected = id_col[tolower(id_col) %in% tolower(names(df))]
  
  if(length(id_col_selected) == 0){
    cli_abort("Cannot assert the absence of duplicates: no ID column ({.val {id_col}}) in `names(df)`.", 
              class="edcimport_assert_no_duplicate_no_col")
  }
  if(length(id_col_selected) > 1){
    cli_abort("Cannot assert the absence of duplicates: too many ID column ({.val {id_col_selected}}) in `names(df)`.", 
              class="edcimport_assert_no_duplicate_many_col")
  }
  
  x = df %>% 
    select(any_of2(id_col_selected), {{by}}) %>% 
    count(across(everything())) %>% 
    filter(n>1)
  
  if(nrow(x)>0){
    dups = head(x[[1]], 10) #because of https://github.com/r-lib/cli/issues/617
    cli_abort("Duplicate on column {.val { names(x[1])}} for {qty(length(x[[1]]))} value{?s} {.val {dups}}.", 
              call=env, class="edcimport_assert_no_duplicate")
  }
  df
}


#' Assert that a dataframe has no rows
#' 
#' Check that a dataframe is empty in a pipeable style. \cr
#' Mostly useful for sanity checks.
#'
#' @param df a dataframe
#' @param msg (optional) a custom message to be output (e.g. with the underlying reason)
#'
#' @return nothing
#' @export
#' @importFrom cli cli_abort
#'
#' @examples
#' tm = edc_example()
#' tm$db0 %>% dplyr::filter(age>100) %>% assert_no_rows()
assert_no_rows = function(df, msg=NULL){
  if(nrow(df)>0){
    if(is.null(msg)) msg = "Dataframe should have no rows but has {nrow(df)}."
    cli_abort(msg)
  }
  invisible(NULL)
}



#' Standardized warning system
#' 
#' When checking your data, filter your dataset to get only problematic rows. \cr
#' Then, use either:
#'  * `edc_data_warn()` to generate a standardized warning that can be forwarded to the datamanager 
#'  * `edc_data_warn()` to abort the script if the problem is too serious
#'  
#' Database issues should be traced in a separate table file, with an identifying row number. 
#' Use `edc_data_warnings()` to generate such a file.
#'
#' @param df the filtered dataframe
#' @param message the message. Can use {cli} formats.
#' @param issue_n (optional) identifying row number
#' @param max_subjid max number of subject ID to show in the message
#' @param col_subjid column name for subject ID. Set to `NULL` to ignore.
#' @param ... unused 
#'
#' @return `df` invisibly
#' @export
#' @importFrom rlang check_dots_empty
#'
#' @examples
#' library(dplyr)
#' tm = edc_example()
#' load_list(tm)
#' db0 %>% 
#'   filter(age>70) %>% 
#'   edc_data_warn("Age should not be >70", issue_n=1)
#' 
#' db0 %>% 
#'   filter(age<25) %>% 
#'   edc_data_warn("Age should not be <25", issue_n=2)
#' 
#' db1 %>% 
#'   filter(n()>1, .by=SUBJID) %>% 
#'   edc_data_warn("There are duplicated patients in `db1`", issue_n=3)
#' 
#' edc_data_warnings()
#' 
#' \dontrun{
#' db0 %>% 
#'   filter(age<25) %>% 
#'   edc_data_warn("Age should *really* not be <25")
#' }
edc_data_warn = function(df, message, ..., 
                         issue_n=NULL, max_subjid=5, 
                         col_subjid=get_subjid_cols()){
  
  if (missing(max_subjid)) max_subjid = getOption("edc_warn_max_subjid", max_subjid)
  check_dots_empty()
  edc_data_condition(df=df, message=message, issue_n=issue_n, 
                     max_subjid=max_subjid, col_subjid=col_subjid, 
                     fun=cli_warn)
}

#' @rdname edc_data_warn
#' @usage edc_data_stop(...) #same arguments
#' @export
#' @importFrom rlang check_dots_empty
edc_data_stop = function(df, message, ..., 
                         issue_n=NULL, max_subjid=5, 
                         col_subjid=get_subjid_cols()){
  
  if (missing(max_subjid)) max_subjid = getOption("edc_warn_max_subjid", max_subjid)
  check_dots_empty()
  edc_data_condition(df=df, message=message, issue_n=issue_n, 
                     max_subjid=max_subjid, col_subjid=col_subjid, 
                     fun=cli_abort)
}

#' @noRd
#' @keywords internal
#' @importFrom cli cli_vec format_inline col_green
#' @importFrom dplyr pull
#' @importFrom stringr str_pad
edc_data_condition = function(df, message, issue_n, max_subjid, 
                              col_subjid, fun){
  if(nrow(df)>0){
    if(is.null(issue_n)) issue_n = "xx"
    else if(is.numeric(issue_n)) issue_n = str_pad(issue_n, width=2, pad="0")
    message = format_inline(message)
    subj_label = ""; subj=NULL
    if(!is.null(col_subjid)){
      subj = df %>% pull(any_of2(col_subjid)) %>% unique() %>% sort()
      n_subj = length(subj)
      subj = paste0("#", subj) %>% 
        cli_vec(style=list("vec_trunc"=max_subjid, "vec-trunc-style"="head"))
      subj_label = format_inline(" ({n_subj} patient{?s}: {subj})")
    }
    
    item = tibble(issue_n, message, subjid=list(subj), fun=caller_arg(fun))
    save_warn_list_item(item)
    fun("Issue #{col_green(issue_n)}: {message}{subj_label}")
  }
  invisible(df)
}


edcimport_env = rlang::env()
edcimport_env$warn_list = list()

#' @noRd
#' @keywords internal
save_warn_list_item = function(item){
  stopifnot(nrow(item)==1)
  issue_n = item$issue_n
  issue_key = paste0("issue_", item$issue_n)
  current = edc_data_warnings()
  if(item$issue_n %in% current$issue_n){
    if(item$issue_n=="xx" && !item$message %in% current$message){
      issue_key = paste0("issue_xx_", nrow(current))
    } else {
      cli_warn("Duplicate warning entry")
    }
  }
  edcimport_env$warn_list[[issue_key]] = item
  
}


#' @rdname edc_data_warn
#' @usage edc_data_warnings()
#' @export
edc_data_warnings = function(){
  edcimport_env$warn_list %>% 
    list_rbind() %>% 
    arrange(across(any_of(c("issue_n", "message"))))
}
