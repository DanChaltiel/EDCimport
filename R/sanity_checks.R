

#' Check the validity of the subject ID column
#' 
#' Compare a subject ID vector to the study's reference subject ID (usually something like `enrolres$subjid`), and warn if any patient is missing or extra. \cr
#' `check_subjid()` is the old, deprecated name.
#'
#' @param x the subject ID vector to check, or a dataframe which ID column will be guessed
#' @param ref the reference for subject ID. Should usually be set through `edc_options(edc_subjid_ref=xxx)`. See example.
#' @param data_name the name of the data (for the warning message)
#' @param col_subjid name of the subject ID column if `x` is a dataframe.
#' @inheritParams edc_data_warn
#'
#' @return nothing, called for errors/warnings
#' @importFrom cli cli_abort cli_warn format_inline
#' @importFrom dplyr any_of pull select setdiff
#' @importFrom rlang caller_arg
#' @importFrom stringr str_pad
#' @importFrom tibble tibble
#' @export
#'
#' @examples
#' tm = edc_example()
#' load_list(tm)
#' options(edc_subjid_ref=db0$SUBJID)
#' #usually, you set something like:
#' #options(edc_subjid_ref=enrolres$subjid)
#' edc_warn_patient_diffs(db1)
#' db1 %>% dplyr::filter(SUBJID>1) %>% edc_warn_patient_diffs()
#' edc_warn_patient_diffs(c(db1$SUBJID, 99, 999))
edc_warn_patient_diffs = function(x, ref=getOption("edc_subjid_ref"), 
                                  issue_n="xx", data_name=NULL, 
                                  col_subjid=get_subjid_cols()){
  if(is.null(ref)){
    cli_abort(c("Argument {.arg ref} cannot be NULL in {.fun edc_warn_patient_diffs}.", 
                " "="See {.help EDCimport::edc_warn_patient_diffs} to see how to set it."))
  }
  if(is.null(data_name)) data_name=caller_arg(x)
  x_bak = x
  
  if(is.data.frame(x)){
    x = x %>% select(subjid=any_of(col_subjid)) %>% pull()
  }
  ref = sort(unique(ref))
  missing_id = setdiff(ref, x) %>% sort()
  extra_id = setdiff(x, ref) %>% sort()
  n_pat = length(missing_id) + length(extra_id)
  
  if(n_pat>0){
    
    par_subj_miss = par_subj_extra = NULL
    if(length(missing_id) > 0){
      par_subj_miss = format_inline("Missing: {format_subj(missing_id, par=FALSE)}")
    }
    if(length(extra_id)>0){
      par_subj_extra = format_inline("Extra: {format_subj(extra_id, par=FALSE)}")
    }
    
    par_issue = ""
    if(!is.null(issue_n)){
      if(data_name=="."){
        cli_warn("In `edc_warn_patient_diffs()`, `data_name` should not be NULL 
                 if `issue_n` is not NULL and function is piped, otherwise the 
                 message will be unreadable.")
      }
      if(is.numeric(issue_n)) issue_n = str_pad(issue_n, width=2, pad="0")
      par_issue = format_inline("Issue #{col_green(issue_n)}: ")
      item1 = tibble(issue_n, message=format_inline("{data_name}: {par_subj_miss}"), 
                     subjid=list(missing_id), fun="cli_warn")
      save_warn_list_item(item1)
      item2 = tibble(issue_n, message=format_inline("{data_name}: {par_subj_extra}"), 
                     subjid=list(extra_id), fun="cli_warn")
      save_warn_list_item(item2)
    }
    
    message = format_inline("{.arg {data_name}} has patient discrepancies:")
    
    cli_warn(c("{par_issue}{message}", 
               i=par_subj_miss, 
               i=par_subj_extra),
             class="edc_edc_patient_diffs_warning")
    
  }
  
  invisible(x_bak)
}

#' Warn if extraction is too old
#'
#' @param max_days the max acceptable age of the data
#'
#' @return nothing
#' @export
#'
#' @examples
#' tm = edc_example()
#' load_list(tm)
#' edc_warn_extraction_date()
#' @importFrom cli cli_warn
edc_warn_extraction_date = function(max_days=30){
  a = round(as.numeric(Sys.time() - datetime_extraction))
  if(a>max_days){
    cli_warn("{col_red('- OUTDATED - ')}Data extraction is {col_red(a)} days old.")
  }
  invisible(TRUE)
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
#' @importFrom dplyr across count everything filter select
#' @importFrom rlang current_env
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





#' Standardized warning system
#' 
#' When checking your data, filter your dataset to get only problematic rows. \cr
#' Then, use either:
#'  * `edc_data_warn()` to generate a standardized warning that can be forwarded to the datamanager 
#'  * `edc_data_warn()` to abort the script if the problem is too serious
#'  
#' Database issues should be traced in a separate file, each with an identifying row number, and the file should be shared with the data-manager. \cr
#' Use `edc_data_warnings()` to generate the table for such a file.
#'
#' @param df the filtered dataframe
#' @param message the message. Can use [cli formats](https://cli.r-lib.org/reference/inline-markup.html#classes). `df` can be accessed using the `.data` special keyword (see example)
#' @param issue_n identifying row number
#' @param csv_path a path to save `df` in a csv file that can be shared with the DM for more details.
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
#'   edc_data_warn("There are duplicated patients in `db1` ({nrow(.data)} rows)", issue_n=3)
#' 
#' db0 %>% 
#'   filter(age<25) %>% 
#'   edc_data_warn("Age should not be <25", issue_n=NULL)
#'   
#' edc_data_warnings()
#' 
#' \dontrun{
#' db0 %>% 
#'   filter(age<25) %>% 
#'   edc_data_warn("Age should not be <25", csv_path="check/check_age_25.csv")
#'   
#' db0 %>% 
#'   filter(age<25) %>% 
#'   edc_data_stop("Age should *never* be <25")
#' }
edc_data_warn = function(df, message, ..., 
                         issue_n="xx", max_subjid=5, 
                         csv_path=FALSE, 
                         col_subjid=get_subjid_cols()){
  
  if (missing(max_subjid)) max_subjid = getOption("edc_warn_max_subjid", max_subjid)
  check_dots_empty()
  edc_data_condition(.data=df, message=message, issue_n=issue_n, csv_path=csv_path,
                     max_subjid=max_subjid, col_subjid=col_subjid, 
                     fun=cli_warn)
}

#' @rdname edc_data_warn
#' @usage edc_data_stop(df, message, ..., issue_n, max_subjid, csv_path, col_subjid)
#' @export
#' @importFrom rlang check_dots_empty
edc_data_stop = function(df, message, ..., 
                         issue_n="xx", max_subjid=5, 
                         csv_path=FALSE, 
                         col_subjid=get_subjid_cols()){
  
  if (missing(max_subjid)) max_subjid = getOption("edc_warn_max_subjid", max_subjid)
  check_dots_empty()
  edc_data_condition(.data=df, message=message, issue_n=issue_n, csv_path=csv_path,
                     max_subjid=max_subjid, col_subjid=col_subjid, 
                     fun=cli_abort)
}




#' @rdname edc_data_warn
#' @usage edc_data_warnings()
#' @export
#' @importFrom dplyr across any_of arrange
#' @importFrom purrr list_rbind
edc_data_warnings = function(){
  edcimport_env$warn_list %>% 
    list_rbind() %>% 
    arrange(across(any_of(c("issue_n", "message"))))
}


# Utils ---------------------------------------------------------------------------------------


#' @noRd
#' @keywords internal
#' @importFrom cli cli_abort cli_vec cli_warn format_inline
#' @importFrom dplyr pull
#' @importFrom rlang caller_arg
#' @importFrom stringr str_ends str_pad
#' @importFrom tibble tibble
#' @importFrom utils write.csv2
edc_data_condition = function(.data, message, issue_n, max_subjid, 
                              csv_path, 
                              col_subjid, fun){
  if(nrow(.data)>0){
    if(is.character(csv_path)){
      assert(str_ends(csv_path, "\\.csv"), call=parent.frame())
      write.csv2(.data, csv_path, row.names=FALSE)
    }
    message = format_inline(message)
    
    par_issue = par_subj = ""; subj=NULL
    if(is.null(issue_n)) issue_n = NA
    
    if(!is.na(issue_n)){
      if(is.numeric(issue_n)) issue_n = str_pad(issue_n, width=2, pad="0")
      par_issue = format_inline("Issue #{col_green(issue_n)}: ")
    }
    
    if(!is.null(col_subjid)){
      col_found = tolower(col_subjid) %in% tolower(names(.data))
      if(sum(col_found)>1){
        cli_warn("Found {length(col_found)} subject identifiers in the input dataset:
                 {.val {col_subjid[col_found]}}. Defaulting to the first one.", 
                  class="edc_data_condition_subjid_multiple_warn", call=parent.frame())
        col_subjid = col_subjid[col_found][1]
      }
      if(!any(col_found)){
        cli_abort("Could not find column {col_subjid} in the input dataset.", 
                  class="edc_data_condition_subjid_error", call=parent.frame())
      }
      subj = .data %>% pull(any_of2(col_subjid)) %>% unique() %>% sort()
      n_subj = length(subj)
      subj = paste0("#", subj) %>% 
        cli_vec(style=list("vec_trunc"=max_subjid, "vec-trunc-style"="head"))
      par_subj = format_inline(" ({n_subj} patient{?s}: {subj})")
    }
    fun_name = caller_arg(fun)
    item = tibble(issue_n, message, subjid=list(subj), fun=fun_name)
    save_warn_list_item(item)
    
    fun("{par_issue}{message}{par_subj}")
  }
  invisible(.data)
}


#' @noRd
#' @keywords internal
#' @importFrom cli cli_warn
save_warn_list_item = function(item){
  stopifnot(nrow(item)==1)
  issue_n = item$issue_n
  issue_key = paste0("issue_", item$issue_n)
  current = edc_data_warnings()
  if(!is.na(item$issue_n) && item$issue_n %in% current$issue_n){
    if(item$issue_n=="xx" && !item$message %in% current$message){
      issue_key = paste0("issue_xx_", nrow(current))
    } else if(getOption("edc_warn_duplicate_verbose", FALSE)){
      cli_warn("Duplicate `edc_data_warn()` entry")
    }
  }
  edcimport_env$warn_list[[issue_key]] = item
}

#' @noRd
#' @keywords internal
reset_warn_list = function(){
  edcimport_env$warn_list = list()
}


#' @noRd
#' @keywords internal
#' @importFrom cli cli_vec format_inline
format_subj = function(subj, max_subjid=5, par=TRUE){
  if(length(subj)==0) return("")
  subj = subj %>% unique() %>% sort()
  n_subj = length(subj)
  subj = paste0("#", subj) %>% 
    cli_vec(style=list("vec_trunc"=max_subjid, "vec-trunc-style"="head"))
  rtn = format_inline("{n_subj} patient{?s}: {subj}")
  if(isTRUE(par)) rtn = format_inline(" ({rtn})")
  rtn
}


# Deprecated ----------------------------------------------------------------------------------

#' @rdname edc_warn_patient_diffs
#' @usage NULL
#' @export
#' @importFrom lifecycle deprecate_warn
check_subjid = function(x){
  deprecate_warn("0.5.0", "check_subjid()", "edc_warn_patient_diffs()")
}


#' @rdname edc_data_warn
#' @usage NULL
#' @export
#' @importFrom lifecycle deprecate_warn
assert_no_rows = function(df, msg=NULL){
  deprecate_warn("0.5.0", "assert_no_rows()", "edc_data_stop()")
}
