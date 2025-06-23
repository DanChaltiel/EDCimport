

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
#' db = edc_example()
#' load_database(db)
#' options(edc_subjid_ref=enrol$subjid)
#' #usually, you set something like:
#' #options(edc_subjid_ref=enrolres$subjid)
#' edc_warn_patient_diffs(data1)
#' data1 %>% dplyr::filter(subjid>1) %>% edc_warn_patient_diffs(issue_n=NULL)
#' edc_warn_patient_diffs(c(data1$subjid, 99, 999))
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
#' db = edc_example()
#' load_database(db)
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
#'  * `edc_data_warn()` to generate a standardized warning that can be forwarded to the datamanager.
#'  * `edc_data_stop()` to abort the script if the problem is too serious.
#'  
#' Each time [edc_data_warn] is used, the warning is saved internally so that a summary of all your warnings can be retrieved using [edc_data_warnings]. \cr The result can be saved into an Excel file using [save_edc_data_warnings()].
#'
#' @param df the filtered dataframe
#' @param message the message. Can use [cli formats](https://cli.r-lib.org/reference/inline-markup.html#classes). `df` can be accessed using the `.data` special keyword (see example)
#' @param issue_n identifying row number
#' @param csv_path a path to save `df` in a csv file that can be shared with the DM for more details.
#' @param max_subjid max number of subject ID to show in the message
#' @param envir the environment to evaluate `message` in.
#' @param col_subjid column name for subject ID. Set to `NULL` to ignore.
#' @param ... unused 
#'
#' @return `df` invisibly
#' @export
#' @importFrom rlang check_dots_empty
#'
#' @examples
#' library(dplyr)
#' db = edc_example()
#' load_database(db)
#' enrol %>% 
#'   filter(age>70) %>% 
#'   edc_data_warn("Age should not be >70", issue_n=1)
#' 
#' enrol %>% 
#'   filter(age<25) %>% 
#'   edc_data_warn("Age should not be <25", issue_n=2)
#' 
#' data1 %>% 
#'   filter(n()>1, .by=subjid) %>% 
#'   edc_data_warn("There are duplicated patients in `data1` ({nrow(.data)} rows)", issue_n=3)
#' 
#' enrol %>% 
#'   filter(age<25) %>% 
#'   edc_data_warn("Age should not be <25", issue_n=NULL)
#'   
#' edc_data_warnings()
#' 
#' \dontrun{
#' enrol %>% 
#'   filter(age<25) %>% 
#'   edc_data_warn("Age should not be <25", csv_path="check/check_age_25.csv")
#'   
#' enrol %>% 
#'   filter(age<25) %>% 
#'   edc_data_stop("Age should *never* be <25")
#' }
edc_data_warn = function(df, message, ..., 
                         issue_n="xx", max_subjid=5, 
                         csv_path=FALSE, 
                         envir=parent.frame(), 
                         col_subjid=get_subjid_cols()){
  
  if (missing(max_subjid)) max_subjid = getOption("edc_warn_max_subjid", max_subjid)
  check_dots_empty()
  .edc_data_condition(tbl=df, message=message, issue_n=issue_n, csv_path=csv_path,
                     max_subjid=max_subjid, .envir=envir, col_subjid=col_subjid, 
                     fun=cli_warn)
}

#' @rdname edc_data_warn
#' @usage edc_data_stop(df, message, ..., issue_n, max_subjid, csv_path, envir, col_subjid)
#' @export
#' @importFrom rlang check_dots_empty
edc_data_stop = function(df, message, ..., 
                         issue_n="xx", max_subjid=5, 
                         csv_path=FALSE, 
                         envir=parent.frame(), 
                         col_subjid=get_subjid_cols()){
  
  if (missing(max_subjid)) max_subjid = getOption("edc_warn_max_subjid", max_subjid)
  check_dots_empty()
  .edc_data_condition(tbl=df, message=message, issue_n=issue_n, csv_path=csv_path,
                     max_subjid=max_subjid, .envir=envir, col_subjid=col_subjid, 
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
    arrange(across(any_of(c("issue_n", "message")))) %>% 
    add_class("edc_warning_summary")
}



#' Save EDCimport warning to Excel
#' 
#' Each time [edc_data_warn] is used, the warning is saved internally so that a summary can be retrieved using [edc_data_warnings]. This summary can then be saved into a `.xlsx` file using `save_edc_data_warnings()`. 
#'
#' @param edc_warnings the result of [edc_data_warnings]
#' @param path a `.xlsx` file path
#' @param overwrite If `TRUE`, overwrite any existing file.
#' @param open If `TRUE`, overwrite any existing file.
#'
#' @returns a logical(1), whether the file could be written, invisibly 
#' @export
save_edc_data_warnings = function(edc_warnings=edc_data_warnings(), 
                                  path="edc_data_warnings.xlsx",
                                  overwrite=TRUE, 
                                  open=FALSE){
  check_installed("openxlsx", reason="for `save_edc_data_warnings()` to work.")
  assert_class(edc_warnings, "edc_warning_summary", null.ok=FALSE)
  assert(path_ext(path)=="xlsx")
  
  edc_warnings$issue_n %>% make.unique
  
  wb = openxlsx::createWorkbook()
  for(i in seq(nrow(edc_warnings))){
    if(!exists("date_extraction")) date_extraction=NULL
    x = edc_warnings[i, ]
    sheet = paste0("issue_", x$issue_n)
    data = x$data[[1]]
    color = if(nrow(data)==0) "green" else "red"
    openxlsx::addWorksheet(wb, sheet, tabColour=color, gridLines=FALSE)
    openxlsx::writeData(wb, sheet, tibble(date_extraction, x$message), colNames=FALSE)
    openxlsx::writeDataTable(wb, sheet, data, startRow=2)
  }
  rtn = openxlsx::saveWorkbook(wb, path, overwrite=overwrite, returnValue=TRUE)
  
  if(isTRUE(open) && isTRUE(rtn)){
    browseURL(path)
  }
  invisible(rtn)
}


# Utils ---------------------------------------------------------------------------------------


#' @noRd
#' @keywords internal
#' @importFrom cli cli_abort cli_vec cli_warn format_inline
#' @importFrom dplyr pull
#' @importFrom fs dir_create
#' @importFrom rlang caller_arg env_bind
#' @importFrom stringr str_ends str_pad
#' @importFrom tibble tibble
#' @importFrom utils write.csv2
.edc_data_condition = function(tbl, message, issue_n, max_subjid, 
                              csv_path, .envir, 
                              col_subjid, fun){
  if(is.character(csv_path)){
    assert(str_ends(csv_path, "\\.csv"), call=parent.frame())
    dir_create(dirname(csv_path))
    write.csv2(tbl, csv_path, row.names=FALSE)
  }
  
  env_bind(.envir, .data=tbl)
  message = format_inline(message, .envir=.envir)
  
  par_issue = par_subj = ""; subj=NULL
  if(is.null(issue_n)) issue_n = NA
  
  if(!is.na(issue_n)){
    if(is.numeric(issue_n)) issue_n = str_pad(issue_n, width=2, pad="0")
    par_issue = format_inline("Issue #{col_green(issue_n)}: ")
  }
  
  if(!is.null(col_subjid)){
    col_found = tolower(col_subjid) %in% tolower(names(tbl))
    if(sum(col_found)>1){
      cli_warn("Found {length(col_found)} subject identifiers in the input dataset:
                 {.val {col_subjid[col_found]}}. Defaulting to the first one.", 
               class="edc_data_condition_subjid_multiple_warn", call=parent.frame())
      col_subjid = col_subjid[col_found][1]
    }
    if(!any(col_found)){
      cli_abort("Could not find column {.val {col_subjid}} in the input dataset.", 
                class="edc_data_condition_subjid_error", call=parent.frame())
    }
    subj = tbl %>% pull(any_of2(col_subjid)) %>% unique() %>% sort()
    n_subj = length(subj)
    subj = paste0("#", subj) %>% 
      cli_vec(style=list("vec_trunc"=max_subjid, "vec-trunc-style"="head"))
    par_subj = format_inline(" ({n_subj} patient{?s}: {subj})")
  }
  
  item_subjid = NULL
  if(nrow(tbl)>0){
    fun("{par_issue}{message}{par_subj}", class="edc_data_condition")
    item_subjid=list(subj)
  }
  
  item = tibble(issue_n, message, subjid=item_subjid, data=list(tbl))
  save_warn_list_item(item)
    
  invisible(tbl)
}


#' @noRd
#' @keywords internal
#' @importFrom cli cli_warn
#' @importFrom rlang has_name
#' @importFrom dplyr semi_join
save_warn_list_item = function(item){
  stopifnot(nrow(item)==1)
  if(is.na(item$issue_n)) return(NULL)
  VERBOSE = getOption("edc_warn_duplicate_verbose", TRUE)
  current = edc_data_warnings()
  
  if(item$issue_n=="xx") {
    n_xx = sum(str_detect(current$issue_n, "^xx\\d+$"))
    item$issue_n = paste0("xx", n_xx+1)
  }
  issue_n = item$issue_n
  issue_key = paste0("issue_", item$issue_n)
  
  #warn if same issue number
  if(issue_n %in% current$issue_n && VERBOSE){
    cli_warn("Duplicated `edc_data_warn()` entry: issue_n={.val {issue_n}}", 
             class="save_warn_list_item_dupln_warning")
  }
  
  #warn & skip if same message and data
  if(nrow(current)>0 && all(has_name(item, c("message", "data")))){
    m = semi_join(item, current, by=c("message", "data"))
    if(nrow(m)>0 && VERBOSE) {
      cli_warn("Duplicated `edc_data_warn()` entry: {.val {item$message}}", 
               class="save_warn_list_item_dupl_warning")
      return(NULL)
    }
  }

  edcimport_env$warn_list[[issue_key]] = item
  NULL
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
