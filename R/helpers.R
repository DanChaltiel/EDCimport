


# User helpers --------------------------------------------------------------------------------


#' Find a keyword in the whole database
#' 
#' Find a keyword in all names and labels of a list of datasets. 
#'
#' @param keyword the keyword to search for. Can handle regular expressions (see examples).
#' @param data the lookup dataframe where to search the keyword. Can be set using `edc_options(edc_lookup=my_data)`, which is done automatically when calling [read_trialmaster()].
#' @param ignore_case should case differences be ignored in the match? Default to `TRUE`.
#'
#' @return a tibble
#' @export
#' @examples 
#' \dontrun{
#' path = system.file("extdata/Example_Export_SAS_XPORT_2022_08_25_15_16.zip", 
#'                    package="EDCimport", mustWork=TRUE)
#' w = read_trialmaster(path, verbose=FALSE)
#' 
#' find_keyword("patient")
#' 
#' #with regex
#' find_keyword("patient$")
#' find_keyword("\\d")
#' find_keyword("(Trial|Form) Name")
#' find_keyword("\\(") #you need to escape special characters
#' }
#' @importFrom cli cli_warn
#' @importFrom dplyr any_of filter mutate pull select where
#' @importFrom purrr map2_chr map2_dbl
#' @importFrom stringr str_detect str_trim
#' @importFrom tidyr unite unnest
find_keyword = function(keyword, data=edc_lookup(), ignore_case=TRUE){
  invalid=names2=labels2=x=NULL
  f = if(isTRUE(ignore_case)) tolower else identity
  f2 = function(x,y) map2_chr(x, y, ~if(.y) {.x} else {f(.x)})
  keyword = f(str_trim(keyword))
  tmp = data %>% 
    select(-any_of(c("nrow", "ncol", "n_id", "rows_per_id"))) %>% 
    unnest(c(names, labels)) %>% 
    mutate(
      labels=unlist(labels), 
      invalid=is_invalid_utf8(labels),
      names2=f2(names, invalid),
      labels2=f2(labels, invalid),
    ) %>% 
    filter(str_detect(names2, keyword) | str_detect(labels2, keyword)) %>% 
    select(-names2, -labels2) %>% 
    mutate(prop_na = map2_dbl(dataset, names, ~{
      if(!exists(.x, envir=globalenv())) return(NA)
      x=get(.x, envir=globalenv())[[.y]]
      mean(is.na(x))
    })) %>% 
    select(-where(~all(is.na(.x))))
  if(nrow(tmp)==0) return(NULL)
  
  if(isTRUE(ignore_case) && any(tmp[["invalid"]])){
    cols = tmp %>% filter(invalid) %>% unite("x", c("dataset", "names"), sep="$") %>% pull(x)
    cli_warn(c("Some columns have labels containing non UTF8 characters. {.arg ignore_case} has been ignored for these.", 
               i="Columns: {.code {cols}}.",
               i='For instance, try to run: {.code attr({cols[1]}, "label")}.'), 
             class="find_keyword_utf8_warning")
  }
  
  tmp %>% 
    select(-invalid)
}



#' Format factor levels as Yes/No
#'
#' Format factor levels as arbitrary values of Yes/No (with Yes always first) while **leaving untouched** all vectors that contain other information. 
#'
#' @param x a vector of any type/class.
#' @param input list of values to be considered as "yes" and "no".
#' @param output the output factor levels.
#' @param strict whether to match the input strictly or use [stringr::str_detect] to find them.
#' @param mutate_character whether to turn characters into factor.
#' @param fail whether to fail if some levels cannot be recoded to yes/no.
#'
#' @return a factor, or `x` untouched.
#' @export
#' @importFrom cli cli_abort
#' @importFrom dplyr case_when
#' @importFrom stringr str_detect
#'
#' @examples 
#' 
#' fct_yesno(c("No", "Yes")) #levels are in order
#' 
#' set.seed(42)
#' N=6
#' x = tibble(
#'   a=sample(c("Yes", "No"), size=N, replace=TRUE),
#'   b=sample(c("Oui", "Non"), size=N, replace=TRUE),
#'   c=sample(0:1, size=N, replace=TRUE),
#'   d=sample(c(TRUE, FALSE), size=N, replace=TRUE),
#'   e=sample(c("1-Yes", "0-No"), size=N, replace=TRUE),
#'   
#'   y=sample(c("aaa", "bbb", "ccc"), size=N, replace=TRUE),
#'   z=1:N,
#' )
#'  
#' x          
#' #y and z are left untouched (or throw an error if fail=TRUE)   
#' sapply(x, fct_yesno, fail=FALSE)
#' 
#' # as "1-Yes" is not in `input`, x$e is untouched/fails if strict=TRUE
#' fct_yesno(x$e)
#' fct_yesno(x$e, strict=TRUE, fail=FALSE) 
#' fct_yesno(x$e, output=c("Ja", "Nein"))
fct_yesno = function(x, 
                     input=list(yes=c("Yes", "Oui"), no=c("No", "Non")), 
                     output=c("Yes", "No"),
                     strict=FALSE,
                     mutate_character=TRUE, 
                     fail=TRUE){
  assert_class(input, "list")
  assert(setequal(names(input), c("yes", "no")))
  
  if (!inherits(x, c("logical", "numeric", "integer", "character", "factor"))) return(x)
  if (is.character(x) && isFALSE(mutate_character)) return(x)
  
  if (missing(input))  input =  getOption("fct_yesno_input", input)
  if (missing(output)) output = getOption("fct_yesno_input", output)
  
  #if logical or numeric AND binary
  if (all(x %in% c(1, 0, NA))) {
    return(factor(as.numeric(x), levels=c(1,0), labels=output) %>% copy_label_from(x))
  } else if(is.numeric(x)){
    return(x)
  }
  
  if (!isFALSE(strict)) {
    fun = if(strict=="ignore_case")tolower else identity
    is_yes = fun(x) %in% fun(input$yes)
    is_no  = fun(x) %in% fun(input$no)
  } else {
    is_yes = str_detect(tolower(x), paste(tolower(input$yes), collapse="|"))
    is_no  = str_detect(tolower(x), paste(tolower(input$no ), collapse="|"))
  }
  if (any(is_yes&is_no, na.rm=TRUE)) {
    v = x[!is.na(x) & is_yes & is_no]
    cli_abort("Values that are both yes and no: {.val {v}}", 
              class="fct_yesno_both_error")
  }
  yesno = case_when(
    is.na(x) ~ NA,
    is_yes ~ TRUE,
    is_no ~ FALSE,
    .default=NA
  )
  if (any(is.na(x) != is.na(yesno))) {
    if(isTRUE(fail)){
      v = x[is.na(x) != is.na(yesno)]
      cli_abort("Values that cannot be parsed: {.val {unique(sort(v))}}", 
                class="fct_yesno_unparsed_error")
    }
    return(x)
  }
  
  factor(yesno, levels=c(TRUE,FALSE), labels=output) %>% copy_label_from(x)
}


#' Select only distinct columns
#' 
#' Select all columns that has only one level for a given grouping scope. 
#' Useful when dealing with mixed datasets containing both long data and repeated short data.
#'
#' @param df a dataframe
#' @param .by optional grouping columns
#'
#' @return `df` with less columns
#' @export
#'
#' @examples
#' tm = edc_example_ae()
#' tm$ae %>% names
#' tm$ae %>% select_distinct() %>% names
#' tm$ae %>% select_distinct(.by=subjid) %>% names
#' @importFrom dplyr across all_of distinct everything n_distinct select summarise where
select_distinct = function(df, .by) {
  a = df %>% 
    summarise(across(everything(), function(.x) n_distinct(.x, na.rm=TRUE)), 
              .by={{.by}}) %>% 
    select(where(~all(.x==1))) %>% 
    names()
  
  df %>% 
    select({{.by}}, all_of(a)) %>% 
    distinct() 
}


#' Get a table with the latest date for each patient
#' 
#' This function search for date columns in every tables and returns the latest date 
#' for each patient with the variable it comes from. Useful in survival analysis to get 
#' the right censoring time.
#'
#' @param except the datasets/columns that should not be searched. Example: a scheduled visit for which the patient may have died before attending should not be considered.
#' @param with_ties in case of tie, whether to return the first `origin` (FALSE) or all the origins that share this tie (TRUE).
#' @param numeric_id set to FALSE if the patient ID column is not numeric
#' @param prefer preferred origins in the event of a tie. Usually the followup table.
#' @param warn_if_future whether to show a warning about dates that are after the extraction date
#'
#' @return a dataframe
#' @export
#'
#' @examples
#' tm = edc_example_plot()
#' load_list(tm)
#' lastnews_table()
#' lastnews_table(except="db3")
#' lastnews_table(except="db3$date9")
#' lastnews_table(prefer="db2") 
#' @importFrom cli cli_abort
#' @importFrom dplyr arrange filter mutate rowwise select slice_max ungroup where
#' @importFrom purrr discard discard_at imap list_rbind
#' @importFrom tidyr pivot_longer
lastnews_table = function(except=NULL, with_ties=FALSE, numeric_id=TRUE, 
                          prefer=NULL,
                          warn_if_future=TRUE) {
  subjid_cols = get_subjid_cols()
  rtn = get_datasets(envir=parent.frame()) %>% 
    discard_at(as.character(except)) %>% 
    imap(~{
      if(!is.data.frame(.x) || !any(subjid_cols %in% names(.x))) return(NULL)
      a = .x %>% 
        select(subjid = any_of2(subjid_cols), where(is.Date)) %>% 
        mutate(subjid = as.character(subjid))
      if(ncol(a)<=1) return(NULL) #only subjid
      a %>% 
        pivot_longer(-subjid) %>% 
        filter(!is.na(value)) %>% 
        mutate(origin_label=unlist(get_label(.x)[name]),
               dataset=.y)
    }) %>% 
    discard(is.null) %>% 
    list_rbind()  %>% 
    select(subjid, last_date=value, origin_data=dataset, origin_col=name, origin_label) %>% 
    mutate(origin = paste0(origin_data, "$", origin_col))
  if(nrow(rtn)==0){
    cli_abort("No data with dates could be found, verify your export settings.")
  }
  if(numeric_id && can_be_numeric(rtn$subjid)) {
    rtn$subjid = as.numeric(as.character(rtn$subjid))
  }
  
  rtn = rtn %>% 
    filter(!origin %in% except) %>% 
    filter(!origin_data %in% except) %>% 
    filter(!origin_col %in% except) %>% 
    slice_max(last_date, by=subjid, with_ties=TRUE) %>% 
    arrange(order(mixedorder(subjid)))
  
  if(!with_ties){
    rtn = rtn %>% 
      rowwise() %>%  
      mutate(prefered = which(prefer %in% c(origin, origin_data, origin_col)) %0% Inf) %>% 
      ungroup() %>% 
      arrange((prefered)) %>% 
      select(-prefered, -origin) %>% 
      slice_max(last_date, by=subjid, with_ties=FALSE) %>% 
      arrange(order(mixedorder(subjid)))
  }
  
  if(exists("datetime_extraction")){
    if(any(rtn$last_date > as.Date(datetime_extraction)) && warn_if_future) {
      rtn %>% 
        filter(last_date>as.Date(datetime_extraction)) %>% 
        edc_data_warn("Date of last news after the extraction date", issue_n=NA)
    }
  }
  
  rtn
}


#' Shows how many code you wrote
#'
#' @param main the main R file, which sources the other ones
#' @param Rdir the R directory, where sourced R files are located
#'
#' @return Nothing
#' @export
#' @importFrom cli cli_inform
#' @importFrom purrr map_dbl
#' @importFrom rlang set_names
#' @importFrom stringr str_extract str_subset
edc_inform_code = function(main="main.R", Rdir="R/"){
  assert_file_exists(main)
  sources = readLines(main) %>% 
    str_subset(Rdir) %>% str_subset("^ *#", negate=TRUE) %>% str_extract('"(.*)"', group=TRUE) %>% 
    set_names()
  
  rslt = sources %>% map_dbl(~readLines(.x) %>% length())
  
  cli_inform("Sourcing {length(rslt)} files in {.path {main}} for a total 
             of {sum(rslt)} lines of code.")
}


#' Save `sessionInfo()` output
#'
#' Save `sessionInfo()` output into a text file.
#'
#' @param path target path to write the file
#' @param with_date whether to insert the date before the file extension
#'
#' @return nothing
#' @export
#' @importFrom fs dir_create path path_dir path_ext path_ext_remove
#' @importFrom utils capture.output sessionInfo
#'
#' @examples
#' \dontrun{
#'    save_sessioninfo()
#' }
save_sessioninfo = function(path="check/session_info.txt", with_date=TRUE){
  target = path %>% 
    path_ext_remove() %>% 
    paste0("_", today_ymd()) %>% 
    path(ext=path_ext(path))
  dir_create(path_dir(target), recurse=TRUE)
  sessionInfo() %>% capture.output() %>% cat(file=target, sep="\n")
  invisible(TRUE)
}

#' Harmonize the subject ID of the database
#' 
#' Turns the subject ID columns of all datasets into a factor containing levels for all 
#' the subjects of the database. Avoid problems when joining tables, and some checks can
#' be performed on the levels.
#'
#' @param datalist a list of dataframes
#' @param preprocess an optional function to modify the subject ID column, for example `as.numeric()`. See examples.
#' @param col_subjid the names of the columns holding the subject ID (as character)
#'
#' @return datalist, with subject id modified
#' @export
#' @importFrom dplyr across any_of mutate select
#' @importFrom purrr discard_at keep map modify
#'
#' @examples
#' db = edc_example()
#' db$db0 = head(db$db0, 10)
#' db$db0$SUBJID %>% head()
#' db = harmonize_subjid(db)
#' db$db0$SUBJID %>% head()
#' db = harmonize_subjid(db, preprocess=function(x) paste0("#", x))
#' db$db0$SUBJID %>% head()
harmonize_subjid = function(datalist, preprocess=NULL, 
                            col_subjid=get_subjid_cols()){
  if(is.null(preprocess)) preprocess = identity
  assert_class(preprocess, "function")
  
  all_subjid = datalist %>% 
    keep(is.data.frame) %>% 
    discard_at(".lookup") %>% 
    map(~select(.x, any_of(col_subjid))) %>% 
    unlist() %>% 
    as.character() %>% 
    preprocess() %>% 
    unique() %>% 
    mixedsort()
  
  a= datalist %>% 
    modify(function(df){
      if(!is.data.frame(df)) return(df)
      df %>% 
        mutate(across(any_of(get_subjid_cols()), 
                      ~factor(preprocess(.x), levels=all_subjid)))
    })
  
  a
}


# Manual correction ---------------------------------------------------------------------------


#' Manual correction
#' 
#' @description
#'  
#' When finding wrong or unexpected values in an exported dataset, it can be useful to temporarily correct them by hard-coding a value. 
#' However, this manual correction should be undone as soon as the central database is updated with the correction. 
#' 
#'  - `manual_correction()` applies a correction in a specific dataset column location and throws an error if the correction is already in place. This check applies only once per R session so you can source your script without errors.
#'  - `reset_manual_correction()` resets all checks. For instance, it is called by [read_trialmaster()].
#'
#' @param data,col,rows the rows of a column of a dataframe where the error lies
#' @param wrong the actual wrong value
#' @param correct the temporary correction value
#' @param verbose whether to print informations (once)
#'
#' @return Nothing, used for side effects
#' @importFrom cli cli_abort cli_inform cli_warn
#' @importFrom dplyr enquo
#' @importFrom glue glue
#' @importFrom rlang as_name caller_arg set_names
#' @export
#'
#' @examples
#' library(dplyr)
#' x = iris %>% mutate(id=row_number(), .before=1) %>% as_tibble()
#' x$Sepal.Length[c(1,3,5)]
#' 
#' #1st correction is silent
#' manual_correction(x, Sepal.Length, rows=c(1,3,5),
#'                   wrong=c(5.1, 4.7, 5.0), correct=c(5, 4, 3))
#' x$Sepal.Length[c(1,3,5)]
#' 
#' #further correction is silent
#' manual_correction(x, Sepal.Length, rows=c(1,3,5),
#'                   wrong=c(5.1, 4.7, 5.0), correct=c(5, 4, 3)) 
#'                   
#' #if the database is corrected, an error is thrown
#' \dontrun{
#' reset_manual_correction()
#' x$Sepal.Length[c(1,3,5)] = c(5, 4, 3) #mimics db correction
#' manual_correction(x, Sepal.Length, rows=c(1,3,5),
#'                   wrong=c(5.1, 4.7, 5.0), correct=c(5, 4, 3))
#' }
manual_correction = function(data, col, rows, wrong, correct, 
                             verbose=getOption("edc_correction_verbose", TRUE)){
  col = as_name(enquo(col))
  stopifnot(is.data.frame(data))
  data_name = caller_arg(data)
  
  if(length(rows)!=length(wrong) || length(rows)!=length(correct)){
    cli_abort("{.arg rows} ({length(rows)}), {.arg wrong} ({length(wrong)}), and {.arg correct} ({length(correct)}) should be the same length.")
  }
  if(any(rows>nrow(data))){
    cli_abort("At least one value of {.arg rows} is larger than the number of rows in {.arg data_name}")
  }
  
  val = data[[col]][rows]
  label = glue("{data_name}${col}")
  collapse = function(..., sep="_") paste(..., collapse=sep)
  opt_key = glue("edc_correction_done_{data_name}_{col}_{collapse(rows)}")
  if(identical(val, correct) && isTRUE(getOption(opt_key))) {
    return(invisible())
  }
  
  if(is.null(val)){
    cli_abort("Could not find column {.val {col}} in data {.val {data_name}}")
  }
  
  if(identical(val, wrong)) {
    if(isTRUE(verbose)) cli_inform(c("Manual correction of {.val {label}}:", 
                                     i="Old: {wrong}", 
                                     i="New: {correct}"))
    data[[col]][rows] = correct
    assign(data_name, data, envir=parent.frame())
    options(set_names(list(TRUE), opt_key))
  } else if(all(is.na(val)) && !all(is.na(wrong))) {
    if(isTRUE(verbose)) cli_warn("Manual correction of {.val {label}}: nothing done (NA)")
    return(invisible(TRUE))
  } else {
    cli_abort("{.val {label}} has been corrected, remove manual correction")
  }
}


#' @name manual_correction
#' @return NULL
#' @export
#' @importFrom purrr map
#' @importFrom stringr str_starts
reset_manual_correction = function(){
  x=options()
  x=x[str_starts(names(x), "edc_correction_done_")] %>% map(~NULL)
  options(x)
}


# Getters -------------------------------------------------------------------------------------


#' Retrieve the datasets as a list of data.frames
#' 
#' Get the datasets from the lookup table as a list of data.frames.
#'
#' @param lookup the lookup table
#' @param envir (internal use)
#'
#' @return a list of all datasets
#' @export
#' @importFrom cli cli_abort cli_warn
#' @importFrom purrr keep
get_datasets = function(lookup=edc_lookup(), envir=parent.frame()){
  if(is.null(lookup)){
    cli_abort("lookup cannot be NULL, did you forgot to import your database?")
  }
  rtn = lookup$dataset %>% 
    mget(envir=envir, ifnotfound=list(NULL), mode="list", inherits=TRUE)
  a = rtn %>% keep(is.null) %>% names()
  if(length(a) > 5){
    cli_warn(c("Could not find {length(a)}/{length(rtn)} datasets from the lookup, did you forget to call {.fn load_list} on your import?",
               i="{.val {a}}"))
  }
  
  rtn
  # lookup$dataset %>% 
  #   set_names() %>% 
  #   map(~{
  #     x = try(get(.x, envir=envir))
  #     
  #   })
}


#' Important column names
#' 
#' Retrieve names of `patient_id` (usually "SUBJID" and "PATNO") and `crfname` (usually "CRFNAME") from the actual names of the datasets
#'
#' @param lookup the lookup table
#'
#' @return a list(2) of characters with names `patient_id` and `crfname`
#' 
#' @export
#' @importFrom cli cli_warn
#' @importFrom dplyr lst mutate select
#' @importFrom lifecycle deprecate_warn
#' @importFrom purrr map map_chr
#' @importFrom stats na.omit
get_key_cols = function(lookup=edc_lookup()){
  patient_id = get_subjid_cols()
  
  crfname = getOption("edc_cols_crfname", "CRFNAME")
  deprecate_warn("1.0.0", "get_key_cols()")
  if(is.null(lookup)) return(lst(patient_id, crfname))
  
  f = function(x, y) x[tolower(x) %in% tolower(y)][1] %0% NA 
  rtn = lookup %>% 
    select(dataset, names) %>% 
    mutate(
      patient_id = map_chr(names, ~f(.x, patient_id)), 
      crfname =    map_chr(names, ~f(.x, crfname))
    )
  
  verbose = getOption("edc_get_key_cols_verbose", FALSE)
  if(verbose && any(is.na(rtn$patient_id))){
    cli_warn(c("Default patient identificator could not be found in some datasets", 
               i='Dataset{?s} without identificator: {rtn[is.na(rtn$patient_id), "dataset"]}', 
               i='Use {.run options(edc_cols_id=c("my_id_col", "my_other_id_col"))}'), 
             class="edcimport_get_key_cols_missing_id")
  }
  if(verbose && any(is.na(rtn$crfname))){
    cli_warn(c("Default CRF form name could not be found in some datasets", 
               i='Dataset{?s} without identificator: {rtn[is.na(rtn$crfname), "dataset"]}', 
               i='Use {.run options(edc_cols_crfname=c("my_crfname_col", "my_other_crfname_col"))}'), 
             class="edcimport_get_key_cols_missing_crfname")
  }
  
  rtn = rtn %>% 
    map(~unique(na.omit(.x)), na.rm=TRUE)
  
  if(length(rtn)==1) return(rtn[[1]])
  rtn
}


#' Get key column names
#' 
#' Retrieve names of patient ID and CRF name from the actual names of the datasets, without respect of the case. Default values should be set through options.
#' 
#' @section options:
#' Use `edc_options()` to set default values:
#' * `edc_cols_subjid` defaults to `c("PTNO", "SUBJID")`  
#' * `edc_cols_crfname` defaults to `c("CRFNAME")`
#'
#' @param lookup the lookup table
#'
#' @return a character vector
#' @export
#'
#' @examples
#' get_subjid_cols()
#' get_crfname_cols()
get_subjid_cols = function(lookup=edc_lookup()){
  subjid_cols=getOption("edc_cols_subjid", c("PTNO", "SUBJID"))
  .get_key_cols(subjid_cols, id_name="patient", lookup)
}

#' @rdname get_subjid_cols
#' @export
get_crfname_cols = function(lookup=edc_lookup()){
  crfname_cols=getOption("edc_cols_crfname", "CRFNAME")
  .get_key_cols(crfname_cols, id_name="CRF", lookup)
}

#' @noRd
#' @keywords internal
#' @importFrom cli cli_warn
#' @importFrom purrr map_chr
#' @importFrom stats na.omit
.get_key_cols = function(x, id_name, lookup){
  if(is.null(lookup)) return(x)
  
  f = function(x, y) x[tolower(x) %in% tolower(y)][1] %0% NA 
  
  x = map_chr(lookup$names, ~f(.x, x))
  
  verbose = getOption("edc_get_key_cols_verbose", FALSE)
  if(verbose && any(is.na(x))){
    cli_warn(c("Default {id_name} identificator could not be found in some datasets", 
               i='Dataset{?s} without identificator: {rtn[is.na(x), "dataset"]}', 
               i='Use {.run options(edc_cols_id=c("my_id_col", "my_other_id_col"))}'), 
             class="edcimport_get_key_cols_missing")
  }
  x %>% na.omit() %>% unique() 
}



#' Get columns shared by most datasets
#' 
#' In most trialmaster exports, many datasets share a certain amount of columns containing 
#' meta-data that are often irrelevant to the point. This function identifies the columns 
#' that are present in at least 95% of datasets (by default)
#'
#' @param min_pct Default=`0.95`. The minimal proportion of datasets a column has to reach. Subject ID is always excluded.
#'
#' @return a character vector
#' @export
#'
#' @examples
#' tm = edc_example_mixed()
#' load_list(tm)
#' meta_cols = get_meta_cols()
#' long_mixed %>% dplyr::select(-dplyr::any_of(meta_cols))
#' @importFrom dplyr filter pull setdiff
get_meta_cols = function(min_pct = getOption("edc_meta_cols_pct", 0.95)){
  a = get_common_cols(min_datasets=0)
  subjid_cols = get_subjid_cols()
  a %>% filter(pct_datasets>min_pct) %>% pull(column) %>% setdiff(subjid_cols)
}

#' Get columns that are common to multiple datasets
#'
#' `r lifecycle::badge("experimental")`
#' Attempt to list all columns in the database and group the ones that are 
#' common to some datasets.
#' Useful to find keys to pivot or summarise data.
#'
#' @param lookup the lookup table, default to [edc_lookup()]
#' @param min_datasets the minimal number of datasets to be considered
#' @param object an object of class "common_cols"
#' @param ... unused
#'
#' @return a tibble of class "common_cols"
#' @export
#' 
#' @importFrom dplyr arrange desc filter mutate rowwise ungroup
#' @importFrom purrr map_lgl
#' @importFrom tibble tibble
#'
#' @examples
#' tm = edc_example()
#' load_list(tm)
#' x = get_common_cols(min_datasets=1)
#' x
#' summary(x)
get_common_cols = function(lookup=edc_lookup(), min_datasets=3){
  all_names = lookup$names %>% unlist() %>% unique() %>% sort()
  rtn = tibble(column=all_names) %>%
    rowwise() %>% 
    mutate(
      name_in = lookup$names %>% map_lgl(~column %in% .x) %>% list(),
      datasets = list(names(.data$name_in[.data$name_in])),
      n_datasets = length(.data$datasets),
      pct_datasets = mean(.data$name_in),
      datasets_in = toString(names(.data$name_in[.data$name_in])),
      datasets_out = toString(names(.data$name_in[!.data$name_in])),
      # aaa = browser(),
    ) %>% 
    ungroup() %>% 
    arrange(desc(.data$pct_datasets)) %>% 
    filter(.data$n_datasets>=min_datasets) 
  class(rtn) = c("common_cols", class(rtn))
  rtn
}


#' @name get_common_cols
#' @export
#' @importFrom dplyr mutate n summarise
summary.common_cols = function(object, ...){
  object %>% 
    summarise(
      n_distinct_datasets = length(unique(.data$datasets)), 
      n_columns = n(),
      columns = list(.data$column), 
      datasets = list(.data$datasets),
      columns_str = toString(.data$column), 
      .by=c(.data$pct_datasets, .data$n_datasets)
    ) %>% 
    mutate(
      pct_datasets = sprintf("%0.0f%%", .data$pct_datasets * 100),
    )
}

# Manage lists --------------------------------------------------------------------------------


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
  
  if(remove) {
    x_name = caller_arg(x)
    if(exists(x_name, where=env, inherits=FALSE)) 
      remove(list=x_name, envir=env)
    else                          
      remove(list=x_name, envir=parent.frame())
  }
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



# Methods -----------------------------------------------------------------

#' @export
#' @importFrom cli cat_rule cli_bullets cli_vec
#' @importFrom purrr discard_at keep
print.tm_database = function(x, ...){
  x = x %>% keep(is.data.frame) %>% discard_at(".lookup")
  cat_rule("Trialmaster database", col = "violet")
  nms = cli_vec(names(x), list("vec-trunc"=3))
  cli_bullets(c(
    "{length(x)} tables: {.arg {nms}}",
    i="Use {.code EDCimport::load_list(tm)} to load the tables in the global environment",
    i="Use {.code print(tm$.lookup)} to see the summary table"
  ))
}
