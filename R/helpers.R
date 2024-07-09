


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
#' @importFrom dplyr any_of filter mutate pull select
#' @importFrom purrr map2_chr map2_dbl
#' @importFrom stringr str_detect str_trim
#' @importFrom tidyr unite unnest
#' @importFrom tidyselect where
find_keyword = function(keyword, data=get_lookup(), ignore_case=TRUE){
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
#' Format factor levels as arbitrary values of Yes/No (with Yes always first) while leaving untouched all vectors that contain other information. 
#' Default level values can be set through options, which is best done before calling [read_trialmaster()].
#' Helper `get_yesno_lvl()` is a helper to provide default TM levels.
#'
#' @param x a vector of any type/class
#' @param lvl list of values to be considered as Yes/No values. Defaults to `get_yesno_lvl()`. See example.
#' @param mutate_character whether to turn characters into factor
#' @param add levels to add to `list(c("Yes", "No"), c("1-Yes", "0-No"))`
#' @param keep_default whether to keep the default
#'
#' @return a factor, or `x` untouched
#' @export
#' @importFrom cli cli_abort
#' @importFrom dplyr union
#' @importFrom purrr keep
#'
#' @examples 
#' set.seed(42)
#' x = tibble(a=sample(c("Yes", "No"), size=20, replace=TRUE),
#'            b=sample(c("1-Yes", "0-No"), size=20, replace=TRUE),
#'            c=sample(c("Oui", "Non"), size=20, replace=TRUE),
#'            x=sample(0:1, size=20, replace=TRUE),
#'            y=1:20)
#' 
#' # leave untouched unhandled vectors (c,x, and y)
#' x %>% purrr::iwalk(~{
#'   cat("--- Levels of ", .y, " ---\n")
#'   print(.x %>% factor() %>% levels())
#'   print(.x %>% fct_yesno() %>% levels())
#' })
#' 
#' #add other levels
#' supp_levels = list(c("Oui", "Non"), c("Ja", "Nein"))
#' options(edc_fct_yesno = get_yesno_lvl(supp_levels))
#' x %>% purrr::iwalk(~{
#'   cat("--- Levels of ", .y, " ---\n")
#'   print(.x %>% factor() %>% levels())
#'   print(.x %>% fct_yesno() %>% levels())
#' })
fct_yesno = function(x, lvl=getOption("edc_fct_yesno", get_yesno_lvl()), 
                     mutate_character=TRUE){
  stopifnot(is.list(lvl))
  if(all(x %in% c(1,0))) return(factor(x, levels=c(1,0), labels=lvl[[1]]))
  if(!is.factor(x) && !is.character(x)) return(x)
  if(is.character(x) && isFALSE(mutate_character)) return(x)
  lvls = lvl %>% keep(~all(x %in% union(.x, NA)))
  if(length(lvls)>1) cli_abort("Duplicated Yes/No levels")
  if(length(lvls)==0) return(x)
  factor(x, levels=lvls[[1]]) %>% copy_label_from(x)
}


#' @rdname fct_yesno
#' @export
get_yesno_lvl = function(add, keep_default=TRUE) {
  default = list(c("Yes", "No"), c("1-Yes", "0-No"))
  if(missing(add)) return(default)
  if(isFALSE(keep_default)) return(add)
  if(!is.list(add)) add = list(add)
  c(default, add)
}



#' Get a table with the latest date for each patient
#' 
#' This function search for date columns in every tables and returns the latest date 
#' for each patient with the variable it comes from. 
#' 
#'
#' @param except the datasets that should not be searched
#' @param with_ties in case of tie, whether to return the first `origin` (FALSE) or all the origins that share this tie (TRUE).
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
#' @importFrom cli cli_abort
#' @importFrom dplyr arrange filter mutate rename select slice_max
#' @importFrom purrr discard discard_at imap list_rbind
#' @importFrom tidyr pivot_longer
#' @importFrom tidyselect where
lastnews_table = function(except=NULL, with_ties=FALSE) {
  subjid_cols = get_subjid_cols()
  a = get_datasets(envir=parent.frame()) %>% 
    discard_at(as.character(except)) %>% 
    imap(~{
      if(!is.data.frame(.x) || !any(subjid_cols %in% names(.x))) return(NULL)
      a = .x %>% select(subjid=any_of2(subjid_cols), where(is.Date)) %>% 
        mutate(subjid = as.character(subjid))
      if(ncol(a)<=1) return(NULL) #only subjid
      a %>% 
        pivot_longer(-subjid) %>% 
        filter(!is.na(value)) %>% 
        mutate(name=paste0(.y,"$",name))
    }) %>% 
    discard(is.null) %>% 
    list_rbind()  %>% 
    rename(origin=name, last_date=value)
  if(nrow(a)==0){
    cli_abort("No data with dates could be found.")
  }
  a %>% 
    filter(!origin %in% except) %>% 
    slice_max(last_date, by=subjid, with_ties=with_ties) %>% 
    arrange(mixedorder(subjid))
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
  
  cli_inform("Sourcing {length(rslt)} files in {.path {main}} for a total of {sum(rslt)} lines of code")
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
#' @importFrom glue glue
#' @importFrom rlang as_name caller_arg enquo set_names
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
get_datasets = function(lookup=get_lookup(), envir=parent.frame()){
  if(is.null(lookup)){
    cli_abort("lookup cannot be NULL, did you forgot to import your database?")
  }
  rtn = lookup$dataset %>% 
    # set_names() %>% 
    mget(envir=envir, ifnotfound=list(NULL))
  a = rtn %>% keep(is.null) %>% names()
  if(length(a) > 5){
    cli_warn("Could not find multiple datasets from the lookup, did you forget to call {.fn load_list} on your import?")
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
#' @importFrom dplyr mutate select
#' @importFrom lifecycle deprecate_warn
#' @importFrom purrr map map_chr
#' @importFrom stats na.omit
#' @importFrom tibble lst
get_key_cols = function(lookup=get_lookup()){
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
get_subjid_cols = function(lookup=get_lookup()){
  subjid_cols=getOption("edc_cols_subjid", c("PTNO", "SUBJID"))
  .get_key_cols(subjid_cols, id_name="patient", lookup)
}

#' @rdname get_subjid_cols
#' @export
get_crfname_cols = function(lookup=get_lookup()){
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
#' @param lookup the lookup table, default to [get_lookup()]
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
get_common_cols = function(lookup=get_lookup(), min_datasets=3){
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
  
  if(remove) remove(list=caller_arg(x), envir=env)
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
