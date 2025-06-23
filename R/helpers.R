


# User helpers --------------------------------------------------------------------------------

#' Format factor levels as Yes/No
#'
#' Format factor levels as arbitrary values of Yes/No (with Yes always first) while **leaving untouched** all vectors that contain other information. 
#'
#' @param x a vector of any type/class.
#' @param input list of values to be considered as "yes", "no", and `NA`.
#' @param output the output factor levels.
#' @param strict whether to match the input strictly or use [stringr::str_detect] to find them. Can also be "ignore_case" to just ignore the case.
#' @param mutate_character whether to turn characters into factor.
#' @param fail whether to fail if some levels cannot be recoded to yes/no.
#'
#' @return a factor, or `x` untouched.
#' @export
#' @importFrom cli cli_abort
#' @importFrom dplyr case_match case_when 
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
#'   e=sample(c("1-Yes", "0-No", "2-NA"), size=N, replace=TRUE),
#'   
#'   y=sample(c("aaa", "bbb", "ccc"), size=N, replace=TRUE),
#'   z=1:N,
#' )
#'  
#' x          
#' #y and z are left untouched (or throw an error if fail=TRUE)   
#' sapply(x, fct_yesno, fail=FALSE, simplify=FALSE)
#' 
#' # as "1-Yes" is not in `input`, x$e is untouched/fails if strict=TRUE
#' fct_yesno(x$e)
#' fct_yesno(x$e, strict=TRUE, fail=FALSE) 
#' fct_yesno(x$e, output=c("Ja", "Nein"))
fct_yesno = function(x, 
                     input=list(yes=c("Yes", "Oui"), no=c("No", "Non"), na=c("NA", "")),
                     output=c("Yes", "No"),
                     strict=FALSE,
                     mutate_character=TRUE, 
                     fail=TRUE){
  assert_class(input, "list")
  default_input = list(yes=c("Yes", "Oui"), no=c("No", "Non"), na=c("NA", ""))
  missing_names = setdiff(names(default_input), names(input))
  input[missing_names] = default_input[missing_names]
  assert(setequal(names(input), c("yes", "no", "na")))
  
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
    fun = if(strict=="ignore_case") tolower else identity
    is_yes = fun(x) %in% fun(input$yes)
    is_no  = fun(x) %in% fun(input$no)
    is_na  = fun(x) %in% fun(input$na)
  } else {
    input = map(input, ~case_match(.x, ""~"^$", .default=.x))
    is_yes = str_detect(tolower(x), paste(tolower(input$yes), collapse="|"))
    is_no  = str_detect(tolower(x), paste(tolower(input$no ), collapse="|"))
    is_na  = str_detect(tolower(x), paste(tolower(input$na ), collapse="|"))
  }
  x[is_na] = NA
  
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
#' @importFrom dplyr across all_of distinct everything n_distinct select summarise where
#'
#' @examples
#' db = edc_example()
#' db$ae %>% colnames()
#' #`crfname` has one level for the whole dataset
#' db$ae %>% select_distinct() %>% colnames()
#' #`n_ae` has one level per patient
#' db$ae %>% select_distinct(.by=subjid) %>% colnames()
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


#' Shows how many code you wrote
#'
#' @param main the main R file, which sources the other ones
#' @param Rdir the R directory, where sourced R files are located
#'
#' @return Nothing
#' @export
#' @importFrom cli cli_inform
#' @importFrom fs file_exists path
#' @importFrom purrr map_dbl
#' @importFrom rlang set_names
#' @importFrom stringr str_extract str_subset
edc_inform_code = function(main="main.R", Rdir="R/"){
  if(!file_exists(main)) main = path(Rdir, main)
  assert_file_exists(main)
  sources = readLines(main) %>% 
    str_subset(Rdir) %>% str_subset("^ *#", negate=TRUE) %>% 
    str_extract('"(.*)"', group=TRUE) %>% 
    set_names()
  
  rslt = sources %>% map_dbl(~readLines(.x, warn=FALSE) %>% length())
  
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


#' Set the project name
#' 
#' Set or override the project name
#'
#' @param db the [edc_database]
#' @param name the project name
#'
#' @returns nothing
#' @export
#'
#' @examples
#' db = edc_example() %>% 
#'  set_project_name("My great project")
#' edc_lookup()
set_project_name = function(db, name){
  lookup = db$.lookup %>% 
    structure(project_name = name)
  .update_lookup(new=lookup)
  db
}


# Joins ---------------------------------------------------------------------------------------


#' @noRd
#' @keywords internal
#' @importFrom cli cli_abort
#' @importFrom dplyr all_of everything select setdiff
#' @importFrom purrr keep
#' @importFrom rlang as_label caller_call
.edc_join = function(type){
  dplyr_join = switch(type, 
                left=dplyr::left_join, right=dplyr::right_join, 
                full=dplyr::full_join, inner=dplyr::inner_join)
  
  function(x, y, by=NULL, suffix=NULL, cols=everything(), remove_dups=FALSE){
    subjid_col = get_subjid_cols() %>% intersect(names(x)) %>% intersect(names(y))
    if(is.null(by)) by = subjid_col
    if(is.null(suffix)){
      suffix = c("", paste0("_", as_label(caller_call(0)[[3]])))
    }
    
    if(length(subjid_col)==0 && is.null(by)){
      cli_abort(c("Could not find a common primary key for {.arg x} and {.arg y}",
                  i="Primary key in current database: {.val {get_subjid_cols()}}"),
                class="edc_subjid_not_found")
    }
    y = y %>% select(any_of(by), !!enquo(cols))
    if(isTRUE(remove_dups)){
      common_col = intersect(names(x), names(y)) %>% 
        setdiff(by) %>% 
        keep(~setequal(x[[.x]], y[[.x]]))
      y = y %>% select(-all_of(common_col))
    }

    .safe_join(x, y, by=by, suffix=suffix, fun=dplyr_join) %>% 
      copy_label_from(y) %>% 
      copy_label_from(x)
  }
}

#' @noRd
#' @keywords internal
#' @importFrom dplyr across all_of mutate
.safe_join = function(x, y, by, suffix, fun){
  j = tryCatch(fun(x, y, by=by, suffix=suffix),
               error=function(e) e)
  if(inherits(j, "dplyr_error_join_incompatible_type")){
    f = function(d){
      d %>% mutate(across(all_of(by), ~copy_label_from(as.character(.x), .x)))
    }
    x = f(x)
    y = f(y)
    j = fun(x, y, by=by, suffix=suffix)
  } else if(inherits(j, "error")){
    stop(j)
  }
  j
}


#' Join within the EDCimport framework
#' 
#' Perform a join with default `by` to the Subject ID and default suffix to the 
#' name of the `y` dataset. See `[dplyr::mutate-joins]` for the description of the
#' join logic.
#'
#' @param x,y Data frames to join
#' @param by The key to join on, as character. Defaults to `get_subjid_cols()`
#' @param suffix The disambiguation suffix. Defaults to the actual name of the `y` dataset.
#' @param cols <[tidy-select][dplyr::dplyr_tidy_select]> The columns to select in `y` before joining.
#' @param remove_dups Whether to remove columns in `y` that already exist in `x`.
#'
#' @returns a dataframe
#' @export
#'
#' @examples
#' db = edc_example()
#' load_database(db)
#' data1$common = data2$common = "Common"
#' x = enrol %>% 
#'   edc_left_join(data2) %>% 
#'   edc_right_join(data1)
#'   
#' #crfname get a suffix, common 
#' names(x)
edc_left_join = .edc_join(type="left")

#' @rdname edc_left_join
#' @usage NULL
#' @export
edc_right_join = .edc_join(type="right")

#' @rdname edc_left_join
#' @usage NULL
#' @export
edc_full_join = .edc_join(type="full")


# Manual correction ---------------------------------------------------------------------------


#' Manual correction
#' 
#' @description
#' `r lifecycle::badge("experimental")`
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
    cli_warn(c("Could not find {length(a)}/{length(rtn)} datasets from the lookup, did you forget to call {.fn load_database} on your import?",
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
#' @importFrom cli cli_warn
#' @importFrom dplyr lst mutate select
#' @importFrom lifecycle deprecate_warn
#' @importFrom purrr map map_chr
#' @importFrom stats na.omit
#' @noRd
#' @keywords internal
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
#' * `edc_cols_subjid` defaults to `c("SUBJID", "PTNO")`  
#' * `edc_cols_crfname` defaults to `c("FORMDESC", "CRFNAME")`
#'
#' @param lookup the lookup table
#'
#' @return a character vector
#' @keywords internal
#'
#' @examples
#' #get_subjid_cols()
#' #get_crfname_cols()
get_subjid_cols = function(lookup=edc_lookup()){
  subjid_cols=getOption("edc_cols_subjid", c("SUBJID", "PTNO"))
  .get_key_cols(subjid_cols, id_name="patient", lookup)
}

#' @noRd
#' @keywords internal
get_crfname_cols = function(lookup=edc_lookup()){
  crfname_cols=getOption("edc_cols_crfname", c("FORMDESC", "CRFNAME"))
  .get_key_cols(crfname_cols, id_name="CRF", lookup)
}

#' @noRd
#' @keywords internal
#' @importFrom cli cli_warn
#' @importFrom purrr map_chr
#' @importFrom stats na.omit
.get_key_cols = function(x, id_name, lookup){
  if(is.null(lookup)) return(x)
  
  f = function(x, y) {
    a = y[tolower(y) %in% tolower(x)][1] %0% NA 
    if(!a %in% x) a = tolower(a)
    a
  }
  rtn = map_chr(lookup$names, ~f(.x, x))
  
  verbose = getOption("edc_get_key_cols_verbose", FALSE)
  if(verbose && any(is.na(rtn))){
    cli_warn(c("Default {id_name} identificator could not be found in some datasets", 
               i='Dataset{?s} without identificator: {rtn[is.na(rtn), "dataset"]}', 
               i='Use {.run options(edc_cols_id=c("my_id_col", "my_other_id_col"))}'), 
             class="edcimport_get_key_cols_missing")
  }
  rtn %>% na.omit() %>% unique()
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
#' @noRd
#' @keywords internal
#'
#' @examples
#' db = edc_example()
#' load_database(db)
#' meta_cols = get_meta_cols()
#' long_mixed %>% dplyr::select(-dplyr::any_of(meta_cols))
#' @importFrom dplyr filter pull setdiff
get_meta_cols = function(lookup=edc_lookup(), min_pct = getOption("edc_meta_cols_pct", 0.95)){
  a = get_common_cols(lookup=edc_lookup(), min_datasets=0)
  subjid_cols = get_subjid_cols(lookup=edc_lookup())
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
#' db = edc_example()
#' load_database(db)
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
#' @param db an [edc_database] object (to be fair, any list would do)
#' @param env the environment onto which the list should be loaded
#' @param remove if `TRUE`, `db` will be removed from the environment afterward
#'
#' @return nothing, called for its side-effect
#' @export
#' @importFrom cli cli_abort cli_warn
#' @importFrom rlang caller_arg
#'
#' @examples
#' db = edc_example()
#' load_database(db, remove=FALSE)
#' print(db)
#' print(lengths(db))
load_database = function(db, env=parent.frame(), remove=TRUE){
  if(length(db)==0){
    cli_warn("List was empty.")
    return(invisible())
  }
  nz = nzchar(names(db))
  if(any(!nz)){
    cli_abort(c("Every member of {.arg db} should have a name.", 
                i="Unnamed member{?s} ind{?ex/ices} of {.arg db}: {as.character(which(!nz))}"), 
              class="load_database_unnamed_error")
  }
  list2env(db, env)
  
  if(remove) {
    x_name = caller_arg(db)
    if(exists(x_name, where=env, inherits=FALSE)) 
      remove(list=x_name, envir=env)
    else                          
      remove(list=x_name, envir=parent.frame())
  }
}


#' @rdname load_database
#' @usage NULL
#' @export
#' @importFrom lifecycle deprecate_warn
load_list = function(db, env=parent.frame(), remove=TRUE){
  deprecate_warn(when="0.6.0", what="load_list()", with="load_database()")
  load_database(db, env, remove)
}



#' Save a list as `.RData` file
#'
#' @param x a list
#' @param filename the filename, with the `.RData` extension.
#'
#' @return nothing, called for its side-effect
#' @noRd
#' @keywords internal
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
print.edc_database = function(x, ...){
  x = x %>% keep(is.data.frame) %>% discard_at(".lookup")
  cat_rule("EDCimport database", col = "violet")
  nms = cli_vec(names(x), list("vec-trunc"=3))
  cli_bullets(c(
    "Contains {length(x)} table{?s}: {.arg {nms}}",
    i="Use {.run EDCimport::load_database(db)} to load the tables in the global environment.",
    i="Use {.run EDCimport::edc_lookup()} to see the summary table."
  ))
}

#' @export
print.tm_database = print.edc_database
