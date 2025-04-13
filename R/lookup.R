

#' Generate a lookup table
#' 
#' @param data_list a list containing at least 1 dataframe
#' @return a dataframe summarizing column names and labels 
#' 
#' @examples
#' x = edc_example()
#' x$.lookup=NULL
#' lk = build_lookup(x)
#' lk
#' lk %>% tidyr::unnest(c(names, labels))  
#' 
#' @seealso [extend_lookup()], [edc_lookup()]
#' 
#' @noRd
#' @keywords internal
#' @importFrom cli cli_abort
#' @importFrom dplyr arrange lst mutate
#' @importFrom purrr discard_at map map_dbl
#' @importFrom rlang caller_arg is_named
#' @importFrom tibble tibble
build_lookup = function(data_list){
  if(is.data.frame(data_list)) data_list = lst(!!caller_arg(data_list):=data_list)
  if(!is.list(data_list)){
    cli_abort(c("{.code data_list} should be a list.", 
                i="{.code class(data_list)}: {.cls {class(data_list)}}"), 
              class="edc_lookup_not_list")
  }
  datetime_extraction = data_list[["datetime_extraction"]]
  data_list_nm = names(data_list)
  exclude = c(".lookup", "date_extraction", "datetime_extraction")
  data_list = data_list %>% discard_at(exclude)
  if(length(data_list)==0){
    cli_abort(c("{.code data_list} is empty or contains only non-dataframe elements.", 
                i="{.code names(data_list)}: {.val {data_list_nm}}"), 
              class="edc_lookup_empty")
  }
  if(!is_named(data_list)){
    cli_abort("Datasets in {.code data_list} should have a name.", 
              class="edc_lookup_unnamed")
  }
  f = function(.x, expr, default) if(is.data.frame(.x)) expr else default
  
  rtn = tibble(dataset=tolower(names(data_list))) %>% 
    mutate(
      nrow=map_dbl(data_list, ~f(.x, nrow(.x), 0)), 
      ncol=map_dbl(data_list, ~f(.x, ncol(.x), 0)), 
      names=map(data_list, ~f(.x, names(.x), NULL)), 
      labels=map(data_list, ~f(.x, get_label(.x), NULL)), 
    ) %>% 
    arrange(dataset) %>% 
    structure(datetime_extraction=datetime_extraction) %>% 
    add_class("edc_lookup")
  
  rtn
}

#' Retrieve the lookup table from options
#' 
#' @param ... passed on to [dplyr::arrange()]
#' @param check whether to check for internal consistency
#'
#' @return the lookup dataframe summarizing the database import 
#' 
#' @export
#' @importFrom cli cli_abort cli_warn format_inline
#' @importFrom dplyr arrange enquos
#' @importFrom purrr discard
#' 
#' @examples
#' db = edc_example()
#' load_database(db)
#' edc_lookup()
#' edc_lookup(dataset)
edc_lookup = function(..., check=TRUE){
  lookup = edcimport_env$lookup
  check = getOption("edc_lookup_check", check)
  if(isTRUE(check)){
    if(is.null(lookup)){
      cli_abort("Lookup is {.val NULL}. It will be created after using {.fn EDCimport::read_trialmaster},  {.fn EDCimport::read_trialmaster}, or any other {.pkg EDCimport} reading function.")
    }
    caller = parent.frame()
    missing_dataset = lookup$dataset %>% discard(~exists(.x, where=caller))
    if(length(missing_dataset) > 0){
      msg = NULL
      if(length(missing_dataset) < nrow(lookup)) {
        msg = c(i=format_inline("Missing datasets: {.val {missing_dataset}}."))
      }
      cli_warn(c("Datasets from this lookup are not available in the global environment.",
                 msg,
                 i="Did you forget to use {.run EDCimport::load_database(db)} to load the tables?"),
               class="edc_lookup_missing_dataset_warning",
               .frequency="once",
               .frequency_id="edc_lookup"
      )
    }
  }
  if(!is.null(lookup)){
    lookup = lookup %>% arrange(!!!enquos(...))
  }
  lookup
}


#' @rdname edc_lookup
#' @export
#' @usage NULL
get_lookup = deprecatedly(edc_lookup, what="get_lookup()", when="0.5.0")

#' @noRd
#' @keywords internal
#' @importFrom cli cli_warn
.set_lookup = function(lookup, verbose=TRUE){
  verbose = getOption("edc_lookup_overwrite_warn", verbose)
  if(verbose && !is.null(edc_lookup(check=FALSE))){
    cli_warn("Option {.val edc_lookup} has been overwritten.", 
             class="edc_lookup_overwrite_warn")
  }
  edcimport_env$lookup = lookup
  invisible(lookup)
}

#' Same as .set_lookup() without warning
#' @noRd
#' @keywords internal
.update_lookup = function(new){
  edcimport_env$lookup = new
  invisible(new)
}

#' @noRd
#' @keywords internal
is_lookup = function(x){
  inherits(x, "edc_lookup")
}

#' Extend the lookup table
#' 
#' This utility extends the lookup table to include: 
#'   * `n_id` the number of patients present in the dataset
#'   * `rows_per_id` the mean number of row per patient
#'   * `crfname` the actual name of the dataset
#'
#' @param lookup \[`data.frame(1)`]\cr the lookup table
#' @param id_cols,crf_cols \[`character(n)`]\cr for experts only
#' @param datasets \[`data.frame(n)`]\cr for experts only
#' @inheritParams read_all_xpt
#'
#' @return the lookup, extended
#' @noRd
#' @keywords internal
#' 
#' @importFrom dplyr arrange desc if_else last_col mutate pull relocate select
#' @importFrom purrr map map_chr
#' @importFrom rlang check_dots_empty
#' @examples
#' #tm = read_trialmaster("filename.zip", pw="xx")
#' db = edc_example()
#' load_database(db)
#' .lookup
#' .lookup = extend_lookup(.lookup)
#' .lookup
extend_lookup = function(lookup, ..., 
                         id_cols = get_subjid_cols(lookup), 
                         crf_cols = get_crfname_cols(lookup), 
                         datasets = get_datasets(lookup, envir=parent.frame())){
  check_dots_empty()  
  
  #case-insensitive column selection (cf. `any_of2`)
  get_subjid = function(data_name){
    data = datasets[[data_name]]
    if(!is.data.frame(data)) return(character(0))
    if(!any(id_cols %in% names(data))) return(character(0))
    x = data %>% select(any_of2(id_cols))
    x %>% pull(1) %>% as.character() %>% unique() %>% sort()
  }
  
  rtn = lookup %>% 
    mutate(
      subjids = map(dataset, ~get_subjid(.x)),
      n_id = lengths(subjids),
      rows_per_id = round(nrow/n_id, 1),
      rows_per_id = if_else(is.nan(rows_per_id) | is.infinite(rows_per_id), NA, rows_per_id),
      crfname = map_chr(dataset, ~get_data_name(datasets[[.x]], crfname=crf_cols))
    ) %>% 
    arrange(desc(n_id), desc(nrow)) %>% 
    relocate(c(subjids, names, labels), .after=last_col())
  
  rtn
}


# Methods -----------------------------------------------------------------

#' @export
#' @importFrom cli format_inline rule
#' @importFrom dplyr any_of select
#' @importFrom utils tail
print.edc_lookup = function(x, n=Inf, ...){
  extraction = attr(x, "datetime_extraction")
  EDCimport_version = attr(x, "EDCimport_version")
  project_name = attr(x, "project_name")
  # clean_names_fun = attr(x, "clean_names_fun")
  # split_mixed = attr(x, "split_mixed")
  
  par_extraction = par_version = par_projname = ""
  if(!is.null(project_name)) 
    par_projname = format_inline("- {project_name} ")
  if(!is.null(extraction)) 
    par_extraction = format_inline("(extraction of {format_ymd(extraction)}) ")
  if(!is.null(EDCimport_version)) 
    par_version = format_inline("- EDCimport v{EDCimport_version}")
  
  a = format_inline("Lookup table {par_projname}{par_extraction}{par_version}")
  print(rule(a, col = "violet"))
  
  x_tbl = remove_class(x, "edc_lookup") %>% 
    select(-any_of("subjids"), -names, -labels) %>% 
    format(n=n) %>% 
    tail(-1) #remove "A tibble: n × p"
  cat(x_tbl, sep="\n")
  
  invisible(x)
}


clean_lookup = function(){
  edcimport_env$lookup = NULL
  invisible(TRUE)
}
