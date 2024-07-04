

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
#' @export
#' @seealso [extend_lookup()], [get_lookup()]
#' 
#' @importFrom cli cli_abort
#' @importFrom dplyr arrange mutate
#' @importFrom labelled var_label
#' @importFrom purrr map map_dbl
#' @importFrom rlang caller_arg is_named
#' @importFrom tibble lst tibble
build_lookup = function(data_list){
  if(is.data.frame(data_list)) data_list = lst(!!caller_arg(data_list):=data_list)
  if(!is.list(data_list)){
    cli_abort(c("{.code data_list} should be a list.", 
                i="{.code class(data_list)}: {.cls {class(data_list)}}"), 
              class="edc_lookup_not_list")
  }
  data_list_n = format(names(data_list))
  data_list[".lookup"] = data_list["date_extraction"] = data_list["datetime_extraction"] = NULL
  if(length(data_list)==0){
    cli_abort(c("{.code data_list} is empty or contains only non-dataframe elements.", 
                i="{.code names(data_list)}: {.val {data_list_n}}"), 
              class="edc_lookup_empty")
  }
  if(!is_named(data_list)){
    cli_abort("Datasets in {.code data_list} should have a name.", 
              class="edc_lookup_unnamed")
  }
  f = function(.x, expr, default) if(is.data.frame(.x)) expr else default
  
  tibble(dataset=tolower(names(data_list))) %>% 
    mutate(
      nrow=map_dbl(data_list, ~f(.x, nrow(.x), 0)), 
      ncol=map_dbl(data_list, ~f(.x, ncol(.x), 0)), 
      names=map(data_list, ~f(.x, names(.x), NULL)), 
      labels=map(data_list, ~f(.x, var_label(.x, unlist=TRUE), NULL)), 
    ) %>% 
    arrange(nrow)
}

#' Retrieve the lookup table from options
#' 
#' @param check_null whether to stop if lookup is NULL
#'
#' @return the lookup dataframe summarizing column names and labels 
#' 
#' @export
#' @seealso [build_lookup()], [extend_lookup()]
#' @importFrom cli cli_abort
get_lookup = function(check_null=TRUE){
  lookup = getOption("edc_lookup")
  if(is.null(lookup) & isTRUE(check_null)){
    cli_abort("Lookup is NULL. Did you forget to import your data?")
  }
  lookup
}


#' @noRd
#' @keywords internal
#' @importFrom cli cli_warn
set_lookup = function(lookup){
  verbose = getOption("edc_lookup_overwrite_warn", TRUE)
  if(verbose && !is.null(get_lookup(check_null=FALSE))){
    cli_warn("Option {.val edc_lookup} has been overwritten.", 
             class="edc_lookup_overwrite_warn")
  }
  options(edc_lookup=lookup)
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
#' @inheritParams read_tm_all_xpt
#'
#' @return the lookup, extended
#' @export
#' @seealso [build_lookup()], [get_lookup()]
#' 
#' @importFrom cli cli_abort cli_warn
#' @importFrom dplyr arrange desc filter mutate relocate select
#' @importFrom purrr keep map map_chr map_dbl map_int map_lgl
#' @importFrom rlang check_dots_empty
#' @importFrom tidyselect last_col
#' @examples
#' #tm = read_trialmaster("filename.zip", pw="xx")
#' tm = edc_example_mixed()
#' load_list(tm)
#' .lookup
#' .lookup = extend_lookup(.lookup)
#' .lookup
extend_lookup = function(lookup, ..., 
                         id_cols = get_subjid_cols(lookup), 
                         crf_cols = get_crfname_cols(lookup), 
                         datasets = get_datasets(lookup, envir=parent.frame())){
  check_dots_empty()
  #case-insensitive column selection (cf. `any_of2`)
  f = function(x, colname){
    rtn = map(colname, ~select(datasets[[x]], any_of2(.x))) %>% 
      keep(~min(dim(.x))>0)
    ncols = map_dbl(rtn, ncol) %>% unique()
    if(length(ncols)>1) cli_abort("Several columns named {.val {colname}} in dataset {.val {x}} with discrepancies.")
    if(length(ncols)==0 || ncols==0) return(NA)
    if(ncols>1) cli_warn("Several columns named {.val {colname}} in dataset {.val {x}}.")
    length(unique(rtn[[1]][[1]]))
  }
  rtn = lookup %>% 
    filter(map_lgl(dataset, ~!inherits(datasets[[.x]], "error"))) %>% 
    mutate(
      n_id = map_int(dataset, ~f(.x, id_cols)),
      rows_per_id = round(nrow/n_id, 1),
      crfname = map_chr(dataset, ~get_data_name(datasets[[.x]]), crfname=crf_cols)
    ) %>% 
    arrange(n_id, desc(nrow)) %>% 
    relocate(c(names, labels), .after=last_col())
  rtn
}


# Methods -----------------------------------------------------------------

#' @export
#' @importFrom cli cat_rule cli_bullets cli_vec
#' @importFrom purrr discard_at keep
print.edc_lookup = function(x, ...){
  print.default(x)
  # x = x %>% keep(is.data.frame) %>% discard_at(".lookup")
  # cat_rule("Trialmaster database", col = "violet")
  # nms = cli_vec(names(x), list("vec-trunc"=3))
  # cli_bullets(c(
  #   "{length(x)} tables: {.arg {nms}}",
  #   i="Use {.code EDCimport::load_list(tm)} to load the tables in the global environment",
  #   i="Use {.code print(tm$.lookup)} to see the summary table"
  # ))
}
