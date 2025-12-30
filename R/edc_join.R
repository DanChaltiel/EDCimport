

# Constructor ---------------------------------------------------------------------------------

#' @noRd
#' @keywords internal
#' @importFrom cli cli_abort format_inline
#' @importFrom dplyr all_of any_of enquo everything select setdiff
#' @importFrom purrr keep
#' @importFrom rlang as_label caller_call set_names
.edc_join = function(type){
  dplyr_join = switch(type, 
                      left=dplyr::left_join, right=dplyr::right_join, 
                      full=dplyr::full_join, inner=dplyr::inner_join)
  
  function(x, y, by=NULL, suffix=NULL, cols=everything(), remove_dups=FALSE){
    subjid_col = getOption("override_subjid_cols", get_subjid_cols())
    by_x = names(x)[tolower(names(x)) %in% tolower(subjid_col)]
    by_y = names(y)[tolower(names(y)) %in% tolower(subjid_col)]
    if(length(by_x)>1) by_x = intersect(by_x, by_y)[1] %0% by_x
    if(length(by_y)>1) by_y = intersect(by_x, by_y)[1] %0% by_y
    if(is.null(by) && length(by_x)>0 && length(by_y)>0) {
      by = set_names(by_y, by_x)
    }
    if(is.null(by)){
      l = format_inline("either {.arg x} or {.arg y}")
      if(length(by_x)>0) l = format_inline("{.arg y}")
      if(length(by_y)>0) l = format_inline("{.arg x}")
      cli_abort(c("Could not find a common primary key for {.arg x} and {.arg y}.",
                  i="Primary key {.val {get_subjid_cols()}} was not found in {l}."),
                class="edc_subjid_not_found")
    }
    if(is.null(suffix)){
      suffix = c("", paste0("_", as_label(caller_call(0)[[3]])))
    }
    
    y = y %>% select(any_of(c(unname(by), by_y)), !!enquo(cols))
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


# Functions -----------------------------------------------------------------------------------


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
