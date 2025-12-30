
#' @noRd
#' @keywords internal
#' @importFrom dplyr across cur_column everything mutate
#' @importFrom purrr map_chr
copy_label_from = function(x, from){
  if(!is.list(x)){
    from_label = attr(from, "label")
    if(is.null(from_label)) return(x)
    attr(x, "label") = from_label
    return(x)
  }
  from_labs = map_chr(from, ~attr(.x, "label") %||% NA)
  mutate(x, across(everything(), ~{
    lab = unname(from_labs[cur_column()])
    if(!is.na(lab)){
      attr(.x, "label") = lab
    }
    .x
  }))
}


#' @noRd
#' @keywords internal
set_label = function(x, lab){
  attr(x, "label") = lab
  x
}


#' @noRd
#' @keywords internal
#' @importFrom purrr map map2
#' @importFrom rlang is_null
get_label = function(x, default=names(x)){
  if (is.list(x)) {
    if (is.null(default)) default = rep(NA, length(x))
    if(inherits(x, "POSIXlt")) x = as.POSIXct(x)
    lab = x %>% map(get_label) %>% map2(default, ~{
      if (is.null(.x)) .y else .x
    })
  } else {
    lab = attr(x, "label", exact=TRUE)
    if (is_null(lab)) lab = default
  }
  lab
}


#' @noRd
#' @keywords internal
#' @importFrom dplyr setdiff
remove_labels = function(x){
  if(is.null(x)) return(x)
  if(is.list(x)){
    for (each in seq_along(x)){
      x[[each]] = remove_labels(x[[each]])
    }
  }
  attr(x, "label") = NULL
  class(x) = setdiff(class(x), c("labelled"))
  x
}
