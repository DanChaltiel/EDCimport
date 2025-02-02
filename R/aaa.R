
#' adverb that adds a deprecated warning
#' @noRd
#' @keywords internal
#' @importFrom lifecycle deprecate_warn
#' @importFrom rlang caller_arg
#' @importFrom stringr str_ends
deprecatedly = function(f, when, what, with=caller_arg(f), details=NULL, type="warn"){
  if(!str_ends(with, "\\(\\)")) with=paste0(with,"()")
  function(...){
    deprecate_warn(when, what, with, details)
    f(...)
  }
}