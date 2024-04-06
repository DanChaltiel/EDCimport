#' @keywords internal
#' @importFrom dplyr %>% 
#' @importFrom rlang %||% := 
#' @importFrom cli qty
"_PACKAGE"

## usethis namespace: start
#' @importFrom lifecycle deprecated
## usethis namespace: end
NULL


# Reexports -----------------------------------------------------------------------------------

#' @importFrom dplyr %>%
#' @export
dplyr::`%>%`

#' @importFrom flextable as_flextable
#' @export
flextable::as_flextable


# Global settings -----------------------------------------------------------------------------

utils::globalVariables(c(".", ":=", "!!", ".data", ".env", "SUBJID", "age", "dataset", "n_id", "name", "value"))
