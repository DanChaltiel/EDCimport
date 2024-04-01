#' @keywords internal
#' @importFrom dplyr %>% 
#' @importFrom rlang %||% := 
"_PACKAGE"

## usethis namespace: start
#' @importFrom lifecycle deprecated
## usethis namespace: end
NULL



# Global settings -----------------------------------------------------------------------------


#' @importFrom dplyr %>%
#' @export
dplyr::`%>%`

utils::globalVariables(c(".", ":=", "!!", ".data", ".env", "SUBJID", "age", "dataset", "n_id", "name", "value"))
