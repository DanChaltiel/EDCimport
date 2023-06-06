#' @keywords internal
#' @importFrom dplyr %>% 
#' @importFrom rlang %||% := 
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL



# Global settings -----------------------------------------------------------------------------


#' @importFrom dplyr %>%
#' @export
dplyr::`%>%`

globalVariables(c(".", ":=", "!!", "SUBJID", "age", "dataset", "n_id", "name", "value"))
