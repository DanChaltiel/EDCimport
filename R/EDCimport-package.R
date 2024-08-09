#' @keywords internal
#' @importFrom dplyr %>% 
#' @importFrom rlang %||% := 
#' @importFrom cli qty col_green col_red
"_PACKAGE"

## usethis namespace: start
#' @importFrom lifecycle deprecated
## usethis namespace: end
NULL


# Reexports -----------------------------------------------------------------------------------

#' @importFrom dplyr %>%
#' @export
dplyr::`%>%`

#' @importFrom tibble tibble
#' @export
tibble::tibble


# Global settings -----------------------------------------------------------------------------

edcimport_env = rlang::new_environment()
edcimport_env$warn_list = list()
edcimport_env$lookup = NULL

utils::globalVariables(c(".", ":=", "!!", ".data", ".env", "SUBJID", "age", "dataset", "n_id", "name", "value", ".id", "aegr", "aesoc", "best_resp", "case_when", "diff_first", "element_blank", "facet_grid", "first_sum", "grade2", "grade_max", "last_sum", "min_sum", "n_ae", "n_arm", "n_soc", "n_term", "replace_na", "resp", "resp_num", "sae", "scale_fill_steps", "star", "star_txt", "subjid", "variable", "vars", "x", "column", "last_date", "origin", "pct_datasets", "arm_", "grade_", "soc_", "subjid_", "term_", "any_ae", "any_severe", "label", "label_percent_positive", "n_severe", "pct_ae", "pct_severe", "severe_", "unit", ".name", "Tot", "any_grade_sup_na", "calc", "col_keys", "crfstat", "datetime_extraction", "first_date", "h1", "h2", "origin_col", "origin_data", "origin_label", "prefered", "resp2", "showNA", "sort_by_count", "title", "weight"))




