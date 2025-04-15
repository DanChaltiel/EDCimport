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


globals = c(
  "!!", ".", ".data", ".env", ".id", ".name", ":=", "aegr", "aesoc", 
  "age", "any_ae", "any_grade_sup_na", "any_severe", "arm_", "best_resp", 
  "calc", "case_when", "col_keys", "column", "crfname", "crfstat", 
  "dataset", "dataset_sum", "enrol_date", "datetime_extraction", 
  "diff_first", "element_blank", "facet_grid", "first_date", "first_sum", 
  "grade_", "grade_max", "grade2", "h1", "h2", "included", "included_sum", 
  "label", "label_percent_positive", "last_date", "last_sum", "min_sum", 
  "n_ae", "n_arm", "n_id", "n_severe", "n_soc", "n_term", "name", "format_file",
  "origin", "origin_col", "origin_data", "origin_label", "pct_ae", 
  "pct_datasets", "pct_severe", "prefered", "preferred", "preferred_last_date", 
  "replace_na", "resp", "resp_num", "resp2", "rows_per_id", "sae", "scale_fill_steps", 
  "severe_", "showNA", "soc_", "sort_by_count", "star", "star_txt", 
  "subjid", "subjid", "subjid_", "subjid_sum", "subjids", "term_", 
  "title", "Tot", "unit", "value", "variable", "vars", "weight", 
  "x", "exclude", "div",  "first", "has_subjid", "column_label", "n_datasets"
)
# dput(sort(globals))
utils::globalVariables(globals)




