# edc_data_warn snapshot

    Code
      enrol %>% filter(age > 60) %>% edc_data_warn("Age should not be >60")
    Condition
      Warning:
      Issue #xx: Age should not be >60 (10 patients: #1, #7, #9, #11, #12, ...)
    Code
      enrol %>% filter(age > 70) %>% edc_data_warn("Age should not be >70", issue_n = NULL)
    Condition
      Warning:
      Age should not be >70 (2 patients: #9 and #12)
    Code
      enrol %>% filter(age > 20) %>% edc_data_warn("Age should not be >20", issue_n = 1,
        max_subjid = 2)
    Condition
      Warning:
      Issue #01: Age should not be >20 (50 patients: #1, #2, ...)
    Code
      enrol %>% filter(age > 70) %>% edc_data_warn("Age should not be >70", issue_n = NULL,
        col_subjid = c("subjid", "PATNO"))
    Condition
      Warning:
      Age should not be >70 (2 patients: #9 and #12)
    Code
      enrol %>% filter(age > 70) %>% edc_data_warn("Age should not be >70", issue_n = 2,
        col_subjid = c("subjid", "arm"))
    Condition
      Warning in `edc_data_warn()`:
      Found 2 subject identifiers in the input dataset: "subjid" and "arm". Defaulting to the first one.
      Warning:
      Issue #02: Age should not be >70 (2 patients: #9 and #12)
    Code
      enrol %>% filter(age > 70) %>% edc_data_warn("Age should not be >70", issue_n = 3,
        col_subjid = c("arm", "subjid"))
    Condition
      Warning in `edc_data_warn()`:
      Found 2 subject identifiers in the input dataset: "arm" and "subjid". Defaulting to the first one.
      Warning:
      Issue #03: Age should not be >70 (2 patients: #Ctl and #Trt)
    Code
      edc_data_warnings()
    Output
      # A tibble: 5 x 4
        issue_n message               subjid     fun     
        <chr>   <chr>                 <list>     <chr>   
      1 01      Age should not be >20 <chr [50]> cli_warn
      2 02      Age should not be >70 <chr [2]>  cli_warn
      3 03      Age should not be >70 <chr [2]>  cli_warn
      4 xx      Age should not be >60 <chr [10]> cli_warn
      5 <NA>    Age should not be >70 <chr [2]>  cli_warn

