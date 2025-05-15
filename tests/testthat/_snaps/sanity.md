# edc_data_warn snapshot

    Code
      enrol %>% select(subjid, enrol_date, age) %>% filter(age > 80) %>%
        edc_data_warn("Age should not be >80")
      enrol %>% select(subjid, enrol_date, age) %>% filter(age > 70) %>%
        edc_data_warn("Age should not be >70")
    Condition
      Warning:
      Issue #xx: Age should not be >70 (2 patients: #9 and #12)
    Code
      enrol %>% select(subjid, enrol_date, age) %>% filter(age > 60) %>%
        edc_data_warn("Age should not be >60", issue_n = NULL)
    Condition
      Warning:
      Age should not be >60 (10 patients: #1, #7, #9, #11, #12, ...)
    Code
      enrol %>% select(subjid, enrol_date, age) %>% filter(age > 60) %>%
        edc_data_warn("No, Age should not be >60", issue_n = NULL)
    Condition
      Warning:
      No, Age should not be >60 (10 patients: #1, #7, #9, #11, #12, ...)
    Code
      enrol %>% select(subjid, enrol_date, age) %>% filter(age > 50) %>%
        edc_data_warn("Age should not be >50", issue_n = 1, max_subjid = 2)
    Condition
      Warning:
      Issue #01: Age should not be >50 (23 patients: #1, #3, ...)
    Code
      enrol %>% select(PATNO = subjid, age) %>% filter(age > 40) %>% edc_data_warn(
        "Age should not be >40", issue_n = NULL, col_subjid = c("subjid", "PATNO"))
    Condition
      Warning:
      Age should not be >40 (42 patients: #1, #2, #3, #4, #5, ...)
    Code
      enrol %>% select(subjid, enrol_date, age, arm) %>% filter(age > 30) %>%
        edc_data_warn("Age should not be >30", issue_n = 2, col_subjid = c("subjid",
          "arm"))
    Condition
      Warning in `edc_data_warn()`:
      Found 2 subject identifiers in the input dataset: "subjid" and "arm". Defaulting to the first one.
      Warning:
      Issue #02: Age should not be >30 (47 patients: #1, #2, #3, #4, #5, ...)
    Code
      enrol %>% select(subjid, enrol_date, age, arm) %>% filter(age > 20) %>%
        edc_data_warn("Age should not be >20", issue_n = 3, col_subjid = c("arm",
          "subjid"))
    Condition
      Warning in `edc_data_warn()`:
      Found 2 subject identifiers in the input dataset: "arm" and "subjid". Defaulting to the first one.
      Warning:
      Issue #03: Age should not be >20 (2 patients: #Ctl and #Trt)
    Code
      enrol %>% filter(age > 50) %>% edc_data_warn("Age should not be >50", issue_n = 1)
    Condition
      Warning:
      Issue #01: Age should not be >50 (23 patients: #1, #3, #4, #5, #7, ...)
      Warning:
      Duplicated `edc_data_warn()` entry: issue_n="01"
    Code
      enrol %>% select(subjid, enrol_date, age) %>% filter(age > 70) %>%
        edc_data_warn("Age should not be >70")
    Condition
      Warning:
      Issue #xx: Age should not be >70 (2 patients: #9 and #12)
      Warning:
      Duplicated `edc_data_warn()` entry: "Age should not be >70"
    Code
      edc_data_warnings()
    Output
      # A tibble: 5 x 4
        issue_n message               data              subjid    
        <chr>   <chr>                 <list>            <list>    
      1 01      Age should not be >50 <tibble [23 x 6]> <chr [23]>
      2 02      Age should not be >30 <tibble [47 x 4]> <chr [47]>
      3 03      Age should not be >20 <tibble [50 x 4]> <chr [2]> 
      4 xx1     Age should not be >80 <tibble [0 x 3]>  <NULL>    
      5 xx2     Age should not be >70 <tibble [2 x 3]>  <chr [2]> 

