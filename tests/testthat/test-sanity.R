
test_that("edc_data_warn snapshot", {
  clean_lookup()
  reset_warn_list()
  db = edc_example()
  load_database(db)
  
  expect_snapshot({
    #default
    enrol %>%
      select(subjid, enrol_date, age) %>% 
      filter(age>80) %>%
      edc_data_warn("Age should not be >80")
    enrol %>%
      select(subjid, enrol_date, age) %>% 
      filter(age>70) %>%
      edc_data_warn("Age should not be >70")
    
    #no issue_n
    enrol %>%
      select(subjid, enrol_date, age) %>% 
      filter(age>60) %>%
      edc_data_warn("Age should not be >60", issue_n=NULL)
    enrol %>%
      select(subjid, enrol_date, age) %>% 
      filter(age>60) %>%
      edc_data_warn("No, Age should not be >60", issue_n=NULL)
    
    #with issue_n & max_subjid
    enrol %>%
      select(subjid, enrol_date, age) %>% 
      filter(age>50) %>%
      edc_data_warn("Age should not be >50", issue_n=1, max_subjid=2)
    
    #alternative SUBJID column
    enrol %>%
      select(PATNO=subjid, age) %>% 
      filter(age>40) %>%
      edc_data_warn("Age should not be >40", issue_n=NULL, 
                    col_subjid=c("subjid", "PATNO"))
    
    
    ## WARNINGS
    
    #warning, multiple subjid found
    enrol %>%
      select(subjid, enrol_date, age, arm) %>% 
      filter(age>30) %>%
      edc_data_warn("Age should not be >30", issue_n=2, col_subjid=c("subjid", "arm"))
    
    #warning, multiple subjid found, reverse order
    enrol %>%
      select(subjid, enrol_date, age, arm) %>% 
      filter(age>20) %>%
      edc_data_warn("Age should not be >20", issue_n=3, col_subjid=c("arm", "subjid"))
    
    ## DUPLICATES 
    
    #duplicate of issue number
    enrol %>%
      filter(age>50) %>%
      edc_data_warn("Age should not be >50", issue_n=1)
    
    #duplicate without issue number
    enrol %>%
      select(subjid, enrol_date, age) %>% 
      filter(age>70) %>%
      edc_data_warn("Age should not be >70")
    
    edc_data_warnings()
  })
  
  
  #post-snapshot: test save_edc_data_warnings()
  w = edc_data_warnings()
  expect_equal(nrow(w), 5)
  
  f = tempfile(fileext=".xlsx")
  save_edc_data_warnings(edc_warnings=w, path=f, open=FALSE)
  assert_file_exists(f)
  file.remove(f)
  
})


test_that("edc_data_warn errors", {
  clean_lookup()
  db = edc_example()
  load_database(db)
  
  #error expected
  enrol %>%
    filter(age>70) %>%
    edc_data_stop("Age should never be >70", issue_n=99) %>% 
    expect_error(class="edc_data_condition")
  
  #error subjid not found
  enrol %>%
    filter(age>70) %>%
    edc_data_warn("Age should not be >70", issue_n=98, col_subjid=c("PATNO")) %>% 
    expect_error(class="edc_data_condition_subjid_error")
  
})


test_that("edc_data_warn CSV", {
  clean_lookup()
  db = edc_example()
  load_database(db)
  
  path = tempfile(fileext=".csv")
  
  expect_false(file_exists(path))
  
  input = enrol %>%
    as.data.frame() %>% 
    filter(age>70) %>%
    select(subjid, age, arm) %>% 
    remove_labels()
  
  input %>% 
    edc_data_warn("Age should not be >70", issue_n=99, csv_path=path) %>% 
    expect_warning()
  
  output = read.csv2(path)
  
  expect_equal(input, output, tolerance = 0.01)
  
  #cleanup
  unlink(path)
  expect_false(file_exists(path))
  
})
