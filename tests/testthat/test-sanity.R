
test_that("edc_data_warn snapshot", {
  clean_lookup()
  reset_warn_list()
  db = edc_example()
  attach(db)
  
  expect_snapshot({
    #default
    enrol %>%
      filter(age>60) %>%
      edc_data_warn("Age should not be >60")
    
    #no issue_n
    enrol %>%
      filter(age>70) %>%
      edc_data_warn("Age should not be >70", issue_n=NULL)
    
    #with issue_n & max_subjid
    enrol %>%
      filter(age>20) %>%
      edc_data_warn("Age should not be >20", issue_n=1, max_subjid=2)
    
    #multiple subjid proposals
    enrol %>%
      filter(age>70) %>%
      edc_data_warn("Age should not be >70", issue_n=NULL, col_subjid=c("subjid", "PATNO"))
    
    
    ## WARNINGS
    
    #warning, multiple subjid found
    enrol %>%
      filter(age>70) %>%
      edc_data_warn("Age should not be >70", issue_n=2, col_subjid=c("subjid", "arm"))
    
    #warning, multiple subjid found, reverse order
    enrol %>%
      filter(age>70) %>%
      edc_data_warn("Age should not be >70", issue_n=3, col_subjid=c("arm", "subjid"))
  
    
    edc_data_warnings()
  })
  
})

test_that("edc_data_warn errors", {
  clean_lookup()
  db = edc_example()
  attach(db)
  
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
  attach(db)
  
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
