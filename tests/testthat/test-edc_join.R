test_that("`edc_xxx_join()` works", {
  db = edc_example()
  load_database(db)
  
  expect_dim = function(x, n_row, n_col){
    expect_equal(nrow(x), n_row)
    expect_equal(ncol(x), n_col)
    x
  }
  
  enrol0 = enrol %>% tail(40)
  short %>% 
    head(20) %>% 
    edc_left_join(enrol0) %>% 
    expect_dim(20, 10)
  short %>% 
    head(20) %>% 
    edc_right_join(enrol0) %>% 
    expect_dim(40, 10)
  short %>% 
    head(20) %>% 
    edc_full_join(enrol0) %>% 
    expect_dim(50, 10)
  
  #Case insensitive
  enrol2 = enrol %>% rename(SUBJID=subjid)
  short %>% 
    rename(SUBJID=subjid) %>% 
    edc_left_join(enrol) %>% 
    names() %>% 
    expect_contains(c("SUBJID", "val1", "enrol_date", "crfname_enrol"))
  
  short %>% 
    edc_left_join(enrol2) %>% 
    names() %>% 
    expect_contains(c("subjid", "val1", "enrol_date", "crfname_enrol2"))
  
  #multiple match
  short2 = short %>% mutate(SUBJID=Inf)
  enrol2 = enrol %>% mutate(SUBJID=Inf)
  ok = function(x) expect_true(names(x)[1]=="subjid")
  edc_left_join(short2, enrol) %>% ok()
  edc_left_join(short, enrol2) %>% ok()
  edc_left_join(enrol, short2) %>% ok()
  edc_left_join(enrol2, short) %>% ok()
  
  
  #Errors
  enrol3 = enrol %>% rename(XXXX=subjid)
  short %>% 
    rename(XXXX=subjid) %>% 
    edc_left_join(enrol) %>% 
    expect_error(class="edc_subjid_not_found", regexp="not found in `x`")
  
  short %>% 
    edc_left_join(enrol3) %>% 
    expect_error(class="edc_subjid_not_found", regexp="not found in `y`")
  
  short %>% 
    rename(XXXX=subjid) %>% 
    edc_left_join(enrol3) %>% 
    expect_error(class="edc_subjid_not_found", regexp="not found in either `x` or `y`")
})
