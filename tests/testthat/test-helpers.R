

# Helpers -------------------------------------------------------------------------------------


test_that("assert_no_duplicate works", {
  
  tibble(not_subjid=c(1:10)) %>% assert_no_duplicate() %>% 
    expect_error(class="edcimport_assert_no_duplicate_no_col")
  
  tibble(subjid=c(1:10)) %>% assert_no_duplicate() %>% expect_silent()
  tibble(subjid=c(1:10, 1)) %>% assert_no_duplicate() %>% 
    expect_error(class="edcimport_assert_no_duplicate")

  tibble(ptno=c(1:10, 1:3)) %>% assert_no_duplicate() %>% 
    expect_error(class="edcimport_assert_no_duplicate")
  tibble(subjid=c(1:10, 4), ptno=c(1:10, 3)) %>% assert_no_duplicate() %>% 
    expect_error(class="edcimport_assert_no_duplicate")
})



test_that("check_subjid works", {
  local_options(edc_subjid_ref = 1:50)
  
  check_subjid(1:50) %>% expect_silent()
  check_subjid(rep(1:50, 3)) %>% expect_silent()
  check_subjid(1:48, ref=1:48) %>% expect_silent()
  
  check_subjid(1:48) %>% expect_warning(class="edc_check_subjid_miss")
  check_subjid(1:52) %>% expect_warning(class="edc_check_subjid_additional")
})

