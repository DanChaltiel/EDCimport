

# Helpers -------------------------------------------------------------------------------------


test_that("assert_no_duplicate works", {
  
  tibble(subjid=c(1:10)) %>% assert_no_duplicate() %>% expect_silent()
  tibble(subjid=c(1:10, 1)) %>% assert_no_duplicate() %>% 
    expect_error(class="edcimport_assert_no_duplicate")

  tibble(ptno=c(1:10, 5)) %>% assert_no_duplicate() %>% 
    expect_error(class="edcimport_assert_no_duplicate")
  tibble(subjid=c(1:10, 4), ptno=c(1:10, 3)) %>% assert_no_duplicate() %>% 
    expect_error(class="edcimport_assert_no_duplicate")
})



test_that("check_subjid works", {
