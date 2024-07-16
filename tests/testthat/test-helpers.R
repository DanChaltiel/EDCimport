
edc_options(edc_lookup_overwrite_warn=FALSE)

# Helpers -------------------------------------------------------------------------------------


test_that("assert_no_duplicate works", {
  tm = edc_example()
  
  tibble(subjid=c(1:10)) %>% assert_no_duplicate() %>% expect_silent()
  
  tibble(subjid=c(1:10, 1)) %>% assert_no_duplicate() %>% 
    expect_error(class="edcimport_assert_no_duplicate")
  tibble(ptno=c(1:10, 1:3)) %>% assert_no_duplicate(id_col=c("SUBJID","PTNO")) %>% 
    expect_error(class="edcimport_assert_no_duplicate")
  tibble(not_subjid=c(1:10)) %>% assert_no_duplicate() %>% 
    expect_error(class="edcimport_assert_no_duplicate_no_col")
  tibble(subjid=c(1:10, 4), ptno=c(1:10, 3)) %>% assert_no_duplicate(id_col=c("SUBJID","PTNO")) %>% 
    expect_error(class="edcimport_assert_no_duplicate_many_col")
  
  #By groups
  df = tibble(subjid=rep(1:10, 2), visit=rep(c("V1", "V2"), each=10))
  df %>% assert_no_duplicate() %>% 
    expect_error(class="edcimport_assert_no_duplicate")
  df %>% assert_no_duplicate(by=visit) %>% 
    expect_silent()
  df = tibble(subjid=rep(1:10, 4), visit=rep(c("V1", "V2"), 2, each=10), group=rep(c("A", "B"), each=20))
  df %>% assert_no_duplicate() %>% 
    expect_error(class="edcimport_assert_no_duplicate")
  df %>% assert_no_duplicate(by=c(visit, group)) %>% 
    expect_silent()
})



test_that("edc_warn_patient_diffs works", {
  local_options(edc_subjid_ref = 1:50)
  
  edc_warn_patient_diffs(1:50) %>% expect_silent()
  edc_warn_patient_diffs(rep(1:50, 3)) %>% expect_silent()
  edc_warn_patient_diffs(1:48, ref=1:48) %>% expect_silent()
  
  edc_warn_patient_diffs(1:48) %>% expect_warning(class="edc_edc_warn_patient_diffs_miss")
  edc_warn_patient_diffs(1:52) %>% expect_warning(class="edc_edc_warn_patient_diffs_additional")
})



test_that("fct_yesno works", {
  set.seed(42)
  x = tibble(a=sample(c("Yes", "No"), size=20, replace=T),
             b=sample(c("1-Yes", "0-No"), size=20, replace=T),
             c=sample(c("Oui", "Non"), size=20, replace=T),
             x=sample(0:1, size=20, replace=T),
             y=1:20)
  edc_reset_options(quiet=TRUE)
  x$a %>% factor() %>% levels() %>% expect_equal(c("No", "Yes"))
  x$a %>% fct_yesno() %>% levels() %>% expect_equal(c("Yes", "No"))
  
  x$b %>% factor() %>% levels() %>% expect_equal(c("0-No", "1-Yes"))
  x$b %>% fct_yesno() %>% levels() %>% expect_equal(c("1-Yes", "0-No"))
  
  x$c %>% factor() %>% levels() %>% expect_equal(c("Non", "Oui"))
  x$c %>% fct_yesno() %>% levels() %>% expect_null()
  
  supp_levels = list(c("Oui", "Non"), c("Ja", "Nein"))
  local_options(edc_fct_yesno = get_yesno_lvl(supp_levels))
  x$c %>% factor() %>% levels() %>% expect_equal(c("Non", "Oui"))
  x$c %>% fct_yesno() %>% levels() %>% expect_equal(c("Oui", "Non"))
})
