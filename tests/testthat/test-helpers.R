
edc_options(edc_lookup_overwrite_warn=FALSE)

test_that("assert_no_duplicate works", {
  tm = edc_example() #to set up the lookup and the subjid column
  
  tibble(subjid=c(1:10)) %>% assert_no_duplicate() %>% expect_silent()
  
  tibble(subjid=c(1:10, 1)) %>% assert_no_duplicate() %>% 
    expect_error(class="edcimport_assert_no_duplicate")
  tibble(ptno=c(1:10, 1:3)) %>% assert_no_duplicate(id_col=c("subjid","PTNO")) %>% 
    expect_error(class="edcimport_assert_no_duplicate")
  tibble(not_subjid=c(1:10)) %>% assert_no_duplicate() %>% 
    expect_error(class="edcimport_assert_no_duplicate_no_col")
  tibble(subjid=c(1:10, 4), ptno=c(1:10, 3)) %>% assert_no_duplicate(id_col=c("subjid","PTNO")) %>% 
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
  
  edc_warn_patient_diffs(1:48) %>% expect_warning(class="edc_edc_patient_diffs_warning")
  edc_warn_patient_diffs(1:52) %>% expect_warning(class="edc_edc_patient_diffs_warning")
})



test_that("harmonize_subjid works", {
  #numeric subjid
  db = list(x=data.frame(subjid=c("1", "2", "3", "005"), a=1))
  h = harmonize_subjid(db, col_subjid="subjid")
  expect_identical(h$x$subjid, factor(c("1", "2", "3", "5")))
  h = harmonize_subjid(db, col_subjid="subjid", preprocess=identity)
  expect_identical(h$x$subjid, factor(c("1", "2", "3", "005"), 
                                      levels = c("1", "2", "3", "005")))
  
  #factor subjid
  db = list(x=data.frame(subjid=as_factor(c("1", "2", "7", "5")), a=1))
  h = harmonize_subjid(db, col_subjid="subjid")
  expect_identical(h$x$subjid, factor(c("1", "2", "7", "5")))
  
  #character subjid
  db = list(x=data.frame(subjid=c("pat-1", "pat-3", "pat-2"), a=1))
  h = harmonize_subjid(db, col_subjid="subjid")
  expect_identical(h$x$subjid, factor(c("pat-1", "pat-3", "pat-2")))
})



test_that("fct_yesno works", {
  edc_reset_options(quiet=TRUE)
  set.seed(42)
  N=10
  dat = tibble(
    a=sample(c("Yes", "No"), size=N, replace=TRUE),
    b=sample(c("Oui", "Non"), size=N, replace=TRUE),
    c=sample(0:1, size=N, replace=TRUE),
    d=sample(c(TRUE, FALSE), size=N, replace=TRUE),
    e=sample(c("1-Yes", "0-No"), size=N, replace=TRUE),
    y=sample(c("aaa", "bbb", "ccc"), size=N, replace=TRUE),
    z=1:N,
  )
  
  rslt = dat %>% mutate(across(everything(), ~fct_yesno(.x, fail=FALSE)))
  
  x1 = rslt %>% 
    select(a:e) %>% 
    pivot_longer(everything()) %>% 
    pull(value)
  
  expect_true(is.factor(x1))
  expect_identical(levels(x1), c("Yes", "No"))
  
  expect_identical(dat$y, rslt$y)
  expect_identical(dat$z, rslt$z)
  
  fct_yesno(dat$y, fail=TRUE) %>% expect_error(class="fct_yesno_unparsed_error")
  fct_yesno(dat$z, fail=TRUE) %>% expect_silent()
})
