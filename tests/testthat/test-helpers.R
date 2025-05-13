
edc_options(edc_lookup_overwrite_warn=FALSE)

test_that("assert_no_duplicate() works", {
  db = edc_example() #to set up the lookup and the subjid column
  
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



test_that("edc_warn_patient_diffs() works", {
  local_options(edc_subjid_ref = 1:50)
  
  edc_warn_patient_diffs(1:50) %>% expect_silent()
  edc_warn_patient_diffs(rep(1:50, 3)) %>% expect_silent()
  edc_warn_patient_diffs(1:48, ref=1:48) %>% expect_silent()
  
  edc_warn_patient_diffs(1:48) %>% expect_warning(class="edc_edc_patient_diffs_warning")
  edc_warn_patient_diffs(1:52) %>% expect_warning(class="edc_edc_patient_diffs_warning")
})



test_that("edc_unify_subjid() works", {
  #numeric subjid
  db = list(x=data.frame(subjid=c("1", "2", "3", "005"), a=1))
  h = edc_unify_subjid(db, col_subjid="subjid")
  expect_identical(h$x$subjid, factor(c("1", "2", "3", "5")))
  h = edc_unify_subjid(db, col_subjid="subjid", preprocess=identity)
  expect_identical(h$x$subjid, factor(c("1", "2", "3", "005"), 
                                      levels = c("1", "2", "3", "005")))
  
  #factor subjid
  db = list(x=data.frame(subjid=as_factor(c("1", "2", "7", "5")), a=1))
  h = edc_unify_subjid(db, col_subjid="subjid")
  expect_identical(h$x$subjid, factor(c("1", "2", "7", "5")))
  
  #character subjid
  db = list(x=data.frame(subjid=c("pat-1", "pat-3", "pat-2"), a=1))
  h = edc_unify_subjid(db, col_subjid="subjid")
  expect_identical(h$x$subjid, factor(c("pat-1", "pat-3", "pat-2")))
})




test_that("edc_xxx_join() works", {
  local_options(edc_lookup_overwrite_warn=FALSE)
  db = edc_example()
  a = db$data1
  b = db$enrol %>% 
    mutate(subjid=set_label(subjid, "Patient No"))
  
  x = a %>% 
    edc_left_join(b)
  expect_true(all(c("subjid", "date1", "arm", "crfname_b", "crfstat_b") %in% names(x)))
  expect_equal(get_label(x$subjid), get_label(a$subjid))  
  
  x = a %>% 
    remove_labels() %>% 
    edc_left_join(b, remove_dups=TRUE)
  expect_true(all(c("subjid", "date1", "arm", "crfname_b") %in% names(x)))
  expect_false("crfstat_b" %in% names(x))
  
  expect_equal(get_label(x$subjid), get_label(b$subjid))  
  
  x = a %>% 
    edc_left_join(b, cols=arm)
  expect_false("enrol_date" %in% names(x))
  
  x = a %>% 
    edc_left_join(b, cols=c(treatment=arm))
  expect_true("treatment" %in% names(x))
  expect_false("arm" %in% names(x))
  
})




test_that("fct_yesno() works", {
  edc_reset_options(quiet=TRUE)
  set.seed(42)
  N=20
  x = tibble(
    eng=sample(c("Yes", "No", ""), size=N, replace=TRUE),
    fra=sample(c("Oui", "Non"), size=N, replace=TRUE),
    bin=sample(0:1, size=N, replace=TRUE),
    log=sample(c(TRUE, FALSE), size=N, replace=TRUE),
    eng2=sample(c("1-Yes", "0-No", "2-NA"), size=N, replace=TRUE),
    
    chr=sample(c("aaa", "bbb", "ccc"), size=N, replace=TRUE),
    num=1:N,
  )
  x[10:11,] = NA
  
  
  rslt = x %>% mutate(across(everything(), ~fct_yesno(.x, fail=FALSE)))
  
  x1 = rslt %>% 
    select(eng:eng2) %>% 
    pivot_longer(everything()) %>% 
    pull(value)
  
  #TESTS
  expect_true(is.factor(x1))
  expect_identical(levels(x1), c("Yes", "No"))
  
  expect_identical(x$chr, rslt$chr)
  expect_identical(x$num, rslt$num)
  
  fct_yesno(x$num, fail=TRUE) %>% expect_silent() #no fct_yesno_unparsed_error
  
  #SNAPSHOT
  expect_snapshot({
    
    fct_yesno("Yes")
    fct_yesno(c("No", "Yes"))
    
    mutate_all(x, fct_yesno, fail=FALSE)
    mutate_all(x, fct_yesno, fail=FALSE, strict=TRUE)
    
    #should not change `fra` and `eng2`
    mutate_all(x, fct_yesno, fail=FALSE, input=list(yes="Ja", no="Nein"))
  })
  
  
  #ERRORS
  mutate_all(x, fct_yesno, fail=TRUE) %>% expect_error(class="fct_yesno_unparsed_error")
  fct_yesno(x$chr) %>% expect_error(class="fct_yesno_unparsed_error")
  fct_yesno("YesNo") %>% expect_error(class="fct_yesno_both_error")
  fct_yesno("foobar") %>% expect_error(class="fct_yesno_unparsed_error")
  
})
