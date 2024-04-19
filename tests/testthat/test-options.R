edc_options(edc_lookup_overwrite_warn=FALSE)

test_that("No missing options", {
  missing_options = missing_options_helper()
  expect_identical(missing_options, character(0))
})


test_that("Options works", {
  edc_reset_options(quiet=TRUE)
  getOption("edc_subjid_ref") %>% expect_null()
  
  x=1
  edc_options(edc_subjid_ref=x)
  getOption("edc_subjid_ref") %>% expect_equal(1)
  
  edc_options(edc_subjid_ref=2)
  getOption("edc_subjid_ref") %>% expect_equal(2)
  
  EDCimport::edc_options(edc_subjid_ref=3)
  getOption("edc_subjid_ref") %>% expect_equal(3)
  
  edc_reset_options(quiet=TRUE)
  getOption("edc_subjid_ref") %>% expect_null()
})
