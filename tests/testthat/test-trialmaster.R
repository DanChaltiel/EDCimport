



skip_on_cran()
cachename = test_path("trialmaster_export_2022-08-25 15h16.rds")
filename = test_path("CRF_Dan_Export_SAS_XPORT_2022_08_25_15_16.zip")
filename_noformat = test_path("CRF_Dan_Export_SAS_XPORT_2022_08_25_15_16_noformat.zip")
filename_bad = test_path("CRF_Dan_Export.zip")

test_that("Read an archive", {
  clean_cache()
  edc_options(edc_lookup_overwrite_warn=TRUE, .local=TRUE)
  w = read_trialmaster(filename) %>% 
    expect_classed_conditions(message_class="read_tm_zip")
  w = read_trialmaster(filename, use_cache=TRUE) %>% 
    expect_classed_conditions(message_class="read_tm_cache", 
                              warning_class="edc_lookup_overwrite_warn")
  
  
  w = read_trialmaster(filename, use_cache="write") %>% 
    expect_classed_conditions(message_class="read_tm_zip", 
                              warning_class="edc_lookup_overwrite_warn")
  w = read_trialmaster(filename, use_cache="read") %>%
    expect_classed_conditions(message_class="read_tm_cache", 
                              warning_class="edc_lookup_overwrite_warn")
  # expect_message(w <- read_trialmaster(filename),
  #                class="read_tm_zip")
  # expect_message(w <- read_trialmaster(filename),
  #                class="read_tm_cache")
  # expect_message(w <- read_trialmaster(filename, use_cache=FALSE),
  #                class="read_tm_zip")
  # expect_warning(w <- read_trialmaster(filename, use_cache=FALSE),
  #                class="edc_lookup_overwrite_warn")
  
  w = read_trialmaster(filename, use_cache=FALSE) %>% 
    expect_classed_conditions(message_class="read_tm_zip", 
                              warning_class="edc_lookup_overwrite_warn")
  # w = read_trialmaster(filename, use_cache=FALSE) %>% 
  #   expect_classed_conditions(message_class=c("read_tm_zip", "xxxx"),
  #                             warning_class="edc_lookup_overwrite_warn")
  
  # class(es[[1]])
  # [1] "expectation_failure" "expectation"         "error"               "condition"      
  expect_length(w, 8)


  load_list(w, remove=FALSE)
  expect_true(exists("pat"))
  expect_length(pat, 35)
  expect_true(nrow(trial)==1)
  expect_true(names(w) %>% map_lgl(exists, envir=current_env()) %>% all())

  expect_s3_class(datetime_extraction, "POSIXct")

  #expect formats
  expect_s3_class(site$INCLSITE, "factor")
  expect_equal(as.character(site$INCLSITE), "Yes")
  expect_equal(dim(.lookup), c(5,8))
  clean_cache()
})



test_that("Read an archive without procformat", {
  expect_warning(w <- read_trialmaster(filename_noformat, use_cache=FALSE, verbose=0),
                 class="edc_tm_no_procformat_warning")
  expect_equal(as.character(w$site$INCLSITE), "1") #format=Yes
  clean_cache()
})


test_that("Read an archive with a bad name", {
  expect_warning(w <- read_trialmaster(filename_bad, use_cache=FALSE, verbose=0),
                 class="edc_tm_bad_name")
  expect_false(is.na(w$datetime_extraction))
  expect_false(is.na(w$date_extraction))
  expect_equal(as.character(w$site$INCLSITE), "Yes")
  clean_cache()
})


test_that("Use cache only if permitted", {
  w  = read_trialmaster(filename, use_cache="write", verbose=0)
  w2 = read_trialmaster(filename, use_cache="read", verbose=0) %>% expect_silent()
  w2 = read_trialmaster(filename, use_cache="read", verbose=0, clean_names_fun=tolower, split_mixed=TRUE) %>% 
    expect_error(class="read_tm_cache_bad_param")
})

