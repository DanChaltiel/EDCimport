



skip_on_cran()
edc_options(edc_lookup_overwrite_warn=FALSE)

if(FALSE){
  cachename = test_path("trialmaster_export_2022-08-25 15h16.rds")
  filename = test_path("CRF_Dan_Export_SAS_XPORT_2022_08_25_15_16.zip")
  filename_noformat = test_path("CRF_Dan_Export_SAS_XPORT_2022_08_25_15_16_noformat.zip")
  filename_bad = test_path("CRF_Dan_Export.zip")
}


test_that("Read TM with cache", {
  clean_cache()
  clean_lookup()
  edc_options(edc_lookup_overwrite_warn=TRUE, .local=TRUE)
  
  #first read, read from zip (default: use_cache=write)
  w = read_trialmaster(filename) %>% 
    expect_classed_conditions(message_class=c("read_tm_zip", "edc_create_cache"))
  
  #2nd, use_cache=TRUE -> read from cache
  w = read_trialmaster(filename, use_cache=TRUE) %>%
    expect_classed_conditions(message_class="read_tm_cache",
                              warning_class="edc_lookup_overwrite_warn")
  
  #3rd, use_cache=write -> read from zip again
  w = read_trialmaster(filename, use_cache="write") %>%
    expect_classed_conditions(message_class=c("read_tm_zip", "edc_create_cache"),
                              warning_class="edc_lookup_overwrite_warn")
  
  #4th, use_cache=read -> read from cache again
  w = read_trialmaster(filename, use_cache="read") %>%
    expect_classed_conditions(message_class="read_tm_cache",
                              warning_class="edc_lookup_overwrite_warn")
  
  #5th, use_cache=F -> read from zip again again
  w = read_trialmaster(filename, use_cache=FALSE) %>%
    expect_classed_conditions(message_class="read_tm_zip",
                              warning_class="edc_lookup_overwrite_warn")
  
  
  expect_length(w, 8)


  load_database(w, remove=FALSE)
  expect_true(exists("pat"))
  expect_length(pat, 35)
  expect_true(nrow(trial)==1)
  expect_true(names(w) %>% map_lgl(exists, envir=current_env()) %>% all())

  expect_s3_class(datetime_extraction, "POSIXct")

  #expect formats
  expect_s3_class(site$INCLSITE, "factor")
  expect_equal(as.character(site$INCLSITE), "Yes")
  expect_equal(dim(.lookup), c(5,9))
  clean_cache()
})



test_that("Read TM without procformat", {
  expect_warning(w <- read_trialmaster(filename_noformat, use_cache=FALSE, verbose=0),
                 class="edc_tm_no_procformat_warning")
  expect_equal(as.character(w$site$INCLSITE), "1") #format=Yes
  clean_cache()
})


test_that("Read TM with a bad name", {
  w = read_trialmaster(filename_bad, use_cache=FALSE, verbose=0) %>% 
    expect_classed_conditions(warning_class=c("edc_tm_bad_name", 
                                              "get_folder_datetime_warning"))
  expect_false(is.na(w$datetime_extraction))
  expect_false(is.na(w$date_extraction))
  expect_equal(as.character(w$site$INCLSITE), "Yes")
  clean_cache()
})

test_that("Read TM with an internal structure", {
  local_options(edc_lookup_overwrite_warn=FALSE)
  tm = read_trialmaster(test_path("CRF_Dan_Export_SAS_XPORT_2022_08_25_15_16_subdir.zip"),
                        use_cache=FALSE, verbose=0, subdirectories=FALSE)
  tm2 = read_trialmaster(test_path("CRF_Dan_Export_SAS_XPORT_2022_08_25_15_16_subdir.zip"),
                         use_cache=FALSE, verbose=0, subdirectories=TRUE)
  expect_contains(names(tm), c("vs", "visit"))
  expect_contains(names(tm2), c("vs", "visit", "sub_vs"))
  expect_false(any(names(tm) == "sub_vs"))
  clean_cache()
})


test_that("Use cache only if permitted", {
  clean_cache()
  clean_lookup()
  w  = read_trialmaster(filename, use_cache="write", verbose=0)
  
  f = function(df) dplyr::rename_with(df, tolower)
  clean_lookup()
  w2 = read_trialmaster(filename, use_cache="read", verbose=0) %>% expect_silent()
  w2 = read_trialmaster(filename, use_cache="read", verbose=0, 
                        clean_names_fun=f) %>% 
    expect_error(class="read_tm_cache_bad_param")
  clean_cache()
})

