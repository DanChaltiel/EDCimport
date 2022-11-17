

skip_on_cran()

test_that("Read an archive", {
  clean_cache()

  expect_message(w <- read_trialmaster(filename),
                 class="read_tm_zip")
  expect_message(w <- read_trialmaster(filename),
                 class="read_tm_cache")
  expect_message(w <- read_trialmaster(filename, use_cache=FALSE),
                 class="read_tm_zip")
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
  expect_equal(dim(.lookup), c(5,5))
  clean_cache()
})



test_that("Read an archive without procformat", {
  expect_warning(w <- read_trialmaster(filename_noformat, use_cache=FALSE, verbose=0),
                 class="edc_tm_no_procformat")
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
