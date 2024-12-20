test_that("lastnews_table() works", {
  local_options(edc_lookup_overwrite_warn=FALSE)
  tm = edc_example()
  #add ties for patients 1 and 2
  tm$db2$date4[1:2] = tm$db3$date10[1:2]
  tm$db2$date5[1:2] = tm$db3$date10[1:2]
  load_list(tm)
  
  
  lnt0 = lastnews_table()
  
  #except dataset
  lnt1 = lastnews_table(except="db3")
  expect_true(!any(lnt1$origin_data=="db3"))
  
  #except column
  lnt2 = lastnews_table(except="db3$date9")
  lnt3 = lastnews_table(except="date10")
  expect_true(!any(lnt2$origin_col=="date9"))
  expect_true(!any(lnt3$origin_col=="date10"))
  
  #prefer dataset
  lnt4 = lastnews_table(prefer="db3")
  expect_setequal(lnt4$origin_data, "db3")
  
  #prefer column
  lnt5 = lastnews_table(prefer="date5")
  expect_true(!any(lnt5$origin_col=="date4"))
  
  #allow ties
  lnt6 = lastnews_table(with_ties=TRUE) %>% 
    filter(n()>1, .by=subjid)
  expect_setequal(lnt6$subjid, 1:2)
  
  #snapshot for the warning and csv output
  expect_snapshot({
    tm = edc_example()
    tm$datetime_extraction = as.POSIXct("2010-08-10 18:58:36 GMT")
    attr(edcimport_env$lookup, "datetime_extraction") = tm$datetime_extraction
    load_list(tm)
    csv_file = tempfile(fileext=".csv")
    lastnews_table(warn_if_future=csv_file) %>% head(10)
    x = read.csv2(csv_file)
    x
  })
})
