
lastnews_example = function(outdated=FALSE){
  local_options(edc_lookup_overwrite_warn=FALSE)
  edc_example(outdated=outdated)
}


test_that("lastnews_table() default", {
  db = lastnews_example()
  load_database(db)
  
  lnt0 = lastnews_table()
  lnt0b = lastnews_table(regex=TRUE)
  expect_identical(lnt0, lnt0b)
  expect_equal(lnt0$origin_col[1:3], c("date4", "date4", "date5"))
})


test_that("lastnews_table() except", {
  db = lastnews_example()
  load_database(db)
  
  #without regex
  lnt1 = lastnews_table(except=c("data3", "data2$date4"))
  lnt1b = lastnews_table(except=c("data3", "date4"), regex=TRUE)
  expect_identical(lnt1, lnt1b)
  expect_true(!any(lnt1$origin_data=="data3"))
  expect_true(!any(lnt1$origin_col=="date4"))
  
  #with regex
  lnt2 = lastnews_table(except=c("2", "d..e\\d$"), regex=TRUE)
  expect_true(!any(lnt2$origin_data=="data2"))
  expect_true(all(lnt2$origin_col=="date10"))
})


test_that("lastnews_table() prefer", {
  db = lastnews_example()
  load_database(db)
  
  #without regex
  lnt3 = lastnews_table(prefer=c("data2$date5", "data3"))
  lnt3b = lastnews_table(prefer=c("date5", "data3"), regex=TRUE)
  expect_identical(lnt3, lnt3b, ignore_attr=TRUE)
  expect_equal(lnt3$origin_col[1:3], c("date10", "date5", "date5"))
  
  #with regex
  lnt4 = lastnews_table(prefer=c("xxxx", "date\\d\\d"), regex=TRUE)
  expect_setequal(lnt4$origin_col[1:3], c("date10"))
  
  #show_delta
  lnt5 = lastnews_table(prefer=c("data2$date5"), show_delta=TRUE)
  expect_contains(names(lnt5), c("preferred_last_date", "preferred_origin", "delta"))
  expect_false(any(is.infinite(lnt5$delta) | is.na(lnt5$delta)))
})


test_that("lastnews_table() with ties", {
  db = lastnews_example()
  load_database(db)
  lnt6 = lastnews_table(with_ties=TRUE) %>% 
    filter(n()>1, .by=subjid)
  expect_setequal(lnt6$subjid, 1:3)
})


test_that("lastnews_table() snapshot", {
  #snapshot for the default, with warning and csv output
  expect_snapshot({
    db = lastnews_example(outdated=TRUE)
    load_database(db)
    csv_file = tempfile(fileext=".csv")
    lastnews_table(warn_if_future=csv_file) %>% head(10)
    x = read.csv2(csv_file)
    x
  })
})


test_that("lastnews_table() error", {
  db = lastnews_example()
  load_database(db)
  lastnews_table(except=c("enrol", "\\d"), regex=TRUE) %>% 
    expect_error(class="edc_no_columns_error")
})


test_that("lastnews_table() error", {
  db = lastnews_example()
  load_database(db)
  lnt6 = lastnews_table(show_delta=TRUE)  %>% 
    expect_classed_conditions(warning_class="edc_no_preferred_column_warning")
  expect_true(!any(c("preferred_last_date", "preferred_origin", "delta") %in% names(lnt6)))
})
