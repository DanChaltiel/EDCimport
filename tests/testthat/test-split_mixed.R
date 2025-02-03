
skip_on_cran()
edc_options(edc_lookup_overwrite_warn=FALSE)


test_that("Split mixed", {
  db = edc_example(N=100) 
  
  #all datasets
  mixed_data_all = edc_split_mixed(db, verbose=FALSE)
  expect_equal(edc_lookup(dataset)$dataset,
               c("ae", "ae_long", "ae_short", "db1", "db2", "db3", "enrol", "long_mixed",
                 "long_mixed_long", "long_mixed_short", "long_pure", "short"))
  
  #tidyselection
  mixed_data = edc_split_mixed(db, c(short, "enrol", starts_with("long")), verbose=FALSE)
  
  expect_equal(edc_lookup(dataset)$dataset,
               c("ae", "db1", "db2", "db3", "enrol", "long_mixed", "long_mixed_long", 
                 "long_mixed_short", "long_pure", "short"))
  expect_equal(nrow(mixed_data$long_mixed_short), 100)
  expect_equal(nrow(mixed_data$long_mixed_long), 200)
  expect_equal(ncol(mixed_data$long_mixed_short), 3)
  expect_equal(ncol(mixed_data$long_mixed_long), 3)
  
  #content
  expect_true(all(c("long_mixed_short", "long_mixed_long") %in% names(mixed_data)))
  expect_true(!any(c("ae_short", "long_pure_short", "short_short") %in% names(mixed_data)))
  
  expect_equal(nrow(mixed_data$long_mixed_short), 100)
  expect_equal(nrow(mixed_data$long_mixed_long), 200)
  expect_equal(ncol(mixed_data$long_mixed_short), 3)
  expect_equal(ncol(mixed_data$long_mixed_long), 3)
  
})
