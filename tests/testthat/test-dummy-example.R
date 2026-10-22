test_that("dummy pipeline runs on the packaged EDC example", {
  db = suppressWarnings(edc_example(N = 10, seed = 1))
  spec = edc_dummy_spec(db)
  file = tempfile(fileext = ".csv")
  on.exit(unlink(file), add = TRUE)

  write.csv(spec, file, row.names = FALSE)
  spec = read.csv(file)
  dummy = edc_dummy_database(spec, seed = 1)

  expect_s3_class(dummy, "edc_dummy")
  expect_s3_class(dummy, "edc_database")
  expect_identical(names(Filter(is.data.frame, dummy))[-length(Filter(is.data.frame, dummy))],
                   names(Filter(is.data.frame, db))[-length(Filter(is.data.frame, db))])
})
