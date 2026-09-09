dummy_test_database = function(){
  n = 12L
  subjects = data.frame(
    SUBJID = sprintf("REAL%03d", seq_len(n)),
    FAC = factor(rep(c("A", "B"), length.out = n), levels = c("A", "B")),
    ORD = ordered(rep(c("low", "mid", "high"), length.out = n), levels = c("low", "mid", "high")),
    CAT = rep(c("Ctl", "Trt"), length.out = n),
    FLAG = c(TRUE, FALSE, NA, rep(c(TRUE, FALSE), length.out = n - 3L)),
    COUNT = c(seq_len(n - 1L), NA_integer_),
    VALUE = c(seq(1.5, 11.5, length.out = n - 1L), NA_real_),
    CONST = rep(7.5, n),
    ALLNA = rep(NA_integer_, n),
    COMMENT = paste("patient secret", seq_len(n)),
    WHEN = as.Date("2025-01-01") + c(seq_len(n - 1L) - 1L, NA_integer_),
    stringsAsFactors = FALSE
  )
  attr(subjects, "label") = "Subjects"
  attr(subjects$SUBJID, "label") = "Subject identifier"
  attr(subjects$VALUE, "label") = "Continuous value"

  visits = data.frame(
    SUBJID = rep(subjects$SUBJID, each = 2),
    VISIT = rep(1:2, times = n),
    SCORE = seq_len(2L * n) / 10,
    NOTE = paste("visit secret", seq_len(2L * n)),
    stringsAsFactors = FALSE
  )
  attr(visits, "label") = "Visits"
  attr(visits$SCORE, "label") = "Visit score"

  db = list(
    subjects = subjects,
    visits = visits,
    datetime_extraction = as.POSIXct("2025-12-31 10:00:00", tz = "UTC"),
    date_extraction = "2025-12-31",
    .lookup = data.frame()
  )
  class(db) = "edc_database"
  db
}


test_that("edc_dummy_spec returns the portable public format", {
  db = dummy_test_database()
  spec = edc_dummy_spec(db)

  expect_s3_class(spec, "data.frame")
  expect_identical(
    names(spec),
    c(
      "dataset", "dataset_label", "n_rows", "n_subjects", "column",
      "column_label", "class", "generator", "depends_on", "param1", "param2",
      "param3", "missing_prop"
    )
  )
  expect_identical(unique(spec$dataset), c("subjects", "visits"))
  expect_identical(spec$column[spec$dataset == "subjects"], names(db$subjects))
  expect_identical(spec$generator[spec$column == "SUBJID"], c("identifier", "identifier"))
  expect_identical(spec$generator[spec$column == "COMMENT"], "text")
  expect_identical(spec$generator[spec$column == "WHEN"], "date")

  spec_values = paste(unlist(spec, use.names = FALSE), collapse = " ")
  expect_false(grepl("REAL001", spec_values, fixed = TRUE))
  expect_false(grepl("patient secret", spec_values, fixed = TRUE))
  expect_false(grepl("2025-01-01", spec_values, fixed = TRUE))
})


test_that("dummy specification survives a CSV round-trip", {
  db = dummy_test_database()
  spec1 = edc_dummy_spec(db)
  file = tempfile(fileext = ".csv")
  on.exit(unlink(file), add = TRUE)

  write.csv(spec1, file, row.names = FALSE)
  spec2 = read.csv(file)
  dummy = edc_dummy_database(spec2, seed = 42)

  expect_s3_class(dummy, "edc_dummy")
  expect_s3_class(dummy, "edc_database")
  expect_identical(names(dummy$subjects), names(db$subjects))
  expect_identical(names(dummy$visits), names(db$visits))
  expect_equal(nrow(dummy$subjects), nrow(db$subjects))
  expect_equal(nrow(dummy$visits), nrow(db$visits))
})


test_that("dummy database reconstructs basic types, labels and metadata", {
  db = dummy_test_database()
  dummy = db %>% edc_dummy_spec() %>% edc_dummy_database(seed = 17)

  expect_true(is.factor(dummy$subjects$FAC))
  expect_true(is.ordered(dummy$subjects$ORD))
  expect_type(dummy$subjects$CAT, "character")
  expect_type(dummy$subjects$FLAG, "logical")
  expect_type(dummy$subjects$COUNT, "integer")
  expect_type(dummy$subjects$VALUE, "double")
  expect_type(dummy$subjects$CONST, "double")
  expect_type(dummy$subjects$ALLNA, "integer")
  expect_s3_class(dummy$subjects$WHEN, "Date")

  expect_identical(attr(dummy$subjects, "label"), "Subjects")
  expect_identical(attr(dummy$subjects$VALUE, "label"), "Continuous value")
  expect_identical(attr(dummy$visits$SCORE, "label"), "Visit score")
  expect_true(all(is.na(dummy$subjects$ALLNA)))
  expect_true(anyNA(dummy$subjects$FLAG))
  expect_true(is.data.frame(dummy$.lookup))
  expect_identical(dummy$date_extraction, "2000-01-01")
  expect_equal(as.Date(dummy$datetime_extraction), as.Date("2000-01-01"))
})


test_that("subject identifiers are artificial and coherent across datasets", {
  db = dummy_test_database()
  dummy = db %>% edc_dummy_spec() %>% edc_dummy_database(seed = 1)

  expect_true(all(grepl("^DUMMY_SUBJECT_", dummy$subjects$SUBJID)))
  expect_length(intersect(db$subjects$SUBJID, dummy$subjects$SUBJID), 0)
  expect_setequal(unique(dummy$subjects$SUBJID), unique(dummy$visits$SUBJID))
})


test_that("real free text and calendar dates are not reproduced", {
  db = dummy_test_database()
  dummy = db %>% edc_dummy_spec() %>% edc_dummy_database(seed = 9)

  text_values = c(dummy$subjects$COMMENT, dummy$visits$NOTE)
  expect_false(any(grepl("patient secret", text_values, fixed = TRUE)))
  expect_false(any(grepl("visit secret", text_values, fixed = TRUE)))

  real_dates = db$subjects$WHEN[!is.na(db$subjects$WHEN)]
  dummy_dates = dummy$subjects$WHEN[!is.na(dummy$subjects$WHEN)]
  expect_length(intersect(real_dates, dummy_dates), 0)
})


test_that("generation is reproducible and preserves the caller RNG state", {
  spec = dummy_test_database() %>% edc_dummy_spec()
  dummy1 = edc_dummy_database(spec, seed = 123)
  dummy2 = edc_dummy_database(spec, seed = 123)
  expect_identical(dummy1, dummy2)

  set.seed(456)
  rng_before = .Random.seed
  edc_dummy_database(spec, seed = 789)
  expect_identical(.Random.seed, rng_before)
})
