

# edc_swimmerplot ---------------------------------------------------------------------------------

test_that("edc_swimmerplot", {
  skip_on_ci()
  skip_on_cran()
  edc_options(edc_lookup_overwrite_warn=FALSE)

  e = edc_example() %>% 
    edc_unify_subjid(preprocess=~paste0("#", .x))
  load_database(e)
  
  p = edc_swimmerplot()
  p2 = edc_swimmerplot(origin="enrol$enrol_date", time_unit="months", 
                       include="data2", exclude="date\\d\\d", id_sort=TRUE)
  p3 = edc_swimmerplot(group="enrol$arm", id_subset=1:10, id_sort=TRUE)
  p4 = edc_swimmerplot(origin="enrol$enrol_date", group="enrol$arm")
  p5 = edc_swimmerplot(aes_color="label", id_subset=enrol$subjid[1:5])
  
  vdiffr::expect_doppelganger("swimmerplot", p)
  vdiffr::expect_doppelganger("swimmerplot-origin-include-exclude-id_sort", p2)
  vdiffr::expect_doppelganger("swimmerplot-group-id_subset-id_sort", p3)
  vdiffr::expect_doppelganger("swimmerplot-origin-group", p4)
  vdiffr::expect_doppelganger("swimmerplot-aes_color-id_subset_chr", p5)
  
  
  expect_error(edc_swimmerplot(origin="aaaaa"), 
               class="edc_swimplot_parse")
  expect_error(edc_swimmerplot(origin="xxx$enrol_date"), 
               class="edc_swimplot_parse_dataset")
  expect_error(edc_swimmerplot(origin="enrol$date_xxxxxxxxx"), 
               class="edc_swimplot_parse_column")
  expect_error(edc_swimmerplot(group="aaaaa"), 
               class="edc_swimplot_parse")
  expect_error(edc_swimmerplot(group="data1$x"), 
               class="edc_swimplot_group_dup")
})

