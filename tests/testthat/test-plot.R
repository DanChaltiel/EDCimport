

# edc_swimmerplot ---------------------------------------------------------------------------------

test_that("edc_swimmerplot", {
  skip("SVG is painful")
  edc_options(edc_lookup_overwrite_warn=FALSE)

  e = edc_example()
  load_database(e)
  
  p = edc_swimmerplot(.lookup, plotly=FALSE)
  p2 = edc_swimmerplot(.lookup, origin="enrol$date_naissance", time_unit="months", plotly=FALSE)
  p3 = edc_swimmerplot(.lookup, group="enrol$group", plotly=FALSE)
  p4 = edc_swimmerplot(.lookup, origin="enrol$date_naissance", group="enrol$group", plotly=FALSE)
  p5 = edc_swimmerplot(.lookup, aes_color="label", plotly=FALSE)
  
  expect_error(edc_swimmerplot(.lookup, origin="aaaaa", plotly=FALSE), 
               class="edc_swimplot_parse")
  expect_error(edc_swimmerplot(.lookup, origin="xxx$date_naissance", plotly=FALSE), 
               class="edc_swimplot_parse_dataset")
  expect_error(edc_swimmerplot(.lookup, origin="enrol$date_xxxxxxxxx", plotly=FALSE), 
               class="edc_swimplot_parse_column")
  
  expect_error(edc_swimmerplot(.lookup, group="aaaaa", plotly=FALSE), 
               class="edc_swimplot_parse")
  expect_error(edc_swimmerplot(.lookup, group="db1$x", plotly=FALSE), 
               class="edc_swimplot_group_dup")
  
  skip_on_cran()
  skip_on_ci()
  vdiffr::expect_doppelganger("swimmerplot", p)
  vdiffr::expect_doppelganger("swimmerplot with origin", p2)
  vdiffr::expect_doppelganger("swimmerplot with group", p3)
  vdiffr::expect_doppelganger("swimmerplot with origin and group", p4)
  vdiffr::expect_doppelganger("swimmerplot with aes_color", p5)
})

