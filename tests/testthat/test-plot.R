

# edc_swimmerplot ---------------------------------------------------------------------------------

test_that("edc_swimmerplot", {
  e = edc_example_plot()
  load_list(e)
  
  p = edc_swimmerplot(.lookup, plotly=FALSE)
  p2 = edc_swimmerplot(.lookup, origin="db0$date_naissance", time_unit="months", plotly=FALSE)
  p3 = edc_swimmerplot(.lookup, group="db0$group", plotly=FALSE)
  p4 = edc_swimmerplot(.lookup, origin="db0$date_naissance", group="db0$group", plotly=FALSE)
  p5 = edc_swimmerplot(.lookup, aes_color="label", plotly=FALSE)
  
  expect_error(edc_swimmerplot(.lookup, origin="aaaaa", plotly=FALSE), 
               class="edc_swimplot_parse")
  expect_error(edc_swimmerplot(.lookup, origin="xxx$date_naissance", plotly=FALSE), 
               class="edc_swimplot_parse_dataset")
  expect_error(edc_swimmerplot(.lookup, origin="db0$date_xxxxxxxxx", plotly=FALSE), 
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

