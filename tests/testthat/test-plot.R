
skip_on_cran()
# swimmerplot ---------------------------------------------------------------------------------


test_that("swimmerplot", {
  e = edc_example_plot()
  load_list(e)
  
  p = swimmerplot(.lookup, plotly=FALSE)
  vdiffr::expect_doppelganger("swimmerplot", p)
  
  p2 = swimmerplot(.lookup, origin="db0$date_naissance", plotly=FALSE)
  vdiffr::expect_doppelganger("swimmerplot with origin", p2)
  
  p3 = swimmerplot(.lookup, group="db0$group", plotly=FALSE)
  vdiffr::expect_doppelganger("swimmerplot with group", p3)
  
  
  expect_error(swimmerplot(.lookup, origin="aaaaa", plotly=FALSE), 
               class="edc_swimplot_parse")
  expect_error(swimmerplot(.lookup, origin="xxx$date_naissance", plotly=FALSE), 
               class="edc_swimplot_parse_dataset")
  expect_error(swimmerplot(.lookup, origin="db0$date_xxxxxxxxx", plotly=FALSE), 
               class="edc_swimplot_parse_column")
  
  expect_error(swimmerplot(.lookup, group="aaaaa", plotly=FALSE), 
               class="edc_swimplot_parse")
  expect_error(swimmerplot(.lookup, group="db1$x", plotly=FALSE), 
               class="edc_swimplot_group_dup")
})

