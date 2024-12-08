
skip_on_cran()
skip_on_ci()

test_that("edc_patient_gridplot", {
  set.seed(42)
  e = edc_example_plot() %>% 
    map(~{
      if(is.data.frame(.x) && !is.null(.x[["SUBJID"]])){
        included = sample(.x$SUBJID, size=0.8*n_distinct(.x$SUBJID))
        .x = .x %>% filter(.x$SUBJID %in% included) %>% 
          mutate(SUBJID=paste0("#", SUBJID))
      }
      .x
    })
  attach(e, warn.conflicts=FALSE)
  
  p0 = edc_patient_gridplot()
  p1 = edc_patient_gridplot(axes_flip=TRUE, sort_rows=FALSE, sort_cols=FALSE,
                            show_grid=FALSE, preprocess=~str_remove(.x, "\\D*"))
  
  
  vdiffr::expect_doppelganger("edc_patient_gridplot_default", p0)
  vdiffr::expect_doppelganger("edc_patient_gridplot_args", p1)
  
})
