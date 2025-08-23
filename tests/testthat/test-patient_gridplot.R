
skip_on_cran()
skip_on_ci()

test_that("edc_patient_gridplot", {
  set.seed(42)
  clean_lookup()
  local_options(edc_lookup_overwrite_warn=FALSE)
  
  select_patients = function(db, p=0.8){
    db %>% 
      map(~{
        if(is.data.frame(.x) && !is.null(.x[["subjid"]])){
          included = sample(.x$subjid, size=p*n_distinct(.x$subjid))
          .x = .x %>% filter(.x$subjid %in% included) %>% 
            mutate(subjid=paste0("#", subjid))
        }
        .x
      }) %>% 
      add_class("edc_database")
  }
  
  e = edc_example()%>% 
    select_patients(p=0.8)
  
  # attach(e, warn.conflicts=FALSE)
  load_database(e)
  
  p0 = edc_patient_gridplot()
  p1 = edc_patient_gridplot(axes_flip=TRUE, sort_rows=FALSE, sort_cols=FALSE,
                            show_grid=FALSE, preprocess=~str_remove(.x, "\\D*"))
  p2 = edc_patient_gridplot(gradient=TRUE)
  
  
  vdiffr::expect_doppelganger("edc_patient_gridplot_default", p0)
  vdiffr::expect_doppelganger("edc_patient_gridplot_args", p1)
  vdiffr::expect_doppelganger("edc_patient_gridplot_gradient", p2)
  
})
