edc_options(edc_lookup_overwrite_warn=FALSE)



# Tables --------------------------------------------------------------------------------------

test_that("ae_table_grade() works", {
  expect_snapshot({
    tm = edc_example_ae()
    load_list(tm)
    
    ae_table_grade(ae, df_enrol=enrolres)
    ae_table_grade(ae, df_enrol=enrolres, arm="ARM")
    ae_table_grade(ae, df_enrol=enrolres, arm="ARM", variant = c("eq", "max"))
    ae_table_grade(ae, df_enrol=enrolres, arm="ARM", percent=FALSE, total=FALSE)
    
  })
})

test_that("ae_table_soc() works", {
  expect_snapshot({
    tm = edc_example_ae()
    load_list(tm)
    
    ae_table_soc(ae, df_enrol=enrolres)
    ae_table_soc(ae, df_enrol=enrolres, sort_by_count=FALSE)
    ae_table_soc(ae, df_enrol=enrolres, arm="ARM", digits=1)
    ae_table_soc(ae, df_enrol=enrolres, arm="ARM", showNA=FALSE, total=FALSE)
    ae_table_soc(ae, df_enrol=enrolres, arm="ARM", variant="sup")
    ae_table_soc(ae, df_enrol=enrolres, arm="ARM", variant="eq")
    
  })
  
  ae_table_soc(df_ae=ae, df_enrol=enrolres,
               arm="ARsM", term="AETEeRM", soc="AEtSOC", grade="AEGeR", subjid="SUBaJID") %>%
    expect_error(class="edc_name_notfound_error")
  
})

# Plots ---------------------------------------------------------------------------------------


test_that("ae_plot_grade() works", {
  expect_snapshot({
    tm = edc_example_ae()
    load_list(tm)
    
    p = ae_plot_grade(df_ae=ae, df_enrol=enrolres)
    vdiffr::expect_doppelganger("ae_plot_grade_1", p)
    p = ae_plot_grade(df_ae=ae, df_enrol=enrolres, arm="ARM", variant=c("sup", "max"))
    vdiffr::expect_doppelganger("ae_plot_grade_2", p)
    p = ae_plot_grade(df_ae=ae, df_enrol=enrolres, arm="ARM", type="absolute")
    vdiffr::expect_doppelganger("ae_plot_grade_3", p)
    p = ae_plot_grade(df_ae=ae, df_enrol=enrolres, arm="ARM", position="fill")
    vdiffr::expect_doppelganger("ae_plot_grade_4", p)
    p = ae_plot_grade(df_ae=ae, df_enrol=enrolres, arm="ARM", position="stack", type="absolute")
    vdiffr::expect_doppelganger("ae_plot_grade_5", p)
    
  })
})

test_that("ae_plot_grade_sum() works", {
  expect_snapshot({
    tm = edc_example_ae()
    load_list(tm)
    
    p = ae_plot_grade_sum(df_ae=ae, df_enrol=enrolres)
    vdiffr::expect_doppelganger("ae_plot_grade_sum_1", p)
    p = ae_plot_grade_sum(df_ae=ae, df_enrol=enrolres, arm="ARM")
    vdiffr::expect_doppelganger("ae_plot_grade_sum_2", p)
    p = ae_plot_grade_sum(df_ae=ae, df_enrol=enrolres, arm="ARM", weights=c(1,1,3,6,10))
    vdiffr::expect_doppelganger("ae_plot_grade_sum_3", p)
    
  })
  
  
})



test_that("butterfly_plot() works", {
  tm = edc_example_ae()
  load_list(tm)
  ae2 = ae %>% 
    mutate(serious = sae=="Yes", 
           bad_serious = sae=="foobar",)
  
  p = butterfly_plot(ae2, df_enrol=enrolres)
  vdiffr::expect_doppelganger("butterfly_plot_1", p)
  p = butterfly_plot(ae2, df_enrol=enrolres, severe="serious", sort_by="severe")
  vdiffr::expect_doppelganger("butterfly_plot_2", p)
  p = butterfly_plot(ae2, df_enrol=enrolres, range_min=1)
  vdiffr::expect_doppelganger("butterfly_plot_3", p)
  
  
  # Warnings
  ae2 %>% 
    butterfly_plot(df_enrol=enrolres, severe="bad_serious") %>% 
    expect_warning(class="edc_butterfly_serious_false_warning")
  
  # Errors
  ae %>% 
    butterfly_plot(df_enrol=enrolres, severe="sae") %>% 
    expect_error(class="edc_butterfly_serious_lgl_error")
  
  ae %>% 
    butterfly_plot(df_enrol=enrolres, arm="crfname") %>% 
    expect_error(class="edc_butterfly_two_arms_error")
  
})
