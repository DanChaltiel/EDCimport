edc_options(edc_lookup_overwrite_warn=FALSE)

test_that("AE plots don't fail", {
  tm = edc_example_ae()

  ae_plot_grade_max(df_ae=tm$ae, df_enrol=tm$enrolres) %>% expect_silent()
  ae_plot_grade_max(df_ae=tm$ae, df_enrol=tm$enrolres, arm=NULL) %>% expect_silent()
  ae_plot_grade_n(df_ae=tm$ae, df_enrol=tm$enrolres) %>% expect_silent()
  ae_plot_grade_n(df_ae=tm$ae, df_enrol=tm$enrolres, arm=NULL) %>% expect_silent()

})

test_that("AE tables snapshots", {
  expect_snapshot({
    tm = edc_example_ae()
    ae_table_grade_max(df_ae=tm$ae, df_enrol=tm$enrolres)
    ae_table_grade_max(df_ae=tm$ae, df_enrol=tm$enrolres, arm=NULL)
    ae_table_grade_n(df_ae=tm$ae, df_enrol=tm$enrolres)
    ae_table_grade_n(df_ae=tm$ae, df_enrol=tm$enrolres, arm=NULL)
    ae_table_soc(df_ae=tm$ae, df_enrol=tm$enrolres, term=NULL)
    ae_table_soc(df_ae=tm$ae, df_enrol=tm$enrolres, term=NULL, arm=NULL)
  })
})

test_that("AE tables errors", {
  tm = edc_example_ae()
  ae_table_soc(df_ae=tm$ae, df_enrol=tm$enrolres,
                 arm="ARsM", term="AETEeRM", soc="AEtSOC", grade="AEGeR", subjid="SUBaJID") %>%
    expect_error(class="edc_ae_cols_notfound_error")
})

test_that("AE tables flextables", {
  tm = edc_example_ae()
  as_flextable = flextable::as_flextable
  ae_table_grade_max(df_ae=tm$ae, df_enrol=tm$enrolres) %>% as_flextable()
  ae_table_grade_max(df_ae=tm$ae, df_enrol=tm$enrolres, arm=NULL) %>% as_flextable()
  ae_table_grade_n(df_ae=tm$ae, df_enrol=tm$enrolres) %>% as_flextable()
  ae_table_grade_n(df_ae=tm$ae, df_enrol=tm$enrolres, arm=NULL) %>% as_flextable()
  ae_table_soc(df_ae=tm$ae, df_enrol=tm$enrolres, term=NULL) %>% as_flextable()
  ae_table_soc(df_ae=tm$ae, df_enrol=tm$enrolres, term=NULL, arm=NULL) %>% as_flextable()
  expect_true(TRUE)
})
