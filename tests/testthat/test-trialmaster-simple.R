
skip_on_cran()
edc_options(edc_lookup_overwrite_warn=FALSE)
# cachename = test_path("trialmaster_export_2022-08-25 15h16.rds")
# filename = test_path("CRF_Dan_Export_SAS_XPORT_2022_08_25_15_16.zip")
# filename_noformat = test_path("CRF_Dan_Export_SAS_XPORT_2022_08_25_15_16_noformat.zip")
# filename_bad = test_path("CRF_Dan_Export.zip")

test_that("Read a TM archive", {
  clean_cache()
  clean_lookup()
  
  expect_snapshot({
    #read
    w = read_trialmaster(filename, use_cache="write", verbose=9)
    #print helpers
    w$datetime_extraction
    w$.lookup
    
    df_list = w %>% 
      keep(is.data.frame) %>% 
      discard_at(".lookup")
    
    #print all labels
    df_list %>% 
      map(~{
        .x %>% get_label() %>% unlist() %>% tibble::enframe()
      })
    
    #print all levels
    df_list %>% map(~{
      .x %>% select(where(is.factor)) %>% map(~head(levels(.x), 3))
    })
  })
  
  clean_cache()
})
