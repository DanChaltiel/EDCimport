
skip_on_cran()
edc_options(edc_lookup_overwrite_warn=FALSE)

test_that("Read a TM archive", {
  clean_cache()
  clean_lookup()
  
  f = function(x) str_remove(x, ",.*Kb") #dont snapshot the database size
  
  expect_snapshot(transform=f, {
    #read
    filename = test_path("CRF_Dan_Export_SAS_XPORT_2022_08_25_15_16.zip")
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
