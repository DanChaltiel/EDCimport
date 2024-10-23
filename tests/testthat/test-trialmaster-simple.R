
skip_on_cran()
edc_options(edc_lookup_overwrite_warn=FALSE)

test_that("Read a TM archive", {
  clean_cache()
  clean_lookup()
  
  f = function(x) {
    x %>% 
      str_replace_all("^7-Zip.*$", "7-Zip Copyright 1999-2016 Igor Pavlov") %>%  #7-Zip version
      str_replace_all(",.*Kb", ", 000 Kb") %>%  #database size
      str_replace_all("v(\\d+\\.?)+", "v0.0.0") #EDCimport version
  }
  expect_snapshot(transform=f, {
    #read
    filename = test_path("CRF_Dan_Export_SAS_XPORT_2022_08_25_15_16.zip")
    w = read_trialmaster(filename, use_cache="write", verbose=9)
    #print helpers
    w$datetime_extraction
    w$.lookup
    
    lu = edc_lookup()
    expect_identical(lu, w$.lookup)
    
    df_list = w %>% 
      keep(is.data.frame) %>% 
      discard_at(".lookup")
    
    #print all **labels**
    df_list %>% 
      map(~{
        .x %>% get_label() %>% unlist() %>% tibble::enframe()
      })
    
    #print all **levels**
    df_list %>% 
      map(~{
        .x %>% select(where(is.factor)) %>% map(~head(levels(.x), 3))
      })
  })
  
  clean_cache()
})
