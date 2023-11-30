
skip_on_cran()


# Zip generation ------------------------------------------------------------------------------

#copy edc_example_mixed() to a dirname. Need to manually turn it to zip for now.
if(FALSE){
  tm = edc_example_mixed()
  path = test_path("edc_example_mixed_SAS_XPORT_2000_01_01_00_00")
  for(i in names(tm)){
    x = tm[[i]]
    if(!is.data.frame(x) || i==".lookup") next
    print(i)
    filename = file.path2(path, i, ext=".xpt")
    haven::write_xpt(tm[[i]], filename)
  }
  # archive::archive_write_files()
}




# Actual test ---------------------------------------------------------------------------------



test_that("Split mixed outside read_trialmaster()", {
  tm = edc_example_mixed()
  mixed_data = split_mixed_datasets(tm, id="SUBJID", verbose=FALSE)
  mixed_data %>% names() %>% expect_equal(c("long_mixed_short", "long_mixed_long" ))
  mixed_data %>% map_dbl(nrow) %>% unname() %>% expect_equal(c(100, 200))
  mixed_data %>% map_dbl(ncol) %>% unname() %>% expect_equal(c(3, 3))
})


test_that("Split mixed inside read_trialmaster()", {
  
  edc_options(edc_read_verbose=0, edc_lookup=NULL, .local=TRUE)
  common = c("date_extraction", "datetime_extraction", ".lookup")
  f = test_path("edc_example_mixed_SAS_XPORT_2000_01_01_00_00.zip")
  tm1 = read_trialmaster(f, pw="foobar")
  # names(tm1) %>% dput()
  expect_equal(names(tm1), c("long_mixed", "long_pure", "short", common))
  
  tm2 = read_trialmaster(f, pw="foobar", split_mixed="short") %>% 
    expect_classed_conditions(warning_class="edc_read_cannot_split_mixed_warn")#no effect
  # names(tm2) %>% dput()
  expect_equal(names(tm2), c("long_mixed", "long_pure", "short", common))
  
  tm3 = read_trialmaster(f, pw="foobar", split_mixed=c("long_pure", "long_mixed"))
  # names(tm3) %>% dput()
  expect_equal(names(tm3), c("long_mixed", "long_pure", "short", "long_mixed_short", 
                            "long_mixed_long", common))
})
