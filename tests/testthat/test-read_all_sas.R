
# Utils ---------------------------------------------------------------------------------------

example_to_sas7bdat = function(dir_path){
  dir_create(dir_path)
  db = edc_example()
  db %>% 
    keep(is_dataset) %>% 
    iwalk(~{
      filename = path(dir_path, .y, ext="sas7bdat")
      suppressWarnings( #deprecated
        haven::write_sas(.x, filename)
      )
    })
  return(TRUE)
}


# Test ----------------------------------------------------------------------------------------

#TODO add a format file?

test_that("read_all_sas() works", {
  local_options(edc_lookup_overwrite_warn=FALSE)

  path = path_temp("edc_example_sasbdat")
  unlink(path)
  example_to_sas7bdat(path)
  
  nms = c("ae", "data1", "data2", "data3", "enrol", "long_mixed", "long_pure", 
          "short", "datetime_extraction", "date_extraction", ".lookup")
  
  #Read from files
  db1  = read_all_sas(path, format_file=NULL, verbose=0)
  expect_edc_database(db1, datasets=nms)
  expect_identical(attr(db1, "source"), "files")
  
  #Read from cache
  db2 = read_all_sas(path, format_file=NULL, verbose=0, use_cache=TRUE)
  expect_edc_database(db2, datasets=nms)
  expect_identical(attr(db2, "source"), "cache")
  expect_identical(names(db2), names(db1))
  
  #errors
  read_all_sas(path, format_file="notfound.sas7bdat", verbose=FALSE) %>% 
    expect_error(class="edc_404_file_not_found")
  
  #cleaning
  unlink(path)
})
