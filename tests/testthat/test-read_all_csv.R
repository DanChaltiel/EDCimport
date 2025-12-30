
# Utils ---------------------------------------------------------------------------------------

#' Helper that copy edc_example() to csv files
example_to_csv = function(dir_path){
  clean_lookup()
  dir_create(path(dir_path, "external"), recurse=TRUE)
  
  data_labels = edc_example() %>% 
    keep_at(~str_detect(.x, "data")) %>%
    imap(~{
      write.csv2(.x, glue("{dir_path}/{.y}.csv"), row.names=FALSE)
      .x
    }) %>%  
    map(~get_label(.x, default=NA)) %>% unlist() %>% 
    set_names(~str_remove(.x, "data\\d\\.")) %>%
    .[!duplicated(.)] %>% 
    tibble::enframe(name="name", value="label") %>% 
    filter(!is.na(label)) %>%
    distinct()
  
  write.csv2(data_labels, glue("{dir_path}/labels.csv"), row.names=FALSE)
  write.csv2(data_labels, glue("{dir_path}/external/external_labels.csv"), row.names=FALSE)
}


# Test ----------------------------------------------------------------------------------------

test_that("read_all_csv() works", {
  local_options(edc_lookup_overwrite_warn=FALSE)
  
  path = path_temp("csv")
  unlink(path)
  example_to_csv(path)
  
  #Read from files
  db = read_all_csv(path, labels_from="labels.csv", verbose=0)
  expect_edc_database(db)
  expect_identical(attr(db, "source"), "files")
  expect_null(db$labels) #labels.csv is not a dataset
  
  #Read from cache
  db2 = read_all_csv(path, format_file=NULL, verbose=0, use_cache=TRUE)
  expect_edc_database(db2)
  expect_identical(attr(db2, "source"), "cache")
  expect_setequal(c(names(db), "labels"), names(db2))
  
  expect_snapshot({
    db %>% 
      keep_at(~str_detect(.x, "data")) %>% 
      map(head)
  })
  
  expect_snapshot({
    db %>% 
      keep_at(~str_detect(.x, "data")) %>% 
      map(get_label)
  })
  
  #read_all_csv() works with external labels
  clean_lookup()
  expect_snapshot({
    db = read_all_csv(path, labels_from="external/external_labels.csv", verbose=0)
    head(db$labels) #not NULL, considered as a normal dataset
    db %>% 
      keep_at(~str_detect(.x, "data")) %>% 
      map(get_label)
  })
  
  unlink(path)
})




