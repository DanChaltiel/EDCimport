
#' Helper that copy edc_example() to csv files (run manually)
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

test_that("read_all_csv() works", {
  path = path_temp("csv")
  example_to_csv(path)
  clean_lookup()
  
  a = read_all_csv(path, labels_from="labels.csv", verbose=0)
  
  expect_s3_class(a, "edc_database")
  expect_s3_class(a$datetime_extraction, "POSIXlt")
  expect_type(a$date_extraction, "character")
  expect_null(a$labels)
  
  expect_snapshot({
    a %>% 
      keep_at(~str_detect(.x, "data")) %>% 
      map(head)
  })
  
  expect_snapshot({
    a %>% 
      keep_at(~str_detect(.x, "data")) %>% 
      map(get_label)
  })
  
  #read_all_csv() works with external labels
  clean_lookup()
  expect_snapshot({
    a = read_all_csv(path, labels_from="external/external_labels.csv", verbose=0)
    head(a$labels) #not NULL, considered as a normal dataset
    a %>% 
      keep_at(~str_detect(.x, "data")) %>% 
      map(get_label)
  })
  
  unlink(path)
})


