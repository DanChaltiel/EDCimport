
#' Helper that copy edc_example() to csv files (run manually)
example_to_csv = function(){
  a = edc_example() %>% 
    keep_at(~str_detect(.x, "db"))
  
  a %>%
    imap(~write.csv2(.x, glue("tests/testthat/csv/{.y}.csv"), row.names=FALSE))

  data_labels = a %>%  
    map(~get_label(.x, default=NA)) %>% unlist() %>% 
    set_names(~str_remove(.x, "db\\d\\.")) %>%
    .[!duplicated(.)] %>% 
    tibble::enframe(name="name", value="label") %>% 
    filter(!is.na(label)) %>%
    distinct()

  write.csv2(data_labels, "tests/testthat/csv/labels.csv", row.names=FALSE)
  write.csv2(data_labels, "tests/testthat/csv/external/external_labels.csv", row.names=FALSE)
}


test_that("read_all_csv() works", {
  clean_lookup()
  path = test_path("csv/")
  
  a = read_all_csv(path, labels_from="labels.csv", verbose=0)
  
  expect_s3_class(a$datetime_extraction, "POSIXlt")
  expect_type(a$date_extraction, "character")
  expect_null(a$labels)
  
  expect_snapshot({
    a %>% 
      keep_at(~str_detect(.x, "db")) %>% 
      map(head)
  })
  
  expect_snapshot({
    a %>% 
      keep_at(~str_detect(.x, "db")) %>% 
      map(get_label)
  })
  
})

test_that("read_all_csv() works with external labels", {
  clean_lookup()
  path = test_path("csv/")
  
  expect_snapshot({
    a = read_all_csv(path, labels_from="external/external_labels.csv", verbose=0)
    head(a$labels) #not NULL, considered as a normal dataset
    a %>% 
      keep_at(~str_detect(.x, "db")) %>% 
      map(get_label)
  })
  
})



