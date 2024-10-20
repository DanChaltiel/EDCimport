
#run manually to copy example to csv files
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

  write.csv2(data_labels, "tests/testthat/csv/labels.csv")
}


test_that("multiplication works", {
  
  
  path = test_path("csv/")
  
  a = read_all_csv(path, label_dict="labels.csv", verbose=0)
  
  expect_s3_class(a$datetime_extraction, "POSIXlt")
  expect_type(a$date_extraction, "character")
  
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
  
  # 
  # get_label(a$db0)
  # get_label(a$db1)
  # a$db0
  # 
  # file_exists(lab)
  # 
  # 
  # # a = files %>% set_names(~str_remove(basename(.x), "\\..*?$")) %>% map(read.csv2) %>%
  # #   map(~apply_label_lookup(.x, data_labels))
  # a$db0 %>% get_label()
  # 
  # a %>% 
  #   keep_at(~str_detect(.x, "db")) %>% 
  #   map(get_label)
  # 
  # a = tempfile()
  # fs::file_touch(a)
  # file.info(a)$mtime %>% round("secs") %>% dput
  # fs::file_info(a)$modification_time %>% dput
})



