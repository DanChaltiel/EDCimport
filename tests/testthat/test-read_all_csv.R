
# Utils ---------------------------------------------------------------------------------------

#' Helper that copy edc_example() to csv files
example_to_csv = function(dir_path, csv2=TRUE){
  clean_lookup()
  dir_create(file.path(dir_path, "external"), recurse=TRUE)
  f = if(csv2) write.csv2 else write.csv
  data_labels = edc_example() %>% 
    keep_at(~str_detect(.x, "data")) %>%
    imap(~{
      .x[2:3, 2:3] = NA
      if(.y=="data1"){
        f(.x, glue("{dir_path}/{.y}.csv"), na=".", row.names=FALSE)
      } else {
        f(.x, glue("{dir_path}/{.y}.csv"), row.names=FALSE)
      }
      .x
    }) %>%  
    map(~get_label(.x, default=NA)) %>% unlist() %>% 
    set_names(~str_remove(.x, "data\\d\\.")) %>%
    .[!duplicated(.)] %>% 
    tibble::enframe(name="name", value="label") %>% 
    filter(!is.na(label)) %>%
    distinct()
  
  f(data_labels, glue("{dir_path}/labels.csv"), row.names=FALSE)
  f(data_labels, glue("{dir_path}/external/external_labels.csv"), row.names=FALSE)
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
  expect_named(db, c("data1", "data2", "data3", "datetime_extraction", "date_extraction", ".lookup"))
  db %>% keep(is_dataset) %>% map_dbl(~sum(is.na(.x))) %>% expect_all_equal(4)
  
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


test_that("read_all_csv() with csv1 and pass arguments", {
  local_options(edc_lookup_overwrite_warn=FALSE)
  
  path = path_temp("csv")
  unlink(path)
  example_to_csv(path, csv2=FALSE) #write.csv, not csv2
  
  #read.csv2 should not work
  file.path(path, "data1.csv") %>% read.csv2() %>% ncol() %>% expect_equal(1)
  
  #Read from files
  #pass nrows=1 to read.csv()
  db = read_all_csv(path, nrows=5, labels_from="labels.csv", verbose=0)
  expect_edc_database(db)
  expect_identical(attr(db, "source"), "files")
  expect_null(db$labels) #labels.csv is not a dataset
  expect_named(db, c("data1", "data2", "data3", "datetime_extraction", "date_extraction", ".lookup"))
  
  db %>% keep(is_dataset) %>% map_dbl(~sum(is.na(.x))) %>% expect_all_equal(4)
  db %>% keep(is_dataset) %>% map_dbl(nrow) %>% expect_all_equal(5)
  
  #error
  read_all_csv(path, foo=1, bar=1, labels_from="labels.csv", verbose=0) %>% 
    expect_error(class="bad_argument_error")
  
  unlink(path)
})




