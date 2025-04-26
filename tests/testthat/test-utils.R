

skip_on_cran()
edc_options(edc_lookup_overwrite_warn=FALSE)


test_that("no exports", {
  skip_if(is_checking())
  testthat::test_path("../../R/utils.R") %>% 
    readLines() %>% str_subset("@export") %>% expect_length(0)
})




# load_database() ---------------------------------------------------------------------------------

test_that("load_database() works", {
  x=list(a=1, b=mtcars)
  load_database(x, remove=TRUE)
  expect_equal(a,1)
  expect_length(b,11)
  expect_false(exists("x", inherits=FALSE))
})

test_that("load_database() works without remove", {
  x=list(a=1, b=mtcars)
  load_database(x, remove=FALSE)
  expect_equal(a,1)
  expect_length(b,11)
  expect_true(exists("x", inherits=FALSE))
})

test_that("load_database() errors", {
  x=list(a=1, 5, b=8, 9)
  expect_error(load_database(x),
               class="load_database_unnamed_error")
})


test_that("save_list() works", {
  x=list(a=1, b=mtcars)
  save_list(x, "test.RData")
  load("test.RData")
  file.remove("test.RData")
  expect_equal(a,1)
  expect_length(b,11)
})

test_that("get_folder_datetime() works", {
  
  #good example
  get_folder_datetime("data/extraction20000101/") %>% 
    expect_s3_class(c("POSIXt", "Date")) %>% 
    as.character() %>% 
    expect_equal("2000-01-01")
  get_folder_datetime("data/extraction-2000_1~1/") %>% #bad format though
    expect_s3_class(c("POSIXt", "Date")) %>% 
    as.character() %>% 
    expect_equal("2000-01-01")
  
  #Warning: multiple dates
  get_folder_datetime("data/template_2020_01_01_extraction_2000-01-01/") %>% 
    expect_classed_conditions(warning_class=c("extract_date_multiple_warning")) %>% 
    expect_s3_class(c("POSIXt", "Date")) %>% 
    as.character() %>% 
    expect_equal("2000-01-01")
  
  #Warning: defaults to modification date
  folder = paste0(tempdir(), "/test_get_datetime")
  fs::dir_create(folder)
  fs::file_create(paste0(folder, "/f", 1:5, ".R"))
  dir(folder, full.names=TRUE)[1:3] %>% purrr::walk(~Sys.setFileTime(.x, "2000-01-01 CET"))
  # dir(folder, full.names=TRUE) %>% file.info() %>% as_tibble(rownames="file")
  get_folder_datetime(folder) %>% 
    expect_classed_conditions(warning_class=c("get_folder_datetime_warning", 
                                              "get_folder_datetime_modiftime_warning")) %>% 
    expect_s3_class(c("POSIXt", "Date")) %>% 
    as.character() %>% 
    expect_equal("2000-01-01")
  fs::dir_delete(folder)
})


# build_lookup() & edc_find_column() ---------------------------------------------------------------

test_that("build_lookup() works", {
  excluded = c(".lookup", "date_extraction", "datetime_extraction")
  x = edc_example() %>% discard_at(excluded)
  lookup = build_lookup(x) %>% arrange(dataset)
  xnames = x[sort(names(x))] %>% map(names)
  
  expect_equal(lookup$names, xnames)
  expect_true(all(nzchar(unlist(lookup$labels))))
  
  x = list(i=iris, mtcars)
  build_lookup(x) %>% 
    expect_error(class="edc_lookup_unnamed")
  x = list(date_extraction=1, datetime_extraction=1, .lookup=mtcars)
  build_lookup(x) %>% 
    expect_error(class="edc_lookup_empty")
})

test_that("edc_find_column() works", {
  x = edc_example()
  # x$.lookup %>% unnest() %>% v
  x1=edc_find_column("visit", lookup=x$.lookup)
  expect_setequal(x1$names, paste0("date", 1:10))
  x2=edc_find_column("SUBJ|\\(", lookup=x$.lookup)
  expect_equal(unique(x2$names), c("subjid", "age"))
  x3=edc_find_column("SUBJ|\\(", lookup=x$.lookup, ignore_case=FALSE)
  expect_equal(unique(x3$names), "age")
})


test_that("edc_find_column() works with read_trialmaster()", {
  clean_cache()
  w = read_trialmaster(filename, use_cache=FALSE, verbose=0)
  local_options(edc_lookup=w$.lookup)
  x1=edc_find_column("sex")
  expect_equal(x1$names, "SEX")
})





# 7-zip ---------------------------------------------------------------------------------------

test_that("Extract zip without password", {
  #This would actually work as well with a random password
  target = temp_target("test_7z1")
  unlink(target, recursive=TRUE)
  extract_7z(filename, target)
  expect_true("procformat.sas" %in% dir(target))
})


test_that("Extract zip subdir", {
  target = temp_target("test_7z2")
  unlink(target, recursive=TRUE)
  src = test_path("CRF_Dan_Export_SAS_XPORT_2022_08_25_15_16_subdir.zip")
  extract_7z(src, target)
  expect_true("procformat.sas" %in% dir(target))
  expect_true(!"PAT.xpt" %in% dir(target))
  expect_true("PAT.xpt" %in% dir(path(target, "sub")))
})


## 7z Errors ----

test_that("7zip not in the path", {
  withr::local_envvar(list(PATH = ""))
  cur_path = Sys.getenv("PATH")
  expect_false(str_detect(cur_path, "7-Zip"))
  target = temp_target("test_7z_path")
  
  #manual path: wrong
  extract_7z(filename, target, path_7zip="foobar")  %>%
    expect_error(class="edc_7z_cmd_error")
  #manual path: correct
  x=extract_7z(filename, target, password="0", path_7zip="C:/Program Files/7-Zip/")
  expect_true("procformat.sas" %in% dir(target))
})



# Expect --------------------------------------------------------------------------------------


test_that("expect_classed_conditions()", {
  fun1 = function(){
    inform("I am a message", class="message1")
    inform("I am a message too", class="message2")
    inform("I am a message three", class="message3")
    warn("Beware, I am a warning", class="warn1")
    warn("Beware, I am a warning 2", class="warn2")
    abort("STOP, I am the error!", class="error1")
    999
  }
  fun2 = function(){
    inform("I am a message", class="message1")
    inform("I am a message too", class="message2")
    inform("I am a message three", class="message3")
    warn("Beware, I am a warning", class="warn1")
    warn("Beware, I am a warning 2", class="warn2")
    999
  }
  
  a = expect_classed_conditions(fun1(), 
                                message_class=c("message1", "message2", "message3"),
                                warning_class=c("warn1", "warn2"), 
                                error_class="error1")
  expect_equal(a, "expect_classed_conditions__error")
  
  b = expect_classed_conditions(fun2(), 
                                message_class=c("message1", "message2", "message3"),
                                warning_class=c("warn1", "warn2"))
  expect_equal(b, 999)
  
  expect_classed_conditions(fun1(), 
                            message_class=c("message1", "message2", "xxxx"),
                            warning_class=c("warn1", "xxxx"), 
                            error_class="xxxx") %>% 
    expect_error("xxxx.*error1")
  expect_classed_conditions(fun1(), 
                            message_class=c("message1", "message2", "xxxx"),
                            warning_class=c("warn1", "xxxx"), 
                            error_class="error1") %>% 
    expect_error("xxxx.*warn2")
  expect_classed_conditions(fun1(), 
                            message_class=c("message1", "message2", "xxxx"),
                            warning_class=c("warn1", "warn2"), 
                            error_class="error1") %>% 
    expect_error("xxxx.*message3")
})



# Misc ----------------------------------------------------------------------------------------




test_that("cli_menu() is not in package cli yet", {
  # exists('cli_menu', where='package:cli', mode='function') %>% expect_false()
  expect_false("package:cli" %in% find("cli_menu"))
})


test_that("edc_db_to_excel() works", {
  db = edc_example()
  load_database(db)
  filename=tempfile(fileext=".xlsx")
  edc_db_to_excel(filename=filename, datasets=get_datasets(), open=FALSE) %>% expect_message()
  expect_true(file.exists(filename))
  file.remove(filename)
})
