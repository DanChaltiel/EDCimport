

skip_on_cran()

# load_list() ---------------------------------------------------------------------------------

test_that("load_list() works", {
  x=list(a=1, b=mtcars)
  load_list(x, remove=TRUE)
  expect_equal(a,1)
  expect_length(b,11)
  expect_false(exists("x", inherits=FALSE))
})

test_that("load_list() works without remove", {
  x=list(a=1, b=mtcars)
  load_list(x, remove=FALSE)
  expect_equal(a,1)
  expect_length(b,11)
  expect_true(exists("x", inherits=FALSE))
})

test_that("load_list() errors", {
  x=list(a=1, 5, b=8, 9)
  expect_error(load_list(x),
               class="load_list_unnamed_error")
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
  folder = paste0(tempdir(), "/test_get_datetime")
  dir.create(folder, showWarnings=FALSE)
  file.create(paste0(folder, "/f1.R"))
  file.create(paste0(folder, "/f2.R"))
  file.create(paste0(folder, "/f3.R"))
  file.create(paste0(folder, "/f4.R"))
  file.create(paste0(folder, "/f5.R"))
  dir(folder, full.names=TRUE)[1:3] %>% purrr::walk(~Sys.setFileTime(.x, "1975-01-01 CET"))
  # dir(folder, full.names=TRUE) %>% file.info() %>% select(mtime)
  x = get_folder_datetime(folder) %>% 
    expect_classed_conditions(warning_class="get_folder_datetime_modiftime_warning")
  expect_equal(as.character(x), "1975-01-01")
})


# get_lookup() & find_keyword() ---------------------------------------------------------------

test_that("get_lookup() works", {
  
  x = edc_example()
  x$.lookup=NULL
  lookup = get_lookup(x)
  expect_equal(lengths(lookup$names), c(db0=5,db2=5,db3=6,db1=6))
  expect_true(all(nzchar(lookup$labels$i)))
  expect_false(any(nzchar(lookup$labels$m)))
  # lookup %>% unnest(everything())
  
  x = list(i=iris, mtcars)
  get_lookup(x) %>% 
    expect_error(class="edc_lookup_unnamed")
  x = list(date_extraction=1, datetime_extraction=1, .lookup=mtcars)
  get_lookup(x) %>% 
    expect_error(class="edc_lookup_empty")
})

test_that("find_keyword() works", {
  x = edc_example()
  # x$.lookup %>% unnest() %>% v
  x1=find_keyword("visit", data=x$.lookup)
  expect_setequal(x1$names, paste0("date", 1:10))
  x2=find_keyword("id|\\(", data=x$.lookup)
  expect_equal(unique(x2$names), c("SUBJID", "age"))
  x3=find_keyword("id|\\(", data=x$.lookup, ignore_case=FALSE)
  expect_equal(unique(x3$names), "age")
})


test_that("find_keyword() works with read_trialmaster()", {
  clean_cache()
  w = read_trialmaster(filename, use_cache=FALSE, verbose=0)
  local_options(edc_lookup=w$.lookup)
  x1=find_keyword("sex")
  expect_equal(x1$names, "SEX")
})





# 7-zip ---------------------------------------------------------------------------------------

test_that("Extract zip without password", {
  #This would actually work as well with a random password
  target = temp_target("test_7z1")
  extract_7z(filename, target)
  expect_true("procformat.sas" %in% dir(target))
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



# Options -------------------------------------------------------------------------------------


test_that("No missing options", {
  missing_options = missing_options_helper()
  expect_identical(missing_options, character(0))
})

# Expect --------------------------------------------------------------------------------------



test_that("expect_classed_conditions()", {
  fun1 = function(){
    rlang::inform("I am a message", class="message1")
    rlang::inform("I am a message too", class="message2")
    rlang::inform("I am a message three", class="message3")
    rlang::warn("Beware, I am a warning", class="warn1")
    rlang::warn("Beware, I am a warning 2", class="warn2")
    rlang::abort("STOP, I am the error!", class="error1")
    999
  }
  fun2 = function(){
    rlang::inform("I am a message", class="message1")
    rlang::inform("I am a message too", class="message2")
    rlang::inform("I am a message three", class="message3")
    rlang::warn("Beware, I am a warning", class="warn1")
    rlang::warn("Beware, I am a warning 2", class="warn2")
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
    expect_error("error1.*xxxx")
  expect_classed_conditions(fun1(), 
                            message_class=c("message1", "message2", "xxxx"),
                            warning_class=c("warn1", "xxxx"), 
                            error_class="error1") %>% 
    expect_error("warn2.*xxxx")
  expect_classed_conditions(fun1(), 
                            message_class=c("message1", "message2", "xxxx"),
                            warning_class=c("warn1", "warn2"), 
                            error_class="error1") %>% 
    expect_error("message3.*xxxx")
})
