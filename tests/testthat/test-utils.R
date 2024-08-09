

skip_on_cran()
edc_options(edc_lookup_overwrite_warn=FALSE)


test_that("no exports", {
  skip_if(is_checking())
  testthat::test_path("../../R/utils.R") %>% 
    readLines() %>% str_subset("@export") %>% expect_length(0)
})




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
  fs::dir_create(folder)
  fs::file_create(paste0(folder, "/f", 1:5, ".R"))
  dir(folder, full.names=TRUE)[1:3] %>% purrr::walk(~Sys.setFileTime(.x, "1975-01-01 CET"))
  # dir(folder, full.names=TRUE) %>% file.info() %>% select(mtime)
  x = get_folder_datetime(folder) %>% 
    expect_classed_conditions(warning_class="get_folder_datetime_modiftime_warning")
  expect_equal(as.character(x), "1975-01-01")
})


# build_lookup() & find_keyword() ---------------------------------------------------------------

test_that("build_lookup() works", {
  
  x = edc_example()
  x$.lookup=NULL
  lookup = build_lookup(x)
  expect_equal(lengths(lookup$names), c(db0=5,db2=5,db3=6,db1=6))
  expect_true(all(nzchar(lookup$labels$i)))
  expect_false(any(nzchar(lookup$labels$m)))
  # lookup %>% unnest(everything())
  
  x = list(i=iris, mtcars)
  build_lookup(x) %>% 
    expect_error(class="edc_lookup_unnamed")
  x = list(date_extraction=1, datetime_extraction=1, .lookup=mtcars)
  build_lookup(x) %>% 
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


test_that("fct_yesno() works", {
  
  set.seed(42)
  N=20
  x = tibble(
    eng=sample(c("Yes", "No"), size=N, replace=TRUE),
    fra=sample(c("Oui", "Non"), size=N, replace=TRUE),
    bin=sample(0:1, size=N, replace=TRUE),
    log=sample(c(TRUE, FALSE), size=N, replace=TRUE),
    eng2=sample(c("1-Yes", "0-No"), size=N, replace=TRUE),

    chr=sample(c("aaa", "bbb", "ccc"), size=N, replace=TRUE),
    num=1:N,
  )
  x[10:11,] = NA
  
  
  expect_snapshot({
    
    fct_yesno("Yes")
    fct_yesno(c("No", "Yes"))
    
    mutate_all(x, fct_yesno, fail=FALSE)
    mutate_all(x, fct_yesno, fail=FALSE, strict=TRUE)
    mutate_all(x, fct_yesno, fail=FALSE, input=list(yes="Ja", no="Nein"))
  })
  
  mutate_all(x, fct_yesno, fail=TRUE) %>% expect_error(class="fct_yesno_unparsed_error")
  fct_yesno(x$chr) %>% expect_error(class="fct_yesno_unparsed_error")
  # fct_yesno(x$num) %>% expect_error(class="fct_yesno_unparsed_error") #TODO?
  fct_yesno("YesNo") %>% expect_error(class="fct_yesno_both_error")
  fct_yesno("foobar") %>% expect_error(class="fct_yesno_unparsed_error")
  
})


test_that("cli_menu() is not in package cli yet", {
  # exists('cli_menu', where='package:cli', mode='function') %>% expect_false()
  expect_false("package:cli" %in% find("cli_menu"))
})


test_that("edc_db_to_excel() works", {
  tm = edc_example()
  load_list(tm)
  filename=tempfile(fileext=".xlsx")
  edc_db_to_excel(filename=filename, datasets=get_datasets(), open=FALSE) %>% expect_message()
  expect_true(file.exists(filename))
  file.remove(filename)
})
