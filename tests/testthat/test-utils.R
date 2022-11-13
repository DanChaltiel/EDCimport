

skip_on_cran()

# load_list() ---------------------------------------------------------------------------------

test_that("load_list() works", {
  x=list(a=1, b=mtcars)
  load_list(x, remove=TRUE)
  expect_equal(a,1)
  expect_length(b,11)
  expect_false(exists("x", inherits=FALSE))
})

test_that("load_list() works cithout remove", {
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


# get_lookup() & find_keyword() ---------------------------------------------------------------

test_that("get_lookup() works", {
  
  lookup = list(i=crosstable::iris2, m=mtcars) %>% get_lookup()
  expect_equal(lengths(lookup$names), c(i=5, m=11))
  expect_true(all(nzchar(lookup$labels$i)))
  expect_false(any(nzchar(lookup$labels$m)))
  # lookup %>% unnest(everything())
  
  x = list(i=crosstable::iris2, mtcars)
  get_lookup(x) %>% 
    expect_error(class="edc_lookup_unnamed")
  x = list(i=1, .lookup=mtcars)
  get_lookup(x) %>% 
    expect_error(class="edc_lookup_empty")
})

test_that("find_keyword() works", {
  lookup = list(iris2=crosstable::iris2, mtcars2=crosstable::mtcars2) %>% get_lookup()
  x1=find_keyword("hp", data=lookup)
  expect_equal(x1$names, c("hp", "hp_date"))
  x2=find_keyword("number|date", data=lookup)
  expect_equal(x2$names, c("cyl", "gear", "carb", "hp_date", "qsec_posix"))
  x3=find_keyword("number|date", data=lookup, ignore_case=FALSE)
  expect_equal(x3$names, "hp_date")
})


test_that("find_keyword() works with read_trialmaster()", {
  clean_cache()
  expect_message(w <- read_trialmaster(filename),
                 class="read_tm_zip")
  local_options(edc_lookup=w$.lookup)
  x1=find_keyword("sex")
  expect_equal(x1$names, "SEX")
})





# 7-zip ---------------------------------------------------------------------------------------

test_that("Extract zip without password", {
  #This would actually work as well with a random password
  target = temp_target("test_7z2")
  extract_7z(filename_nopw, target)
  expect_true("procformat.sas" %in% dir(target))
})
test_that("Extract zip with password", {
  target = temp_target("test_7z1")
  extract_7z(filename, target, password="0")
  expect_true("procformat.sas" %in% dir(target))
})


## 7z Errors ----

test_that("Extract zip with wrong password", {
  target = temp_target("test_7zerr")
  skip_if(is_testing_in_buildpane(), 
          "Run manually, build pane is behaving wrong: https://stackoverflow.com/q/74308687/3888000")
  x=extract_7z(filename, target, password="foobar") %>%
    expect_error(class="edc_7z_bad_password_error")
})

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

