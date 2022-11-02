


# load_list() ---------------------------------------------------------------------------------
skip_on_cran()

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


# find_keyword() ------------------------------------------------------------------------------

#TODO tester aussi quand certaines variables sont manquantes ?
test_that("find_keyword() works", {
  mtcars2=crosstable::mtcars2
  iris2=crosstable::iris2
  tablelist = tibble(
    dataset=c("mtcars2", "iris2"),
    names=map(dataset, ~names(get(.x))),
    labels=map(dataset, ~var_label(get(.x)))
  )
  x1=find_keyword("hp", data=tablelist)
  expect_equal(x1$names, c("hp", "hp_date"))
  x2=find_keyword("number|date", data=tablelist)
  expect_equal(x2$names, c("cyl", "gear", "carb", "hp_date", "qsec_posix"))
  x3=find_keyword("number|date", data=tablelist, ignore_case=FALSE)
  expect_equal(x3$names, "hp_date")
})

# 7-zip ---------------------------------------------------------------------------------------


new_target = function(name){
  target = file.path2(tempdir(), "name")
  unlink(target, recursive=TRUE)
  dir.create(target, showWarnings=FALSE)
  target
}

test_that("Extract zip without password", {
  #This would actually work as well with a random password
  target = new_target("test_7z2")
  extract_7z(filename_nopw, target)
  expect_true("procformat.sas" %in% dir(target))
})
test_that("Extract zip with password", {
  target = new_target("test_7z1")
  extract_7z(filename, target, password="0")
  expect_true("procformat.sas" %in% dir(target))
})


## 7z Errors ----

test_that("Extract zip with wrong password", {
  target = new_target("test_7zerr")
  x=extract_7z(filename, target, password="foobar") %>%
    expect_error(class="edc_7z_error")
})

test_that("7zip not in the path", {
  withr::local_envvar(list(PATH = ""))
  cur_path = Sys.getenv("PATH")
  expect_false(str_detect(cur_path, "7-Zip"))
  target = new_target("test_7z_path")
  
  #manual path: wrong
  extract_7z(filename, target, path_7zip="foobar")  %>%
    expect_error(class="edc_7z_cmd_error")
  #manual path: correct
  x=extract_7z(filename, target, password="0", path_7zip="C:/Program Files/7-Zip/")
  expect_true("procformat.sas" %in% dir(target))
})

