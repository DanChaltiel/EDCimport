
skip_if(is_checking())

test_that("edc_new_project works", {
  path = tempdir() %>% paste0("/test/test_init_project.dir")
  
  edc_new_project(path, open=FALSE)
  
  childs = dir_ls(path, type="any", recurse=TRUE)
  expect_length(childs, 11)
  
  unlink(path, recursive=TRUE)
})

