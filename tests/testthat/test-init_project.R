
skip_if(is_checking())

test_that("edc_new_project works", {
  path = path_temp("/test/test_init_project_dir")
  unlink(path, recursive=TRUE)
  
  edc_new_project(path, open=FALSE, verbose=FALSE)
  
  copied_files = dir_ls(path, type="file", recurse=TRUE)
  templ_dir = path_package("/init_proj", package="EDCimport")
  pkg_files = dir_ls(templ_dir, type="file", recurse=TRUE)
  
  expect_equal(length(copied_files), length(pkg_files))
  
  
  edc_new_project(path, open=FALSE, verbose=FALSE) %>% 
    expect_error(class="edc_new_project_notempty_error")
  
  unlink(path, recursive=TRUE)
})

