
test_that("read_all_sas errors", {
  skip("No example for `read_all_sas()` yet")

  read_all_sas(path, format_file="notfound.csv", verbose=FALSE) %>% 
    expect_error(class="edc_404_file_not_found")
})
