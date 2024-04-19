
skip_on_cran()
edc_options(edc_lookup_overwrite_warn=FALSE)
filename_pw = test_path("CRF_Dan_Export_SAS_XPORT_2022_08_25_15_16_pw.zip")


test_that("Extract zip with password", {
  target = temp_target("test_7z_pw")
  extract_7z(filename_pw, target, password="0")
  expect_true("procformat.sas" %in% dir(target))
})


test_that("Extract zip with wrong password", {
  target = temp_target("test_7zerr")
  skip_if(is_testing_in_buildpane(), 
          "Run manually, build pane is behaving wrong: https://stackoverflow.com/q/74308687/3888000")
  x=extract_7z(filename_pw, target, password="foobar") %>%
    expect_error(class="edc_7z_bad_password_error")
})
