
mutate_list = function(.x, ...) {
  dots = rlang::enquos(...)
  for (nm in names(dots)) {
    .x[[nm]] = rlang::eval_tidy(dots[[nm]], data = .x)
  }
  .x
}

edc_options(edc_lookup_overwrite_warn=FALSE)

test_that("compare_databases() works", {
  
  db1 = edc_example()
  db2 = edc_example(N=60) %>% 
    mutate_list(
      data99 = data1, #new data
      enrol = enrol %>% mutate(a=1, b=2), #add columns
      data1 = data1 %>% select(-date2, -date3), #remove columns
      data2 = data2 %>% mutate(a=1, date5=NULL), #both
    )
  db3 = db2 %>% 
    mutate_list(
      data99 = data1, #new data
      enrol = enrol %>% mutate(c=1, d=2), #add columns
      data1 = data1 %>% select(-crfstat), #remove columns
      data2 = data2 %>% mutate(b=1, date6=NULL), #both
    )
  
  comparison = compare_databases(list(db1, db2, db3)) %>% 
    expect_classed_conditions(warning_class="edc_compare_databases_unique_date_warning")
  
  
  data_compare = comparison$table$`_data` %>% 
    mutate_all(~.x %>% str_remove_all("\n") %>% str_remove_all(".*____"))
  
  expect_snapshot({
    data_compare
  })
  
})



