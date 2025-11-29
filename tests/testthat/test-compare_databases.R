

edc_options(edc_lookup_overwrite_warn=FALSE)

test_that("compare_databases() works", {
  
  mult = edc_example_multiple()
  
  comparison = compare_databases(mult) %>% 
    expect_classed_conditions(warning_class="edc_compare_databases_unique_date_warning")
  
  data_compare_table = comparison$table$`_data` %>% 
    mutate_all(~.x %>% str_remove_all("\n") %>% str_remove_all(".*____"))
  data_compare_plot = comparison$plot %>% as.list() %>%
    map("data") %>% list_rbind(names_to="plot")
  
  expect_snapshot({
    data_compare_table
    data_compare_plot
  })
  
})


