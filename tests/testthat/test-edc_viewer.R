
expect_dt = function(x, nrows = NULL){
  expect_s3_class(x, c("datatables", "htmlwidget"))
  if (!is.null(nrows)) {
    expect_equal(nrow(x$x$data), nrows)
  }
}

test_that("edc_viewer() on a database", {

  db = edc_example()
  load_database(db)
  
  data = NULL
  input = .resolve_input(data)
  server = edc_viewer_server(datasets=input$datasets, lookup=input$lookup)

  shiny::testServer(server, {
    session$flushReact()

    #initially, table 1 = "ae", should have 175 rows
    session$setInputs(input_table_rows_selected = 1)

    sidebar = dt_sidebar() %>% expect_no_error()
    expect_dt(sidebar, 8)

    main = dt_main() %>% expect_no_error()
    expect_dt(main, 175)

    info = dt_info() %>% expect_no_error()
    expect_dt(info, 7)

    #table 2 = "data1", filtered on subjid 1 and 2, should have 4 rows
    session$setInputs(input_table_rows_selected = 2)
    session$setInputs(subjid_selected = c(1,2))
    main = dt_main() %>% expect_no_error()
    expect_dt(main, 4)
  })
})

test_that("edc_viewer() on custom datasets", {

  data = lst(mtcars, iris)
  input = .resolve_input(data)
  server = edc_viewer_server(datasets=input$datasets, lookup=input$lookup)

  shiny::testServer(server, {
    session$flushReact()

    #initially, table 1 = "mtcars", should have 32 rows
    session$setInputs(input_table_rows_selected = 1)
    sidebar = dt_sidebar() %>% expect_no_error()
    expect_dt(sidebar, 2)

    main = dt_main() %>% expect_no_error()
    expect_dt(main, 32)

    info = dt_info() %>% expect_no_error()
    expect_dt(info, 11)

    #table 2 = "iris", filtered 150 rows
    session$setInputs(input_table_rows_selected = 2)
    main = dt_main() %>% expect_no_error()
    expect_dt(main, 150)
  })
})
