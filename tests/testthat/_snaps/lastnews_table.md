# lastnews_table() snapshot

    Code
      db = lastnews_example(outdated = TRUE)
      load_database(db)
      csv_file = tempfile(fileext = ".csv")
      lastnews_table(warn_if_future = csv_file) %>% head(10)
    Condition
      Warning:
      Date of last news after the extraction date on columns "data3$date9" and "data3$date10" (4 patients: #9, #15, #16, and #25)
    Output
      # A tibble: 10 x 5
         subjid last_date  origin_data origin_col origin_label    
          <dbl> <date>     <chr>       <chr>      <chr>           
       1      1 2010-08-01 data2       date4      Date at visit 4 
       2      2 2010-07-31 data2       date4      Date at visit 4 
       3      3 2010-07-21 data2       date5      Date at visit 5 
       4      4 2010-07-23 data3       date10     Date at visit 10
       5      5 2010-07-14 data3       date10     Date at visit 10
       6      6 2010-07-20 data3       date10     Date at visit 10
       7      7 2010-07-28 data3       date9      Date at visit 9 
       8      8 2010-07-19 data3       date9      Date at visit 9 
       9      9 2010-08-10 data3       date9      Date at visit 9 
      10     10 2010-07-30 data3       date10     Date at visit 10
    Code
      x = read.csv2(csv_file)
      x
    Output
        subjid  last_date origin_data origin_col     origin_label       origin
      1      9 2010-08-10       data3      date9  Date at visit 9  data3$date9
      2     15 2010-08-13       data3     date10 Date at visit 10 data3$date10
      3     16 2010-08-11       data3     date10 Date at visit 10 data3$date10
      4     25 2010-08-23       data3     date10 Date at visit 10 data3$date10

