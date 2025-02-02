# lastnews_table() snapshot

    Code
      db = lastnews_example(outdated = TRUE)
      load_database(db)
      csv_file = tempfile(fileext = ".csv")
      lastnews_table(warn_if_future = csv_file) %>% head(10)
    Condition
      Warning:
      Date of last news after the extraction date on columns "db3$date9" and "db3$date10" (4 patients: #9, #15, #16, and #25)
    Output
      # A tibble: 10 x 5
         subjid last_date           origin_data origin_col origin_label    
          <dbl> <dttm>              <chr>       <chr>      <chr>           
       1      1 2010-08-01 18:59:37 db2         date4      Date at visit 4 
       2      2 2010-07-31 15:32:45 db2         date4      Date at visit 4 
       3      3 2010-07-22 11:24:37 db2         date5      Date at visit 5 
       4      4 2010-07-23 20:38:32 db3         date10     Date at visit 10
       5      5 2010-07-15 07:09:47 db3         date10     Date at visit 10
       6      6 2010-07-20 12:27:00 db3         date10     Date at visit 10
       7      7 2010-07-28 16:24:09 db3         date9      Date at visit 9 
       8      8 2010-07-19 15:24:18 db3         date9      Date at visit 9 
       9      9 2010-08-11 03:48:27 db3         date9      Date at visit 9 
      10     10 2010-07-30 20:41:23 db3         date10     Date at visit 10
    Code
      x = read.csv2(csv_file)
      x
    Output
        subjid                  last_date origin_data origin_col     origin_label
      1      9 2010-08-11 03:48:27.679536         db3      date9  Date at visit 9
      2     15 2010-08-13 22:03:24.791022         db3     date10 Date at visit 10
      3     16 2010-08-11 21:54:54.579528         db3     date10 Date at visit 10
      4     25 2010-08-23 18:58:36.044095         db3     date10 Date at visit 10
            origin
      1  db3$date9
      2 db3$date10
      3 db3$date10
      4 db3$date10

