# read_all_csv() works

    Code
      a %>% keep_at(~ str_detect(.x, "data")) %>% map(head)
    Output
      $data1
      # A tibble: 6 x 7
        subjid date1      date2      date3      x     crfname crfstat   
         <int> <chr>      <chr>      <chr>      <chr> <chr>   <chr>     
      1      1 2010-04-26 2010-05-15 2010-05-12 X     data1   Incomplete
      2      2 2010-04-15 2010-05-13 2010-04-27 Y     data1   Complete  
      3      3 2010-05-08 2010-04-22 2010-05-24 X     data1   Complete  
      4      4 2010-04-29 2010-05-21 2010-05-10 X     data1   Incomplete
      5      5 2010-04-23 2010-04-26 2010-05-08 X     data1   Incomplete
      6      6 2010-04-25 2010-05-04 2010-04-30 X     data1   Incomplete
      
      $data2
      # A tibble: 6 x 6
        subjid date4      date5      date6      crfname crfstat   
         <int> <chr>      <chr>      <chr>      <chr>   <chr>     
      1      1 2010-08-01 2010-05-22 2010-06-11 data2   No Data   
      2      2 2010-07-31 2010-07-31 2010-06-19 data2   Incomplete
      3      3 2010-06-03 2010-07-21 2010-06-12 data2   Complete  
      4      4 2010-06-12 2010-06-03 2010-06-19 data2   Complete  
      5      5 2010-05-09 2010-06-14 2010-06-10 data2   Complete  
      6      6 2010-05-11 2010-05-22 2010-06-11 data2   Complete  
      
      $data3
      # A tibble: 6 x 7
        subjid date7      date8      date9      date10     crfname crfstat   
         <int> <chr>      <chr>      <chr>      <chr>      <chr>   <chr>     
      1      1 2010-06-29 2010-07-15 2010-06-28 2010-08-01 data3   Complete  
      2      2 2010-06-13 2010-06-23 2010-07-09 2010-07-31 data3   Incomplete
      3      3 2010-06-29 2010-07-02 2010-07-13 2010-07-21 data3   Complete  
      4      4 2010-06-13 2010-07-02 2010-07-07 2010-07-23 data3   Complete  
      5      5 2010-06-17 2010-06-26 2010-07-13 2010-07-14 data3   Complete  
      6      6 2010-07-03 2010-06-22 2010-07-08 2010-07-20 data3   Complete  
      

---

    Code
      a %>% keep_at(~ str_detect(.x, "data")) %>% map(get_label)
    Output
      $data1
      $data1$subjid
      [1] "Subject ID"
      
      $data1$date1
      [1] "Date at visit 1"
      
      $data1$date2
      [1] "Date at visit 2"
      
      $data1$date3
      [1] "Date at visit 3"
      
      $data1$x
      [1] "Covariate"
      
      $data1$crfname
      [1] "Form name"
      
      $data1$crfstat
      [1] "CRF status"
      
      
      $data2
      $data2$subjid
      [1] "Subject ID"
      
      $data2$date4
      [1] "Date at visit 4"
      
      $data2$date5
      [1] "Date at visit 5"
      
      $data2$date6
      [1] "Date at visit 6"
      
      $data2$crfname
      [1] "Form name"
      
      $data2$crfstat
      [1] "CRF status"
      
      
      $data3
      $data3$subjid
      [1] "Subject ID"
      
      $data3$date7
      [1] "Date at visit 7"
      
      $data3$date8
      [1] "Date at visit 8"
      
      $data3$date9
      [1] "Date at visit 9"
      
      $data3$date10
      [1] "Date at visit 10"
      
      $data3$crfname
      [1] "Form name"
      
      $data3$crfstat
      [1] "CRF status"
      
      

---

    Code
      a = read_all_csv(path, labels_from = "external/external_labels.csv", verbose = 0)
      head(a$labels)
    Output
      # A tibble: 6 x 2
        name    label
        <chr>   <chr>
      1 subjid  <NA> 
      2 date1   <NA> 
      3 date2   <NA> 
      4 date3   <NA> 
      5 x       <NA> 
      6 crfname <NA> 
    Code
      a %>% keep_at(~ str_detect(.x, "data")) %>% map(get_label)
    Output
      $data1
      $data1$subjid
      [1] "Subject ID"
      
      $data1$date1
      [1] "Date at visit 1"
      
      $data1$date2
      [1] "Date at visit 2"
      
      $data1$date3
      [1] "Date at visit 3"
      
      $data1$x
      [1] "Covariate"
      
      $data1$crfname
      [1] "Form name"
      
      $data1$crfstat
      [1] "CRF status"
      
      
      $data2
      $data2$subjid
      [1] "Subject ID"
      
      $data2$date4
      [1] "Date at visit 4"
      
      $data2$date5
      [1] "Date at visit 5"
      
      $data2$date6
      [1] "Date at visit 6"
      
      $data2$crfname
      [1] "Form name"
      
      $data2$crfstat
      [1] "CRF status"
      
      
      $data3
      $data3$subjid
      [1] "Subject ID"
      
      $data3$date7
      [1] "Date at visit 7"
      
      $data3$date8
      [1] "Date at visit 8"
      
      $data3$date9
      [1] "Date at visit 9"
      
      $data3$date10
      [1] "Date at visit 10"
      
      $data3$crfname
      [1] "Form name"
      
      $data3$crfstat
      [1] "CRF status"
      
      

