# read_all_csv() works

    Code
      a %>% keep_at(~ str_detect(.x, "db")) %>% map(head)
    Output
      $db0
      # A tibble: 6 x 5
        SUBJID   age date_naissance             group crfname
         <int> <dbl> <chr>                      <chr> <chr>  
      1      1  63.7 2010-02-08 18:58:11.901665 B     db0    
      2      2  44.4 2010-02-28 03:31:39.220086 A     db0    
      3      3  53.6 2010-02-18 20:50:57.052604 A     db0    
      4      4  56.3 2010-02-16 04:06:46.709314 B     db0    
      5      5  54.0 2010-02-18 10:58:32.168806 B     db0    
      6      6  48.9 2010-02-23 13:28:11.581903 A     db0    
      
      $db1
      # A tibble: 6 x 6
        SUBJID date1                      date2                    date3 x     crfname
         <int> <chr>                      <chr>                    <chr> <chr> <chr>  
      1      1 2010-04-26 17:15:43.429136 2010-05-15 12:13:54.084~ 2010~ X     db1    
      2      2 2010-04-15 15:52:43.155079 2010-05-13 22:44:24.939~ 2010~ Y     db1    
      3      3 2010-05-09 06:10:28.5771   2010-04-23 11:13:47.729~ 2010~ X     db1    
      4      4 2010-04-29 22:17:45.00014  2010-05-21 23:38:08.363~ 2010~ X     db1    
      5      5 2010-04-24 09:32:33.198662 2010-04-26 19:58:27.774~ 2010~ X     db1    
      6      6 2010-04-26 06:22:19.84566  2010-05-04 13:19:23.933~ 2010~ X     db1    
      
      $db2
      # A tibble: 6 x 5
        SUBJID date4                      date5                      date6     crfname
         <int> <chr>                      <chr>                      <chr>     <chr>  
      1      1 2010-05-03 11:46:37.1386   2010-05-22 12:55:21.007293 2010-06-~ db2    
      2      2 2010-05-26 20:06:23.498583 2010-06-02 23:46:19.58961  2010-06-~ db2    
      3      3 2010-06-04 05:07:04.910038 2010-05-21 12:21:39.579934 2010-06-~ db2    
      4      4 2010-06-13 02:17:21.905347 2010-06-04 09:36:16.414756 2010-06-~ db2    
      5      5 2010-05-09 17:33:11.57912  2010-06-15 11:26:57.897259 2010-06-~ db2    
      6      6 2010-05-11 23:47:40.791298 2010-05-23 03:52:13.103359 2010-06-~ db2    
      
      $db3
      # A tibble: 6 x 6
        SUBJID date7                      date8                   date9 date10 crfname
         <int> <chr>                      <chr>                   <chr> <chr>  <chr>  
      1      1 2010-06-29 17:48:06.16379  2010-07-15 20:22:44.47~ 2010~ 2010-~ db3    
      2      2 2010-06-14 04:11:14.35854  2010-06-23 19:22:29.19~ 2010~ 2010-~ db3    
      3      3 2010-06-29 19:48:24.612568 2010-07-03 01:19:00.72~ 2010~ 2010-~ db3    
      4      4 2010-06-13 18:44:15.184322 2010-07-02 23:46:33.81~ 2010~ 2010-~ db3    
      5      5 2010-06-17 23:11:04.550297 2010-06-26 17:11:40.65~ 2010~ 2010-~ db3    
      6      6 2010-07-04 09:00:29.616735 2010-06-22 12:18:09.80~ 2010~ 2010-~ db3    
      

---

    Code
      a %>% keep_at(~ str_detect(.x, "db")) %>% map(get_label)
    Output
      $db0
      $db0$SUBJID
      [1] "Subject ID"
      
      $db0$age
      [1] "Age (years)"
      
      $db0$date_naissance
      [1] "Date of birth"
      
      $db0$group
      [1] "Treatment"
      
      $db0$crfname
      [1] "Form name"
      
      
      $db1
      $db1$SUBJID
      [1] "Subject ID"
      
      $db1$date1
      [1] "Date at visit 1"
      
      $db1$date2
      [1] "Date at visit 2"
      
      $db1$date3
      [1] "Date at visit 3"
      
      $db1$x
      [1] "Covariate"
      
      $db1$crfname
      [1] "Form name"
      
      
      $db2
      $db2$SUBJID
      [1] "Subject ID"
      
      $db2$date4
      [1] "Date at visit 4"
      
      $db2$date5
      [1] "Date at visit 5"
      
      $db2$date6
      [1] "Date at visit 6"
      
      $db2$crfname
      [1] "Form name"
      
      
      $db3
      $db3$SUBJID
      [1] "Subject ID"
      
      $db3$date7
      [1] "Date at visit 7"
      
      $db3$date8
      [1] "Date at visit 8"
      
      $db3$date9
      [1] "Date at visit 9"
      
      $db3$date10
      [1] "Date at visit 10"
      
      $db3$crfname
      [1] "Form name"
      
      

# read_all_csv() works with external labels

    Code
      a = read_all_csv(path, labels_from = "external/external_labels.csv", verbose = 0)
      head(a$labels)
    Output
      # A tibble: 6 x 2
        name           label
        <chr>          <chr>
      1 SUBJID         <NA> 
      2 age            <NA> 
      3 date_naissance <NA> 
      4 group          <NA> 
      5 crfname        <NA> 
      6 date1          <NA> 
    Code
      a %>% keep_at(~ str_detect(.x, "db")) %>% map(get_label)
    Output
      $db0
      $db0$SUBJID
      [1] "Subject ID"
      
      $db0$age
      [1] "Age (years)"
      
      $db0$date_naissance
      [1] "Date of birth"
      
      $db0$group
      [1] "Treatment"
      
      $db0$crfname
      [1] "Form name"
      
      
      $db1
      $db1$SUBJID
      [1] "Subject ID"
      
      $db1$date1
      [1] "Date at visit 1"
      
      $db1$date2
      [1] "Date at visit 2"
      
      $db1$date3
      [1] "Date at visit 3"
      
      $db1$x
      [1] "Covariate"
      
      $db1$crfname
      [1] "Form name"
      
      
      $db2
      $db2$SUBJID
      [1] "Subject ID"
      
      $db2$date4
      [1] "Date at visit 4"
      
      $db2$date5
      [1] "Date at visit 5"
      
      $db2$date6
      [1] "Date at visit 6"
      
      $db2$crfname
      [1] "Form name"
      
      
      $db3
      $db3$SUBJID
      [1] "Subject ID"
      
      $db3$date7
      [1] "Date at visit 7"
      
      $db3$date8
      [1] "Date at visit 8"
      
      $db3$date9
      [1] "Date at visit 9"
      
      $db3$date10
      [1] "Date at visit 10"
      
      $db3$crfname
      [1] "Form name"
      
      

