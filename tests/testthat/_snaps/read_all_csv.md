# multiplication works

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
      
      

