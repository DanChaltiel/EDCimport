# compare_databases() works

    Code
      data_compare_table
    Output
      # A tibble: 9 x 4
        dataset    extract_2024_01_01__0 extract_2024_01_01__1 extract_2024_04_01
        <chr>      <chr>                 <chr>                 <chr>             
      1 ae         Added                 Unchanged             Unchanged         
      2 data1      Added                 +0 -2                 +0 -1             
      3 data2      Added                 +1 -1                 +1 -1             
      4 data3      Added                 Unchanged             Unchanged         
      5 data99     Absent                Added                 +0 -2             
      6 enrol      Added                 +2 -0                 +2 -0             
      7 long_mixed Added                 Unchanged             Unchanged         
      8 long_pure  Added                 Unchanged             Unchanged         
      9 short      Added                 Unchanged             Unchanged         
    Code
      data_compare_plot
    Output
      # A tibble: 108 x 5
           plot source                dataset    value group_dodge                    
          <int> <chr>                 <chr>      <dbl> <chr>                          
        1     1 extract_2024_01_01__0 ae         175   extract_2024_01_01__0 ae       
        2     1 extract_2024_01_01__1 ae         202   extract_2024_01_01__1 ae       
        3     1 extract_2024_04_01    ae         202   extract_2024_04_01 ae          
        4     1 extract_2024_01_01__0 data1      100   extract_2024_01_01__0 data1    
        5     1 extract_2024_01_01__1 data1      120   extract_2024_01_01__1 data1    
        6     1 extract_2024_04_01    data1      120   extract_2024_04_01 data1       
        7     1 extract_2024_01_01__0 data2       50   extract_2024_01_01__0 data2    
        8     1 extract_2024_01_01__1 data2       60   extract_2024_01_01__1 data2    
        9     1 extract_2024_04_01    data2       60   extract_2024_04_01 data2       
       10     1 extract_2024_01_01__0 data3       50   extract_2024_01_01__0 data3    
       11     1 extract_2024_01_01__1 data3       60   extract_2024_01_01__1 data3    
       12     1 extract_2024_04_01    data3       60   extract_2024_04_01 data3       
       13     1 extract_2024_01_01__0 data99      NA   extract_2024_01_01__0 data99   
       14     1 extract_2024_01_01__1 data99     120   extract_2024_01_01__1 data99   
       15     1 extract_2024_04_01    data99     120   extract_2024_04_01 data99      
       16     1 extract_2024_01_01__0 enrol       50   extract_2024_01_01__0 enrol    
       17     1 extract_2024_01_01__1 enrol       60   extract_2024_01_01__1 enrol    
       18     1 extract_2024_04_01    enrol       60   extract_2024_04_01 enrol       
       19     1 extract_2024_01_01__0 long_mixed 100   extract_2024_01_01__0 long_mix~
       20     1 extract_2024_01_01__1 long_mixed 120   extract_2024_01_01__1 long_mix~
       21     1 extract_2024_04_01    long_mixed 120   extract_2024_04_01 long_mixed  
       22     1 extract_2024_01_01__0 long_pure  150   extract_2024_01_01__0 long_pure
       23     1 extract_2024_01_01__1 long_pure  180   extract_2024_01_01__1 long_pure
       24     1 extract_2024_04_01    long_pure  180   extract_2024_04_01 long_pure   
       25     1 extract_2024_01_01__0 short       50   extract_2024_01_01__0 short    
       26     1 extract_2024_01_01__1 short       60   extract_2024_01_01__1 short    
       27     1 extract_2024_04_01    short       60   extract_2024_04_01 short       
       28     2 extract_2024_01_01__0 ae           7   extract_2024_01_01__0 ae       
       29     2 extract_2024_01_01__1 ae           7   extract_2024_01_01__1 ae       
       30     2 extract_2024_04_01    ae           7   extract_2024_04_01 ae          
       31     2 extract_2024_01_01__0 data1        7   extract_2024_01_01__0 data1    
       32     2 extract_2024_01_01__1 data1        5   extract_2024_01_01__1 data1    
       33     2 extract_2024_04_01    data1        4   extract_2024_04_01 data1       
       34     2 extract_2024_01_01__0 data2        6   extract_2024_01_01__0 data2    
       35     2 extract_2024_01_01__1 data2        6   extract_2024_01_01__1 data2    
       36     2 extract_2024_04_01    data2        6   extract_2024_04_01 data2       
       37     2 extract_2024_01_01__0 data3        7   extract_2024_01_01__0 data3    
       38     2 extract_2024_01_01__1 data3        7   extract_2024_01_01__1 data3    
       39     2 extract_2024_04_01    data3        7   extract_2024_04_01 data3       
       40     2 extract_2024_01_01__0 data99      NA   extract_2024_01_01__0 data99   
       41     2 extract_2024_01_01__1 data99       7   extract_2024_01_01__1 data99   
       42     2 extract_2024_04_01    data99       5   extract_2024_04_01 data99      
       43     2 extract_2024_01_01__0 enrol        6   extract_2024_01_01__0 enrol    
       44     2 extract_2024_01_01__1 enrol        8   extract_2024_01_01__1 enrol    
       45     2 extract_2024_04_01    enrol       10   extract_2024_04_01 enrol       
       46     2 extract_2024_01_01__0 long_mixed   6   extract_2024_01_01__0 long_mix~
       47     2 extract_2024_01_01__1 long_mixed   6   extract_2024_01_01__1 long_mix~
       48     2 extract_2024_04_01    long_mixed   6   extract_2024_04_01 long_mixed  
       49     2 extract_2024_01_01__0 long_pure    4   extract_2024_01_01__0 long_pure
       50     2 extract_2024_01_01__1 long_pure    4   extract_2024_01_01__1 long_pure
       51     2 extract_2024_04_01    long_pure    4   extract_2024_04_01 long_pure   
       52     2 extract_2024_01_01__0 short        5   extract_2024_01_01__0 short    
       53     2 extract_2024_01_01__1 short        5   extract_2024_01_01__1 short    
       54     2 extract_2024_04_01    short        5   extract_2024_04_01 short       
       55     3 extract_2024_01_01__0 ae          48   extract_2024_01_01__0 ae       
       56     3 extract_2024_01_01__1 ae           0   extract_2024_01_01__1 ae       
       57     3 extract_2024_04_01    ae           0   extract_2024_04_01 ae          
       58     3 extract_2024_01_01__0 data1       50   extract_2024_01_01__0 data1    
       59     3 extract_2024_01_01__1 data1        0   extract_2024_01_01__1 data1    
       60     3 extract_2024_04_01    data1        0   extract_2024_04_01 data1       
       61     3 extract_2024_01_01__0 data2       50   extract_2024_01_01__0 data2    
       62     3 extract_2024_01_01__1 data2        0   extract_2024_01_01__1 data2    
       63     3 extract_2024_04_01    data2        0   extract_2024_04_01 data2       
       64     3 extract_2024_01_01__0 data3       50   extract_2024_01_01__0 data3    
       65     3 extract_2024_01_01__1 data3        0   extract_2024_01_01__1 data3    
       66     3 extract_2024_04_01    data3        0   extract_2024_04_01 data3       
       67     3 extract_2024_01_01__0 data99      NA   extract_2024_01_01__0 data99   
       68     3 extract_2024_01_01__1 data99       0   extract_2024_01_01__1 data99   
       69     3 extract_2024_04_01    data99       0   extract_2024_04_01 data99      
       70     3 extract_2024_01_01__0 enrol       50   extract_2024_01_01__0 enrol    
       71     3 extract_2024_01_01__1 enrol        0   extract_2024_01_01__1 enrol    
       72     3 extract_2024_04_01    enrol        0   extract_2024_04_01 enrol       
       73     3 extract_2024_01_01__0 long_mixed  50   extract_2024_01_01__0 long_mix~
       74     3 extract_2024_01_01__1 long_mixed   0   extract_2024_01_01__1 long_mix~
       75     3 extract_2024_04_01    long_mixed   0   extract_2024_04_01 long_mixed  
       76     3 extract_2024_01_01__0 long_pure   50   extract_2024_01_01__0 long_pure
       77     3 extract_2024_01_01__1 long_pure    0   extract_2024_01_01__1 long_pure
       78     3 extract_2024_04_01    long_pure    0   extract_2024_04_01 long_pure   
       79     3 extract_2024_01_01__0 short       50   extract_2024_01_01__0 short    
       80     3 extract_2024_01_01__1 short        0   extract_2024_01_01__1 short    
       81     3 extract_2024_04_01    short        0   extract_2024_04_01 short       
       82     4 extract_2024_01_01__0 ae           3.6 extract_2024_01_01__0 ae       
       83     4 extract_2024_01_01__1 ae          NA   extract_2024_01_01__1 ae       
       84     4 extract_2024_04_01    ae          NA   extract_2024_04_01 ae          
       85     4 extract_2024_01_01__0 data1        2   extract_2024_01_01__0 data1    
       86     4 extract_2024_01_01__1 data1       NA   extract_2024_01_01__1 data1    
       87     4 extract_2024_04_01    data1       NA   extract_2024_04_01 data1       
       88     4 extract_2024_01_01__0 data2        1   extract_2024_01_01__0 data2    
       89     4 extract_2024_01_01__1 data2       NA   extract_2024_01_01__1 data2    
       90     4 extract_2024_04_01    data2       NA   extract_2024_04_01 data2       
       91     4 extract_2024_01_01__0 data3        1   extract_2024_01_01__0 data3    
       92     4 extract_2024_01_01__1 data3       NA   extract_2024_01_01__1 data3    
       93     4 extract_2024_04_01    data3       NA   extract_2024_04_01 data3       
       94     4 extract_2024_01_01__0 data99      NA   extract_2024_01_01__0 data99   
       95     4 extract_2024_01_01__1 data99      NA   extract_2024_01_01__1 data99   
       96     4 extract_2024_04_01    data99      NA   extract_2024_04_01 data99      
       97     4 extract_2024_01_01__0 enrol        1   extract_2024_01_01__0 enrol    
       98     4 extract_2024_01_01__1 enrol       NA   extract_2024_01_01__1 enrol    
       99     4 extract_2024_04_01    enrol       NA   extract_2024_04_01 enrol       
      100     4 extract_2024_01_01__0 long_mixed   2   extract_2024_01_01__0 long_mix~
      101     4 extract_2024_01_01__1 long_mixed  NA   extract_2024_01_01__1 long_mix~
      102     4 extract_2024_04_01    long_mixed  NA   extract_2024_04_01 long_mixed  
      103     4 extract_2024_01_01__0 long_pure    3   extract_2024_01_01__0 long_pure
      104     4 extract_2024_01_01__1 long_pure   NA   extract_2024_01_01__1 long_pure
      105     4 extract_2024_04_01    long_pure   NA   extract_2024_04_01 long_pure   
      106     4 extract_2024_01_01__0 short        1   extract_2024_01_01__0 short    
      107     4 extract_2024_01_01__1 short       NA   extract_2024_01_01__1 short    
      108     4 extract_2024_04_01    short       NA   extract_2024_04_01 short       

