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
      5 data99     Absent                Added                 Unchanged         
      6 enrol      Added                 +2 -0                 +2 -0             
      7 long_mixed Added                 Unchanged             Unchanged         
      8 long_pure  Added                 Unchanged             Unchanged         
      9 short      Added                 Unchanged             Unchanged         
    Code
      data_compare_plot[[1]]
    Output
      # A tibble: 27 x 4
         source                dataset    value group_dodge                     
         <chr>                 <chr>      <dbl> <chr>                           
       1 extract_2024_01_01__0 ae           175 extract_2024_01_01__0 ae        
       2 extract_2024_01_01__1 ae           202 extract_2024_01_01__1 ae        
       3 extract_2024_04_01    ae           202 extract_2024_04_01 ae           
       4 extract_2024_01_01__0 data1        100 extract_2024_01_01__0 data1     
       5 extract_2024_01_01__1 data1        120 extract_2024_01_01__1 data1     
       6 extract_2024_04_01    data1        120 extract_2024_04_01 data1        
       7 extract_2024_01_01__0 data2         50 extract_2024_01_01__0 data2     
       8 extract_2024_01_01__1 data2         60 extract_2024_01_01__1 data2     
       9 extract_2024_04_01    data2         60 extract_2024_04_01 data2        
      10 extract_2024_01_01__0 data3         50 extract_2024_01_01__0 data3     
      11 extract_2024_01_01__1 data3         60 extract_2024_01_01__1 data3     
      12 extract_2024_04_01    data3         60 extract_2024_04_01 data3        
      13 extract_2024_01_01__0 data99        NA extract_2024_01_01__0 data99    
      14 extract_2024_01_01__1 data99       120 extract_2024_01_01__1 data99    
      15 extract_2024_04_01    data99       120 extract_2024_04_01 data99       
      16 extract_2024_01_01__0 enrol         50 extract_2024_01_01__0 enrol     
      17 extract_2024_01_01__1 enrol         60 extract_2024_01_01__1 enrol     
      18 extract_2024_04_01    enrol         60 extract_2024_04_01 enrol        
      19 extract_2024_01_01__0 long_mixed   100 extract_2024_01_01__0 long_mixed
      20 extract_2024_01_01__1 long_mixed   120 extract_2024_01_01__1 long_mixed
      21 extract_2024_04_01    long_mixed   120 extract_2024_04_01 long_mixed   
      22 extract_2024_01_01__0 long_pure    150 extract_2024_01_01__0 long_pure 
      23 extract_2024_01_01__1 long_pure    180 extract_2024_01_01__1 long_pure 
      24 extract_2024_04_01    long_pure    180 extract_2024_04_01 long_pure    
      25 extract_2024_01_01__0 short         50 extract_2024_01_01__0 short     
      26 extract_2024_01_01__1 short         60 extract_2024_01_01__1 short     
      27 extract_2024_04_01    short         60 extract_2024_04_01 short        
    Code
      data_compare_plot[[2]]
    Output
      # A tibble: 27 x 4
         source                dataset    value group_dodge                     
         <chr>                 <chr>      <dbl> <chr>                           
       1 extract_2024_01_01__0 ae             7 extract_2024_01_01__0 ae        
       2 extract_2024_01_01__1 ae             7 extract_2024_01_01__1 ae        
       3 extract_2024_04_01    ae             7 extract_2024_04_01 ae           
       4 extract_2024_01_01__0 data1          7 extract_2024_01_01__0 data1     
       5 extract_2024_01_01__1 data1          5 extract_2024_01_01__1 data1     
       6 extract_2024_04_01    data1          4 extract_2024_04_01 data1        
       7 extract_2024_01_01__0 data2          6 extract_2024_01_01__0 data2     
       8 extract_2024_01_01__1 data2          6 extract_2024_01_01__1 data2     
       9 extract_2024_04_01    data2          6 extract_2024_04_01 data2        
      10 extract_2024_01_01__0 data3          7 extract_2024_01_01__0 data3     
      11 extract_2024_01_01__1 data3          7 extract_2024_01_01__1 data3     
      12 extract_2024_04_01    data3          7 extract_2024_04_01 data3        
      13 extract_2024_01_01__0 data99        NA extract_2024_01_01__0 data99    
      14 extract_2024_01_01__1 data99         7 extract_2024_01_01__1 data99    
      15 extract_2024_04_01    data99         7 extract_2024_04_01 data99       
      16 extract_2024_01_01__0 enrol          6 extract_2024_01_01__0 enrol     
      17 extract_2024_01_01__1 enrol          8 extract_2024_01_01__1 enrol     
      18 extract_2024_04_01    enrol         10 extract_2024_04_01 enrol        
      19 extract_2024_01_01__0 long_mixed     6 extract_2024_01_01__0 long_mixed
      20 extract_2024_01_01__1 long_mixed     6 extract_2024_01_01__1 long_mixed
      21 extract_2024_04_01    long_mixed     6 extract_2024_04_01 long_mixed   
      22 extract_2024_01_01__0 long_pure      4 extract_2024_01_01__0 long_pure 
      23 extract_2024_01_01__1 long_pure      4 extract_2024_01_01__1 long_pure 
      24 extract_2024_04_01    long_pure      4 extract_2024_04_01 long_pure    
      25 extract_2024_01_01__0 short          5 extract_2024_01_01__0 short     
      26 extract_2024_01_01__1 short          5 extract_2024_01_01__1 short     
      27 extract_2024_04_01    short          5 extract_2024_04_01 short        
    Code
      data_compare_plot[[3]]
    Output
      # A tibble: 27 x 4
         source                dataset    value group_dodge                     
         <chr>                 <chr>      <int> <chr>                           
       1 extract_2024_01_01__0 ae            48 extract_2024_01_01__0 ae        
       2 extract_2024_01_01__1 ae            58 extract_2024_01_01__1 ae        
       3 extract_2024_04_01    ae            58 extract_2024_04_01 ae           
       4 extract_2024_01_01__0 data1         50 extract_2024_01_01__0 data1     
       5 extract_2024_01_01__1 data1         60 extract_2024_01_01__1 data1     
       6 extract_2024_04_01    data1         60 extract_2024_04_01 data1        
       7 extract_2024_01_01__0 data2         50 extract_2024_01_01__0 data2     
       8 extract_2024_01_01__1 data2         60 extract_2024_01_01__1 data2     
       9 extract_2024_04_01    data2         60 extract_2024_04_01 data2        
      10 extract_2024_01_01__0 data3         50 extract_2024_01_01__0 data3     
      11 extract_2024_01_01__1 data3         60 extract_2024_01_01__1 data3     
      12 extract_2024_04_01    data3         60 extract_2024_04_01 data3        
      13 extract_2024_01_01__0 data99        NA extract_2024_01_01__0 data99    
      14 extract_2024_01_01__1 data99        60 extract_2024_01_01__1 data99    
      15 extract_2024_04_01    data99        60 extract_2024_04_01 data99       
      16 extract_2024_01_01__0 enrol         50 extract_2024_01_01__0 enrol     
      17 extract_2024_01_01__1 enrol         60 extract_2024_01_01__1 enrol     
      18 extract_2024_04_01    enrol         60 extract_2024_04_01 enrol        
      19 extract_2024_01_01__0 long_mixed    50 extract_2024_01_01__0 long_mixed
      20 extract_2024_01_01__1 long_mixed    60 extract_2024_01_01__1 long_mixed
      21 extract_2024_04_01    long_mixed    60 extract_2024_04_01 long_mixed   
      22 extract_2024_01_01__0 long_pure     50 extract_2024_01_01__0 long_pure 
      23 extract_2024_01_01__1 long_pure     60 extract_2024_01_01__1 long_pure 
      24 extract_2024_04_01    long_pure     60 extract_2024_04_01 long_pure    
      25 extract_2024_01_01__0 short         50 extract_2024_01_01__0 short     
      26 extract_2024_01_01__1 short         60 extract_2024_01_01__1 short     
      27 extract_2024_04_01    short         60 extract_2024_04_01 short        
    Code
      data_compare_plot[[4]]
    Output
      # A tibble: 27 x 4
         source                dataset    value group_dodge                     
         <chr>                 <chr>      <dbl> <chr>                           
       1 extract_2024_01_01__0 ae           3.6 extract_2024_01_01__0 ae        
       2 extract_2024_01_01__1 ae           3.5 extract_2024_01_01__1 ae        
       3 extract_2024_04_01    ae           3.5 extract_2024_04_01 ae           
       4 extract_2024_01_01__0 data1        2   extract_2024_01_01__0 data1     
       5 extract_2024_01_01__1 data1        2   extract_2024_01_01__1 data1     
       6 extract_2024_04_01    data1        2   extract_2024_04_01 data1        
       7 extract_2024_01_01__0 data2        1   extract_2024_01_01__0 data2     
       8 extract_2024_01_01__1 data2        1   extract_2024_01_01__1 data2     
       9 extract_2024_04_01    data2        1   extract_2024_04_01 data2        
      10 extract_2024_01_01__0 data3        1   extract_2024_01_01__0 data3     
      11 extract_2024_01_01__1 data3        1   extract_2024_01_01__1 data3     
      12 extract_2024_04_01    data3        1   extract_2024_04_01 data3        
      13 extract_2024_01_01__0 data99      NA   extract_2024_01_01__0 data99    
      14 extract_2024_01_01__1 data99       2   extract_2024_01_01__1 data99    
      15 extract_2024_04_01    data99       2   extract_2024_04_01 data99       
      16 extract_2024_01_01__0 enrol        1   extract_2024_01_01__0 enrol     
      17 extract_2024_01_01__1 enrol        1   extract_2024_01_01__1 enrol     
      18 extract_2024_04_01    enrol        1   extract_2024_04_01 enrol        
      19 extract_2024_01_01__0 long_mixed   2   extract_2024_01_01__0 long_mixed
      20 extract_2024_01_01__1 long_mixed   2   extract_2024_01_01__1 long_mixed
      21 extract_2024_04_01    long_mixed   2   extract_2024_04_01 long_mixed   
      22 extract_2024_01_01__0 long_pure    3   extract_2024_01_01__0 long_pure 
      23 extract_2024_01_01__1 long_pure    3   extract_2024_01_01__1 long_pure 
      24 extract_2024_04_01    long_pure    3   extract_2024_04_01 long_pure    
      25 extract_2024_01_01__0 short        1   extract_2024_01_01__0 short     
      26 extract_2024_01_01__1 short        1   extract_2024_01_01__1 short     
      27 extract_2024_04_01    short        1   extract_2024_04_01 short        

