# compare_databases() works

    Code
      data_compare
    Output
      # A tibble: 9 x 4
        dataset    extract_2024_01_01 extract_2024_01_01_1 extract_2024_01_01_2
        <chr>      <chr>              <chr>                <chr>               
      1 ae         Added              +0 -0                +0 -0               
      2 data1      Added              +0 -2                +0 -1               
      3 data2      Added              +1 -1                +1 -1               
      4 data3      Added              +0 -0                +0 -0               
      5 data99     Absent             Added                +0 -2               
      6 enrol      Added              +2 -0                +2 -0               
      7 long_mixed Added              +0 -0                +0 -0               
      8 long_pure  Added              +0 -0                +0 -0               
      9 short      Added              +0 -0                +0 -0               

