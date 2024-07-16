# fct_yesno() works

    Code
      fct_yesno("Yes")
    Output
      [1] Yes
      Levels: Yes No
    Code
      fct_yesno(c("No", "Yes"))
    Output
      [1] No  Yes
      Levels: Yes No
    Code
      mutate_all(x, fct_yesno, fail = FALSE)
    Output
      # A tibble: 20 x 7
         eng   fra   bin   log   eng2  chr     num
         <fct> <fct> <fct> <fct> <fct> <chr> <int>
       1 Yes   Yes   No    Yes   No    bbb       1
       2 Yes   Yes   No    No    Yes   aaa       2
       3 Yes   Yes   Yes   No    No    bbb       3
       4 Yes   Yes   Yes   No    Yes   aaa       4
       5 No    Yes   Yes   No    Yes   bbb       5
       6 No    No    Yes   No    No    ccc       6
       7 No    Yes   Yes   No    No    aaa       7
       8 No    Yes   Yes   Yes   Yes   aaa       8
       9 Yes   Yes   Yes   No    Yes   ccc       9
      10 <NA>  <NA>  <NA>  <NA>  <NA>  <NA>     NA
      11 <NA>  <NA>  <NA>  <NA>  <NA>  <NA>     NA
      12 No    No    No    Yes   No    ccc      12
      13 Yes   No    Yes   No    Yes   aaa      13
      14 No    No    Yes   No    No    aaa      14
      15 Yes   Yes   Yes   No    No    bbb      15
      16 Yes   No    Yes   No    No    ccc      16
      17 No    Yes   No    Yes   Yes   bbb      17
      18 No    No    Yes   Yes   No    aaa      18
      19 No    No    No    Yes   No    aaa      19
      20 No    No    No    Yes   No    aaa      20
    Code
      mutate_all(x, fct_yesno, fail = FALSE, strict = TRUE)
    Output
      # A tibble: 20 x 7
         eng   fra   bin   log   eng2  chr     num
         <fct> <fct> <fct> <fct> <chr> <chr> <int>
       1 Yes   Yes   No    Yes   0-No  bbb       1
       2 Yes   Yes   No    No    1-Yes aaa       2
       3 Yes   Yes   Yes   No    0-No  bbb       3
       4 Yes   Yes   Yes   No    1-Yes aaa       4
       5 No    Yes   Yes   No    1-Yes bbb       5
       6 No    No    Yes   No    0-No  ccc       6
       7 No    Yes   Yes   No    0-No  aaa       7
       8 No    Yes   Yes   Yes   1-Yes aaa       8
       9 Yes   Yes   Yes   No    1-Yes ccc       9
      10 <NA>  <NA>  <NA>  <NA>  <NA>  <NA>     NA
      11 <NA>  <NA>  <NA>  <NA>  <NA>  <NA>     NA
      12 No    No    No    Yes   0-No  ccc      12
      13 Yes   No    Yes   No    1-Yes aaa      13
      14 No    No    Yes   No    0-No  aaa      14
      15 Yes   Yes   Yes   No    0-No  bbb      15
      16 Yes   No    Yes   No    0-No  ccc      16
      17 No    Yes   No    Yes   1-Yes bbb      17
      18 No    No    Yes   Yes   0-No  aaa      18
      19 No    No    No    Yes   0-No  aaa      19
      20 No    No    No    Yes   0-No  aaa      20
    Code
      mutate_all(x, fct_yesno, fail = FALSE, input = list(yes = "Ja", no = "Nein"))
    Output
      # A tibble: 20 x 7
         eng   fra   bin   log   eng2  chr     num
         <chr> <chr> <fct> <fct> <chr> <chr> <int>
       1 Yes   Oui   No    Yes   0-No  bbb       1
       2 Yes   Oui   No    No    1-Yes aaa       2
       3 Yes   Oui   Yes   No    0-No  bbb       3
       4 Yes   Oui   Yes   No    1-Yes aaa       4
       5 No    Oui   Yes   No    1-Yes bbb       5
       6 No    Non   Yes   No    0-No  ccc       6
       7 No    Oui   Yes   No    0-No  aaa       7
       8 No    Oui   Yes   Yes   1-Yes aaa       8
       9 Yes   Oui   Yes   No    1-Yes ccc       9
      10 <NA>  <NA>  <NA>  <NA>  <NA>  <NA>     NA
      11 <NA>  <NA>  <NA>  <NA>  <NA>  <NA>     NA
      12 No    Non   No    Yes   0-No  ccc      12
      13 Yes   Non   Yes   No    1-Yes aaa      13
      14 No    Non   Yes   No    0-No  aaa      14
      15 Yes   Oui   Yes   No    0-No  bbb      15
      16 Yes   Non   Yes   No    0-No  ccc      16
      17 No    Oui   No    Yes   1-Yes bbb      17
      18 No    Non   Yes   Yes   0-No  aaa      18
      19 No    Non   No    Yes   0-No  aaa      19
      20 No    Non   No    Yes   0-No  aaa      20

