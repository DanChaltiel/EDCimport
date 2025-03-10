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
       1 Yes   No    Yes   No    Yes   ccc       1
       2 Yes   Yes   Yes   No    <NA>  aaa       2
       3 Yes   Yes   Yes   Yes   <NA>  aaa       3
       4 Yes   Yes   Yes   No    Yes   bbb       4
       5 No    Yes   No    Yes   Yes   ccc       5
       6 No    No    Yes   No    No    bbb       6
       7 No    No    No    Yes   Yes   aaa       7
       8 Yes   No    Yes   No    No    aaa       8
       9 <NA>  No    Yes   No    No    aaa       9
      10 <NA>  <NA>  <NA>  <NA>  <NA>  <NA>     NA
      11 <NA>  <NA>  <NA>  <NA>  <NA>  <NA>     NA
      12 Yes   Yes   No    Yes   No    bbb      12
      13 No    No    Yes   Yes   Yes   ccc      13
      14 No    No    No    Yes   No    aaa      14
      15 No    No    No    Yes   <NA>  bbb      15
      16 <NA>  Yes   No    No    Yes   aaa      16
      17 <NA>  Yes   Yes   Yes   Yes   ccc      17
      18 Yes   No    Yes   No    <NA>  bbb      18
      19 Yes   No    Yes   Yes   No    aaa      19
      20 <NA>  No    Yes   Yes   No    aaa      20
    Code
      mutate_all(x, fct_yesno, fail = FALSE, strict = TRUE)
    Output
      # A tibble: 20 x 7
         eng   fra   bin   log   eng2  chr     num
         <fct> <fct> <fct> <fct> <chr> <chr> <int>
       1 Yes   No    Yes   No    1-Yes ccc       1
       2 Yes   Yes   Yes   No    2-NA  aaa       2
       3 Yes   Yes   Yes   Yes   2-NA  aaa       3
       4 Yes   Yes   Yes   No    1-Yes bbb       4
       5 No    Yes   No    Yes   1-Yes ccc       5
       6 No    No    Yes   No    0-No  bbb       6
       7 No    No    No    Yes   1-Yes aaa       7
       8 Yes   No    Yes   No    0-No  aaa       8
       9 <NA>  No    Yes   No    0-No  aaa       9
      10 <NA>  <NA>  <NA>  <NA>  <NA>  <NA>     NA
      11 <NA>  <NA>  <NA>  <NA>  <NA>  <NA>     NA
      12 Yes   Yes   No    Yes   0-No  bbb      12
      13 No    No    Yes   Yes   1-Yes ccc      13
      14 No    No    No    Yes   0-No  aaa      14
      15 No    No    No    Yes   2-NA  bbb      15
      16 <NA>  Yes   No    No    1-Yes aaa      16
      17 <NA>  Yes   Yes   Yes   1-Yes ccc      17
      18 Yes   No    Yes   No    2-NA  bbb      18
      19 Yes   No    Yes   Yes   0-No  aaa      19
      20 <NA>  No    Yes   Yes   0-No  aaa      20
    Code
      mutate_all(x, fct_yesno, fail = FALSE, input = list(yes = "Ja", no = "Nein"))
    Output
      # A tibble: 20 x 7
         eng   fra   bin   log   eng2  chr     num
         <chr> <chr> <fct> <fct> <chr> <chr> <int>
       1 Yes   Non   Yes   No    1-Yes ccc       1
       2 Yes   Oui   Yes   No    <NA>  aaa       2
       3 Yes   Oui   Yes   Yes   <NA>  aaa       3
       4 Yes   Oui   Yes   No    1-Yes bbb       4
       5 No    Oui   No    Yes   1-Yes ccc       5
       6 No    Non   Yes   No    0-No  bbb       6
       7 No    Non   No    Yes   1-Yes aaa       7
       8 Yes   Non   Yes   No    0-No  aaa       8
       9 <NA>  Non   Yes   No    0-No  aaa       9
      10 <NA>  <NA>  <NA>  <NA>  <NA>  <NA>     NA
      11 <NA>  <NA>  <NA>  <NA>  <NA>  <NA>     NA
      12 Yes   Oui   No    Yes   0-No  bbb      12
      13 No    Non   Yes   Yes   1-Yes ccc      13
      14 No    Non   No    Yes   0-No  aaa      14
      15 No    Non   No    Yes   <NA>  bbb      15
      16 <NA>  Oui   No    No    1-Yes aaa      16
      17 <NA>  Oui   Yes   Yes   1-Yes ccc      17
      18 Yes   Non   Yes   No    <NA>  bbb      18
      19 Yes   Non   Yes   Yes   0-No  aaa      19
      20 <NA>  Non   Yes   Yes   0-No  aaa      20

