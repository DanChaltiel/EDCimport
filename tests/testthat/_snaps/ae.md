# AE tables snapshots

    Code
      tm = edc_example_ae()
      ae_table_grade_max(df_ae = tm$ae, df_enrol = tm$enrolres)
    Output
      # A tibble: 7 x 6
        .id       label     variable Ctl      Trt      Total    
        <chr>     <chr>     <chr>    <chr>    <chr>    <chr>    
      1 grade_max Max grade Grade 0  0 (0%)   3 (13%)  3 (6%)   
      2 grade_max Max grade Grade 1  3 (11%)  1 (4%)   4 (8%)   
      3 grade_max Max grade Grade 2  5 (19%)  5 (22%)  10 (20%) 
      4 grade_max Max grade Grade 3  10 (37%) 8 (35%)  18 (36%) 
      5 grade_max Max grade Grade 4  8 (30%)  1 (4%)   9 (18%)  
      6 grade_max Max grade Grade 5  1 (4%)   5 (22%)  6 (12%)  
      7 grade_max Max grade Total    27 (54%) 23 (46%) 50 (100%)
    Code
      ae_table_grade_max(df_ae = tm$ae, df_enrol = tm$enrolres, arm = NULL)
    Output
      # A tibble: 7 x 4
        .id       label     variable value    
        <chr>     <chr>     <chr>    <chr>    
      1 grade_max Max grade Grade 0  3 (6%)   
      2 grade_max Max grade Grade 1  4 (8%)   
      3 grade_max Max grade Grade 2  10 (20%) 
      4 grade_max Max grade Grade 3  18 (36%) 
      5 grade_max Max grade Grade 4  9 (18%)  
      6 grade_max Max grade Grade 5  6 (12%)  
      7 grade_max Max grade Total    50 (100%)
    Code
      ae_table_grade_n(df_ae = tm$ae, df_enrol = tm$enrolres)
    Output
      # A tibble: 5 x 5
        .id   label    variable Ctl      Trt     
        <chr> <chr>    <chr>    <chr>    <chr>   
      1 grade AE grade Grade 1  15 (56%) 13 (57%)
      2 grade AE grade Grade 2  17 (63%) 8 (35%) 
      3 grade AE grade Grade 3  14 (52%) 13 (57%)
      4 grade AE grade Grade 4  8 (30%)  3 (13%) 
      5 grade AE grade Grade 5  1 (4%)   5 (22%) 
    Code
      ae_table_grade_n(df_ae = tm$ae, df_enrol = tm$enrolres, arm = NULL)
    Output
      # A tibble: 5 x 4
        .id   label    variable `All patients`
        <chr> <chr>    <chr>    <chr>         
      1 grade AE grade Grade 1  28 (56%)      
      2 grade AE grade Grade 2  25 (50%)      
      3 grade AE grade Grade 3  27 (54%)      
      4 grade AE grade Grade 4  11 (22%)      
      5 grade AE grade Grade 5  6 (12%)       
    Code
      ae_table_soc(df_ae = tm$ae, df_enrol = tm$enrolres, term = NULL)
    Output
      # A tibble: 23 x 13
         soc    ctl_G1 ctl_G2 ctl_G3 ctl_G4 ctl_G5 ctl_Tot trt_G1 trt_G2 trt_G3 trt_G4
         <chr>  <glue> <glue> <glue> <glue> <glue> <glue>  <glue> <glue> <glue> <glue>
       1 Injur~ 1 (4%) <NA>   1 (4%) 1 (4%) <NA>   3 (11%) 3 (13~ 1 (4%) 1 (4%) <NA>  
       2 Neopl~ 2 (7%) 1 (4%) <NA>   <NA>   <NA>   3 (11%) 2 (9%) <NA>   2 (9%) <NA>  
       3 Nervo~ 1 (4%) 1 (4%) 3 (11~ <NA>   <NA>   5 (19%) <NA>   <NA>   3 (13~ <NA>  
       4 Eye d~ 2 (7%) <NA>   2 (7%) 1 (4%) <NA>   5 (19%) 1 (4%) 1 (4%) <NA>   <NA>  
       5 Hepat~ <NA>   2 (7%) <NA>   1 (4%) <NA>   3 (11%) 2 (9%) 1 (4%) 1 (4%) <NA>  
       6 Infec~ 2 (7%) 2 (7%) 1 (4%) <NA>   <NA>   5 (19%) <NA>   <NA>   1 (4%) <NA>  
       7 Skin ~ <NA>   <NA>   1 (4%) 1 (4%) <NA>   2 (7%)  <NA>   2 (9%) 2 (9%) 1 (4%)
       8 Ear a~ 1 (4%) <NA>   <NA>   <NA>   <NA>   1 (4%)  1 (4%) 1 (4%) 1 (4%) 2 (9%)
       9 Repro~ 2 (7%) 4 (15~ <NA>   <NA>   <NA>   6 (22%) <NA>   <NA>   <NA>   <NA>  
      10 Respi~ 2 (7%) <NA>   1 (4%) 1 (4%) <NA>   4 (15%) <NA>   <NA>   2 (9%) <NA>  
      # i 13 more rows
      # i 2 more variables: trt_G5 <glue>, trt_Tot <glue>
    Code
      ae_table_soc(df_ae = tm$ae, df_enrol = tm$enrolres, term = NULL, arm = NULL)
    Output
      # A tibble: 23 x 7
         soc                                all_G1 all_G2 all_G3 all_G4 all_G5 all_Tot
         <chr>                              <glue> <glue> <glue> <glue> <glue> <glue> 
       1 Injury, poisoning and procedural ~ 4 (8%) 1 (2%) 2 (4%) 1 (2%) 1 (2%) 9 (18%)
       2 Neoplasms benign, malignant and u~ 4 (8%) 1 (2%) 2 (4%) <NA>   1 (2%) 8 (16%)
       3 Nervous system disorders           1 (2%) 1 (2%) 6 (12~ <NA>   <NA>   8 (16%)
       4 Eye disorders                      3 (6%) 1 (2%) 2 (4%) 1 (2%) <NA>   7 (14%)
       5 Hepatobiliary disorders            2 (4%) 3 (6%) 1 (2%) 1 (2%) <NA>   7 (14%)
       6 Infections and infestations        2 (4%) 2 (4%) 2 (4%) <NA>   1 (2%) 7 (14%)
       7 Skin and subcutaneous tissue diso~ <NA>   2 (4%) 3 (6%) 2 (4%) <NA>   7 (14%)
       8 Ear and labyrinth disorders        2 (4%) 1 (2%) 1 (2%) 2 (4%) <NA>   6 (12%)
       9 Reproductive system and breast di~ 2 (4%) 4 (8%) <NA>   <NA>   <NA>   6 (12%)
      10 Respiratory, thoracic and mediast~ 2 (4%) <NA>   3 (6%) 1 (2%) <NA>   6 (12%)
      # i 13 more rows

