# Read a TM archive

    Code
      filename = test_path("CRF_Dan_Export_SAS_XPORT_2022_08_25_15_16.zip")
      w = read_trialmaster(filename, use_cache = "write", verbose = 9)
    Message
      Unzipping 'CRF_Dan_Export_SAS_XPORT_2022_08_25_15_16.zip'
      7-Zip Copyright 1999-2016 Igor Pavlov
      
      Scanning the drive for archives:
      1 file, 13576 bytes (14 KiB)
      
      Extracting archive: CRF_Dan_Export_SAS_XPORT_2022_08_25_15_16.zip
      --
      Path = CRF_Dan_Export_SAS_XPORT_2022_08_25_15_16.zip
      Type = zip
      Physical Size = 13576
      
      Everything is Ok
      
      Files: 6
      Size: 61533
      Compressed: 13576
      Writing cache file './trialmaster_export_2022-08-25 15h16.rds'
      v Database loaded: 8 tables, 000 Kb
    Code
      w$datetime_extraction
    Output
      [1] "2022-08-25 15:16:00 CEST"
    Code
      w$.lookup
    Output
      -- Lookup table - CRF (extraction of 2022-08-25) - EDCimport v0.0.0 ----------------------------------------------------
        dataset  nrow  ncol  n_id rows_per_id crfname                       
        <chr>   <dbl> <dbl> <int>       <dbl> <chr>                         
      1 site        1    33    NA          NA Trial Site Setup & Information
      2 trial       1    23    NA          NA Trial                         
      3 pat         0    35    NA          NA <NA>                          
      4 visit       0    27    NA          NA <NA>                          
      5 vs          0    31    NA          NA <NA>                          
    Code
      lu = edc_lookup()
      expect_identical(lu, w$.lookup)
      df_list = w %>% keep(is.data.frame) %>% discard_at(".lookup")
      df_list %>% map(~ {
        .x %>% get_label() %>% unlist() %>% tibble::enframe()
      })
    Output
      $pat
      # A tibble: 35 x 2
         name     value                                   
         <chr>    <chr>                                   
       1 TRNAME   Trial Name                              
       2 TRNO     Trial No                                
       3 TRCAP    Trial Caption                           
       4 STNAME   Site Name                               
       5 STNO     Site No                                 
       6 SITEC    Site Caption                            
       7 CRFNAME  Form Name                               
       8 CRFINSNO Form Instance                           
       9 CRFSTAT  Form Status                             
      10 GRPINSNO Group Instance                          
      11 DATAMIN  Minimum Data Timestamp                  
      12 DATAMAX  Maximum Data Timestamp                  
      13 STATMIN  Minimum Status Change Timestamp         
      14 STATMAX  Maximum Status Change Timestamp         
      15 FORMCAP  The Form caption as defined in TrialBuil
      16 FORMDESC The Form description, as defined in Tria
      17 BITMASK  The state bitmask for the form, which ex
      18 REFUSLAB Message from promotor                   
      19 HCHECK   Hard check                              
      20 FNAME    First name initial                      
      21 LNAME    Last name initial                       
      22 PTNO     Patient Number                          
      23 DNAIS    Birthdate                               
      24 SEX      Sex                                     
      25 PATIEN   Patient Caption                         
      26 PTACTV   Active                                  
      27 PTDROP   Dropped                                 
      28 PTENRL   Enrolled                                
      29 RGSTDT   Registration Date                       
      30 PTERDT   Enroll Date                             
      31 PTRNNO   Randomization Number                    
      32 PTRNDT   Randomization Date                      
      33 CHECKDUP Check of duplicate patient              
      34 OVERDUP  Overrule duplicate check (DM only)      
      35 CHECKDU1 Check of duplicate patient              
      
      $site
      # A tibble: 33 x 2
         name     value                                   
         <chr>    <chr>                                   
       1 TRNAME   Trial Name                              
       2 TRNO     Trial No                                
       3 TRCAP    Trial Caption                           
       4 STNAME   Site Name                               
       5 STNO     Site No                                 
       6 SITEC    Site Caption                            
       7 CRFNAME  Form Name                               
       8 CRFINSNO Form Instance                           
       9 CRFSTAT  Form Status                             
      10 GRPINSNO Group Instance                          
      11 DATAMIN  Minimum Data Timestamp                  
      12 DATAMAX  Maximum Data Timestamp                  
      13 STATMIN  Minimum Status Change Timestamp         
      14 STATMAX  Maximum Status Change Timestamp         
      15 FORMCAP  The Form caption as defined in TrialBuil
      16 FORMDESC The Form description, as defined in Tria
      17 BITMASK  The state bitmask for the form, which ex
      18 STADDA   Address 1                               
      19 STADDB   Address 2                               
      20 STCITY   City                                    
      21 STATE    State                                   
      22 STZIP    Postal Code                             
      23 STCTRY   Country                                 
      24 STTMZN   Time Zone                               
      25 STCFN    Contact First Name                      
      26 STCLN    Contact Last Name                       
      27 STCPH    Phone                                   
      28 TGENRL   Target Enroll                           
      29 TRGTDT   Target Enroll Date                      
      30 INCLSITE Authorisation of inclusion site         
      31 INVSYS   Investigator names list                 
      32 DESACINV Investigator inactive                   
      33 INVDAT   Deactivation date                       
      
      $trial
      # A tibble: 23 x 2
         name     value                                   
         <chr>    <chr>                                   
       1 TRNAME   Trial Name                              
       2 TRNO     Trial No                                
       3 CRFNAME  Form Name                               
       4 CRFINSNO Form Instance                           
       5 CRFSTAT  Form Status                             
       6 GRPINSNO Group Instance                          
       7 DATAMIN  Minimum Data Timestamp                  
       8 DATAMAX  Maximum Data Timestamp                  
       9 STATMIN  Minimum Status Change Timestamp         
      10 STATMAX  Maximum Status Change Timestamp         
      11 FORMCAP  The Form caption as defined in TrialBuil
      12 FORMDESC The Form description, as defined in Tria
      13 BITMASK  The state bitmask for the form, which ex
      14 TRIALC   Trial Caption                           
      15 TRSPON   Sponsor                                 
      16 TRSTDT   Start Date                              
      17 TRSPDT   End Date                                
      18 TREFN    Email From Name                         
      19 TREFA    Email From Address                      
      20 TREXPP   Export Password                         
      21 TRTMZN   Trial Time Zone                         
      22 TRMSG    Message of the Day                      
      23 INCLTRIA Authorisation of inclusion              
      
      $visit
      # A tibble: 27 x 2
         name     value                                   
         <chr>    <chr>                                   
       1 TRNAME   Trial Name                              
       2 TRNO     Trial No                                
       3 TRCAP    Trial Caption                           
       4 STNAME   Site Name                               
       5 STNO     Site No                                 
       6 SITEC    Site Caption                            
       7 SUBJINIT Patient Initials                        
       8 SUBJID   Patient No                              
       9 VISITN   Visit Name                              
      10 CRFNAME  Form Name                               
      11 CRFINSNO Form Instance                           
      12 CRFSTAT  Form Status                             
      13 GRPINSNO Group Instance                          
      14 DATAMIN  Minimum Data Timestamp                  
      15 DATAMAX  Maximum Data Timestamp                  
      16 STATMIN  Minimum Status Change Timestamp         
      17 STATMAX  Maximum Status Change Timestamp         
      18 FORMCAP  The Form caption as defined in TrialBuil
      19 FORMDESC The Form description, as defined in Tria
      20 BITMASK  The state bitmask for the form, which ex
      21 VISIT1   Visit Number                            
      22 VISIT2   Visit Date                              
      23 EEXPVDT  Earliest Expected Visit Date            
      24 LEXPVDT  Latest Expected Visit Date              
      25 EXPVDT   Expected Visit Date                     
      26 VISIT3   Visit Caption                           
      27 PRV      PRV                                     
      
      $vs
      # A tibble: 31 x 2
         name     value                                   
         <chr>    <chr>                                   
       1 TRNAME   Trial Name                              
       2 TRNO     Trial No                                
       3 TRCAP    Trial Caption                           
       4 STNAME   Site Name                               
       5 STNO     Site No                                 
       6 SITEC    Site Caption                            
       7 SUBJINIT Patient Initials                        
       8 SUBJID   Patient No                              
       9 VISITN   Visit Name                              
      10 VISITNO  Visit No                                
      11 VISITDT  Visit Date                              
      12 CRFNAME  Form Name                               
      13 CRFINSNO Form Instance                           
      14 CRFSTAT  Form Status                             
      15 GRPINSNO Group Instance                          
      16 DATAMIN  Minimum Data Timestamp                  
      17 DATAMAX  Maximum Data Timestamp                  
      18 STATMIN  Minimum Status Change Timestamp         
      19 STATMAX  Maximum Status Change Timestamp         
      20 FORMCAP  The Form caption as defined in TrialBuil
      21 FORMDESC The Form description, as defined in Tria
      22 BITMASK  The state bitmask for the form, which ex
      23 VSYN     Clinical Examination done ?             
      24 VSDT     Date of clinical assessment             
      25 HEIGHT   Height (cm)                             
      26 WEIGHT   WEIGHT (kg)                             
      27 VSPS     PS (performance status measured using th
      28 VSSYS    Systolic blood pressure (mmHG)          
      29 VSDIAG   Diastolic blood pressure (mmHG)         
      30 ECG      ECG                                     
      31 ECGDAT   Date of ECG                             
      
    Code
      df_list %>% map(~ {
        .x %>% select(where(is.factor)) %>% map(~ head(levels(.x), 3))
      })
    Output
      $pat
      $pat$SEX
      [1] "Male"   "Female"
      
      $pat$PTACTV
      [1] "NO"  "YES"
      
      $pat$PTDROP
      [1] "NO"  "YES"
      
      $pat$PTENRL
      [1] "NO"  "YES"
      
      $pat$OVERDUP
      [1] "No"  "Yes"
      
      
      $site
      $site$STCTRY
      [1] "AFGHANISTAN"   "ALAND ISLANDS" "ALBANIA"      
      
      $site$STTMZN
      [1] "(GMT+00:00) Dublin, Edinburgh, Lisbon, London" "(GMT+00:00) Monrovia, Reykjavik"              
      [3] "(GMT+00:00) Sao Tome"                         
      
      $site$INCLSITE
      [1] "No"  "Yes"
      
      
      $trial
      $trial$TRTMZN
      [1] "(GMT+00:00) Dublin, Edinburgh, Lisbon, London" "(GMT+00:00) Monrovia, Reykjavik"              
      [3] "(GMT+00:00) Sao Tome"                         
      
      $trial$INCLTRIA
      [1] "No"  "Yes"
      
      
      $visit
      named list()
      
      $vs
      $vs$VSYN
      [1] "Not done" "Done"    
      
      $vs$ECG
      [1] "Not done" "Done"    
      
      

