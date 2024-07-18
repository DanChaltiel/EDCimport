# population_plot() works

    Code
      edc_population_plot(l)$data %>% summary()
    Output
           SUBJID                        name     value           group    
       Min.   :  1.0   Evaluable population:179   Yes:885   (0,50]   :250  
       1st Qu.: 45.0   mITT population     :179   No : 10   (50,100] :245  
       Median : 91.0   Safety population   :179             (100,150]:250  
       Mean   : 90.7   ITT population      :179             (150,200]:150  
       3rd Qu.:136.0   Total population    :179                            
       Max.   :180.0                                                       
    Code
      edc_population_plot(l[-1], ref = pop_total)$data %>% summary()
    Output
           SUBJID                        name     value           group    
       Min.   :  1.0   Evaluable population:179   Yes:706   (0,50]   :200  
       1st Qu.: 45.0   mITT population     :179   No : 10   (50,100] :196  
       Median : 91.0   Safety population   :179             (100,150]:200  
       Mean   : 90.7   ITT population      :179             (150,200]:120  
       3rd Qu.:136.0                                                       
       Max.   :180.0                                                       
    Code
      edc_population_plot(l, ref = 1:200)$data %>% summary()
    Output
           SUBJID                         name     value           group    
       Min.   :  1.00   Evaluable population:200   Yes:885   (0,50]   :250  
       1st Qu.: 50.75   mITT population     :200   No :115   (50,100] :250  
       Median :100.50   Safety population   :200             (100,150]:250  
       Mean   :100.50   ITT population      :200             (150,200]:250  
       3rd Qu.:150.25   Total population    :200                            
       Max.   :200.00                                                       
    Code
      edc_population_plot(l, id_per_row = 60)$data %>% summary()
    Output
           SUBJID                        name     value           group    
       Min.   :  1.0   Evaluable population:179   Yes:885   (0,60]   :295  
       1st Qu.: 45.0   mITT population     :179   No : 10   (60,120] :300  
       Median : 91.0   Safety population   :179             (120,180]:300  
       Mean   : 90.7   ITT population      :179                            
       3rd Qu.:136.0   Total population    :179                            
       Max.   :180.0                                                       

