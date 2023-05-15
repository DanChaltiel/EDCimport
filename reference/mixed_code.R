## `long_pure` (dim=300x4) ---- 

long_pure_short = long_pure %>% 
  select(SUBJID, crfname) %>% 
  group_by(SUBJID) %>% 
  summarise(across(everything(), unify)) #dim=100x2 

 long_pure_long = long_pure %>% 
  select(SUBJID, val1, val2) #dim=300x3


## `long_mixed` (dim=200x5) ---- 

long_mixed_short = long_mixed %>% 
  select(SUBJID, crfname, val3) %>% 
  group_by(SUBJID) %>% 
  summarise(across(everything(), unify)) #dim=100x3 

 long_mixed_long = long_mixed %>% 
  select(SUBJID, val1, val2) #dim=200x3