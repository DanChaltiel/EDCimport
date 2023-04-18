
# swimmerplot ---------------------------------------------------------------------------------


test_that("swimmerplot", {
  set.seed(42)
  N = 50
  start = ISOdate(2010, 04, 13)
  day=3600*24
  db = tibble(SUBJID=1:N, age=rnorm(N, 50, 10), date_naissance=start-age*day)
  
  for(i in 1:10){
    db[[paste0("date",i)]] = start+rnorm(N, i*10, 10)*day 
  }
  
  db0=db %>% select(SUBJID, 1:3)
  db1=db %>% select(SUBJID, 4:6)
  db2=db %>% select(SUBJID, 7:9)
  db3=db %>% select(SUBJID, 10:13)
  
  .lookup = tibble(dataset=paste0("db", 0:3))
  
  p = swimmerplot(.lookup, plotly=FALSE)
  vdiffr::expect_doppelganger("swimmerplot", p)
})

