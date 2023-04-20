
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
  
  db0=db %>% select(SUBJID, 1:3) %>% mutate(group=ifelse(runif(n())>0.5, "A", "B"))
  db1=db %>% select(SUBJID, 4:6) %>% mutate(x=ifelse(runif(n())>0.5, "X", "Y"))
  db1=bind_rows(db1, db1)
  db2=db %>% select(SUBJID, 7:9)
  db3=db %>% select(SUBJID, 10:13)
  
  .lookup = tibble(dataset=paste0("db", 0:3))
  
  p = swimmerplot(.lookup, plotly=FALSE)
  vdiffr::expect_doppelganger("swimmerplot", p)
  
  p2 = swimmerplot(.lookup, origin="db0$date_naissance", plotly=FALSE)
  vdiffr::expect_doppelganger("swimmerplot with origin", p2)
  
  p3 = swimmerplot(.lookup, group="db0$group", plotly=FALSE)
  vdiffr::expect_doppelganger("swimmerplot with group", p3)
  
  
  expect_error(swimmerplot(.lookup, origin="aaaaa", plotly=FALSE), 
               class="edc_swimplot_parse")
  expect_error(swimmerplot(.lookup, origin="xxx$date_naissance", plotly=FALSE), 
               class="edc_swimplot_parse_dataset")
  expect_error(swimmerplot(.lookup, origin="db0$date_xxxxxxxxx", plotly=FALSE), 
               class="edc_swimplot_parse_column")
  
  expect_error(swimmerplot(.lookup, group="aaaaa", plotly=FALSE), 
               class="edc_swimplot_parse")
  expect_error(swimmerplot(.lookup, group="db1$x", plotly=FALSE), 
               class="edc_swimplot_group_dup")
})

