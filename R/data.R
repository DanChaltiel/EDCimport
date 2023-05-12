


#' Example databases
#' 
#' List of tables used in EDCimport examples.
#'
#' @param N the number of patients
#' @param seed the random seed
#'
#' @return a list of tables
#' @export
#' @name data_example
#' @rdname data_example
#' @importFrom stats rnorm
#' @importFrom tibble lst tibble
edc_example_mixed = function(N=100){
  
  short = tibble(SUBJID=1:N, crfname="short data", val1=rnorm(N), val2=rnorm(N)+10)
  
  long_pure = tibble(SUBJID=rep(1:N, each=3), crfname="long data", val1=rnorm(3*N), val2=rnorm(3*N)+10)
  
  long_mixed = tibble(SUBJID=rep(1:N, each=2), crfname="both short and long data", val1=rnorm(2*N), val2=rnorm(2*N)+10, 
                      val3=LETTERS[SUBJID%%26+1])
  
  rtn = lst(short, long_pure, long_mixed)
  rtn$date_extraction = "2022-08-25"
  rtn$datetime_extraction = as.POSIXct("2022-08-25 15:16:00 CEST")
  rtn$.lookup=get_lookup(rtn)
  rtn
}


#' @rdname data_example
#' @importFrom dplyr bind_rows mutate n select
#' @importFrom stats rnorm runif
#' @importFrom tibble lst tibble
edc_example_plot = function(N=50, seed=42){
  set.seed(seed)
  start = ISOdate(2010, 04, 13)
  day = 3600*24
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
  rtn = lst(db0, db1, db2, db3, .lookup)
}
