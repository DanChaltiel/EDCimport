

#' Example database
#' 
#' A list of tables that simulates the extraction of a clinical database. Used in `EDCimport` examples and tests.
#'
#' @param N the number of patients
#' @param seed the random seed
#' @param outdated whether to simulate times after the data extraction date
#'
#' @return A list of tables of class `edc_database`.
#' @export
edc_example = function(N=50, seed=42, outdated=FALSE){
  x1 = .example_dates(N, seed)
  x2 = .example_mixed(N, seed)
  x3 = .example_ae(N, seed)
  rtn = c(x1, x2, x3)
  
  ext1 = as.POSIXct("2024-01-01 01:00:00 CET")
  ext2 = as.POSIXct("2010-08-10 18:58:36 CET")
  rtn$datetime_extraction = if(isTRUE(outdated)) ext2 else ext1
  rtn$date_extraction = format(rtn$datetime_extraction, "%Y/%m/%d")
  
  rtn$.lookup = build_lookup(rtn)
  .set_lookup(rtn$.lookup)
  class(rtn) = "edc_database"
  rtn
}


#' @rdname edc_example
#' @export
#' @usage NULL
edc_example_plot = edc_example



# Utils ---------------------------------------------------------------------------------------


#' @noRd
#' @keywords internal
#' @importFrom dplyr lst mutate n select
#' @importFrom purrr imap
#' @importFrom stats rnorm runif
#' @importFrom tibble tibble
.example_dates = function(N, seed){
  set.seed(seed)
  start = ISOdate(2010, 04, 13, tz="CET")
  day = 3600*24
  db = tibble(subjid=1:N, age=rnorm(N, 50, 10), date_naissance=start-age*day)
  attr(db$subjid, "label") = "Subject ID"
  attr(db$age, "label")    = "Age (years)"
  attr(db$date_naissance, "label") = "Date of birth"
  
  for(i in 1:10){
    db[[paste0("date",i)]] = (start+rnorm(N, i*10, 10)*day) %>% 
      set_label(paste0("Date at visit ",i))
  }
  
  enrol = db %>% select(subjid, age, date_naissance) %>%
    mutate(arm = ifelse(runif(n()) > 0.5, "Trt", "Ctl") %>% set_label("Treatment arm"))
  db1 = db %>% select(subjid, 4:6) %>%
    mutate(x = ifelse(runif(n()) > 0.5, "X", "Y") %>% set_label("Covariate"))
  db1 = rbind(db1, db1) #long table
  db2 = db %>% select(subjid, 7:9)
  db3 = db %>% select(subjid, 10:13)
  
  #add ties for patients 1:3 (for `lastnews_table()`)
  db2$date4[1:2] = db3$date10[1:2]
  db2$date5[2:3] = db3$date10[2:3]
  
  lst(enrol, db1, db2, db3)%>% 
    imap(~.x %>% mutate(crfname=.y %>% set_label("Form name")))
}

#' @noRd
#' @keywords internal
#' @importFrom dplyr lst mutate
#' @importFrom purrr imap
#' @importFrom stats rnorm
#' @importFrom tibble tibble
.example_mixed = function(N, seed){
  set.seed(seed)
  
  short = tibble(subjid=1:N,
                 crfname="short data", 
                 val1=rnorm(N), val2=rnorm(N)+10)
  
  long_pure = tibble(subjid=rep(1:N, each=3), 
                     crfname="long data", 
                     val1a=rnorm(3*N), val2a=rnorm(3*N)+10)
  
  long_mixed = tibble(subjid=rep(1:N, each=2), 
                      crfname="both short and long data", 
                      val1b=rnorm(2*N), val2b=rnorm(2*N)+10, 
                      val3b=LETTERS[subjid%%26+1])
  
  attr(short$subjid, "label") = "Subject ID"
  attr(long_pure$subjid, "label") = "Subject ID"
  attr(long_mixed$subjid, "label") = "Subject ID"
  lst(short, long_pure, long_mixed)
}


#' @noRd
#' @keywords internal
#' @importFrom dplyr lst mutate
#' @importFrom purrr imap
#' @importFrom stats rnorm rbinom
#' @importFrom tibble tibble
#' @importFrom tidyr unnest
.example_ae = function(N, seed){
  set.seed(seed)
  
  ae = tibble(subjid=1:N, n_ae=rbinom(n=N, size=15, prob=0.2)) %>% 
    mutate(x = map(n_ae, ~seq_len(.x))) %>% 
    unnest(x) %>% 
    mutate(
      crfname = "Adverse events",
      aegr = sample(1:5, size=n(), replace=TRUE, prob=c(0.3,0.25,0.2,0.1,0.05)) %>% set_label("AE grade"),
      aesoc = sample(sample_soc, size=n(), replace=TRUE) %>% set_label("AE SOC"),
      sae = fct_yesno(runif(n())<0.1) %>% set_label("Serious AE"),
    ) %>% 
    select(subjid, crfname, aesoc, aegr, n_ae, sae)
  
  attr(ae$subjid, "label") = "Subject ID"
  lst(ae)
}


sample_soc = c(
  "Gastrointestinal disorders",
  "General disorders and administration site conditions",
  "Renal and urinary disorders",
  "Blood and lymphatic system disorders",
  "Reproductive system and breast disorders",
  "Infections and infestations",
  "Investigations",
  "Metabolism and nutrition disorders",
  "Skin and subcutaneous tissue disorders",
  "Ear and labyrinth disorders",
  "Nervous system disorders",
  "Musculoskeletal and connective tissue disorders",
  "Vascular disorders",
  "Endocrine disorders",
  "Respiratory, thoracic and mediastinal disorders",
  "Psychiatric disorders",
  "Hepatobiliary disorders",
  "Cardiac disorders",
  "Immune system disorders",
  "Injury, poisoning and procedural complications",
  "Eye disorders",
  "Neoplasms benign, malignant and unspecified (incl cysts and polyps)",
  "Surgical and medical procedures"
)
