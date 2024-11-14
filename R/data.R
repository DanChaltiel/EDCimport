
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


#' Example databases
#' 
#' List of tables used in EDCimport examples: 
#' \itemize{
#'   \item `edc_example()` can be used as the result of [read_trialmaster()]
#'   \item `edc_example_plot()` can be used to test [edc_swimmerplot()] 
#'   \item `edc_example_mixed()` can be used to test [split_mixed_datasets()]
#' }
#'
#' @param N the number of patients
#' @param seed the random seed
#'
#' @return a list of tables
#' @export
#' @name data_example
#' @rdname data_example
#' @importFrom dplyr lst mutate
#' @importFrom purrr imap
#' @importFrom stats rnorm
#' @importFrom tibble tibble
edc_example_mixed = function(N=100, seed=42){
  set.seed(seed)
  
  short = tibble(SUBJID=1:N, crfname="short data", val1=rnorm(N), val2=rnorm(N)+10)
  
  long_pure = tibble(SUBJID=rep(1:N, each=3), crfname="long data", val1a=rnorm(3*N), val2a=rnorm(3*N)+10)
  
  long_mixed = tibble(SUBJID=rep(1:N, each=2), crfname="both short and long data", val1b=rnorm(2*N), 
                      val2b=rnorm(2*N)+10, val3b=LETTERS[SUBJID%%26+1])
  
  rtn = lst(short, long_pure, long_mixed) %>% 
    imap(~.x %>% mutate(crfname=.y))
  rtn$date_extraction = "2022-08-25"
  rtn$datetime_extraction = as.POSIXct("2022-08-25 15:16:00 CEST")
  rtn$.lookup=build_lookup(rtn)
  .set_lookup(rtn$.lookup)
  rtn
}


#' @rdname data_example
#' @export
#' @importFrom dplyr lst mutate n select
#' @importFrom purrr imap
#' @importFrom stats rnorm runif
#' @importFrom tibble tibble
edc_example = function(N=50, seed=42){
  set.seed(seed)
  start = ISOdate(2010, 04, 13)
  day = 3600*24
  db = tibble(SUBJID=1:N, age=rnorm(N, 50, 10), date_naissance=start-age*day)
  attr(db$SUBJID, "label") = "Subject ID"
  attr(db$age, "label")    = "Age (years)"
  attr(db$date_naissance, "label") = "Date of birth"
  
  for(i in 1:10){
    db[[paste0("date",i)]] = (start+rnorm(N, i*10, 10)*day) %>% 
      set_label(paste0("Date at visit ",i))
  }
  
  db0 = db %>% select(SUBJID, 1:3) %>%
    mutate(group = ifelse(runif(n()) > 0.5, "A", "B") %>% set_label("Treatment"))
  db1 = db %>% select(SUBJID, 4:6) %>%
    mutate(x = ifelse(runif(n()) > 0.5, "X", "Y") %>% set_label("Covariate"))
  db1 = rbind(db1, db1)
  db2 = db %>% select(SUBJID, 7:9)
  db3 = db %>% select(SUBJID, 10:13)
  
  .lookup = tibble(dataset=paste0("db", 0:3))
  # rtn$.lookup=build_lookup(rtn) %>% extend_lookup()
  rtn = lst(db0, db1, db2, db3) %>% 
    imap(~.x %>% mutate(crfname=.y %>% set_label("Form name")))
  # rtn$.lookup = build_lookup(rtn) %>% extend_lookup()
  rtn$date_extraction = "2024/01/01"
  rtn$datetime_extraction = structure(1704067200, class = c("POSIXct", "POSIXt"), 
                                      tzone = "Europe/Paris")
  rtn$.lookup=build_lookup(rtn)
  .set_lookup(rtn$.lookup)
  rtn
}


#' @rdname data_example
#' @export
edc_example_plot = edc_example


#' @rdname data_example
#' @export
#' @importFrom dplyr lst mutate n select
#' @importFrom purrr imap map
#' @importFrom stats rbinom runif
#' @importFrom tibble tibble
#' @importFrom tidyr unnest
edc_example_ae = function(N=50, seed=42){
  set.seed(seed)
  
  enrolres = tibble(subjid=1:N, arm=sample(c("Trt", "Ctl"), size=N, replace=TRUE))
  
  ae = tibble(subjid=1:N, n_ae=rbinom(n=N, size=15, prob=0.2)) %>% 
    mutate(x = map(n_ae, ~seq_len(.x))) %>% 
    unnest(x) %>% 
    mutate(
      aegr = sample(1:5, size=n(), replace=TRUE, prob=c(0.3,0.25,0.2,0.1,0.05)) %>% set_label("AE grade"),
      aesoc = sample(sample_soc, size=n(), replace=TRUE) %>% set_label("AE SOC"),
      sae = fct_yesno(runif(n())<0.1) %>% set_label("Serious AE"),
    ) %>% 
    select(subjid, aesoc, aegr, n_ae, sae)
  
  rtn = lst(enrolres, ae) %>% 
    imap(~.x %>% mutate(crfname=.y %>% set_label("Form name")))
  rtn$.lookup = build_lookup(rtn)
  .set_lookup(rtn$.lookup)
  rtn
}
