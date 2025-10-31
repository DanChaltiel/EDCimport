


mutate_list = function(.x, ...) {
  dots = rlang::enquos(...)
  for (nm in names(dots)) {
    .x[[nm]] = rlang::eval_tidy(dots[[nm]], data = .x)
  }
  .x
}



if(!is_testing() && !is_checking()){
  
  

# tm = read_atezolacc()
# tm = read_hrnbl2_multi()
#   
# # tm2 = edc_split_mixed(tm, rc, by=c(SUBJID, RCDT))
# # tm2 = edc_split_mixed(tm, rc)
# 
# load_database(tm, remove=FALSE, env=rlang::global_env())
# load_database(tm, remove=FALSE)


# edc_viewer()

# edc_viewer(background=F, title="prout")
# edc_viewer(lst(mtcars, iris), background=F, port=1212)
# edc_viewer(lst(mtcars, iris), port=1209)
# 
# print(edcimport_env$viewers)
# .edc_viewer_kill()
}


# options(edc_lookup_overwrite_warn=FALSE)
# db = edc_example()
# load_database(db)
# 
# 
# enrol2 = enrol %>% rename(SUBJID=subjid)
# short %>% 
#   rename(SUBJID=subjid) %>% 
#   edc_left_join(enrol) %>% 
#   names() %>% 
#   expect_contains(c("SUBJID", "val1", "enrol_date", "crfname_enrol"))




# fixed regression in .edc_join 
# cannot select by_y if it doesn't exist
