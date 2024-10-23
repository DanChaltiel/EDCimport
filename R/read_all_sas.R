
read_all_sas = function(path, ..., 
                        format_file=NULL, 
                        clean_names_fun=NULL, 
                        split_mixed=FALSE,
                        extend_lookup=TRUE,
                        datetime_extraction="guess", 
                        verbose=getOption("edc_read_verbose", 1),
                        directory="deprecated",
                        key_columns="deprecated"){
  check_dots_empty()
  reset_manual_correction()
  if(missing(path)) path = directory
  assert(is_dir(path))
  
  if(identical(datetime_extraction, "guess") || is.null(datetime_extraction)){
    datetime_extraction = get_folder_datetime(path, verbose=verbose)
  }
  assert_class(datetime_extraction, c("POSIXt", "Date"))
  
  format_file = .locate_file(format_file, path)
  catalog_file = NULL
  
  if(path_ext(format_file)=="sas7bcat") catalog_file=format_file
  
  rtn = dir_ls(path, regexp="\\.sas7bdat$") %>% 
    .read_all(haven::read_sas, clean_names_fun=clean_names_fun, 
              catalog_file=catalog_file) %>%
    .clean_labels_utf8() %>% 
    .add_lookup_and_date(
      datetime_extraction=datetime_extraction,
      extend_lookup=extend_lookup,
      clean_names_fun=.get_clean_names_fun(clean_names_fun), 
      split_mixed=split_mixed,
      EDCimport_version=packageVersion("EDCimport")
    )
  
  if(path_ext(format_file) %in% c("sas", "sas7bdat", "csv")){
    rtn = rtn %>% 
      .apply_sas_formats(format_file, path)
  }
  
  .warn_bad_tables(rtn)
  .warn_bad_columns(rtn)
  .set_lookup(rtn$.lookup)
  
  class(rtn) = "tm_database"
  rtn
}