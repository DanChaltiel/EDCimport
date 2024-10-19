


#' TODO Documentation
#'
#' @export
#' 
#' @importFrom utils read.csv2
#' @importFrom fs path_ext_remove
read_all_csv = function(path, ..., label_dict=NULL, 
                        read_fun=utils::read.csv2,
                        clean_names_fun=NULL, 
                        datetime_extraction="guess", 
                        verbose=getOption("edc_read_verbose", 1)){
  
  clean_names_fun = .get_clean_names_fun(clean_names_fun)
  files = dir_ls(path, regexp="\\.csv")
  rtn = files %>% 
    set_names(~path_ext_remove(basename(.x))) %>% 
    map(read_fun) %>% 
    map(as_tibble) %>% 
    map(clean_names_fun)
  if(!is.null(label_dict)){
    label_df_name = path_ext_remove(basename(label_dict))
    if(label_df_name %in% names(rtn)) {
      data_labels = rtn[[label_df_name]]
      rtn[[label_df_name]] = NULL
    } else {
      if(!file.exists(label_dict)) label_dict = path(path, label_dict)
      assert_file_exists(label_dict)
      data_labels = read_fun(label_dict)
    }
    
    rtn = map(rtn, ~apply_label_lookup(.x, data_labels))
  }
  if(identical(datetime_extraction, "guess")){
    datetime_extraction = get_folder_datetime(path, verbose=verbose)
    rtn$datetime_extraction = datetime_extraction
    rtn$date_extraction = format_ymd(datetime_extraction)
  }
  rtn
}

apply_label_lookup = function(data, data_labels, name_from="name", label_from="label"){
  data_labels = as.data.frame(data_labels) %>%
    select(all_of(c(name_from, label_from))) %>%
    deframe()
  data %>% mutate(across(everything(), ~{
    label = unname(data_labels[cur_column()])
    attr(.x, "label") = label
    .x
  }))
}


