

#' Read all `.csv` files in a directory
#' 
#' Read all `.csv` files in a directory, with labels if specified.
#'
#' @param path \[`character(1)`]\cr path to the directory containing `.csv` files.
#' @param ... unused
#' @param read_fun \[`function`]\cr a function to read the files in path, e.g. `read.csv()`, `read.csv2()`,...
#' @param label_dict \[`character(1)`]\cr path to file containing the labels.
#' @param clean_names_fun \[`function`]\cr a function to clean column names, e.g. [tolower], [janitor::clean_names()],...
#' @param datetime_extraction \[`dateish(1)`]\cr the datetime of database extraction (database lock). If "guess", the datetime will be inferred from the files modification time.
#' @param verbose \[`numeric(1)`]\cr the level of verbosity
#'
#' @export
#' 
#' @importFrom utils read.csv2
#' @importFrom fs path_ext_remove
read_all_csv = function(path, ..., 
                        read_fun=utils::read.csv2, 
                        label_dict=NULL,
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


