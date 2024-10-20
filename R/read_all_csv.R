

#' Read all `.csv` files in a directory
#' 
#' Read all `.csv` files in a directory, with labels if specified.
#'
#' @param path \[`character(1)`]\cr path to the directory containing `.csv` files.
#' @param ... unused
#' @param read_fun \[`function`]\cr a function to read the files in path, e.g. `read.csv()`, `read.csv2()`,...
#' @param label_dict \[`misc`]\cr list of path to file containing the labels.
#' @param clean_names_fun \[`function`]\cr a function to clean column names, e.g. [tolower], [janitor::clean_names()],...
#' @param datetime_extraction \[`dateish(1)`]\cr the datetime of database extraction (database lock). If "guess", the datetime will be inferred from the files modification time.
#' @param verbose \[`numeric(1)`]\cr the level of verbosity
#'
#' @export
#' 
#' @importFrom utils read.csv2
#' @importFrom fs path_ext_remove
read_all_csv = function(path, ..., 
                        label_dict=NULL,
                        clean_names_fun=NULL, 
                        read_fun="guess", 
                        datetime_extraction="guess", 
                        verbose=getOption("edc_read_verbose", 1)){
  
  check_dots_empty()
  files = dir_ls(path, regexp="\\.csv")
  clean_names_fun = .get_clean_names_fun(clean_names_fun)
  if(identical(datetime_extraction, "guess")){
    datetime_extraction = get_folder_datetime(path, verbose=verbose)
  }  
  if(identical(read_fun, "guess")){
    read_fun = guess_read_function(files[1])
  }
  assert_class(datetime_extraction, c("POSIXt", "Date"))
  assert_class(read_fun, c("function"))
  
  rtn = files %>% 
    .read_all(read_fun, clean_names_fun=clean_names_fun) %>% 
    .add_labels(label_dict, path, read_fun) %>% 
    .add_lookup_and_date(
      datetime_extraction=datetime_extraction,
      clean_names_fun=.get_clean_names_fun(clean_names_fun), 
      EDCimport_version=packageVersion("EDCimport")
    )
  
  .set_lookup(rtn$.lookup)
  
  rtn
}


#' @noRd
#' @keywords internal
.add_labels = function(datalist, label_dict, path, read_fun){
  if(!is.null(label_dict)){
    label_df_name = path_ext_remove(basename(label_dict))
    if(label_df_name %in% names(datalist)) {
      data_labels = datalist[[label_df_name]]
      datalist[[label_df_name]] = NULL
    } else {
      if(!file.exists(label_dict)) label_dict = path(path, label_dict)
      assert_file_exists(label_dict)
      data_labels = read_fun(label_dict)
    }
    
    datalist = map(datalist, ~.apply_label_lookup(.x, data_labels))
  }
  datalist
}


#' @noRd
#' @keywords internal
.apply_label_lookup = function(data, data_labels, name_from="name", label_from="label"){
  data_labels = as.data.frame(data_labels) %>%
    select(all_of(c(name_from, label_from))) %>%
    deframe()
  data %>% mutate(across(everything(), ~{
    label = unname(data_labels[cur_column()])
    attr(.x, "label") = label
    .x
  }))
}
