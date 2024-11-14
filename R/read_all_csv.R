

#' Read all `.csv` files in a directory
#' 
#' Read all `.csv` files in a directory, with labels if specified.
#'
#' @param path \[`character(1)`]\cr path to the directory containing `.csv` files.
#' @param ... unused
#' @param read_fun \[`function`]\cr a function to read the files in path, e.g. `read.csv()`, `read.csv2()`,...
#' @param labels_from \[`misc`]\cr list of path to file containing the labels.
#' @param clean_names_fun \[`function`]\cr a function to clean column names, e.g. [tolower], [janitor::clean_names()],...
#' @param datetime_extraction \[`dateish(1)`]\cr the datetime of database extraction (database lock). If "guess", the datetime will be inferred from the files modification time.
#' @param verbose \[`numeric(1)`]\cr the level of verbosity
#'
#' @section Labels file: 
#' `labels_from` should contain the information about column labels. It should be a data file (`.csv`) containing 2 columns: one for the column name and the other for its associated label. Use `options(edc_col_name="xxx", edc_col_label="xxx")` to specify the names of the columns.
#'  
#'  
#' @return a list containing one dataframe for each `.csv` file in the folder, the extraction date (`datetime_extraction`), and a summary of all imported tables (`.lookup`).
#' 
#' @export
#' @importFrom fs dir_ls is_dir
#' @importFrom rlang check_dots_empty
#' @importFrom utils packageVersion
read_all_csv = function(path, ..., 
                        labels_from=NULL,
                        clean_names_fun=NULL, 
                        read_fun="guess", 
                        datetime_extraction="guess", 
                        verbose=getOption("edc_read_verbose", 1)){
  check_dots_empty()
  reset_manual_correction()
  assert(is_dir(path))
  
  if(identical(datetime_extraction, "guess") || is.null(datetime_extraction)){
    datetime_extraction = get_folder_datetime(path, verbose=verbose)
  }  
  assert_class(datetime_extraction, c("POSIXt", "Date"))
  
  if(identical(read_fun, "guess")){
    files = dir_ls(path, regexp="\\.csv")
    read_fun = guess_read_function(files[1])
  }
  assert_class(read_fun, c("function"))

  clean_names_fun = .get_clean_names_fun(clean_names_fun)
  rtn = dir_ls(path, regexp="\\.csv") %>% 
    .read_all(read_fun, clean_names_fun=clean_names_fun) %>% 
    .add_labels(labels_file=labels_from, path, read_fun) %>% 
    .add_lookup_and_date(
      datetime_extraction=datetime_extraction,
      clean_names_fun=clean_names_fun, 
      EDCimport_version=packageVersion("EDCimport")
    )
  
  .set_lookup(rtn$.lookup)
  
  rtn
}


#' @noRd
#' @keywords internal
#' @importFrom fs path path_ext_remove
#' @importFrom purrr map
.add_labels = function(datalist, labels_file, path, read_fun){
  if(!is.null(labels_file)){
    label_df_name = path_ext_remove(basename(labels_file))
    if(label_df_name %in% names(datalist)) {
      data_labels = datalist[[label_df_name]]
      datalist[[label_df_name]] = NULL
    } else {
      if(!file.exists(labels_file)) labels_file = path(path, labels_file)
      assert_file_exists(labels_file)
      data_labels = read_fun(labels_file)
    }
    
    datalist = map(datalist, ~.apply_label_lookup(.x, data_labels))
  }
  datalist
}


#' @noRd
#' @keywords internal
#' @importFrom dplyr across all_of cur_column everything mutate pull select
.apply_label_lookup = function(data, data_labels, name_from="name", label_from="label"){
  name_from  = getOption("edc_col_name", default=name_from)
  label_from = getOption("edc_col_label", default=label_from)
  assert_class(data_labels, "data.frame")
  label_vector = as.data.frame(data_labels) %>%
    select(name=all_of(name_from), label=all_of(label_from)) %>%
    pull(label, name=name)
  data %>% mutate(across(everything(), ~{
    label = unname(label_vector[cur_column()])
    attr(.x, "label") = label
    .x
  }))
}
